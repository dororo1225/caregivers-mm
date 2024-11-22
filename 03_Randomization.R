library(tidyverse)
library(here)
library(lme4)
library(car)
library(broom.mixed)

par_remove <- str_c("participant_test_", c("04", "16", "26", "28", "39"))

read_csv(here("Data", "comments.csv"), col_types = "ddcd") -> d_comment

read_csv(here("Data", "gaze.csv"), col_types = "cccdddddlll") %>% 
  select(participant_name, image_name, frame_id, ends_with("timestamp"), gaze, FaceInView, FaceLooking, MM) %>% 
  filter(!participant_name %in% par_remove) %>% 
  mutate(across(where(is.logical), ~as.numeric(.))) -> d_gaze

d_gaze %>% 
  select(participant_name, MM) %>% 
  distinct() -> d_participant

# Randomization (requires long time)
w_size_tmp <- 2.5

df_effect_rnd <- tibble(effect = character(),
                        group = character(),
                        term = character(),
                        estimate = numeric(),
                        std.error = numeric(),
                        statistic = numeric(),
                        p.value = numeric(),
                        Repeat = numeric())

set.seed(12345)
for (j in 1:1000){
  print(j)
  d_gaze %>% 
    arrange(participant_name, frame_id) %>% 
    select(participant_name, gaze, FaceInView, FaceLooking) %>% 
    group_by(participant_name) %>% 
    nest() %>% 
    mutate(N_row = map_dbl(data, ~nrow(.x)),
           data_rnd = map2(data, N_row, ~sample_n(.x, size = .y)),
           data_rnd2 = map(data_rnd, ~rename(.x,
                                             gaze_rnd = "gaze",
                                             FaceInView_rnd = "FaceInView",
                                             FaceLooking_rnd = "FaceLooking"))) %>% 
    unnest(c(data, data_rnd2)) %>% 
    select(!c(N_row, data_rnd)) %>% 
    ungroup() %>% 
    bind_cols(arrange(d_gaze, participant_name, frame_id) %>% select(frame_id:gaze_timestamp)) %>% 
    relocate(frame_id:gaze_timestamp, .after = "participant_name") -> d_gaze_rnd
  
  tibble(N_window = numeric(),
         Prop_NA = numeric(),
         Prop_FaceLooking = numeric()) -> d_mmtime
  
  for (i in 1:nrow(d_comment)){
    d_comment %>% 
      filter(id == i) -> d_target 
    d_gaze_rnd %>% 
      filter(participant_name == d_target$participant_name) %>% 
      mutate(diff_time = movie_timestamp - d_target$comment_timestamp) %>% 
      filter(diff_time > - w_size_tmp * 1000 * 1000 & diff_time < w_size_tmp * 1000 * 1000) %>% 
      summarise(N_window = n(), 
                Prop_NA = sum(FaceInView_rnd == 0 | gaze_rnd ==0)/N_window, 
                Prop_FaceLooking = if_else(Prop_NA == 1, NA_real_, sum(FaceLooking_rnd, na.rm = TRUE)/N_window)) -> d_tmp
    bind_rows(d_mmtime, d_tmp) -> d_mmtime
  }
  
  bind_cols(d_comment, d_mmtime) %>% 
    mutate(w_size_set = w_size_tmp,
           category = if_else(category == 1, "Appropriate", "the Others")) %>% 
    left_join(d_participant, by = "participant_name") %>% 
    group_by(participant_name, category) %>%
    summarise(Comment_count = n(),
              NA_count = sum(is.na(Prop_FaceLooking)),
              Cooccurrence_count = if_else(Comment_count == NA_count, NA_integer_, sum(Prop_FaceLooking > 0, na.rm = TRUE)),
              Prop_Cooccurrence = Cooccurrence_count / Comment_count,
              .groups = "drop") -> d_cooccurence_rnd 
  fit_rnd <- glmer(cbind(Cooccurrence_count, Comment_count - Cooccurrence_count) ~ category + (1 |participant_name),
                   data = d_cooccurence_rnd, family = "binomial")
  tidy(fit_rnd) %>% 
    mutate(Repeat = j) -> d_effect_rnd
  
  df_effect_rnd <- bind_rows(df_effect_rnd, d_effect_rnd)
} 

write_csv(df_effect_rnd, here("Randomization", "df_effect_rnd_2.5s.csv")) 

# Visualization
here("Result", "wsize_2.5s.csv") %>% 
  read_csv(col_types = "ccdcdcddddd") %>%
  group_by(w_size_set, participant_name, category) %>%
  summarise(Comment_count = n(),
            NA_count = sum(is.na(Prop_FaceLooking)),
            Cooccurrence_count = if_else(Comment_count == NA_count, NA_integer_, sum(Prop_FaceLooking > 0, na.rm = TRUE)),
            Prop_Cooccurrence = Cooccurrence_count / Comment_count,
            .groups = "drop") %>% 
  glmer(cbind(Cooccurrence_count, Comment_count - Cooccurrence_count) ~ category + (1 |participant_name),
        data = ., family = "binomial") %>%
  tidy() %>% 
  filter(term == "categorythe Others") %>% 
  pull(estimate) -> effect_obs

df_effect_rnd <- read_csv(here("Randomization", "df_effect_rnd_2.5s.csv"), col_types = "cccdddd")

df_effect_rnd %>% 
  filter(term == "categorythe Others") %>% 
  summarise(N = max(Repeat),
            Mean = mean(estimate),
            Median = median(estimate),
            lwr = quantile(estimate, probs = 0.025),
            upr = quantile(estimate, probs = 0.975)) %>% 
  mutate(effect_obs = effect_obs) -> df_rnd_sum

df_rnd_sum 

df_effect_rnd %>% 
  filter(term == "categorythe Others") %>% 
  ggplot(aes(x = estimate)) +
  geom_histogram(binwidth = 0.02, fill = "white", color = "black") +
  geom_vline(xintercept = df_rnd_sum$lwr, lty = 2) +
  geom_vline(xintercept = df_rnd_sum$upr, lty = 2) +
  geom_vline(xintercept = effect_obs, lty = 1, color = "red", lwd = 2) +
  labs(x = "Effect size", y = "Count") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12))
ggsave(here("Figure", "FigureS2.jpg"), dpi = 300, width = 5, height = 5)
