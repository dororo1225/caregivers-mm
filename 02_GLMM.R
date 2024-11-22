library(tidyverse)
library(lubridate)
library(here)
library(lme4)
library(car)
library(broom.mixed)

w_size <- c(2, 2.5, 3)

read_csv(here("Data", "comments.csv"), col_types = "ddcd") -> d_comment

read_csv(here("Data", "gaze.csv"), col_types = "cccdddddlll") %>% 
  select(participant_name, image_name, frame_id, ends_with("timestamp"), gaze, FaceInView, FaceLooking, MM) %>% 
  mutate(across(where(is.logical), ~as.numeric(.))) -> d_gaze

d_gaze %>% 
  select(participant_name, MM) %>% 
  distinct() -> d_participant

for (j in 1:length(w_size)){
  w_size_tmp <- w_size[j]
  
  tibble(N_window = numeric(),
         Prop_NA = numeric(),
         Prop_FaceLooking = numeric()) -> d_mmtime
  
  for (i in 1:nrow(d_comment)){
    d_comment %>% 
      filter(id == i) -> d_target 
    d_gaze %>% 
      filter(participant_name == d_target$participant_name) %>% 
      mutate(diff_time = movie_timestamp - d_target$comment_timestamp) %>% 
      filter(diff_time > - w_size_tmp * 1000 * 1000 & diff_time < w_size_tmp * 1000 * 1000) %>% # 発話時刻との時差が閾値以下の画像を抽出
      summarise(N_window = n(), # 時間窓内にある画像枚数
                Prop_NA = sum(FaceInView == 0 | gaze ==0)/N_window, # 顔または視線が検出されていない割合
                Prop_FaceLooking = if_else(Prop_NA == 1, NA_real_, sum(FaceLooking, na.rm = TRUE)/N_window)) -> d_tmp
    bind_rows(d_mmtime, d_tmp) -> d_mmtime
  }
  
  filename <- str_c("wsize_", w_size_tmp, "s.csv")
  
  bind_cols(d_comment, d_mmtime) %>% 
    mutate(w_size_set = w_size_tmp,
           category = if_else(category == 1, "Appropriate", "the Others")) %>% 
    write_csv(here("Result", filename))
}


list.files("Result", pattern = "s.csv", recursive = TRUE) %>%
  here("Result", .) %>% 
  map_df(~read_csv(.x, col_types = "ccdcdcddddd")) -> df_mm

df_mm %>% 
  group_by(w_size_set, participant_name, category) %>%
  summarise(Comment_count = n(),
            NA_count = sum(is.na(Prop_FaceLooking)),
            Cooccurrence_count = if_else(Comment_count == NA_count, NA_integer_, sum(Prop_FaceLooking > 0, na.rm = TRUE)),
            Prop_Cooccurrence = Cooccurrence_count / Comment_count,
            .groups = "drop") -> df_cooccurrence

# time window = 5 sec
df_cooccurrence %>%
  filter(w_size_set ==  2.5) %>% 
  filter(!is.na(Prop_Cooccurrence)) %>% 
  left_join(d_participant, by = "participant_name") -> df

fit <- glmer(cbind(Cooccurrence_count, Comment_count - Cooccurrence_count) ~ category + (1 |participant_name),
             data = df, family = "binomial")
Anova(fit)
summary(fit)
tidy(fit)

df %>%
  mutate(y_pred = predict(fit, re.form = NA, type = "response"),
         category = if_else(category == "the Others", "others", "approppriate")) %>% 
  ggplot(aes(x = category, y = Prop_Cooccurrence)) +
  geom_point(size = 2, alpha = 0.75) +
  geom_line(aes(group = participant_name), lwd = 1, alpha = 0.5) +
  geom_line(aes(group = participant_name, y = y_pred), lwd = 1.5, color = "red") +
  labs(x = "Comment type", y = "Proportion of comments with\nmaternal looking at infants' faces") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12))
ggsave(here("Figure", "Figure3.jpg"), dpi = 300, width = 5, height = 4.5)

# check robustness of the result
df_cooccurrence %>%
  mutate(w_size_set = w_size_set *2) %>% 
  filter(!is.na(Prop_Cooccurrence)) %>% 
  filter(!is.na(category)) %>% 
  left_join(d_participant, by = "participant_name") %>% 
  mutate(category = if_else(category == "Appropriate", "appropriate", "others")) %>%
  group_by(w_size_set) %>% 
  nest() %>% 
  mutate(fit = map(data, ~glmer(cbind(Cooccurrence_count, Comment_count - Cooccurrence_count) ~ category + (1 |participant_name),
                                data = ., family = "binomial")),
         y_pred = map(fit, ~predict(., re.form = NA, type = "response"))) %>% 
  ungroup() %>% 
  unnest(c("data", "y_pred")) %>%
  filter(w_size_set >= 4) %>% 
  mutate(w_size_set = str_c(w_size_set, " sec")) %>% 
  ggplot(aes(x = category, y = Prop_Cooccurrence)) +
  geom_point(size = 2, alpha = 0.75) +
  geom_line(aes(group = participant_name), lwd = 1, alpha = 0.5) +
  geom_line(aes(group = participant_name, y = y_pred), lwd = 1.5, color = "red") +
  facet_wrap(~w_size_set) +
  labs(x = "Comment type", y = "Proportion of comments with\nmaternal looking at infants' faces") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12))
ggsave(here("Figure", "FigureS1.jpg"), dpi = 300, width = 9, height = 5)

df_cooccurrence %>% 
  filter(!is.na(Prop_Cooccurrence)) %>% 
  left_join(d_participant, by = "participant_name") %>% 
  group_by(w_size_set) %>% 
  nest() %>% 
  mutate(fit = map(data, ~glmer(cbind(Cooccurrence_count, Comment_count - Cooccurrence_count) ~ category + (1 |participant_name),
                                data = ., family = "binomial")),
         LRT = map(fit, ~Anova(.))) %>% 
  unnest(cols = "LRT") %>% 
  ungroup() %>% 
  mutate(Effect = rep(c("category"), times = length(w_size)),
         significant = if_else(`Pr(>Chisq)` < 0.05, 1, 0)) %>% 
  select(w_size_set, Effect, Chisq, Df, `Pr(>Chisq)`, significant) 

