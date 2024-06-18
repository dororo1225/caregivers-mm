library(tidyverse)
library(here)
library(car)
library(parameters)
library(modelbased)

par_remove <- str_c("participant_test_", c("04", "16", "26", "28", "39"))

read_csv(here("Data", "ParticipantInformation.csv"), col_types = "dcdcdccdddddddddddddd_") %>% 
  filter(!participant_name %in% par_remove) %>% 
  mutate(Nonattuned_comment = mind_comment - app_comment,
         MM = app_comment / Total_comment * 100,
         PropNonattuned = Nonattuned_comment / Total_comment * 100) -> d_participant 

read_csv(here("Data", "united2.csv"), col_types = "cccccdddddcdddldll") %>% 
  mutate(across(where(is.logical), ~as.numeric(.x))) %>%
  group_by(participant_name) %>%
  summarise(Total_image = n(),
            Gaze_image = sum(gaze, na.rm = TRUE),
            Face_image = sum(FaceInView, na.rm = TRUE),
            Both_image = sum(gaze == 1 & FaceInView == 1),
            FaceLooking = sum(FaceLooking, na.rm = TRUE)) -> d_gaze

d_participant %>% 
  select(participant_name, Month, Total_comment, app_comment, Nonattuned_comment, MM, PropNonattuned) %>% 
  left_join(d_gaze, by = "participant_name") -> df

# Comments
## Descriptive statistics of mothers' comments (Counts)
df %>%
  pivot_longer(ends_with("comment"), names_to = "Comment", values_to = "Count") %>%
  mutate(Comment = case_when(Comment == "app_comment" ~ "Appropriate",
                             Comment == "Nonattuned_comment" ~ "Nonattuned",
                             Comment == "Total_comment" ~ "Total")) %>% 
  group_by(Comment) %>% 
  summarise(Mean = mean(Count),
            SD = sd(Count),
            Max = max(Count),
            Min = min(Count)) 

## Descriptive statistics of mothers' mind-relatted comments (Proportion)
df %>% 
  select(participant_name, MM, PropNonattuned) %>% 
  pivot_longer(!participant_name, names_to = "comment", values_to = "Prop") %>% 
  group_by(comment) %>% 
  summarise(Mean = mean(Prop),
            SD = sd(Prop),
            Max = max(Prop),
            Min = min(Prop)) 

# Gaze
# manual coding data (Prop)
df %>%
  select(participant_name, Total_image, Gaze_image, Face_image, Both_image) %>%
  mutate(Prop_Gaze_image = Gaze_image/Total_image,
         Prop_Face_image = Face_image/Total_image,
         Prop_Both_image = Both_image/Total_image) %>%
  pivot_longer(starts_with("Prop"), names_to = "Image", values_to = "Count", names_prefix = "Prop_") %>%
  mutate(Image = str_replace_all(Image, pattern = "_image", replacement = "")) %>%
  group_by(Image) %>%
  summarise(Mean = mean(Count),
            SD = sd(Count),
            Median = median(Count),
            Max = max(Count),
            Min = min(Count))

# manual coding data (Prop_FaceLooking)
df %>%
  select(participant_name, Total_image, Gaze_image, Face_image, Both_image, FaceLooking) %>% 
  mutate(Prop_FaceLook1 = FaceLooking/Total_image,
         Prop_FaceLook2 = FaceLooking/Both_image) %>% 
  pivot_longer(starts_with("Prop"), names_to = "Measure",  names_prefix = "Prop_") %>% 
  group_by(Measure) %>% 
  summarise(Mean = mean(value),
            SD = sd(value),
            Max = max(value),
            Min = min(value)) 

# GLM
fit_MM <- glm(cbind(FaceLooking, Both_image - FaceLooking) ~ MM, data = df, family = "binomial")
Anova(fit_MM)
summary(fit_MM)
model_parameters(fit_MM, standardize = "refit")

tibble(MM = seq(min(df$MM), max(df$MM), length = 100)) %>%
  mutate(estimate_expectation(fit_MM, data = .)) -> df_predict_MM

df %>% 
  mutate(Prop_FaceLook = FaceLooking/Both_image) %>% 
  ggplot(aes(x = MM)) +
  geom_point(aes(y = Prop_FaceLook, size = Both_image), alpha = 0.5) +
  geom_ribbon(data = df_predict_MM, aes(ymax = CI_high, ymin = CI_low), alpha = 0.25, fill = "#F8766D") +
  geom_line(data = df_predict_MM, aes(y = Predicted), lwd = 1, color = "#F8766D") +
  labs(x = "Proportion of appropriate mind-related comments (%)", y = "Proportion of face looking") +
  guides(size = "none") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        plot.tag = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)) 
ggsave(here("Figure", "Figure2.jpg"), dpi = 300, width = 7.5, height = 6)