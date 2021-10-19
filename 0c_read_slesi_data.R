pacman::p_load(readr, dplyr)

load("data/slesi/wakeapp_datasets.RDta")

slesi_key <- read_csv("data/slesi/slesi_key.csv") %>% 
  select(id, age, woman_from_key = woman, condition_from_key = sd, menstrual_cycle_day_estimated,menstral_phase_estimated,oral_contraceptive,preventivmedal_yes1_no2)

slesi_data <- list()

slesi_data$math <- data$math %>% 
  left_join(slesi_key, by = c("id")) %>% 
  mutate(menstral_phase_estimated = replace(menstral_phase_estimated, woman == 0, "Male"),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Unknown", NA),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Folliucular", "Follicular"),
         menstrual_condition = if_else(!is.na(oral_contraceptive), "Contraceptive", if_else(preventivmedal_yes1_no2 == 1, "Contraceptive", if_else(menstral_phase_estimated == "Male", NA_character_,menstral_phase_estimated))))


slesi_data$rt <- data$rt %>% 
  left_join(slesi_key, by = c("id")) %>% 
  mutate(reaction_time_centeredandscaled = scale(reaction_time),
         reaction_time_centeredandscaled_1sdlapse = if_else(reaction_time_centeredandscaled > 1, 1, 0),
         reaction_time_centeredandscaled = replace(reaction_time_centeredandscaled, reaction_time_centeredandscaled > 5, NA),
         reaction_time_centeredandscaled = replace(reaction_time_centeredandscaled, reaction_time_centeredandscaled < -1.5, NA),
         menstral_phase_estimated = replace(menstral_phase_estimated, woman == 0, "Male"),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Unknown", NA),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Folliucular", "Follicular"),
         menstrual_condition = if_else(!is.na(oral_contraceptive), "Contraceptive", if_else(preventivmedal_yes1_no2 == 1, "Contraceptive", if_else(menstral_phase_estimated == "Male", NA_character_,menstral_phase_estimated)))) 

slesi_data$stm <- data$stm %>% 
  left_join(slesi_key, by = c("id")) %>% 
  mutate(menstral_phase_estimated = replace(menstral_phase_estimated, woman == 0, "Male"),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Unknown", NA),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Folliucular", "Follicular"),
         menstrual_condition = if_else(!is.na(oral_contraceptive), "Contraceptive", if_else(preventivmedal_yes1_no2 == 1, "Contraceptive", if_else(menstral_phase_estimated == "Male", NA_character_,menstral_phase_estimated))))

slesi_data$wm <- data$wm %>% 
  left_join(slesi_key, by = c("id"))%>% 
  mutate(menstral_phase_estimated = replace(menstral_phase_estimated, woman == 0, "Male"),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Unknown", NA),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Folliucular", "Follicular"),
         menstrual_condition = if_else(!is.na(oral_contraceptive), "Contraceptive", if_else(preventivmedal_yes1_no2 == 1, "Contraceptive", if_else(menstral_phase_estimated == "Male", NA_character_,menstral_phase_estimated))))

slesi_data$stroop <- data$stroop %>% 
  left_join(slesi_key, by = c("id"))%>% 
  mutate(menstral_phase_estimated = replace(menstral_phase_estimated, woman == 0, "Male"),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Unknown", NA),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Folliucular", "Follicular"),
         menstrual_condition = if_else(!is.na(oral_contraceptive), "Contraceptive", if_else(preventivmedal_yes1_no2 == 1, "Contraceptive", if_else(menstral_phase_estimated == "Male", NA_character_,menstral_phase_estimated))))

slesi_data$ratings <- data$kss %>% 
  left_join(slesi_key, by = c("id"))%>% 
  mutate(menstral_phase_estimated = replace(menstral_phase_estimated, woman == 0, "Male"),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Unknown", NA),
         menstral_phase_estimated = replace(menstral_phase_estimated, menstral_phase_estimated == "Folliucular", "Follicular"),
         menstrual_condition = if_else(!is.na(oral_contraceptive), "Contraceptive", if_else(preventivmedal_yes1_no2 == 1, "Contraceptive", if_else(menstral_phase_estimated == "Male", NA_character_,menstral_phase_estimated))))

save(slesi_data, file="slesi_data.RData")
