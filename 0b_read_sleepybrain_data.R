pacman::p_load(dplyr, readr, tidyr, stringr)
#### Read data ####

sleepybrain_data = list()

sleepbrain_key <- read_csv("data/sleepybrain/sleepybrain_key.csv") %>% 
  select("id", "Sl_cond", "Sex", "AgeGroup","MenstrualPhaseSession1","MenstrualPhaseSession2", "Contraceptives") %>% 
  pivot_longer(cols = starts_with("MenstrualPhase"), names_to = "Session", values_to = "Menstrual_phase") %>% 
  mutate(session = str_replace(Session,"MenstrualPhase", ""),
         session = str_replace(session,"Session1", "ses-1"),
         session = str_replace(session,"Session2", "ses-2"),
         sleep_restricted = if_else(Sl_cond == 1 & session == "ses-1", 1, 
                                    if_else(Sl_cond == 2 & session == "ses-2", 1, 0))) %>% 
  select(id, session, sleep_restricted, Sex, AgeGroup, Menstrual_phase, Contraceptives) %>% 
  rename_all(tolower)

sleepybrain_data$pvt <-  read_csv("data/sleepybrain/sleepybrain_pvt.csv") %>% 
  mutate(id_session = str_replace(file_id,"_task-PVT_events.tsv",""),
         onset = as.numeric(str_replace(onset,"\r",""))) %>% 
  separate(id_session, into = c("id", "session"), sep = "_") %>% 
  select(id, session, onset, response_time) %>% 
  right_join(sleepbrain_key, by=c("id", "session")) %>% 
  select(id, session,sleep_restricted, onset,response_time,sex, agegroup, contraceptives, menstrual_phase) %>% 
  filter(agegroup =="Young") %>% 
  mutate(female = as.factor(if_else(sex == "Female", 1, 0)),
         reaction_time = response_time*2000,
         lapse = if_else(reaction_time > 1000, 1, 0),
         reaction_time=replace(reaction_time, reaction_time>3000, NA),
         reaction_time=replace(reaction_time, reaction_time<100, NA),
         id = as.factor(id),
         female = as.factor(female),
         reaction_time_centeredandscaled = scale(response_time),
         reaction_time_centeredandscaled_1sdlapse = if_else(reaction_time_centeredandscaled > 1, 1, 0),
         reaction_time_centeredandscaled = replace(reaction_time_centeredandscaled, reaction_time_centeredandscaled > 5, NA),
         reaction_time_centeredandscaled = replace(reaction_time_centeredandscaled, reaction_time_centeredandscaled < -1.5, NA),
         menstrual_phase = replace(menstrual_phase, menstrual_phase == "Menstrual", "Follicular"),
         menstrual_condition = if_else(menstrual_phase == "Male", NA_character_, if_else(contraceptives == "n/a", menstrual_phase, if_else(contraceptives == "No", menstrual_phase, "Contraceptive"))))

sleepybrain_data$wm <-  read_csv("data/sleepybrain/sleepybrain_wm.csv") %>% 
  mutate(id_session = str_replace(file_id,"_task-workingmemorytest_events.tsv","")) %>% 
  separate(id_session, into = c("id", "session"), sep = "_") %>% 
  select(id, session, onset, response_time, order_in_test, correct) %>% 
  right_join(sleepbrain_key, by=c("id", "session")) %>% 
  filter(agegroup =="Young") %>% 
  mutate(mistake = if_else(correct == 1, 0, 1),
         menstrual_phase = replace(menstrual_phase, menstrual_phase == "Menstrual", "Follicular"),
         menstrual_condition = if_else(menstrual_phase == "Male", NA_character_, if_else(contraceptives == "n/a", menstrual_phase, if_else(contraceptives == "No", menstrual_phase, "Contraceptive"))))

x <- read_csv("data/sleepybrain/sleepybrain_session_timing.csv") %>% 
  select(id = "file_id",session = "session_id", "working_mem_KSS_rating", "working_mem_perform_rating","working_mem_effort_rating") %>% 
  mutate(id = str_replace(id,"_sessions.tsv", ""))

sleepybrain_data$ratings <-  read_csv("data/sleepybrain/sleepybrain_key.csv") %>%
  select(id, "pvteffort_ses-1"= "PVTEffort1","pvteffort_ses-2"= "PVTEffort2", "pvtmotivation_ses-1"="PVTMotivation1", "pvtmotivation_ses-2"="PVTMotivation1.1") %>% 
  pivot_longer(cols = starts_with("PVT"), names_to = c(".value", "session"), names_sep = "_") %>% 
  left_join(x, by=c("id", "session")) %>% 
  right_join(sleepbrain_key, by=c("id", "session"))  %>% 
  mutate(menstrual_phase = replace(menstrual_phase, menstrual_phase == "Menstrual", "Follicular"),
         pvteffort = as.numeric(replace(pvteffort,pvteffort == "n/a", NA)),
         pvtmotivation= as.numeric(replace(pvtmotivation,pvtmotivation == "n/a", NA)),
         working_mem_KSS_rating = as.numeric(replace(working_mem_KSS_rating,working_mem_KSS_rating == "n/a", NA)),
         working_mem_perform_rating= as.numeric(replace(working_mem_perform_rating,working_mem_perform_rating == "n/a", NA)),
         working_mem_effort_rating= as.numeric(replace(working_mem_effort_rating,working_mem_effort_rating == "n/a", NA)),
         female = as.factor(if_else(sex == "Female", 1, 0)),
         menstrual_condition = if_else(menstrual_phase == "Male", NA_character_, if_else(contraceptives == "n/a", menstrual_phase, if_else(contraceptives == "No", menstrual_phase, "Contraceptive")))) %>% 
  filter(agegroup=="Young")

save(sleepybrain_data, file="sleepybrain_data.RData")

