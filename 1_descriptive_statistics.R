load("nyds_data.RData")
load("slesi_data.RData")
load("sleepybrain_data.RData")

pacman::p_load(dplyr, broom)
#descriptive statistics study 1 - Slesi

slesi_data$ratings %>% distinct(id, .keep_all = T) %>% summarise(
  mean_age = mean(age.x,na.rm=T), #this is the most complete age information
  sd_age = sd(age.x,na.rm=T),
  low_range_age = min(age.x), #says 17, but ppt was actually 18
  high_range_age = max(age.x),
  n_total = n(),
  n_women = sum(woman),
  n_men = n()-n_women
) %>% as.data.frame()

slesi_data$ratings %>% distinct(id, .keep_all = T) %>% group_by(sd_group) %>% summarise( #same as above but grouped by condition
  mean_age = mean(age.x,na.rm=T),
  sd_age = sd(age.x,na.rm=T),
  low_range_age = min(age.x), #says 17, but ppt was actually 18
  high_range_age = max(age.x),
  n_total = n(),
  n_women = sum(woman),
  n_men = n()-n_women
) %>% as.data.frame()

slesi_data$ratings %>% distinct(id, .keep_all = T) %>% count(menstral_phase_estimated)
slesi_data$ratings %>% distinct(id, .keep_all = T) %>% filter(woman ==1) %>%  count(menstrual_condition)

slesi_data$ratings %>% distinct(id, .keep_all = T) %>% filter(woman ==1) %>% group_by(condition_from_key) %>% count(menstrual_condition)

#descriptive statistics study 2 - nyds

nyds_data$ratings %>% distinct(id, .keep_all = T) %>% summarise(
  mean_age = mean(age,na.rm=T), 
  sd_age = sd(age,na.rm=T),
  low_range_age = min(age,na.rm=T), 
  high_range_age = max(age,na.rm=T),
  n_total = n(),
  n_women = sum(as.numeric(female)-1),
  n_men = n()-n_women
) %>% as.data.frame()

nyds_data$ratings %>% distinct(id, .keep_all = T) %>% group_by(sr) %>% summarise( #same as above but grouped by condition
  mean_age = mean(age,na.rm=T),
  sd_age = sd(age,na.rm=T),
  low_range_age = min(age,na.rm=T), 
  high_range_age = max(age,na.rm=T),
  n_total = n(),
  n_women = sum(as.numeric(female)-1),
  n_men = n()-n_women
) %>% as.data.frame()



#descriptive statistics study 3 - sleepybrain

sleepybrain_data$ratings %>% distinct(id, .keep_all = T) %>% summarise(
  mean_age = mean(age,na.rm=T), 
  sd_age = sd(age,na.rm=T),
  low_range_age = min(age,na.rm=T), 
  high_range_age = max(age,na.rm=T),
  n_total = n(),
  n_women = sum(as.numeric(female)-1),
  n_men = n()-n_women
) %>% as.data.frame()

sleepybrain_data$ratings %>% distinct(id, .keep_all = T) %>% count(menstrual_phase)
sleepybrain_data$ratings %>% distinct(id, .keep_all = T) %>% count(menstrual_condition)

sleepybrain_data$ratings %>% distinct(id, .keep_all = T) %>% filter(female == 1) %>%  count(menstrual_condition)
sleepybrain_data$ratings %>% distinct(id, .keep_all = T) %>% filter(female == 1) %>%  group_by(sleep_restricted) %>%  count(menstrual_condition)
