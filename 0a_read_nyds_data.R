library(tidyverse)

nyds_key <- read_csv("data/nyds/nyds_key.csv")
#### Read data ####

nyds_data = list()
nyds_data$math <-  read_csv("data/nyds/math_raw_data.csv") %>% 
  distinct() %>% 
  right_join(nyds_key, by="id") %>% 
  filter(time_m <15000 | time_m >100) %>% 
  mutate(mistake = if_else(correct == 1, 0, 1),
         id = as.factor(id),
         sr = as.factor(sr),
         female = as.factor(female)) %>% 
  filter(!is.na(answer_m), #at the end of every dataframe there was no response (out of time)
         answer_m>9)%>%  #since the questions were always xx + xx i remove all one digit answers
  mutate(id = paste("nyds_", id, sep = ""))

nyds_data$rt <- read_csv("data/nyds/reaction_times_raw_data.csv") %>% 
  distinct() %>% 
  right_join(nyds_key, by="id")%>% 
  mutate(wakeapp_lapse = if_else(reaction_time > 1000, 1, 0),
         wakeapp_reaction_time=replace(reaction_time, reaction_time>3000, NA),
         id = as.factor(id),
         sr = as.factor(sr),
         female = as.factor(female),
         reaction_time_centeredandscaled = scale(reaction_time),
         reaction_time_centeredandscaled_1sdlapse = if_else(reaction_time_centeredandscaled > 1, 1, 0),
         reaction_time_centeredandscaled = replace(reaction_time_centeredandscaled, reaction_time_centeredandscaled > 5, NA),
         reaction_time_centeredandscaled = replace(reaction_time_centeredandscaled, reaction_time_centeredandscaled < -1.5, NA)) %>% 
  mutate(id = paste("nyds_", id, sep = ""))

nyds_data$stm <- read_csv("data/nyds/short_term_memory_raw_data.csv") %>% 
  distinct() %>% 
  right_join(nyds_key, by="id") %>% 
  mutate(mistake = if_else(correct == 1, 0, 1),
         id = as.factor(id),
         sr = as.factor(sr),
         female = as.factor(female))%>% 
  mutate(id = paste("nyds_", id, sep = ""))

nyds_data$wm <- read_csv("data/nyds/working_memory_raw_data.csv") %>% 
  distinct() %>% 
  right_join(nyds_key, by="id") %>% 
  filter(response_time <10 | response_time >0.1) %>% 
  mutate(mistake = if_else(correct == 1, 0, 1),
         id = as.factor(id),
         sr = as.factor(sr),
         female = as.factor(female))%>% 
  mutate(id = paste("nyds_", id, sep = ""))

nyds_data$stroop <- read_csv("data/nyds/stroop_raw_data.csv") %>% 
  distinct() %>% 
  right_join(nyds_key, by="id") %>% 
  filter(reaction_time <4000 | reaction_time >500) %>% 
  mutate(mistake = if_else(correct == 1, 0, 1),
         id = as.factor(id),
         sr = as.factor(sr),
         female = as.factor(female))%>% 
  mutate(id = paste("nyds_", id, sep = ""))

nyds_data$ratings  <- read_csv("data/nyds/ratings_raw_data.csv") %>% 
  distinct() %>% 
  right_join(nyds_key, by="id") %>% 
  mutate(id = as.factor(id),
         sr = as.factor(sr),
         female = as.factor(female),
         test_type = replace(test_type, test_type == "reactionTimeInhibited","reactionTime"))%>% 
  mutate(id = paste("nyds_", id, sep = ""))

save(nyds_data, file="nyds_data.RData")
