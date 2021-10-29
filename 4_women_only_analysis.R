#menstrual phase effects
library(brms)
library(tidyverse)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)
set.seed(321)


#######importing data
load("slesi_data.RData")
load("sleepybrain_data.RData")
women_models <- list()

############### ATTENTION
#preparing data
slesi_testdata_women <- slesi_data$rt %>% 
  mutate(sleep_loss = sd,
         female = as.factor(woman),
         id = as.factor(id)) %>% 
  select(id, female,sleep_loss, reaction_time=reaction_time_centeredandscaled,lapse=reaction_time_centeredandscaled_1sdlapse, menstrual_condition) %>% 
  filter(female== 1,
         menstrual_condition != "Contraceptive")

sleepybrain_testdata_women <- sleepybrain_data$pvt %>% 
  mutate(sleep_loss = if_else(sleep_restricted == 1, 0.375, 0))  %>% 
  select(id, female,sleep_loss,reaction_time=reaction_time_centeredandscaled, lapse = reaction_time_centeredandscaled_1sdlapse,menstrual_condition) %>% 
  filter(female== 1,
         menstrual_condition != "Contraceptive")

combined_test_attention_women <- bind_rows(slesi_testdata_women, sleepybrain_testdata_women, .id ="study") %>% 
  mutate(study = as.factor(study),
         id = as.factor(id),
         menstrual_condition = factor(menstrual_condition, levels = c("Follicular", "Luteal"))) %>% 
  drop_na(menstrual_condition)

#rt
women_attention_rt_prior <- c(set_prior("normal(0,1)", class = "Intercept"),
             set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
             set_prior("normal(0,1)", class = "b", coef = "menstrual_conditionLuteal"),
             set_prior("normal(0,1)", class = "b", coef = "sleep_loss:menstrual_conditionLuteal"))

women_models$attention$rt <- brm(reaction_time ~ sleep_loss*menstrual_condition + study + (1|id), prior = women_attention_rt_prior, combined_test_attention_women, family = gaussian(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95), sample_prior =T,iter = 6000)

#lapse
women_attention_lapse_prior <- c(set_prior("normal(0,3)", class = "Intercept"),
                                 set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
                                 set_prior("normal(0,1)", class = "b", coef = "menstrual_conditionLuteal"),
                                 set_prior("normal(0,1)", class = "b", coef = "sleep_loss:menstrual_conditionLuteal"))

women_models$attention$lapse<- brm(lapse ~ sleep_loss*menstrual_condition + study + (1|id), prior = women_attention_lapse_prior, combined_test_attention_women, family = bernoulli(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95), sample_prior =T,iter = 6000)

########### WORKING MEMORY
slesi_wm_data_women <- slesi_data$wm %>% 
  mutate(sleep_loss = sd,
         female = as.factor(woman),
         id = as.factor(id)) %>% 
  select(id, female,sleep_loss, mistake,response_time,menstrual_condition)%>% 
  filter(female== 1,
         menstrual_condition != "Contraceptive")

sleepybrain_wm_data_women <- sleepybrain_data$wm %>% 
  mutate(sleep_loss = if_else(sleep_restricted == 1, 0.375, 0),
         female = as.factor(if_else(sex == "Female", 1, 0)),
         id = as.factor(id)) %>% 
  select(id, female,sleep_loss, mistake,response_time, menstrual_condition)%>% 
  filter(female== 1,
         menstrual_condition != "Contraceptive")

women_wm_data <- bind_rows(slesi_wm_data_women, sleepybrain_wm_data_women, .id ="study") %>% 
  mutate(study = as.factor(study),
         id = as.factor(id),
         menstrual_condition = factor(menstrual_condition, levels = c("Follicular", "Luteal"))) %>% 
  drop_na(menstrual_condition)

#mistakes
women_priors_wm_mistakes <- c(set_prior("normal(0,3)", class = "Intercept"),
                              set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
                              set_prior("normal(0,1)", class = "b", coef = "menstrual_conditionLuteal"),
                              set_prior("normal(0,1)", class = "b", coef = "sleep_loss:menstrual_conditionLuteal"))

women_models$wm$mistakes <- brm(mistake ~ sleep_loss*menstrual_condition + study + (1|id), prior = women_priors_wm_mistakes, women_wm_data, family = bernoulli(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T,iter = 6000)

################## EPISODIC MEMORY
women_stm_slesi <- slesi_data$stm %>% 
  filter(woman==1,
         menstrual_condition != "Contraceptive")%>% 
  mutate(sleep_loss = sd,
         id = as.factor(id),
         menstrual_condition = factor(menstrual_condition, levels = c("Follicular", "Luteal"))) %>% 
  select(id, sleep_loss, mistake,menstrual_condition) %>% 
  drop_na(menstrual_condition) 

#mistakes
women_priors_stm_mistakes <- c(set_prior("normal(0,3)", class = "Intercept"),
                               set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
                               set_prior("normal(0,1)", class = "b", coef = "menstrual_conditionLuteal"),
                               set_prior("normal(0,1)", class = "b", coef = "sleep_loss:menstrual_conditionLuteal"))


women_models$stm$mistakes <- brm(mistake ~ sleep_loss*menstrual_condition +  (1|id), prior = women_priors_stm_mistakes, women_stm_slesi, family = bernoulli(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T ,iter = 6000)

######### MATHS
women_maths_slesi <- slesi_data$math %>% 
  filter(woman==1,
         menstrual_condition != "Contraceptive") %>% 
  mutate(sleep_loss = sd,
         id = as.factor(id),
         response_time = time_m,
         menstrual_condition = factor(menstrual_condition, levels = c("Follicular", "Luteal"))) %>% 
  select(id,sleep_loss, mistake,menstrual_condition,response_time) %>% 
  drop_na(menstrual_condition) 
#mistakes
women_priors_maths_mistakes <- c(set_prior("normal(0,3)", class = "Intercept"),
                                 set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
                                 set_prior("normal(0,1)", class = "b", coef = "menstrual_conditionLuteal"),
                                 set_prior("normal(0,1)", class = "b", coef = "sleep_loss:menstrual_conditionLuteal"))


women_models$maths$mistakes <- brm(mistake ~ sleep_loss*menstrual_condition +  (1|id), prior = women_priors_maths_mistakes, women_maths_slesi, family = bernoulli(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T,iter = 6000 )

#rt
women_priors_maths_rt <- c(set_prior("normal(5000,500)", class = "Intercept"),
                     set_prior("normal(0,500)", class = "b", coef = "sleep_loss"),
                     set_prior("normal(0,500)", class = "b", coef = "menstrual_conditionLuteal"),
                     set_prior("normal(0,500)", class = "b", coef = "sleep_loss:menstrual_conditionLuteal")) 
women_models$maths$rt <- brm(response_time ~ sleep_loss*menstrual_condition + (1|id), prior = women_priors_maths_rt, women_maths_slesi, family = gaussian(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T,iter = 6000)

######## STROOP
women_stroop_data <- slesi_data$stroop %>% 
  filter(woman==1,
         menstrual_condition != "Contraceptive") %>% 
  mutate(sleep_loss = sd,
         id = as.factor(id),
         incongruent = as.factor(if_else(congruent == "Incongruent", 1,0)),
         menstrual_condition = factor(menstrual_condition, levels = c("Follicular", "Luteal", "Contraceptive"))) %>% 
  select(id,sleep_loss, mistake, reaction_time,incongruent,menstrual_condition) 

women_priors_stroop_rt <- c(set_prior("normal(500,50)", class = "Intercept"),
                            set_prior("normal(0,50)", class = "b", coef = "sleep_loss"),
                            set_prior("normal(0,50)", class = "b", coef = "menstrual_conditionLuteal"),
                            set_prior("normal(0,50)", class = "b", coef = "sleep_loss:menstrual_conditionLuteal"))  
#rt
women_models$stroop$rt <- brm(reaction_time ~ incongruent*sleep_loss*menstrual_condition + (1|id), prior = women_priors_stroop_rt, women_stroop_data, family = gaussian(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T,iter = 6000)

#### RATINGS
women_slesi_rating_data <- slesi_data$ratings %>% 
  filter(woman==1,
         menstrual_condition != "Contraceptive") %>% 
  mutate(sleep_loss = sd,
         id = as.factor(id),
         menstrual_condition = factor(menstrual_condition, levels = c("Follicular", "Luteal"))) %>% 
  select(id,sleep_loss, test_type, sleepiness = rating1, performance = rating2, effort = rating3,menstrual_condition)%>% 
  drop_na(menstrual_condition) 

women_sleepybrain_rating_data <- sleepybrain_data$ratings %>% 
  filter(woman==1,
         menstrual_condition != "Contraceptive") %>% 
  mutate(sleep_loss = if_else(sleep_restricted == 1, 0.375, 0),
         test_type = "W",
         menstrual_condition = factor(menstrual_condition, levels = c("Follicular", "Luteal")))  %>% 
  select(id,sleep_loss,sleepiness=working_mem_KSS_rating,performance = working_mem_perform_rating, effort = working_mem_effort_rating, test_type, menstrual_condition)%>% 
  drop_na(menstrual_condition) 

women_ratings_data<- bind_rows(women_slesi_rating_data, women_sleepybrain_rating_data, .id ="study") %>% 
  mutate(study = as.factor(study),
         id = as.factor(id))

women_priors_ratings <- c(set_prior("normal(0,1)", class = "Intercept"),
                          set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
                          set_prior("normal(0,1)", class = "b", coef = "menstrual_conditionLuteal"),
                          set_prior("normal(0,1)", class = "b", coef = "sleep_loss:menstrual_conditionLuteal"))

# effort
women_models$ratings$effort <- brm(effort ~ sleep_loss*menstrual_condition + study + (1|id), prior = women_priors_ratings, women_ratings_data, family = gaussian(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95), sample_prior =T,iter = 6000)

# performance
women_models$ratings$performance <- brm(performance ~ sleep_loss*menstrual_condition + study + (1|id), prior = women_priors_ratings, women_ratings_data, family = gaussian(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95), sample_prior =T,iter = 6000)

# sleepiness
women_models$ratings$sleepiness <- brm(sleepiness ~ sleep_loss*menstrual_condition + study + (1|id), prior = women_priors_ratings, women_ratings_data, family = gaussian(), save_all_pars =T, chains = 8, control = list(adapt_delta = 0.95), sample_prior =T,iter = 6000)

#######SAVE MODELS
save(women_models, file="fitted_models/women_analyses.RData")
