#evidence synthesis
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)
library(bayestestR)
library(brms)
library(dplyr)
set.seed(321)

#### importing data
load("nyds_data.RData")
load("slesi_data.RData")
load("sleepybrain_data.RData")
combined_models <- list()

############### ATTENTION
#preparing data
slesi_testdata <- slesi_data$rt %>% 
  mutate(sleep_loss = sd,
         female = factor(woman, levels = c(0,1), labels=c("Man", "Woman")),
         id = as.factor(id),
         reaction_time=reaction_time_centeredandscaled[,1]) %>% 
  select(id, female,sleep_loss, reaction_time,lapse=reaction_time_centeredandscaled_1sdlapse)

nyds_testdata <- nyds_data$rt %>% 
  mutate(sleep_loss = as.numeric(sr)-1,
         female = factor(female, levels = c(0,1), labels=c("Man", "Woman")),
         reaction_time=reaction_time_centeredandscaled[,1]) %>% 
  select(id, female,sleep_loss, reaction_time,lapse = reaction_time_centeredandscaled_1sdlapse) 

sleepybrain_testdata <- sleepybrain_data$pvt %>% 
  mutate(sleep_loss = if_else(sleep_restricted == 1, 0.375, 0),
         female = factor(female, levels = c(0,1), labels=c("Man", "Woman")),
         reaction_time=reaction_time_centeredandscaled[,1])  %>% 
  select(id, female,sleep_loss,reaction_time, lapse = reaction_time_centeredandscaled_1sdlapse)

combined_test_attention <- bind_rows(slesi_testdata, nyds_testdata,sleepybrain_testdata, .id ="study") %>% 
  mutate(study = as.factor(study),
         id = as.factor(id))
#rt
priors1 <- c(set_prior("normal(0,1)", class = "Intercept"),
             set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
            set_prior("normal(0,1)", class = "b", coef = "femaleWoman"),
            set_prior("normal(0,1)", class = "b", coef = "sleep_loss:femaleWoman"))

combined_models$attention$rt <- brm(reaction_time ~ sleep_loss*female + study + (1|id), prior = priors1, combined_test_attention, family = gaussian(), save_all_pars =T,iter = 6000, chains = 8, control = list(adapt_delta = 0.95), sample_prior =T)

#lapse
priors_rt_lapse <- c(set_prior("normal(0,3)", class = "Intercept"),
             set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
             set_prior("normal(0,1)", class = "b", coef = "femaleWoman"),
             set_prior("normal(0,1)", class = "b", coef = "sleep_loss:femaleWoman"))

combined_models$attention$lapse <- brm(lapse ~ sleep_loss*female + study + (1|id), prior = priors_rt_lapse, combined_test_attention, family = bernoulli(), save_all_pars =T, iter = 6000, chains = 8, control = list(adapt_delta = 0.95), sample_prior=T)

###############WORKING MEMORY
#preparing data
slesi_testdata <- slesi_data$wm %>% 
  mutate(sleep_loss = sd,
         female = factor(woman, levels = c(0,1), labels=c("Man", "Woman")),
         id = as.factor(id)) %>% 
  select(id, female,sleep_loss, mistake,response_time)

nyds_testdata <- nyds_data$wm %>% 
  mutate(sleep_loss = as.numeric(sr)-1,
         female = factor(female, levels = c(0,1), labels=c("Man", "Woman"))) %>% 
  select(id, female,sleep_loss, mistake,response_time)

sleepybrain_testdata <- sleepybrain_data$wm %>% 
  mutate(sleep_loss = if_else(sleep_restricted == 1, 0.375, 0),
         female = as.factor(if_else(sex == "Female", 1, 0)),
         female = factor(female, levels = c(0,1), labels=c("Man", "Woman")),
         id = as.factor(id)) %>% 
  select(id, female,sleep_loss, mistake,response_time)

combined_test_wm <- bind_rows(nyds_testdata, slesi_testdata,sleepybrain_testdata, .id ="study") %>% 
  mutate(study = as.factor(study),
         id = as.factor(id))

#mistakes
priors_wm_mistakes <- c(set_prior("normal(0,3)", class = "Intercept"),
                     set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
                     set_prior("normal(0,1)", class = "b", coef = "femaleWoman"),
                     set_prior("normal(0,1)", class = "b", coef = "sleep_loss:femaleWoman"))

combined_models$wm$mistakes <- brm(mistake ~ sleep_loss*female + study + (1|id), prior = priors_wm_mistakes, combined_test_wm, family = bernoulli(), save_all_pars =T, iter = 6000, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T)

############ EPISODIC MEMORY
#preparing data
slesi_testdata <- slesi_data$stm %>% 
  mutate(sleep_loss = sd,
         female = factor(woman, levels = c(0,1), labels=c("Man", "Woman")),
         id = as.factor(id)) %>% 
  select(id, female,sleep_loss, mistake)

nyds_testdata <- nyds_data$stm %>% 
  mutate(sleep_loss = as.numeric(sr)-1,
         female = factor(female, levels = c(0,1), labels=c("Man", "Woman"))) %>% 
  select(id, female,sleep_loss, mistake)

combined_test_stm <- bind_rows(nyds_testdata, slesi_testdata, .id ="study") %>% 
  mutate(study = as.factor(study),
         id = as.factor(id))

#mistakes
priors_stm_mistakes <- c(set_prior("normal(0,3)", class = "Intercept"),
                        set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
                        set_prior("normal(0,1)", class = "b", coef = "femaleWoman"),
                        set_prior("normal(0,1)", class = "b", coef = "sleep_loss:femaleWoman"))

combined_models$stm$mistakes <- brm(mistake ~ sleep_loss*female + study + (1|id), prior = priors_stm_mistakes, combined_test_stm, family = bernoulli(), save_all_pars =T, iter = 6000, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T )

######################## MATHS
#preparing data
slesi_testdata <- slesi_data$math %>% 
  mutate(sleep_loss = sd,
         female = factor(woman, levels = c(0,1), labels=c("Man", "Woman")),
         id = as.factor(id),
         response_time = time_m) %>% 
  select(id, female,sleep_loss, mistake, response_time)

nyds_testdata <- nyds_data$math %>% 
  mutate(sleep_loss = as.numeric(sr)-1,
         mistake = as.integer(mistake),
         response_time = time_m,
         female = factor(female, levels = c(0,1), labels=c("Man", "Woman"))) %>% 
  select(id, female,sleep_loss, mistake,response_time) 

combined_test_math <- bind_rows(slesi_testdata, nyds_testdata, .id ="study")%>% 
  mutate(study = as.factor(study),
         id = as.factor(id))

#mistakes
priors_maths_mistakes <- c(set_prior("normal(0,3)", class = "Intercept"),
                         set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
                         set_prior("normal(0,1)", class = "b", coef = "femaleWoman"),
                         set_prior("normal(0,1)", class = "b", coef = "sleep_loss:femaleWoman")) 

combined_models$maths$mistakes <- brm(mistake ~ sleep_loss*female + study + (1|id), prior = priors_maths_mistakes, combined_test_math, family = bernoulli(), save_all_pars =T, iter = 6000, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T)

#rt
priors_maths_rt <- c(set_prior("normal(5000,500)", class = "Intercept"),
                           set_prior("normal(0,500)", class = "b", coef = "sleep_loss"),
                           set_prior("normal(0,500)", class = "b", coef = "femaleWoman"),
                           set_prior("normal(0,500)", class = "b", coef = "sleep_loss:femaleWoman")) 

combined_models$maths$rt <- brm(response_time ~ sleep_loss*female + study + (1|id), prior = priors_maths_rt, combined_test_math, family = gaussian(), save_all_pars =T, iter = 6000, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T)

###################STROOP
#preparing data
slesi_testdata <- slesi_data$stroop %>% 
  mutate(sleep_loss = sd,
         female = factor(woman, levels = c(0,1), labels=c("Man", "Woman")),
         id = as.factor(id),
         incongruent = if_else(congruent == "Incongruent", 1,0)) %>% 
  select(id, female,sleep_loss, mistake, reaction_time,incongruent)

nyds_testdata <- nyds_data$stroop %>% 
  mutate(sleep_loss = as.numeric(sr)-1,
         mistake = as.integer(mistake),
         incongruent = if_else(congruent == 1, 0,1),
         female = factor(female, levels = c(0,1), labels=c("Man", "Woman")),) %>% 
  select(id, female,sleep_loss, mistake,reaction_time, incongruent)

combined_test_stroop <- bind_rows(slesi_testdata, nyds_testdata, .id ="study") %>% 
  mutate(incongruent = as.factor(incongruent),
         study = as.factor(study),
         id = as.factor(id))
#rt
priors_stroop_rt <- c(set_prior("normal(500,50)", class = "Intercept"),
                     set_prior("normal(0,50)", class = "b", coef = "sleep_loss"),
                     set_prior("normal(0,50)", class = "b", coef = "femaleWoman"),
                     set_prior("normal(0,50)", class = "b", coef = "sleep_loss:femaleWoman")) 

combined_models$stroop$rt <- brm(reaction_time ~ incongruent*sleep_loss*female + study + (1|id), prior = priors_stroop_rt, combined_test_stroop, family = gaussian(), save_all_pars =T, iter = 6000, chains = 8, control = list(adapt_delta = 0.95),sample_prior=T)


#### RATINGS
#preparing data
slesi_rating_data <- slesi_data$ratings %>% 
  mutate(sleep_loss = sd,
         female = factor(woman, levels = c(0,1), labels=c("Man", "Woman")),
         id = as.factor(id)) %>% 
  select(id, female,sleep_loss, test_type, sleepiness = rating1, performance = rating2, effort = rating3)

nyds_rating_data <- nyds_data$ratings %>% 
  mutate(sleep_loss = as.numeric(sr)-1,
         female = factor(female, levels = c(0,1), labels=c("Man", "Woman"))) %>% 
  select(id, female,sleep_loss, test_type, sleepiness = rating1, performance = rating2, effort = rating3) 

sleepybrain_rating_data <- sleepybrain_data$ratings %>% 
  mutate(sleep_loss = if_else(sleep_restricted == 1, 0.375, 0),
         test_type = "W",
         female = factor(female, levels = c(0,1), labels=c("Man", "Woman")))  %>% 
  select(id, female,sleep_loss,sleepiness=working_mem_KSS_rating,performance = working_mem_perform_rating, effort = working_mem_effort_rating, test_type)

combined_test_ratings<- bind_rows(slesi_rating_data, nyds_rating_data,sleepybrain_rating_data, .id ="study") %>% 
  mutate(study = as.factor(study),
         id = as.factor(id))

priors_ratings <- c(set_prior("normal(0,1)", class = "Intercept"),
             set_prior("normal(0,1)", class = "b", coef = "sleep_loss"),
             set_prior("normal(0,1)", class = "b", coef = "femaleWoman"),
             set_prior("normal(0,1)", class = "b", coef = "sleep_loss:femaleWoman"))

#Effort
combined_models$ratings$effort <- brm(effort ~ sleep_loss*female + study + (1|id), prior = priors_ratings, combined_test_ratings, family = gaussian(), save_all_pars =T, iter = 6000, chains = 8, control = list(adapt_delta = 0.95), sample_prior =T)

#PERFORMANCE
combined_models$ratings$performance <- brm(performance ~ sleep_loss*female + study + (1|id), prior = priors_ratings, combined_test_ratings, family = gaussian(), save_all_pars =T, iter = 6000, chains = 8, control = list(adapt_delta = 0.95), sample_prior =T)

#SLEEPINESS
combined_models$ratings$sleepiness <- brm(sleepiness ~ sleep_loss*female + study + (1|id), prior = priors_ratings, combined_test_ratings, family = gaussian(), save_all_pars =T, iter = 6000, chains = 8, control = list(adapt_delta = 0.95), sample_prior =T)

#######SAVE MODELS
save(combined_models, file="fitted_models/main_analyses.RData")

