library(tidyverse)
library(brms)
library(performance)
devtools::install_github("mvuorre/brmstools")
library(brmstools)

options(mc.cores = parallel::detectCores())

load("nyds_data.RData")
load("slesi_data.RData")
load("sleepybrain_data.RData")

#NYDS 
nyds_models <- list()
#ATTENTION: rt
#prior1 <- c(set_prior("normal(0,100)", class = "b", coef = "sr1"),
#            set_prior("normal(0,100)", class = "b", coef = "female1"),
#            set_prior("normal(0,100)", class = "b", coef = "sr1:female1"))

nyds_models$attention$rt$max <- brm(reaction_time ~ sr*female + (sr|id), nyds_data$rt, save_all_pars =T, family = lognormal()) %>% add_criterion(c("waic","loo"))
nyds_models$attention$rt$female <- update(nyds_models$attention$rt$max, formula = ~ .-sr:female) %>% add_criterion(c("waic","loo"))
nyds_models$attention$rt$sr <- update(nyds_models$attention$rt$female, formula = ~ .-female)%>% add_criterion(c("waic","loo"))
nyds_models$attention$rt$intercept <- update(nyds_models$attention$rt$sr, formula = ~ .-sr)%>% add_criterion(c("waic","loo"))
loo_compare(nyds_models$attention$rt$intercept,nyds_models$attention$rt$max,nyds_models$attention$rt$female,nyds_models$attention$rt$sr)
bayes_factor(nyds_models$attention$rt$intercept,nyds_models$attention$rt$sr)
bayes_factor(nyds_models$attention$rt$sr,nyds_models$attention$rt$max)
map(nyds_models$attention$rt, bayes_R2)

#attention: lapse
nyds_models$attention$lapse$max <- brm(lapse ~ sr*female + (sr|id), nyds_data$rt, family = bernoulli(link = "logit"), save_all_pars =T) %>% add_criterion(c("waic","loo"))
nyds_models$attention$lapse$female <- update(nyds_models$attention$lapse$max, formula = ~ .-sr:female) %>% add_criterion(c("waic","loo"))
nyds_models$attention$lapse$sr<- update(nyds_models$attention$lapse$female, formula = ~ .-female) %>% add_criterion(c("waic","loo"))
nyds_models$attention$lapse$intercept <- update(nyds_models$attention$lapse$sr, formula = ~ .-sr) %>% add_criterion(c("waic","loo"))
loo_compare(nyds_models$attention$lapse$max,nyds_models$attention$lapse$female,nyds_models$attention$lapse$sr,nyds_models$attention$lapse$intercept)
bayes_factor(nyds_models$attention$lapse$intercept,nyds_models$attention$lapse$sr)
bayes_factor(nyds_models$attention$lapse$female,nyds_models$attention$lapse$intercept)
bayes_factor(nyds_models$attention$lapse$max,nyds_models$attention$lapse$intercept)
map(nyds_models$attention$lapse, bayes_R2)

#MATHS: rt
nyds_models$math$rt$max <- brm(time_m ~ sr*female + (sr|id), nyds_data$math, save_all_pars =T, family = lognormal()) %>% add_criterion(c("waic","loo"))
nyds_models$math$rt$female <- update(nyds_models$math$rt$max, formula = ~ .-sr:female, family = lognormal()) %>% add_criterion(c("waic","loo"))
nyds_models$math$rt$sr <- update(nyds_models$math$rt$female, formula = ~ .-female, family = lognormal()) %>% add_criterion(c("waic","loo"))
nyds_models$math$rt$intercept <- update(nyds_models$math$rt$sr, formula = ~ .-sr, family = lognormal()) %>% add_criterion(c("waic","loo"))
loo_compare(nyds_models$math$rt$intercept,nyds_models$math$rt$max,nyds_models$math$rt$female,nyds_models$math$rt$sr)
bayes_factor(nyds_models$math$rt$intercept,nyds_models$math$rt$sr)
map(nyds_models$math$rt, bayes_R2)

#MATHS: mistake
nyds_models$math$mistake$max <- brm(mistake ~ sr*female + (sr|id), nyds_data$math, family = bernoulli(link = "logit"), save_all_pars =T) %>% add_criterion(c("waic","loo"))
nyds_models$math$mistake$female <- update(nyds_models$math$mistake$max, formula = ~ .-sr:female) %>% add_criterion(c("waic","loo"))
nyds_models$math$mistake$sr <- update(nyds_models$math$mistake$female, formula = ~ .-female) %>% add_criterion(c("waic","loo"))
nyds_models$math$mistake$intercept <- update(nyds_models$math$mistake$sr, formula = ~ .-sr) %>% add_criterion(c("waic","loo"))

#EPISODIC MEMORY: mistake
nyds_models$stm$mistake$max <- brm(mistake ~ sr*female + (sr|id), nyds_data$stm, family = bernoulli(link = "logit"), save_all_pars =T) %>% add_criterion(c("waic","loo"))
nyds_models$stm$mistake$female <- update(nyds_models$stm$mistake$max, formula = ~ .-sr:female) %>% add_criterion(c("waic","loo"))
nyds_models$stm$mistake$sr <- update(nyds_models$stm$mistake$female, formula = ~ .-female) %>% add_criterion(c("waic","loo"))
nyds_models$stm$mistake$intercept <- update(nyds_models$stm$mistake$sr, formula = ~ .-sr) %>% add_criterion(c("waic","loo"))
loo_compare(nyds_models$stm$mistake$max,nyds_models$stm$mistake$female,nyds_models$stm$mistake$sr,nyds_models$stm$mistake$intercept)
bayes_factor(nyds_models$stm$mistake$intercept,nyds_models$stm$mistake$sr)

#WORKING MEMORY: rt
nyds_models$wm$rt$max <- brm(response_time ~ sr*female + (1|id), nyds_data$wm, save_all_pars =T, family = lognormal())
nyds_models$wm$rt$female <- brm(response_time ~ sr+female + (1|id), nyds_data$wm, save_all_pars =T, family = lognormal())
nyds_models$wm$rt$sr <- brm(response_time ~ sr + (1|id), nyds_data$wm, save_all_pars =T, family = lognormal())
nyds_models$wm$rt$intercept <- brm(response_time ~ 1 + (1|id), nyds_data$wm, save_all_pars =T, family = lognormal())

#WORKING MEMORY: mistake
nyds_models$wm$mistake$max <- brm(mistake ~ sr*female + (1|id), nyds_data$wm, save_all_pars =T, family = bernoulli(link = "logit"))
nyds_models$wm$mistake$female <- brm(mistake ~ sr+female + (1|id), nyds_data$wm, save_all_pars =T, family = bernoulli(link = "logit"))
nyds_models$wm$mistake$sr <- brm(mistake ~ sr + (1|id), nyds_data$wm, save_all_pars =T, family = bernoulli(link = "logit"))
nyds_models$wm$mistake$intercept <- brm(mistake ~ 1 + (1|id), nyds_data$wm, save_all_pars =T, family = bernoulli(link = "logit"))

#STROOP: rt
nyds_models$stroop$rt$max <- brm(reaction_time ~ congruent*sr*female + (1|id), nyds_data$stroop, save_all_pars =T, family = lognormal())
nyds_models$stroop$rt$female <- brm(reaction_time ~ congruent*sr + female + (1|id), nyds_data$stroop, save_all_pars =T, family = lognormal())
nyds_models$stroop$rt$interaction <- brm(reaction_time ~ congruent*sr + (1|id), nyds_data$stroop, save_all_pars =T, family = lognormal())
nyds_models$stroop$rt$sr <- brm(reaction_time ~ congruent + sr + (1|id), nyds_data$stroop, save_all_pars =T, family = lognormal())
nyds_models$stroop$rt$congruent <- brm(reaction_time ~ congruent + (1|id), nyds_data$stroop, save_all_pars =T, family = lognormal())
nyds_models$stroop$rt$intercept <- brm(reaction_time ~ 1 + (1|id), nyds_data$stroop, save_all_pars =T, family = lognormal())

#STROOP: mistake
nyds_models$stroop$mistake$max <- brm(mistake ~ congruent*sr*female + (1|id), nyds_data$stroop, save_all_pars =T, family = bernoulli(link = "logit"))
nyds_models$stroop$mistake$interaction2 <- brm(mistake ~ congruent*sr + female*sr + (1|id), nyds_data$stroop, save_all_pars =T, family = bernoulli(link = "logit"))
nyds_models$stroop$mistake$female <- brm(mistake ~ congruent*sr + female + (1|id), nyds_data$stroop, save_all_pars =T, family = bernoulli(link = "logit"))
nyds_models$stroop$mistake$interaction <- brm(mistake ~ congruent*sr + (1|id), nyds_data$stroop, save_all_pars =T, family = bernoulli(link = "logit"))
nyds_models$stroop$mistake$sr <- brm(mistake ~ congruent + sr + (1|id), nyds_data$stroop, save_all_pars =T, family = bernoulli(link = "logit"))
nyds_models$stroop$mistake$congruent <- brm(mistake ~ congruent + (1|id), nyds_data$stroop, save_all_pars =T, family = bernoulli(link = "logit"))
nyds_models$stroop$mistake$intercept <- brm(mistake ~ 1 + (1|id), nyds_data$stroop, save_all_pars =T, family = bernoulli(link = "logit"))

#EFFORT
nyds_models$ratings$effort$max <- brm(rating3 ~ sr*test_type*female + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$effort$interaction2 <- brm(rating3 ~ sr*test_type + sr*female + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$effort$female <- brm(rating3 ~ sr*test_type + female + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$effort$interaction <- brm(rating3 ~ sr*test_type + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$effort$sr <- brm(rating3 ~ sr + test_type + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$effort$test_type <- brm(rating3 ~ test_type + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$effort$intercept <- brm(rating3 ~ 1 + (1|id), nyds_data$ratings, save_all_pars =T)
with(nyds_models$ratings$effort,
  compare_performance(test_type,sr,interaction,female,interaction2,max))


#PERFORMANCE
nyds_models$ratings$performance$max <- brm(rating2 ~ sr*test_type*female + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$performance$interaction2 <- brm(rating2 ~ sr*test_type + sr*female + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$performance$female <- brm(rating2 ~ sr*test_type + female + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$performance$interaction <- brm(rating2 ~ sr*test_type + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$performance$sr <- brm(rating2 ~ sr + test_type + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$performance$test_type <- brm(rating2 ~ test_type + (1|id), nyds_data$ratings, save_all_pars =T)
nyds_models$ratings$performance$intercept <- brm(rating2 ~ 1 + (1|id), nyds_data$ratings, save_all_pars =T)
with(nyds_models$ratings$performance,
     compare_performance(test_type,sr,interaction,female,interaction2,max))

forest(nyds_models$attention$rt$max, pars = "sr1",grouping = "id")
