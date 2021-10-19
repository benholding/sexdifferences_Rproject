#5. interpreting the results
pacman::p_load(ggplot2, sjPlot, emmeans,tidybayes, dplyr)
load("fitted_models/main_analyses.RData")
load("fitted_models/women_analyses.RData")

##### results and plots of sex analysis ####
tab_model(combined_models$attention$rt,
          combined_models$attention$lapse, file = "tables/table S1. attention_sex_differences.doc")

tab_model(combined_models$wm$mistakes, file = "tables/table S2. wm_sex_differences.doc")

tab_model(combined_models$maths$mistakes,
          combined_models$maths$rt, file = "tables/table S3. maths_sex_differences.doc")

tab_model(combined_models$stm$mistakes, file = "tables/table S4. stm_sex_differences.doc")

tab_model(combined_models$stroop$rt, file = "tables/table S5. stroop_sex_differences.doc")

tab_model(combined_models$ratings$effort,
          combined_models$ratings$performance,
          combined_models$ratings$sleepiness, file = "tables/table S6. ratings_sex_differences.doc")

#plots
combined_plot1 <- plot_model(combined_models$attention$rt, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1)) + theme_sjplot() + ylab("Reaction time z-score") + xlab("Sleep deprivation") + labs(color='Woman') + ggtitle("Attention: RT") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T)
combined_plot2 <- plot_model(combined_models$attention$lapse, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1)) + theme_sjplot() + ylab("Lapses") + xlab("Sleep deprivation") + labs(color='Woman') + ggtitle("Attention: Lapses") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T) 
combined_plot3 <- plot_model(combined_models$wm$mistakes, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1))+ theme_sjplot() + ylab("Mistakes") + xlab("Sleep deprivation") + labs(color='Woman') + ggtitle("W. Memory: Mistakes") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T) 
combined_plot4 <- plot_model(combined_models$maths$mistakes, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1))+ theme_sjplot() + ylab("Mistakes") + xlab("Sleep deprivation") + labs(color='Woman') + ggtitle("Cog. TP: Mistakes") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T) 
combined_plot5 <- plot_model(combined_models$maths$rt, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1)) + theme_sjplot() + ylab("Response speed (ms)") + xlab("Sleep deprivation") + labs(color='Woman') + ggtitle("Cog. TP: RT") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T) 
combined_plot6 <- plot_model(combined_models$stm$mistakes, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1))+ theme_sjplot() + ylab("Mistakes") + xlab("Sleep deprivation") + labs(color='Woman') + ggtitle("E. Memory: Mistakes") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T) 
combined_plot7 <- plot_model(combined_models$stroop$rt, type = "pred", terms = c("incongruent","female","sleep_loss"), dodge = .5, ci.lvl = 0.95) + theme_sjplot() + ylab("Response speed (ms)") + xlab("Incongruent trial") + labs(color='Woman') + ggtitle("Conflict Processing: RT") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T) 
combined_plot8 <- plot_model(combined_models$ratings$effort, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1))+ theme_sjplot() + ylab("Effort rating") + xlab("Sleep deprivation") + labs(color='Woman') + ggtitle("Ratings: Effort") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T) 
combined_plot9 <- plot_model(combined_models$ratings$performance, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1))+ theme_sjplot() + ylab("Performance rating") + xlab("Sleep deprivation") + labs(color='Woman') + ggtitle("Ratings: Performance") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T) 
combined_plot10 <- plot_model(combined_models$ratings$sleepiness, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1)) + theme_sjplot() + ylab("Sleepiness rating") + xlab("Sleep deprivation") + labs(color='Woman') + ggtitle("Ratings: Sleepiness") + scale_color_sjplot(palette = "circus", reverse = T) + scale_fill_sjplot(palette = "circus", reverse = T) 

combined_panel_plot <- ggpubr::ggarrange(combined_plot1,combined_plot2,combined_plot3,combined_plot4,combined_plot5,combined_plot6, combined_plot7, combined_plot8, combined_plot9,combined_plot10, common.legend = T)
ggsave(combined_panel_plot, filename = "plots/Fig 1. sex_differences_plot", device = "png", width = 12, height = 10, units = "in")

##### results and plots of women-only analysis #####

tab_model(women_models$attention$rt,
          women_models$attention$lapse, file = "tables/table S7. attention_women.doc" )

tab_model(women_models$wm$mistakes, file = "tables/table S8. wm_women.doc")

tab_model(women_models$maths$mistakes,
          women_models$maths$rt, file = "tables/table S9. maths_women.doc")

tab_model(women_models$stm$mistakes, file = "tables/table S10. stm_women.doc")

tab_model(women_models$stroop$rt, file = "tables/table S11. stroop_women.doc")

tab_model(women_models$ratings$effort,
          women_models$ratings$performance,
          women_models$ratings$sleepiness, file = "tables/table S12. ratings_women.doc")


women_plot1 <- plot_model(women_models$attention$rt, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1)) + theme_sjplot() + ylab("Reaction time z-score") + xlab("Sleep deprivation") + labs(color='Condition') + ggtitle("Attention: RT")
women_plot2 <- plot_model(women_models$attention$lapse, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1)) + theme_sjplot() + ylab("Lapses") + xlab("Sleep deprivation") + labs(color='Condition') + ggtitle("Attention: Lapses")
women_plot3 <- plot_model(women_models$wm$mistakes, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1))+ theme_sjplot() + ylab("Mistakes") + xlab("Sleep deprivation") + labs(color='Condition') + ggtitle("W. Memory: Mistakes")
women_plot4 <- plot_model(women_models$maths$mistakes, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1))+ theme_sjplot() + ylab("Mistakes") + xlab("Sleep deprivation") + labs(color='Condition') + ggtitle("Cog. TP: Mistakes")
women_plot5 <- plot_model(women_models$maths$rt, type = "int", ci.lvl = 0.95) + scale_x_continuous(breaks = c(0,1)) + theme_sjplot() + ylab("Response speed (ms)") + xlab("Sleep deprivation") + labs(color='Condition') + ggtitle("Cog. TP: RT")
women_plot6 <- plot_model(women_models$stm$mistakes, type = "int", ci.lvl = 0.95) + scale_x_continuous(breaks = c(0,1)) + theme_sjplot() + ylab("Mistakes") + xlab("Sleep deprivation") + labs(color='Condition') + ggtitle("E. Memory: Mistakes")
women_plot7 <- plot_model(women_models$stroop$rt, type = "pred", terms = c("incongruent","menstrual_condition","sleep_loss"), dodge = .5, ci.lvl = 0.95) + theme_sjplot() + ylab("Response speed (ms)") + xlab("Incongruent trial") + labs(color='Condition') + ggtitle("Conflict Processing: RT")
women_plot8 <- plot_model(women_models$ratings$effort, type = "int", ci.lvl = 0.95)+ scale_x_continuous(breaks = c(0,1)) + theme_sjplot() + ylab("Effort rating") + xlab("Sleep deprivation") + labs(color='Condition') + ggtitle("Ratings: Effort")
women_plot9 <- plot_model(women_models$ratings$performance, type = "int", ci.lvl = 0.95) + scale_x_continuous(breaks = c(0,1))+ theme_sjplot() + ylab("Performance rating") + xlab("Sleep deprivation") + labs(color='Condition') + ggtitle("Ratings: Performance")
women_plot10 <- plot_model(women_models$ratings$sleepiness, type = "int", ci.lvl = 0.95) + scale_x_continuous(breaks = c(0,1))+ theme_sjplot() + ylab("Sleepiness rating") + xlab("Sleep deprivation") + labs(color='Condition') + ggtitle("Ratings: Sleepiness")

women_panel_plot <- ggpubr::ggarrange(women_plot1,women_plot2,women_plot3,women_plot4,women_plot5,women_plot6,women_plot7, women_plot8, women_plot9, women_plot10, common.legend = T)
ggsave(women_panel_plot, filename = "plots/Fig 2. women_differences_plot", device = "png", width = 12, height = 10, units = "in")



#women plot 1 - version 2
emmeans(women_models$attention$rt, "menstrual_condition", at = list(sleep_loss = 0)) %>% pairs()
emtrends(women_models$attention$rt, pairwise ~ menstrual_condition, var = "sleep_loss")

Women_plot_1b <- women_models$attention$rt %>%
  emtrends(pairwise ~ menstrual_condition, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  filter(!is.na(menstrual_condition)) %>% 
  ggplot(aes(x = menstrual_condition, y = .value)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip() +
  ylab("Sleep loss effect (Z-score)") +
  labs(title = "Attention: RT") +
  scale_y_continuous(limits = c(0, 0.45))

#women plot 2 - version 2
emmeans(women_models$attention$lapse, "menstrual_condition", at = list(sleep_loss = 0)) %>% pairs()
emtrends(women_models$attention$lapse, pairwise ~ menstrual_condition, var = "sleep_loss")

Women_plot_2b <- women_models$attention$lapse %>%
  emtrends(pairwise ~ menstrual_condition, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  mutate(.value = exp(.value)) %>% 
  filter(!is.na(menstrual_condition)) %>% 
  ggplot(aes(x = menstrual_condition, y = .value)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip() +
  ylab("Sleep loss effect (odds ratio)") +
  labs(title = "Attention: Lapses") +
  scale_y_continuous(limits = c(0, 5))

#women plot 3 - version 2
emmeans(women_models$wm$mistakes, "menstrual_condition", at = list(sleep_loss = 0)) %>% pairs()
emtrends(women_models$wm$mistakes, pairwise ~ menstrual_condition, var = "sleep_loss")

Women_plot_3b <- women_models$wm$mistakes %>%
  emtrends(pairwise ~ menstrual_condition, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  mutate(.value = exp(.value)) %>% 
  filter(!is.na(menstrual_condition)) %>% 
  ggplot(aes(x = menstrual_condition, y = .value)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip() +
  ylab("Sleep loss effect (odds ratio)") +
  labs(title = "W. memory: Mistakes") +
  scale_y_continuous(limits = c(0, 3.5))
  

#women plot 4- version 2
emmeans(women_models$maths$mistakes, "menstrual_condition", at = list(sleep_loss = 0)) %>% pairs()
emtrends(women_models$maths$mistakes, pairwise ~ menstrual_condition, var = "sleep_loss")

Women_plot_4b <- women_models$maths$mistakes %>%
  emtrends(pairwise ~ menstrual_condition, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  mutate(.value = exp(.value)) %>% 
  filter(!is.na(menstrual_condition)) %>% 
  ggplot(aes(x = menstrual_condition, y = .value)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip() +
  ylab("Sleep loss effect (odds ratio)") +
  labs(title = "Cog. TP: Mistakes")+
  scale_y_continuous(limits = c(0, 3.5))

#women plot 5- version 2
emmeans(women_models$maths$rt, "menstrual_condition", at = list(sleep_loss = 0)) %>% pairs()

emtrends(women_models$maths$rt, pairwise ~ menstrual_condition, var = "sleep_loss")

Women_plot_5b <- women_models$maths$rt %>%
  emtrends(pairwise ~ menstrual_condition, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  filter(!is.na(menstrual_condition)) %>% 
  ggplot(aes(x = menstrual_condition, y = .value)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip() +
  ylab("Sleep loss effect (ms)") +
  labs(title = "Cog. TP: RT")

#women plot 6 - version 2
emmeans(women_models$stm$mistakes, "menstrual_condition", at = list(sleep_loss = 0)) %>% pairs()

emtrends(women_models$stm$mistakes, pairwise ~ menstrual_condition, var = "sleep_loss")

Women_plot_6b <- women_models$stm$mistakes %>%
  emtrends(pairwise ~ menstrual_condition, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  mutate(.value = exp(.value)) %>% 
  filter(!is.na(menstrual_condition)) %>% 
  ggplot(aes(x = menstrual_condition, y = .value)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip() +
  ylab("Sleep loss effect (odds ratio)") +
  labs(title = "E. Memory: Mistakes") +
  scale_y_continuous(limits = c(0, 3.5))

#women plot 7 - version 2
emmeans(women_models$stroop$rt, pairwise ~ incongruent |menstrual_condition, at = list(sleep_loss = 0))

emtrends(women_models$stroop$rt, pairwise ~ incongruent |menstrual_condition, var = "sleep_loss")

Women_plot_7b <- women_models$stroop$rt %>%
  emtrends(pairwise ~ menstrual_condition * incongruent, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  filter(!is.na(menstrual_condition)) %>% 
  group_by(menstrual_condition) %>% 
  mutate(value2 = .value - .value[incongruent == "1"]) %>% 
  filter(value2 != 0) %>% 
  ggplot(aes(x = menstrual_condition, y = value2)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip() +
  ylab("Sleep loss effect*incongruent effect (ms)") +
  labs(title = "Conflict Processing: RT")

#women plot 8 - version 2
emmeans(women_models$ratings$effort, "menstrual_condition", at = list(sleep_loss = 0)) %>% pairs()

emtrends(women_models$ratings$effort, pairwise ~ menstrual_condition, var = "sleep_loss")

Women_plot_8b <- women_models$ratings$effort %>%
  emtrends(pairwise ~ menstrual_condition, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  filter(!is.na(menstrual_condition)) %>% 
  ggplot(aes(x = menstrual_condition, y = .value)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip()  +
  ylab("Sleep loss effect ") +
  labs(title = "Ratings: Effort")  +
  scale_y_continuous(limits = c(-1.5, 1.5))

#women plot 9 - version 2
emmeans(omen_models$ratings$performance, "menstrual_condition", at = list(sleep_loss = 0)) %>% pairs()

emtrends(women_models$ratings$performance, pairwise ~ menstrual_condition, var = "sleep_loss")

Women_plot_9b <- women_models$ratings$performance %>%
  emtrends(pairwise ~ menstrual_condition, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  filter(!is.na(menstrual_condition)) %>% 
  ggplot(aes(x = menstrual_condition, y = .value)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip()  +
  ylab("Sleep loss effect ") +
  labs(title = "Ratings: Performance") +
  scale_y_continuous(limits = c(-1.5, 1.5))

#women plot 10 - version 2
emmeans(women_models$ratings$sleepiness, "menstrual_condition", at = list(sleep_loss = 0)) %>% pairs()

emtrends(women_models$ratings$sleepiness, pairwise ~ menstrual_condition, var = "sleep_loss")

Women_plot_10b <- women_models$ratings$sleepiness %>%
  emtrends(pairwise ~ menstrual_condition, var = "sleep_loss") %>%
  gather_emmeans_draws() %>%
  filter(!is.na(menstrual_condition)) %>% 
  ggplot(aes(x = menstrual_condition, y = .value)) +
  stat_halfeye() +
  theme_sjplot()+ 
  theme(axis.title.y = element_blank()) + 
  coord_flip() +
  ylab("Sleep loss effect ") +
  labs(title = "Ratings: Sleepiness")  +
  scale_y_continuous(limits = c(0, 3.5))

women_panel_plotb <- ggpubr::ggarrange(Women_plot_1b,Women_plot_2b,Women_plot_3b,Women_plot_4b,Women_plot_5b,Women_plot_6b,Women_plot_7b, Women_plot_8b, Women_plot_9b, Women_plot_10b, common.legend = T)
ggsave(women_panel_plotb, filename = "plots/Fig 3. menstrualphase_differences_plot.tiff", device = "png", width = 12, height = 10, units = "in")
