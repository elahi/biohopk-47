library(tidyverse)

#imported files for complete dataset.csv files

#tried transforming data with a different set using log()to try to decrease variance spread (this didn't work because spread is both upper and lower) could try logit log(p/1-p)
M1_data_2$logit_ETR_dif <- log(M1_data_2$ETR_dif_A_FDES/(1-M1_data_2$ETR_dif_A_FDES))
M1_data_2%>%
  ggplot(data = M1_data_2, mapping = aes(x = Type, y = M1_data_2$logit_ETR_dif)) + geom_point() +
  geom_boxplot(data = dat, mapping = aes(x = Type, y = M1_data_2$logit_ETR_dif))  + labs(x = "Type", y = "log(ETR Rate Difference (F-DES))", title = "Site 1 logit")

complete_dataset$rwc <- ((complete_dataset$Wt_DES - complete_dataset$Wt_DRY) / (complete_dataset$Wt_F - complete_dataset$Wt_DRY))
complete_dataset$ETR_dif_A_FDES <- (complete_dataset$ETR_F_A-complete_dataset$ETR_DES_A)
complete_dataset$ETR_dif_max_FDES <- (complete_dataset$ETR_F_max-complete_dataset$ETR_DES_max)
complete_dataset$ETR_dif_A_DESF2 <- (complete_dataset$ETR_DES_A-complete_dataset$ETR_F2_A)
complete_dataset$ETR_dif_max_DESF2 <-(complete_dataset$ETR_DES_max-complete_dataset$ETR_F2_max)


Figure_1 <- lm(ETR_dif_A_FDES~Spp*Site, data = complete_dataset)
summary(Figure_1)

ggplot(data = Figure_1, mapping = aes(x = Spp, y = plot_data_1)) + geom_point() 

anova(Figure_1)

Figure_1_plotting <- complete_dataset %>% 
  group_by(Spp,Site) %>% 
  summarise(mean = mean(ETR_dif_A_FDES), 
            sd = sd(ETR_dif_A_FDES),
            n = n(),
            se = sd(ETR_dif_A_FDES)/sqrt(n)) %>% 
  ungroup() %>%
  mutate(Site = as.factor(Site),
         Spp = ifelse(Spp == "M", "Mastocarpus papillatus", "Porphyra perforata"))

my.dodge <- 0.2 

Figure_1_dat <- Figure_1_plotting %>% ungroup()

ggplot(data = Figure_1_dat, mapping = aes(x = Spp, y = mean, color = Site, shape = Site)) + 
  geom_point(position = position_dodge(my.dodge)) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1, position = position_dodge(my.dodge)) +
  labs(x = "Species", y = "Mean Difference A (Fresh - Desiccated)")


Figure_2 <- lm(ETR_dif_max_FDES~Spp*Site, data = complete_dataset)
summary(Figure_2)

ggplot(data = Figure_2, mapping = aes(x = Spp, y = plot_data_1)) + geom_point() 

anova(Figure_2)

Figure_2_plotting <- complete_dataset %>% 
  group_by(Spp,Site) %>% 
  summarise(mean = mean(ETR_dif_max_FDES), 
            sd = sd(ETR_dif_max_FDES),
            n = n(),
            se = sd(ETR_dif_max_FDES)/sqrt(n)) %>% 
  ungroup() %>%
  mutate(Site = as.factor(Site),
         Spp = ifelse(Spp == "M", "Mastocarpus papillatus", "Porphyra perforata"))

my.dodge <- 0.2 

Figure_2_dat <- Figure_2_plotting %>% ungroup()

ggplot(data = Figure_2_dat, mapping = aes(x = Spp, y = mean, color = Site, shape = Site)) + 
  geom_point(position = position_dodge(my.dodge)) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1, position = position_dodge(my.dodge)) +
  labs(x = "Species", y = "Mean Difference Max (Fresh - Desiccated)")




Figure_3 <- lm(ETR_dif_A_DESF2~Spp*Site, data = complete_dataset)
summary(Figure_3)

ggplot(data = Figure_3, mapping = aes(x = Spp, y = plot_data_1)) + geom_point() 

anova(Figure_3)

Figure_3_plotting <- complete_dataset %>% 
  group_by(Spp,Site) %>% 
  summarise(mean = mean(ETR_dif_A_DESF2), 
            sd = sd(ETR_dif_A_DESF2),
            n = n(),
            se = sd(ETR_dif_A_DESF2)/sqrt(n)) %>% 
  ungroup() %>%
  mutate(Site = as.factor(Site),
         Spp = ifelse(Spp == "M", "Mastocarpus papillatus", "Porphyra perforata"))

my.dodge <- 0.2 

Figure_3_dat <- Figure_3_plotting %>% ungroup()

ggplot(data = Figure_3_dat, mapping = aes(x = Spp, y = mean, color = Site, shape = Site)) + 
  geom_point(position = position_dodge(my.dodge)) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1, position = position_dodge(my.dodge)) +
  labs(x = "Species", y = "Mean Difference A (Desiccated - Rehydrated)")




Figure_4 <- lm(ETR_dif_max_DESF2~Spp*Site, data = complete_dataset)
summary(Figure_4)

ggplot(data = Figure_4, mapping = aes(x = Spp, y = plot_data_1)) + geom_point() 

anova(Figure_4)

Figure_4_plotting <- complete_dataset %>% 
  group_by(Spp,Site) %>% 
  summarise(mean = mean(ETR_dif_max_DESF2), 
            sd = sd(ETR_dif_max_DESF2),
            n = n(),
            se = sd(ETR_dif_max_DESF2)/sqrt(n)) %>% 
  ungroup() %>%
  mutate(Site = as.factor(Site),
         Spp = ifelse(Spp == "M", "Mastocarpus papillatus", "Porphyra perforata"))

my.dodge <- 0.2 

Figure_4_dat <- Figure_4_plotting %>% ungroup()

ggplot(data = Figure_4_dat, mapping = aes(x = Spp, y = mean, color = Site, shape = Site)) + 
  geom_point(position = position_dodge(my.dodge)) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1, position = position_dodge(my.dodge)) +
  labs(x = "Species", y = "Mean Difference Max (Desiccated - Rehydrated)")





