library(lme4)
library(performance)
library(tidyverse)
library(lmerTest)

index <- read.csv('./Data/Vegetation Index_yr1.csv') %>%
  filter(branch == 'top') %>% filter(days < 59)

VI <- 'ARI'
index$VI <- index$ARI
bestMOD <- evaluate_model(1)
anova(bestMOD)
ggpredict(bestMOD, c('days [all]', 'branch', 'treatment', 'clone'))  %>% plot()
saveRDS(bestMOD, file = paste('./3.2/CloneModels/',VI,'_mod.rda', sep = ''))


index

anova(lmer(VI ~ days*treatment*clone +
       (1|id), data = index))
