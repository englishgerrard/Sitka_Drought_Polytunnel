# get model data 
index <- read.csv('./Data/Index.csv') %>%
  filter(branch == 'top') %>% filter(days < 59)

data <- index
data$treatment <- as.factor(data$treatment)
data$clone <- factor(data$clone, labels = c('c1', 'c2','c3','c4','c5','c6'))
contrasts(data$clone) <- contr.sum(levels(data$clone))
data$days_scaled <- drop(scale(data$days))
rm(index)