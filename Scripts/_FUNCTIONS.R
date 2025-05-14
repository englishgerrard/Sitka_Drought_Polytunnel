# get model data 
index <- read.csv('./Data/Index.csv') %>%
  filter(branch == 'top') %>% filter(days < 59)

# Define the list of dependent variables
dependent_vars <- 
  c('CIgr', 'CIre', 'NDVI', 'NRVIre', 'SR', 'CCI', 
    'ARI1', 'ARI2', 'CRI1', 'CRI2', 'PRI',
    'MSI', 'NDWI1', 'NDWI2', 'SRWI', 'WI', 'WI_NDVI')

data <- index
data$treatment <- as.factor(data$treatment)
data$clone <- factor(data$clone, labels = c('c1', 'c2','c3','c4','c5','c6'))
contrasts(data$clone) <- contr.sum(levels(data$clone))
data$days_scaled <- drop(scale(data$days))
rm(index)

# data in long format for plotting
data_long <- data_frame(treatment = rep(data$treatment, times = 17),
                        days_scaled = rep(data$days_scaled, times = 17),
                        clone = rep(data$clone, times = 17),
                        date = rep(data$date, times = 17),
                        VI = rep(dependent_vars, each = 538),
                        VI_type = c(rep("Green", times = 538*5),
                                    rep("Pigment", times = 538*6),
                                    rep("Water", times = 538*6)),
                        value = c(data$CIgr, # green
                                  data$CIre, data$NDVI,
                                  data$NRVIre, data$SR,
                                  data$CCI, # pigment
                                  data$ARI1, data$ARI2,
                                  data$CRI1, data$CRI2,
                                  data$PRI,
                                  data$MSI,# water
                                  data$NDWI1, data$NDWI2,
                                  data$SRWI, data$WI,
                                  data$WI_NDVI))
