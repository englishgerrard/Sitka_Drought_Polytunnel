# get model data 
index <- read.csv('./Data/Index.csv') %>%
  filter(branch == 'top') %>% filter(days < 59)

# Define the list of dependent variables
# dependent_vars <- 
#   c('CIgr', 'CIre', 'NDVI', 'NRVIre', 'SR', 'CCI', 
#     'ARI1', 'ARI2', 'CRI1', 'CRI2', 'PRI',
#     'MSI', 'NDWI1', 'NDWI2', 'SRWI', 'WI', 'WI_NDVI')


dependent_vars <- # updated VI
  c('CIgr', 'CIre', 'NDVI', 'NRVIre', 'CCI', 
    'ARI1', 'ARI2', 'PRI',
    'MSI', 'NDWI1', 'NDWI2', 'SRWI', 'WI', 'WI_NDVI')

data <- index
data$treatment <- as.factor(data$treatment)
data$clone <- factor(data$clone, labels = c('c1', 'c2','c3','c4','c5','c6'))
contrasts(data$clone) <- contr.sum(levels(data$clone))
data$days_scaled <- drop(scale(data$days))
rm(index)

dv_l <- length(dependent_vars)
# data in long format for plotting
data_long <- data_frame(treatment = rep(data$treatment, times = dv_l),
                        days_scaled = rep(data$days_scaled, times = dv_l),
                        clone = rep(data$clone, times = dv_l),
                        date = rep(data$date, times = dv_l),
                        VI = rep(dependent_vars, each = 538),
                        VI_type = c(rep("Green", times = 538*4),
                                    rep("Pigment", times = 538*4),
                                    rep("Water", times = 538*6)),
                        value = c(data$CIgr, # green
                                  data$CIre, data$NDVI,
                                  data$NRVIre, # data$SR,
                                  data$CCI, # pigment
                                  data$ARI1, data$ARI2,
                                  #data$CRI1, data$CRI2,
                                  data$PRI,
                                  data$MSI,# water
                                  data$NDWI1, data$NDWI2,
                                  data$SRWI, data$WI,
                                  data$WI_NDVI))


####### function to get lines 
# i = 1 the moddle lines 
# i - 2 the contrast lines (T diff from C)
get_em_treat_days_clone <- function(x =1, num.contrast = 8, i = 1){
  VI <- dependent_vars[x]
  mod <- readRDS(paste0('./models/',VI,'_poly2.rds'))
  fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects2.csv'))
  ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples2.rds'))
  
  
  em_week <- emmeans(
    mod, 
    specs = pairwise ~ treatment | days_scaled | clone,
    at = list(days_scaled = seq(min(data$days_scaled), 
                                max(data$days_scaled), 
                                length = num.contrast))  
  )
  
  
  df1 <- as.data.frame(em_week$emmeans) %>%
    mutate(VI =dependent_vars[x] ) %>%
    mutate(VI_type = ifelse(x %in% 1:4, 'Green', ifelse(x %in% 5:8, 'Pigment', 'Water')))
  
  df2 <- as.data.frame(em_week$contrasts ) %>%
    mutate(VI =dependent_vars[x] ) %>%
    mutate(lower.HPD.80 = summary(em_week$contrasts, level = 0.80)$lower.HPD) %>%
    mutate(upper.HPD.80 = summary(em_week$contrasts, level = 0.80)$upper.HPD) %>%
    mutate(lower.HPD.85 = summary(em_week$contrasts, level = 0.85)$lower.HPD) %>%
    mutate(upper.HPD.85 = summary(em_week$contrasts, level = 0.85)$upper.HPD) %>%
    mutate(lower.HPD.90 = summary(em_week$contrasts, level = 0.9)$lower.HPD) %>%
    mutate(upper.HPD.90 = summary(em_week$contrasts, level = 0.9)$upper.HPD) %>%
    mutate(lower.HPD.95 = summary(em_week$contrasts, level = 0.95)$lower.HPD) %>%
    mutate(upper.HPD.95 = summary(em_week$contrasts, level = 0.95)$upper.HPD) %>%
    mutate(lower.HPD.957 = summary(em_week$contrasts, level = 0.975)$lower.HPD) %>%
    mutate(upper.HPD.957 = summary(em_week$contrasts, level = 0.975)$upper.HPD) %>%
    mutate(lower.HPD.99 = summary(em_week$contrasts, level = 0.99)$lower.HPD) %>%
    mutate(upper.HPD.99 = summary(em_week$contrasts, level = 0.99)$upper.HPD) %>%
    mutate(VI_type = ifelse(x %in% 1:4, 'Green', ifelse(x %in% 5:8, 'Pigment', 'Water')))
  if(i == 1)
  {return(df1)}
  {return(df2)}
}

# i = 1 the moddle lines 
# i - 2 the contrast lines (T diff from C)
get_em_treat_days <- function(x =1, num.contrast = 8, i = 1){
  VI <- dependent_vars[x]
  mod <- readRDS(paste0('./models/',VI,'_poly2.rds'))
  fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects2.csv'))
  ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples2.rds'))
  
  
  em_week <- emmeans(
    mod, 
    specs = pairwise ~ treatment | days_scaled,
    at = list(days_scaled = seq(min(data$days_scaled), 
                                max(data$days_scaled), 
                                length = num.contrast))  
  )
  
 summary(em_week$contrasts, level = 0.9)$lower.HPD
  
  df1 <- as.data.frame(em_week$emmeans) %>%
    mutate(VI =dependent_vars[x] ) %>%
    mutate(VI_type = ifelse(x %in% 1:4, 'Green', ifelse(x %in% 5:8, 'Pigment', 'Water')))
  

  df2 <- as.data.frame(em_week$contrasts ) %>%
    mutate(lower.HPD.80 = summary(em_week$contrasts, level = 0.80)$lower.HPD) %>%
    mutate(upper.HPD.80 = summary(em_week$contrasts, level = 0.80)$upper.HPD) %>%
    mutate(lower.HPD.85 = summary(em_week$contrasts, level = 0.85)$lower.HPD) %>%
    mutate(upper.HPD.85 = summary(em_week$contrasts, level = 0.85)$upper.HPD) %>%
    mutate(lower.HPD.90 = summary(em_week$contrasts, level = 0.9)$lower.HPD) %>%
    mutate(upper.HPD.90 = summary(em_week$contrasts, level = 0.9)$upper.HPD) %>%
    mutate(lower.HPD.95 = summary(em_week$contrasts, level = 0.95)$lower.HPD) %>%
    mutate(upper.HPD.95 = summary(em_week$contrasts, level = 0.95)$upper.HPD) %>%
    mutate(lower.HPD.957 = summary(em_week$contrasts, level = 0.975)$lower.HPD) %>%
    mutate(upper.HPD.957 = summary(em_week$contrasts, level = 0.975)$upper.HPD) %>%
    mutate(lower.HPD.99 = summary(em_week$contrasts, level = 0.99)$lower.HPD) %>%
    mutate(upper.HPD.99 = summary(em_week$contrasts, level = 0.99)$upper.HPD) %>%
    mutate(VI =dependent_vars[x] ) %>%
    mutate(VI_type = ifelse(x %in% 1:4, 'Green', ifelse(x %in% 5:8, 'Pigment', 'Water')))
  
  
  
  if(i == 1)if(i == 1)if(i == 1)
  {return(df1)}
  {return(df2)}
}
