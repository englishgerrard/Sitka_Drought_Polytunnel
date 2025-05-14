# JUST VI OVER TIME 

treatment <- bind_rows(lapply(1:17, function(x){
  VI <- dependent_vars[x]
  mod <- readRDS(paste0('./models/',VI,'_poly2.rds'))
  fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects2.csv'))
  ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples2.rds'))
  
  
  new_data <- fe %>%
    filter(str_detect(model_term, 'b_treatmentD:p')) %>%
    filter(!str_detect(model_term, 'clone')) %>%
    mutate(VI = VI)
  
  new_data$contains_zero <- (new_data$lower <= 0) & (new_data$upper >= 0)
  return(new_data)
}))

ggplot(treatment, aes(x = estimate_mean, y = VI , colour = contains_zero)) +
  geom_point() +
  facet_wrap(~model_term) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) # +
  xlim(-.5, .5)

  ### no drought effect in SR, NDVI and CRI1
  # for the rest 
  # without a signifcat linear realtionship we cant detrmine the tunring point
  # so for a sig lin only the effect is largely linear (PRI, CRI2, CIgr)
  # for sig both terms the effect is uni-directional (goes from positive to negative) 
  # but this effect isnt linear (SRWI, NRVIre, NDWI2, MSI, CIre, ARI1, ARI2) - we can caluate the turning point here i think
  # and for only second order - the effect is not uni directional over the expereimnt
  
 
  treatment_clone <- bind_rows(lapply(1:17, function(x){
    VI <- dependent_vars[x]
    mod <- readRDS(paste0('./models/',VI,'_poly_diff_contr.rds'))
    fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects.csv'))
    ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples.rds'))
    
    f <- fixef(mod)
    
    new_data <- fe %>%
      filter(str_detect(model_term, 'b_treatmentD:p')) %>%
      filter(str_detect(model_term, 'clone')) %>%
      mutate(VI = VI)
    
    new_data$contains_zero <- (new_data$lower <= 0) & (new_data$upper >= 0)
    return(new_data)
  }))
  
  ggplot(treatment_clone, aes(x = estimate_mean, y = model_term , colour = contains_zero)) +
    geom_point() +
    facet_wrap(~VI, scales = "free_x",) +
    geom_errorbarh(aes(xmin = lower, xmax = upper)) # +
  xlim(-.5, .5)
  ### so this is the effect of each clone realtive to the control
  
  treatment_clone2 <- bind_rows(lapply(1:17, function(x){
    VI <- dependent_vars[x]
    mod <- readRDS(paste0('./models/',VI,'_poly_diff_contr.rds'))
    fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects2.csv'))
    ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples2.rds'))
    
    f <- fixef(mod)
    
    new_data <- fe %>%
      filter(str_detect(model_term, 'b_treatmentD:p')) %>%
      filter(str_detect(model_term, 'clone')) %>%
      filter(str_detect(model_term, 'abs')) %>%
      mutate(VI = VI)
    
    new_data$contains_zero <- (new_data$lower <= 0) & (new_data$upper >= 0)
    return(new_data)
  }))
  
  ggplot(treatment_clone2, aes(x = estimate_mean, y = model_term , colour = contains_zero)) +
    geom_point() +
    facet_wrap(~VI, scales = "free_x",) +
    geom_errorbarh(aes(xmin = lower, xmax = upper)) # +
  xlim(-.5, .5)
  
  # the absolute params are just interecept + coef  and intercept is the average clone so the cacluations in
  # generate posteriors are largely useless (except the clone 6 calculation)