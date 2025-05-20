# JUST VI OVER TIME 
source('./Scripts/_PACKAGES.R')
source('./Scripts/_FUNCTIONS.R')


estimates <- bind_rows(lapply(1:dv_l, function(x){
    VI <- dependent_vars[x]
    mod <- readRDS(paste0('./models/',VI,'_poly2.rds'))
    fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects2.csv'))
    ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples2.rds'))
    
    
    new_data <- fe %>%
      filter(str_detect(model_term, 'b_')) %>%
      filter(!str_detect(model_term, 'b_clone')) %>%
      filter(!str_detect(model_term, 'D:clone')) %>%
      filter(!str_detect(model_term, 'Inter')) %>%
      filter(!str_detect(model_term, 'b_polydays_scaled21:clone')) %>%
      filter(!str_detect(model_term, 'b_polydays_scaled22:clone')) %>%
      filter(model_term != 'b_treatmentD') %>%
      mutate(VI = VI) %>%
      mutate(VI_type = add_VI_type(x))
      
    
    new_data$contains_zero <- (new_data$lower <= 0) & (new_data$upper >= 0)
    return(new_data)
  }))

  # Adjust levels and labels for model_term
  estimates$model_term <- factor(estimates$model_term, 
                                       levels = c("b_polydays_scaled21", "b_polydays_scaled22",
                                                  "b_treatmentD:polydays_scaled21","b_treatmentD:polydays_scaled22",
                                                  "b_treatmentD:polydays_scaled21:clone1", "b_treatmentD:polydays_scaled22:clone1",
                                                  "b_treatmentD:polydays_scaled21:clone2", "b_treatmentD:polydays_scaled22:clone2",
                                                  "b_treatmentD:polydays_scaled21:clone3", "b_treatmentD:polydays_scaled22:clone3",
                                                  "b_treatmentD:polydays_scaled21:clone4", "b_treatmentD:polydays_scaled22:clone4",
                                                  "b_treatmentD:polydays_scaled21:clone5", "b_treatmentD:polydays_scaled22:clone5",
                                                  "b_treatmentD:polydays_scaled21:clone6", "b_treatmentD:polydays_scaled22:clone6"),  # Specify the order
                                       labels = c('Time (linear)', 'T. (quad.)',
                                                  'Drought (linear)', 'D. (quad.)',
                                                  'Clone 1 (linear)', 'C1. (quad.)',
                                                  'Clone 2 (linear)', 'C2. (quad.)',
                                                  'Clone 3 (linear)', 'C3. (quad.)',
                                                  'Clone 4 (linear)', 'C4. (quad.)',
                                                  'Clone 5 (linear)', 'C5. (quad.)',
                                                  'Clone 6 (linear)', 'C6. (quad.)'))   
alpha = 0.1
plot_estimates <- function(VI.type = 'Green'){
  ggplot(filter(estimates, VI_type==VI.type), aes(x = estimate_mean, y = factor(model_term,
                                                    levels = rev(levels(model_term)))) )+

  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 12.5, ymax = 14.5), 
            fill = "lightblue", alpha = alpha) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 10.5), 
            fill = "lightblue", alpha = alpha) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 6.5), 
            fill = "lightblue", alpha = alpha) + 
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 2.5), 
              fill = "lightblue", alpha = alpha) + 
  
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    
    facet_wrap(~VI, scales = "free_x") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height  =0.2) + 
  geom_point(aes(
    colour = contains_zero)) +
  theme_bw()+
    theme(legend.position = "none",
          axis.title.y = element_blank()) +
   labs(x = 'Standardised effect on VI (± 95% CI) ')}

plot_estimates('Green')
plot_estimates('Pigment')  
plot_estimates('Water')  


  # the absolute params are just interecept + coef  and intercept is the average clone so the cacluations in
  # generate posteriors are largely useless (except the clone 6 calculation)






#### OLD CODE #####
# treatment <- bind_rows(lapply(1:14, function(x){
#   VI <- dependent_vars[x]
#   mod <- readRDS(paste0('./models/',VI,'_poly2.rds'))
#   fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects2.csv'))
#   ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples2.rds'))
#   
#   
#   new_data <- fe %>%
#     filter(str_detect(model_term, 'b_treatmentD:p')) %>%
#     filter(!str_detect(model_term, 'clone')) %>%
#     mutate(VI = VI) %>%
#     mutate(VI_type = ifelse(x %in% 1:4, 'Green', ifelse(x %in% 5:8, 'Pigment', 'Water')))
#   
#   new_data$contains_zero <- (new_data$lower <= 0) & (new_data$upper >= 0)
#   return(new_data)
# }))
# 
# ggplot(filter(treatment, VI_type == 'Green'), aes(x = estimate_mean, y = model_term , colour = contains_zero)) +
#   geom_vline(xintercept = 0) +
#   geom_point() +
#   facet_wrap(~VI, scales = 'free_x') +
#   geom_errorbarh(aes(xmin = lower, xmax = upper, height = 0))  
# geom_vline(xintercept = 0)
# 
# ggplot(filter(treatment, VI_type == 'Pigment'), aes(x = estimate_mean, y = model_term , colour = contains_zero)) +
#   geom_vline(xintercept = 0) +
#   geom_point() +
#   facet_wrap(~VI, scales = 'free_x') +
#   geom_errorbarh(aes(xmin = lower, xmax = upper))  
# geom_vline(xintercept = 0)
# 
# ggplot(filter(treatment, VI_type == 'Water'), aes(x = estimate_mean, y = model_term , colour = contains_zero)) +
#   geom_vline(xintercept = 0) +
#   geom_point() +
#   facet_wrap(~VI, scales = 'free_x') +
#   geom_errorbarh(aes(xmin = lower, xmax = upper))  
# geom_vline(xintercept = 0)
# 
# ### no drought effect in SR, NDVI and CRI1
# # for the rest 
# # without a signifcat linear realtionship we cant detrmine the tunring point
# # so for a sig lin only the effect is largely linear (PRI, CRI2, CIgr)
# # for sig both terms the effect is uni-directional (goes from positive to negative) 
# # but this effect isnt linear (SRWI, NRVIre, NDWI2, MSI, CIre, ARI1, ARI2) - we can caluate the turning point here i think
# # and for only second order - the effect is not uni directional over the expereimnt
# 
# 
# treatment_clone <- bind_rows(lapply(1:dv_l, function(x){
#   VI <- dependent_vars[x]
#   mod <- readRDS(paste0('./models/',VI,'_poly_diff_contr.rds'))
#   fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects.csv'))
#   ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples.rds'))
#   
#   f <- fixef(mod)
#   
#   new_data <- fe %>%
#     filter(str_detect(model_term, 'b_treatmentD:p')) %>%
#     filter(str_detect(model_term, 'clone')) %>%
#     mutate(VI = VI)
#   
#   new_data$contains_zero <- (new_data$lower <= 0) & (new_data$upper >= 0)
#   return(new_data)
# }))
# 
# 
# ggplot(treatment_clone, aes(x = estimate_mean, y = reorder(factor(model_term), -estimate_mean) , colour = contains_zero)) +
#   geom_point() +
#   facet_wrap(~VI, scales = "free_x",) +
#   geom_errorbarh(aes(xmin = lower, xmax = upper)) # +
# xlim(-.5, .5)
# ### so this is the effect of each clone realtive to the control
