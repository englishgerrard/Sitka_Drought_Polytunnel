# JUST VI OVER TIME 
source('./Scripts/_PACKAGES.R')
source('./Scripts/_FUNCTIONS.R')
season <- bind_rows(lapply(1:17, function(x){
VI <- dependent_vars[x]
mod <- readRDS(paste0('./models/',VI,'_poly2.rds'))
fe <- read.csv(paste0('./Data/VI/',VI,'_fixed_effects.csv'))
ps <- readRDS(paste0('./Data/VI/',VI,'_posterior_samples.rds'))


new_data <- fe %>%
  filter(str_detect(model_term, 'b_polydays')) %>%
  filter(!str_detect(model_term, 'clone')) %>%
  mutate(VI = VI)

new_data$contains_zero <- (new_data$lower <= 0) & (new_data$upper >= 0)
return(new_data)
}))
  
ggplot(season, aes(x = estimate_mean, y = VI , colour = contains_zero)) +
  geom_point() +
  facet_wrap(~model_term) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) 

ggplot(season, aes(x = estimate_mean, y = VI )) +
  geom_point() +
  facet_wrap(~model_term) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  xlim(-0.5, 0.5)

# basically all VI had some temporal varaition regardless of drought.
# except (WI, CRI2 and CIgr) - fairly consistent across time.
# this is saying the VI are chaing over time (at a broadly consistent rate) -
# as only first order polynomial (liner) effect is "significant".
# no change in rate is detected (non sig second order polynomials).
# exceptions - ARI1 has a poly nomial 2 effect 



