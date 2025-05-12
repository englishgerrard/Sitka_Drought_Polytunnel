
source('./Scripts/.PACKAGES.R')

# Define the list of dependent variables
dependent_vars <- 
  c('CCI', 'CIgr', 'CIre', 'NDVI', 'NRVIre', 'SR',
    'ARI1', 'ARI2', 'CRI1', 'CRI2', 'PRI',
    'MSI', 'NDWI1', 'NDWI2', 'SRWI', 'WI', 'WI_NDVI')

# read mod
VI <- dependent_vars[5]

mod <- readRDS(paste0('./models/',VI,'_poly2.2.rds'))

index <- read.csv('./Data/Index.csv') %>%
  filter(branch == 'top') %>% filter(days < 59)

data <- index
data$treatment <- as.factor(data$treatment)
data$clone <- factor(data$clone, labels = c('c1', 'c2','c3','c4','c5','c6'))
contrasts(data$clone) <- contr.sum(levels(data$clone))
data$days_scaled <- drop(scale(data$days))



## model checks
pp_check(mod)
mcmc_plot(mod,type = "rhat") 
## draw samples 
posterior_samples <- as_draws_df(mod)
## CI values
lwr_ci = 0.05
upr_ci = 0.95

# Calculate the posterior for clone 6
posterior_samples$b_clone6 <- -(
  posterior_samples$b_clone1 + 
    posterior_samples$b_clone2 + 
    posterior_samples$b_clone3 + 
    posterior_samples$b_clone4 + 
    posterior_samples$b_clone5
)

# calculate posterior for clone effect over time poly1
posterior_samples$`b_polydays_scaled21:clone6` <- -(
  posterior_samples$`b_polydays_scaled21:clone1` + 
    posterior_samples$`b_polydays_scaled21:clone2`+ 
    posterior_samples$`b_polydays_scaled21:clone3` + 
    posterior_samples$`b_polydays_scaled21:clone4` + 
    posterior_samples$`b_polydays_scaled21:clone5`
)

# calculate posterior for clone effect over time poly2
posterior_samples$`b_polydays_scaled22:clone6` <- -(
  posterior_samples$`b_polydays_scaled22:clone1` + 
    posterior_samples$`b_polydays_scaled22:clone2`+ 
    posterior_samples$`b_polydays_scaled22:clone3` + 
    posterior_samples$`b_polydays_scaled22:clone4` + 
    posterior_samples$`b_polydays_scaled22:clone5`
)


# calculate posterior for treatment effect over time poly1
posterior_samples$`b_treatmentD:polydays_scaled21:clone6` <- -(
  posterior_samples$`b_treatmentD:polydays_scaled21:clone1` + 
    posterior_samples$`b_treatmentD:polydays_scaled21:clone2`+ 
    posterior_samples$`b_treatmentD:polydays_scaled21:clone3` + 
    posterior_samples$`b_treatmentD:polydays_scaled21:clone4` + 
    posterior_samples$`b_treatmentD:polydays_scaled21:clone5`
)

# calculate posterior for treatment effect over time poly2
posterior_samples$`b_treatmentD:polydays_scaled22:clone6` <- -(
  posterior_samples$`b_treatmentD:polydays_scaled22:clone1` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone2`+ 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone3` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone4` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone5`
)

pos <- posterior_samples
fixed_effects <-  pos %>%
  select(starts_with("b")) %>%
  pivot_longer(cols = everything(), names_to = "model_term", values_to = "estimate") %>%
  group_by(model_term) %>%
  summarise(
    lower = quantile(estimate, lwr_ci),
    upper = quantile(estimate, upr_ci),
    estimate_mean = mean(estimate)) 

saveRDS(posterior_samples, './Data/VI/VI_posterior_samples.rds')
write.csv(fixed_effects, './Data/VI/VI_fixed_effect.csv')

ggplot(fixed_effects, aes(x = estimate_mean, y = model_term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper))

print(VI)
ggplot(data, aes(y = PRI, x =days, colour = treatment)) +
  geom_point(alpha = .5)+
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 1, raw=TRUE), linetype = 1, ) +
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE), linetype = 4) +
  facet_wrap(~clone)

