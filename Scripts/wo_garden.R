
source('./Scripts/.PACKAGES.R')

# Define the list of dependent variables
dependent_vars <- 
  c('CCI', 'CIgr', 'CIre', 'NDVI', 'NRVIre', 'SR',
    'ARI1', 'ARI2', 'CRI1', 'CRI2', 'PRI',
    'MSI', 'NDWI1', 'NDWI2', 'SRWI', 'WI', 'WI_NDVI')


index <- read.csv('./Data/Index.csv') %>%
  filter(branch == 'top') %>% filter(days < 59)

data <- index
data$treatment <- as.factor(data$treatment)
data$clone <- factor(data$clone, labels = c('c1', 'c2','c3','c4','c5','c6'))
contrasts(data$clone) <- contr.sum(levels(data$clone))
data$days_scaled <- drop(scale(data$days))


# read mod
mod <- readRDS(paste0('./models/',dependent_vars[1],'_poly2.rds'))

pp_check(mod)
mcmc_plot(mod,type = "trace") 
mcmc_plot(mod,type = "rhat") 


# Set the row name for clone6
rownames(clone6_row) <- "clone6"

# Combine the new row with the existing data frame
updated_fixed_effects_df <- rbind(fixed_effects_df, clone6_row)

####
posterior_samples <- posterior_samples(mod)











# Calculate the posterior for clone 6
posterior_samples$b_clone6 <- -(
  posterior_samples$b_clone1 + 
  posterior_samples$b_clone2 + 
  posterior_samples$b_clone3 + 
  posterior_samples$b_clone4 + 
  posterior_samples$b_clone5
)
 # calculate posterior for treatment effect over time
posterior_samples$`b_treatmentD:polydays_scaled22:clone6` <- -(
  posterior_samples$`b_treatmentD:polydays_scaled22:clone1` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone2`+ 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone3` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone4` + 
    posterior_samples$`b_treatmentD:polydays_scaled22:clone5`
)

# Plot posterior distributions for clones 1 to 5 and clone 6
clone_effect <- posterior_samples %>%
  select(b_clone1, b_clone2, b_clone3, b_clone4, b_clone5, b_clone6) %>%
  pivot_longer(cols = everything(), names_to = "Clone", values_to = "Effect")

drought_clone <- posterior_samples %>%
  select(`b_treatmentD:polydays_scaled22:clone1`, `b_treatmentD:polydays_scaled22:clone2`,
         `b_treatmentD:polydays_scaled22:clone3`, `b_treatmentD:polydays_scaled22:clone4`,
         `b_treatmentD:polydays_scaled22:clone5`, `b_treatmentD:polydays_scaled22:clone6`) %>%
  pivot_longer(cols = everything(), names_to = "Clone", values_to = "Effect")

# Create the plot
ggplot(clone_effect, aes(x = Effect, fill = Clone)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Create the plot
ggplot(drought_clone, aes(x = Effect, fill = Clone)) +
  geom_density(alpha = 0.2) +
  theme_minimal() +
  theme(legend.title = element_blank())

##########


em_week <- emmeans(
  mod, 
  specs = pairwise ~ treatment | days_scaled,
  at = list(days_scaled = seq(min(data$days_scaled), 
                              max(data$days_scaled), 
                              length = 8))  # Evaluates at original weeks
)

# Plot treatment differences
plot(em_week$contrasts) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Drought vs. Control: Weekly Contrasts",
       y = "VI Difference (Drought - Control)")

em_clone <- emmeans(
  mod, 
  specs = pairwise ~ treatment | days_scaled| clone,
  at = list(days_scaled = seq(min(data$days_scaled), 
                              max(data$days_scaled), 
                              length = 8))  # Evaluates at original weeks
)

contrasts_df <- as.data.frame(em_clone$contrasts)
ggplot(contrasts_df, aes(x = days_scaled, y = estimate, color = contrast)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Drought vs. Control: Weekly Contrasts by Clone",
       y = "VI Difference (Drought - Control)",
       x = "Days Scaled") +
  facet_wrap(~ clone) +  # Facet by clone
  theme_minimal() +
  theme(legend.position = "bottom")


