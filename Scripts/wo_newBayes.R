source('./Scripts/.PACKAGES.R')

index <- read.csv('./Data/Index.csv') %>%
  filter(branch == 'top') %>% filter(days < 59)

data <- index
data$treatment <- as.factor(data$treatment)
data$clone <- factor(data$clone, labels = c('c1', 'c2','c3','c4','c5','c6'))
contrasts(data$clone) <- contr.sum(levels(data$clone))
data$days_scaled <- drop(scale(data$days))



ggplot(data, aes(x = days, y = PRI, color = treatment, group = interaction(id, treatment))) +  
  geom_point(alpha = 0.3) +  
  geom_smooth(aes(group = treatment), method = "lm", se = TRUE) +  # Treatment-level trends
  facet_wrap(~clone, ncol = 3) +                                   # Organize clones
  labs(title = "PRI Trajectories by Clone and Treatment",
       x = "Days", y = "PRI") +
  theme_minimal() 

priors <- c(
  # Fixed effects (conservative)
  set_prior("normal(0, 0.05)", class = "Intercept"),  # Baseline NDVI near 0
  set_prior("normal(0, 0.05)", class = "b", coef = "treatmentD"),  # Drought effect
  set_prior("normal(0, 0.02)", class = "b", coef = "clone1"),
  set_prior("normal(0, 0.02)", class = "b", coef = "clone2"), 
  set_prior("normal(0, 0.02)", class = "b", coef = "clone3"), 
  set_prior("normal(0, 0.02)", class = "b", coef = "clone4"), 
  set_prior("normal(0, 0.02)", class = "b", coef = "clone5"), 
   # Clone differences
             
  set_prior("normal(0, 0.02)", class = "b", coef = "treatmentD:days_scaled") # Drought slope
                 
)

PRIMOD <- brm(
  bf(
    PRI ~ treatment *days_scaled * clone + # Tests all interactions
      (1 +days_scaled| id)        # Random slopes per tree
  ),
  data = data,
  family = gaussian(),                       # Use Beta() if NDVI is bounded [0,1]
  prior = c(
    set_prior("normal(0, 0.08)", class = "b")
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  file = "./PRIMOD2.RDS"  # Save to avoid re-fitting
)


PRIMOD <- brm(
  bf(
    PRI ~ treatment * poly(days_scaled,4) * clone + # Tests all interactions
      (1 +days_scaled| id)        # Random slopes per tree
  ),
  data = data,
  family = gaussian(),                       # Use Beta() if NDVI is bounded [0,1]
  prior = c(
    set_prior("normal(0, 0.08)", class = "b")
  ),
  chains = 4,
  iter = 4000,
  cores = 4,
  #file = "./PRIMOD2.RDS"  # Save to avoid re-fitting
)

pp_check(pri_model)

emtrends(pri_model, pairwise ~ treatment, var = "days_scaled")

em_week <- emmeans(
  PRIMODpoly, 
  specs = pairwise ~ treatment | days_scaled,
  at = list(days_scaled = seq(min(data$days_scaled), 
                              max(data$days_scaled), 
                              length = 8))  # Evaluates at original weeks
)

# Plot treatment differences
plot(em_week$contrasts) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Drought vs. Control: Weekly Contrasts",
       y = "PRI Difference (Drought - Control)")

contrasts_df <- as.data.frame(em_week$contrasts)
ggplot(contrasts_df, aes(x = days_scaled, y = estimate, color = contrast)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower.HPD, ymax = upper.HPD), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Drought vs. Control: Weekly Contrasts by Clone",
       y = "PRI Difference (Drought - Control)",
       x = "Days Scaled") +
  facet_wrap(~ clone) +  # Facet by clone
  theme_minimal() +
  theme(legend.position = "bottom")


loo_lin <- loo(PRIMOD)
loo_poly <- loo(PRIMODpoly)

# Print LOO results
print(loo_lin)
print(loo_poly)

# Compare LOO values
loo_comparison <- loo_compare(loo_lin, loo_poly)
print(loo_comparison)
