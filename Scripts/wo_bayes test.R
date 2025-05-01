data <- index

data$treatment <- as.factor(data$treatment)
data$clone <- factor(data$clone, labels = c('c1', 'c2','c3','c4','c5','c6'))
data$days_scaled <- scale(data$days)

ndvi_mod <- model
PRI_model <- brm(PRI ~ days * treatment*clone + (1 | id), 
             data = data, 
             family = gaussian(),
             prior = set_prior("normal(0, 5)", class = "b"),
             iter = 2000, 
             warmup = 1000,
             cores = 4,
             chains = 5,
             control = list(max_treedepth = 15))

model <- PRI_model

new_data <- expand.grid(
  days = seq(min(data$days), max(data$days), length.out = 50),
  treatment = c('C', 'D'),
  clone = unique(data$clone)
)

predictions <- posterior_predict(model, newdata = new_data)
new_data$predicted_ndvi <- apply(predictions, 2, mean)

# Plotting
ggplot(new_data, aes(x = days, y = predicted_ndvi, color = treatment)) +
  geom_line() +
  facet_wrap(~ clone) +
  labs(title = "Modeled NDVI Over Time", x = "Days Since Start of Experiment", y = "Predicted NDVI") +
  theme_minimal()



# new mod 
new_model <- brm(
  bf(
    PRI ~ treatment * clone * days_scaled +  # Fixed effects
      (1 + days_scaled | id)        # Random intercept & slope per tree
  ),
  data = data,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 1)", class = "b"),
    set_prior("exponential(1)", class = "sd"),
    set_prior("exponential(1)", class = "sigma")
  ),
  control = list(max_treedepth = 20),
  chains = 4,
  cores = 4
)

saveRDS(new_model, './full_pri.rds')

saveRDS(model, './mod.rds')


pri <- readRDS('./full_pri.rds')

mod <- readRDS('./mod.rds')

em_treatment_week <- emmeans(
  pri, 
  ~ treatment | days_scaled,
  at = list(days_scaled = c(-1.59129465,-1.14640321,-0.75712321,-0.31223177,
                             -0.03417462, 0.41071682, 0.74438539, 1.13366540,
                              1.57855684)))

pp_check(pri)


em_week <- emmeans(
  pri, 
  specs = pairwise ~ treatment | days_scaled,
  at = list(days_scaled = seq(min(data$days_scaled), 
                              max(data$days_scaled), 
                              length = 8))  # Evaluates at original weeks
)

# Plot treatment differences
plot(em_week$contrasts) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Drought vs. Control: Weekly Contrasts",
       y = "NDVI Difference (Drought - Control)")

# Test if drought effect varies by clone (3-way interaction)
hypothesis(pri, c(
  "treatmentdrought:cloneB:week_scaled = 0",
  "treatmentdrought:cloneC:week_scaled = 0",
  "treatmentdrought:cloneF:week_scaled = 0"  # Add all clones
))
