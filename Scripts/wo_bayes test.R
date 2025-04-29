data <- index

data$treatment <- as.factor(data$treatment)
data$clone <- as.factor(data$clone)
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
