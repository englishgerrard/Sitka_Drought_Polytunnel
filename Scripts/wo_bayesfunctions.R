source('./Scripts/.PACKAGES.R')
# Defined a list of priors for each dependent variable
source('./Scripts/fun_load_VI_priors.R')

index <- read.csv('./Data/Index.csv') %>%
  filter(branch == 'top') %>% filter(days < 59)

data <- index
data$treatment <- as.factor(data$treatment)
data$clone <- factor(data$clone, labels = c('c1', 'c2','c3','c4','c5','c6'))
contrasts(data$clone) <- contr.sum(levels(data$clone))
data$days_scaled <- drop(scale(data$days))


fit_model <- function(dependent_var, priors) {
  # Create the formula for the model
  formula <- as.formula(paste(dependent_var, "~ treatment * poly(days_scaled, 3) * clone + (1 + days_scaled | id)"))
  
  # Fit the model using brms
  model <- brm(
    formula,
    data = data,
    family = gaussian(),
    prior = priors,
    chains = 4,
    iter = 4000,
    cores = 6,
    file = paste0('./',dependent_var,'_poly3.rds')
    #silent = TRUE
  )
  
  pbPost("note", title = "R Alert", body = paste(dependent_var, 'model run'))
  # Return the model summary
  return(model)
}

# Define the list of dependent variables
dependent_vars <- 
  c('CCI', 'CIgr', 'CIre', 'NDVI', 'NRVIre', 'SR',
    'ARI1', 'ARI2', 'CRI1', 'CRI2', 'PRI',
    'MSI', 'NDWI1', 'NDWI2', 'SRWI', 'WI', 'WI_NDVI')


start.time <- Sys.time()
tryCatch({

# Use lapply to fit models for each dependent variable
model_summaries <- lapply(seq_along(dependent_vars), function(i) {
  fit_model(dependent_vars[i], priors_list[[dependent_vars[i]]])
})
}, error = function(e) {
  pbPost(
    type = "note",
    title = "🚨 R Script Failed!",
    body = paste(
      'Error',
      #"Error:", e$message, "\n",
      "Time:", format(Sys.time(), "%Y-%m-%d %I:%M %p")
    )
  )
})
#time.taken <- (Sys.time() - start.time)
pbPost("note", title = "R Alert", body = paste("script finished @", Sys.time(),
                                               "time elapsed = ", time.taken, 'mins 💃'))
# Name the list elements with the dependent variable names
names(model_summaries) <- dependent_vars

# Print the summaries
model_summaries

