dependent_vars <- 
  c('CCI', 'CIgr', 'CIre', 'NDVI', 'NRVIre', 'SR',
    'ARI1', 'ARI2', 'CRI1', 'CRI2', 'PRI',
    'MSI', 'NDWI1', 'NDWI2', 'SRWI', 'WI', 'WI_NDVI')


zoo_test <- function(x){
  
mod_lin <- readRDS(paste0('./models/',dependent_vars[x],'_lin.rds'))
mod_poly2 <- readRDS(paste0('./models/',dependent_vars[x],'_poly2.rds'))
mod_poly3 <- readRDS(paste0('./models/',dependent_vars[x],'_poly3.rds'))

loo_mod1 <- loo(mod_lin)
loo_mod2 <- loo(mod_poly2)
loo_mod3 <- loo(mod_poly3)

# Print LOO results
#print(loo_lin)
#print(loo_poly)   

'./models/'

readRDS('models/CCI_lin.rds')

# Compare LOO values
loo_comparison <- loo_compare(loo_mod1, loo_mod2, loo_mod3)


# Convert to data frame for easier manipulation
loo_comparison_df <- as.data.frame(loo_comparison)

# Set custom order with mod_lin as the reference
loo_comparison_df$model <- rownames(loo_comparison_df)
loo_comparison_df$model <- factor(loo_comparison_df$model, levels = c("mod_lin", "mod_poly2", "mod_poly3"))

# Reorder the data frame based on the custom factor levels
loo_comparison_df <- loo_comparison_df[order(loo_comparison_df$model), ]
return(loo_comparison)}


zoo_res <- lapply(1:length(dependent_vars), zoo_test)
#pbPost("note", title = "R Alert", body = paste('Script finished 💃 @', Sys.time()))


# function to calculate credible intervals 
# if credible interval contains 0 models perfom similary 
lapply(1:17,function(x){
  
  lower_bound <- zoo_res[[x]][2,1] - 1.96 * zoo_res[[x]][2,2]
  upper_bound <- zoo_res[[x]][2,1] + 1.96 * zoo_res[[x]][2,2]
  return(cbind(lower_bound,upper_bound))})


library(loo)





########### needs review
zoo_test <- function(x) {
  
  # Load models
  mod_lin <- readRDS(paste0('./models/', dependent_vars[x], '_lin.rds'))
  mod_poly2 <- readRDS(paste0('./models/', dependent_vars[x], '_poly2.rds'))
  mod_poly3 <- readRDS(paste0('./models/', dependent_vars[x], '_poly3.rds'))
  
  # Compute LOO for each model
  loo_mod1 <- loo(mod_lin)
  loo_mod2 <- loo(mod_poly2)
  loo_mod3 <- loo(mod_poly3)
  
  # Compare LOO values
  loo_comparison <- loo_compare(loo_mod1, loo_mod2, loo_mod3)
  
  # Convert to data frame for easier manipulation
  loo_comparison_df <- as.data.frame(loo_comparison)
  
  # Add a reference model column
  loo_comparison_df$model <- rownames(loo_comparison_df)
  
  # Set the reference model
  reference_model <- "mod_lin"
  
  # Adjust elpd_diff to reflect the reference model
  loo_comparison_df$elpd_diff <- ifelse(loo_comparison_df$model == reference_model, 0, loo_comparison_df$elpd_diff - loo_comparison_df$elpd_diff[loo_comparison_df$model == reference_model])
  
  # Reorder the data frame to have the reference model first
  loo_comparison_df <- loo_comparison_df[match(c(reference_model, setdiff(rownames(loo_comparison_df), reference_model)), rownames(loo_comparison_df)), ]
  
  # Return the adjusted comparison
  return(loo_comparison_df)
}

# Apply the function to all dependent variables
zoo_res <- lapply(1:length(dependent_vars), zoo_test)
