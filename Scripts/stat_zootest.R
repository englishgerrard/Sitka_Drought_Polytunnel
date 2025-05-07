dependent_vars <- 
  c('CCI', 'CIgr', 'CIre', 'NDVI', 'NRVIre', 'SR',
    'ARI1', 'ARI2', 'CRI1', 'CRI2', 'PRI',
    'MSI', 'NDWI1', 'NDWI2', 'SRWI', 'WI', 'WI_NDVI')


loo_test <- function(x){
  
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


loo_res <- lapply(1:length(dependent_vars), loo_test)
#pbPost("note", title = "R Alert", body = paste('Script finished 💃 @', Sys.time()))






