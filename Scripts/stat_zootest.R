
zoo_test <- function(x){
  
pol2 <- readRDS(paste0('./',dependent_vars[x],'.rds'))
pol3 <- readRDS(paste0('./',dependent_vars[x],'_linear.rds'))

loo_pol2 <- loo(pol2)
loo_pol3 <- loo(pol3)

# Print LOO results
#print(loo_lin)
#print(loo_poly)   

# Compare LOO values
loo_comparison <- loo_compare(loo_pol2, loo_pol3)
return(loo_comparison)}


zoo_res <- lapply(1:length(dependent_vars), zoo_test)
#pbPost("note", title = "R Alert", body = paste('Script finished 💃 @', Sys.time()))
