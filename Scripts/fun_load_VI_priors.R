

# list of model priors
priors_list <- list(
  CCI = c(set_prior("normal(0.3, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),    # test
  CIgr = c(set_prior("normal(3.5, 2)", class = "b"), set_prior("normal(0, 2)", class = "sd")),   # test
  CIre = c(set_prior("normal(1.6, 0.8)", class = "b"), set_prior("normal(0, 0.8)", class = "sd")),   # test
  NDVI = c(set_prior("normal(0.85, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),  # test
  NRVIre = c(set_prior("normal(0.4, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")), # test
  SR = c(set_prior("normal(13, 5)", class = "b"), set_prior("normal(0, 5)", class = "sd")),     # test

  ARI1 = c(set_prior("normal(0, 2)", class = "b"), set_prior("normal(0, 2)", class = "sd")),
  ARI2 = c(set_prior("normal(0, 2)", class = "b"), set_prior("normal(0, 2)", class = "sd")),
  CRI1 = c(set_prior("normal(5, 6)", class = "b"), set_prior("normal(0, 6)", class = "sd")),    # test chnaed 5 to 2
  CRI2 = c(set_prior("normal(5, 6)", class = "b"), set_prior("normal(0, 6)", class = "sd")),    # test
  PRI = c(set_prior("normal(0.2, 0.08)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),     # test

  MSI = c(set_prior("normal(0.3, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),     # test
  NDWI1 = c(set_prior("normal(0.7, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),   # test
  NDWI2 = c(set_prior("normal(0.5, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),   # test
  SRWI = c(set_prior("normal(1.3, 0.7)", class = "b"), set_prior("normal(0, 0.7)", class = "sd")),    # test
  WI = c(set_prior("normal(1.1, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),      # test
  WI_NDVI = c(set_prior("normal(1.3, 0.7)", class = "b"), set_prior("normal(0, 0.7)", class = "sd"))  # test
  )

#   
# mean_prior <- function(x)
# {paste("normal(", summary(data[,(11+x)])[4], ", ", 
#        sd(summary(data[,(11+x)])), ")", sep = "")}
# sd_prior <- function(x){
#   paste("normal(0, ", sd(summary(data[,(11+x)])), ")", sep = "")
# }
# 
# gen_prior <- function(x)
# {c(set_prior(mean_prior(x), class = "b"), set_prior(sd_prior(x), class = "sd"))}
# 
# priors_list <- list(
#   CCI = gen_prior(1),
#   CIgr = gen_prior(2),   
#   CIre = gen_prior(3),    
#   NDVI = gen_prior(4),    
#   NRVIre =  gen_prior(5),
#   SR = gen_prior(6),      
#   ARI1 = gen_prior(7),    
#   ARI2 = gen_prior(8),    
#   CRI1 = gen_prior(9),   
#   CRI2 = gen_prior(10),    
#   PRI = gen_prior(11),
#   MSI = gen_prior(12),     
#   NDWI1 = gen_prior(13),   
#   NDWI2 = gen_prior(14),   
#   SRWI =  gen_prior(15),
#   WI = gen_prior(16),
#   WI_NDVI = gen_prior(17))


