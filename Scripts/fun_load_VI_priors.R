# list of model priors
priors_list <- list(
  CCI = c(set_prior("normal(0.3, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),    # test
  CIgr = c(set_prior("normal(3.5, 2)", class = "b"), set_prior("normal(0, 2)", class = "sd")),   # test
  CIre = c(set_prior("normal(1.6, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),   # test
  NDVI = c(set_prior("normal(0.85, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),  # test
  NRVIre = c(set_prior("normal(0.4, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")), # test
  SR = c(set_prior("normal(1.3, 0.3)", class = "b"), set_prior("normal(0, 0.3)", class = "sd")),     # test
  
  ARI1 = c(set_prior("normal(0, 2)", class = "b"), set_prior("normal(0, 2)", class = "sd")),
  ARI2 = c(set_prior("normal(0, 2)", class = "b"), set_prior("normal(0, 2)", class = "sd")),
  CRI1 = c(set_prior("normal(6.5, 5)", class = "b"), set_prior("normal(0, 5)", class = "sd")),    # test
  CRI2 = c(set_prior("normal(6.5, 5)", class = "b"), set_prior("normal(0, 5)", class = "sd")),    # test
  PRI = c(set_prior("normal(0.2, 0.08)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),     # test 
  
  MSI = c(set_prior("normal(0.3, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),     # test
  NDWI1 = c(set_prior("normal(0.7, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),   # test
  NDWI2 = c(set_prior("normal(0.5, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),   # test
  SRWI = c(set_prior("normal(1.3, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),    # test
  WI = c(set_prior("normal(1.1, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd")),      # test
  WI_NDVI = c(set_prior("normal(1.3, 0.2)", class = "b"), set_prior("normal(0, 0.2)", class = "sd"))  # test
  )


