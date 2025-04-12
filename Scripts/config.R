config <- list(
  symbols = c("Brent", "Dubai", "Gasoline", "HO", "WTI"),
  data_path = "Data/",
  mcmc = list(q = 2, draws = 150000, burnin = 50000, thin = 1),
  prediction = list(steps = 50, alpha = 0.05, smax = sqrt(qchisq(0.95, df = 5))),
  weights = list(wc = 0.7, ws = 0.3),
  search_space = list(
    B_mu = c(1, 200),  
    a_0 = c(1, 100),  
    b_0 = c(1, 100),
    B_sigma = c(0.01, 1),
    a_i = c(0.1, 0.9),
    c = c(0.5, 15),
    d = c(0.5, 15)
  )
)

# Create file paths for symbols
config$symbols_files <- paste0(config$data_path, config$symbols, ".csv")
p <- length(config$search_space)
m <- length(config$symbols)
