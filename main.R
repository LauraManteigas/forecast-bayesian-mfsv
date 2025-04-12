# =====================
# SETUP
# =====================
source("Scripts/setup.R")

# =====================
# DATA PREPROCESSING
# =====================
data <- calculate_log_returns(config$symbols_files, config$symbols, config$prediction$steps)
descriptive_statistics(data, config$symbols)

# =====================
# BAYESIAN OPTIMIZATION
# =====================
# score_summary <- run_bayesian_optimization(config$search_space)
# # gam_params(score_summary)
# 
# 
# best_params <- as.list(score_summary[1, 1:p])
# worst_params <- as.list(score_summary[.N, 1:p])  

run_bayesian_optimization(search_space = config$search_space)
