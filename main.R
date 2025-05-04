source("Scripts/setup.R")


# =====================
# DATA PREPROCESSING
# =====================
data <- calculate_log_returns(config$symbols_files, 
                              config$symbols, 
                              config$prediction$steps)

descriptive_statistics(data, config$symbols)


# =====================
# BAYESIAN OPTIMIZATION
# =====================
score_summary <- run_bayesian_optimization(config$search_space)

best_params <- as.list(score_summary[1, 1:p])
worst_params <- as.list(score_summary[.N, 1:p])


# =====================
# FSV COMPUTATION
# =====================
fsv_results <- list(
  best = load_or_compute_wrapper(
    "Results/MFSV/fsvsample_best.RData", 
    compute_fsv, 
    best_params),
  worst = load_or_compute_wrapper(
    "Results/MFSV/fsvsample_worst.RData", 
    compute_fsv, 
    worst_params
    )
)


# =====================
# CONVERGENCE CHECKS
# =====================
check_convergence()


# =====================
# PREDICTIONS
# =====================
prediction_results <- list(
  best = load_or_compute_wrapper(
    "Results/MFSV/predictions_best.RData",
    compute_predictions_wrapper,
    config$prediction$steps,
    fsv_results$best$fsv_data),
  worst = load_or_compute_wrapper(
    "Results/MFSV/predictions_worst.RData",
    compute_predictions_wrapper,
    config$prediction$steps,
    fsv_results$worst$fsv_data)
)


# =====================
# VISUALIZATION
# =====================
plot_test()
