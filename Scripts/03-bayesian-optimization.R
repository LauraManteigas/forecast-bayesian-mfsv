#' Evaluate Forecast Accuracy and Uncertainty
#'
#' Evaluate the forecast performance of a MFSV model. Computes coverage rate, 
#' sharpness, and a weighted performance score per series.
#'
#' @param fsv_data A list of model outputs containing posterior samples.
#' @return A `data.table` with one row per series, containing:
#'   \item{coverage_rate}{Proportion of true values within prediction intervals}
#'   \item{sharpness}{Average interval width (lower = more confident)}
#'   \item{score}{Weighted performance score (lower = better)}
evaluate_model <- function(fsv_data) {
  steps <- config$prediction$steps
  wc <- config$weights$wc
  ws <- config$weights$ws
  alpha <- config$prediction$alpha
  smax <- config$prediction$smax

  pred_draws <- compute_predictions(steps, fsv_data)

  coverage <- compare_predicted_real(data, pred_draws)
  
  coverage[, coverage := (real >= lower & real <= upper)]
  
  metrics <- coverage[, .(
    coverage_rate = mean(coverage),
    sharpness = mean(upper - lower)
  ), by = series]
  
  metrics[, score := wc * log(1 + exp(coverage_rate - (1-alpha))) +
            ws * log(1 + exp(sharpness - smax)), 
          by = series]
  
  return(metrics)
}


#' Objective Function for Hyperparameter Optimization
#'
#' Wraps MFSV model training and evaluation into a single scalar loss function
#' used for optimization.
#'
#' @param B_mu Mean of the prior on factor loadings.
#' @param a_0, b_0 Parameters for the prior on volatility.
#' @param B_sigma Scale of prior on idiosyncratic volatilities.
#' @param a_i Parameter controlling idiosyncratic volatility evolution.
#' @param c, d Parameters for shrinkage priors.
#' @return A named list with one element, `Score`, where lower is better.
objective_function <- function(B_mu, a_0, b_0, B_sigma, a_i, c, d) {
  model <- calculate_fsv_sample(B_mu, a_0, b_0, B_sigma, a_i, c, d)
  results <- evaluate_model(model)
  return(list(Score = -log(sum(exp(results$score)))))
}


#' Run Bayesian Optimization on Model Hyperparameters
#'
#' Performs Bayesian optimization to find the best hyperparameters for the MFSV 
#' model.
#'
#' @param search_space A named list with lower and upper bounds for each 
#' hyperparameter.
#' @param initPoints Number of initial random evaluations.
#' @param iters.n Number of optimization iterations.
#' @param file_path Path to save/load optimization results.
#' @return A `data.table` of optimized hyperparameters sorted by score.
#'
#' @details If results already exist at `file_path`, they are loaded instead of 
#' re-running.
run_bayesian_optimization <- function(
    search_space, 
    initPoints = 70, 
    iters.n = 50, 
    file_path = "Results/bayesian_optimization.csv") {
  
  if (file.exists(file_path)) {
    cat("Bayesian optimization results found. Loading from file...\n")
    return(fread(file_path))
  }
  
  cat("\nRunning Bayesian Optimization...\n")
  
  opt_result <- bayesOpt(
    FUN = function(B_mu, a_0, b_0, B_sigma, a_i, c, d) {
      cat(sprintf("\nEvaluating (B_mu = %.2f, a_0 = %.2f, B_sigma = %.2f, a_i = %.2f)", 
                  B_mu, a_0, B_sigma, a_i))
      return(objective_function(B_mu, a_0, b_0, B_sigma, a_i, c, d))
    },
    bounds = search_space,
    initPoints = initPoints,
    iters.n = iters.n,
    acq = "ei"
  )
  
  score_summary <- opt_result$scoreSummary[, .(B_mu, a_0, b_0, B_sigma, a_i, c, d, Score)][order(-Score), ]
  
  fwrite(score_summary, file_path, row.names = FALSE)
  cat("\nResults saved to:", file_path, "\n")
  
  return(score_summary)
}


#' Fit GAM to Hyperparameter Optimization Results
#'
#' Fits a Generalized Additive Model (GAM) to assess nonlinear effects of 
#' hyperparameters on model performance.
#'
#' @param score_summary A `data.table` of parameter values and associated scores.
#' @return A GAM summary object (useful for interpretation and visualization).
gam_params <- function(score_summary){
  gam_model <- gam(Score ~ s(B_mu) + s(a_0) + s(b_0) + s(B_sigma) + 
                   s(a_i) + s(c) + s(d) + ti(a_0, b_0) + ti(c,d),
                 data = score_summary, method = "REML")

  return(summary(gam_model))
}
