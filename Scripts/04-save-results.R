#' Load or Compute Data
#' 
#' Loads data from a file if it exists, or computes it using a provided function
#' and saves the result.
#' @param file_path Path to the file to load or save data.
#' @param compute_func Function used to compute data if the file does not exist.
#' @param ... Additional arguments passed to `compute_func`.
#' @return A list of computed data if the file doesn't exist, or loaded data 
#' from the file if it does.
load_or_compute <- function(file_path, compute_func, ...) {
  dir.create("Results", showWarnings = FALSE, recursive = TRUE)
  if (!grepl("^Results/", file_path)) {
    file_path <- file.path("Results", file_path)
  }
  
  if (file.exists(file_path)) {
    tryCatch({
      env <- new.env()
      load(file_path, envir = env)
      cat("Loaded existing data from:", file_path, "\n")
      return(as.list(env))
    }, error = function(e) {
      stop("Failed to load the file: ", e$message)
    })
  } 
  
  cat("Computing data and saving to:", file_path, "\n")
  
  computed_data <- compute_func(...)
  
  if (!is.list(computed_data)) {
    stop("compute_func must return a list of named values.")
  }
  
  save(list = names(computed_data), 
       file = file_path, 
       envir = list2env(computed_data))
  
  return(computed_data)
}


#' Compute MFSV Model Data
#' 
#' Computes the MFSV model data, including the covariance and log-determinant.
#' @param params List of parameters to be passed to the `calculate_fsv_sample` 
#' function.
#' @return A list containing `fsv_data`, `cov_fsv_data`, and `log_det_fsv_data`.
compute_fsv <- function(params) {
  fsv_data <- do.call(calculate_fsv_sample, params)
  cov_fsv_data <- covmat(fsv_data)
  log_det_fsv_data <- compute_log_determinant(cov_fsv_data)
  
  list(fsv_data = fsv_data, cov_fsv_data = cov_fsv_data, log_det_fsv_data = log_det_fsv_data)
}


#' Wrapper for Compute Predictions
#' 
#' Computes predictions using the FSV data and compares predicted versus real 
#' values.
#' 
#' @param steps Number of steps to predict.
#' @param fsv_data Data resulting from the `compute_fsv` function.
#' @return A list containing `pred_draws` and `real_and_hdi` (comparison of 
#' predictions and real values).
compute_predictions_wrapper <- function(steps, fsv_data) {
  pred_draws <- compute_predictions(steps, fsv_data)
  real_and_hdi <- compare_predicted_real(data, pred_draws)
  
  list(pred_draws = pred_draws, real_and_hdi = real_and_hdi)
}


#' Wrapper for Load or Compute Data
#' 
#' A wrapper around `load_or_compute` to simplify usage.
#' 
#' @param path Path to the file to load or save data.
#' @param compute_function Function used to compute data if the file doesn't 
#' exist.
#' @param ... Additional arguments passed to `load_or_compute`.
#' @return The result of the `load_or_compute` function.
load_or_compute_wrapper <- function(path, compute_function, ...) {
  load_or_compute(path, compute_function, ...)
}
