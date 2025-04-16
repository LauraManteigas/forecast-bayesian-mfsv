#' Run FSV Sampler with Specified Prior Parameters
#'
#' Uses the `fsvsample` function to run Bayesian estimation of a MFSV model.
#'
#' @param B_mu Scalar prior for mean of log-volatility innovations
#' @param a_0 Shape parameter of Beta prior for persistence
#' @param b_0 Shape parameter of Beta prior for persistence
#' @param B_sigma Prior variance of idiosyncratic and factor volatilities
#' @param a_i Prior for factor loadings
#' @param c NG prior shape
#' @param d NG prior rate
#' @return An object of class `fsvdraws` containing posterior samples
calculate_fsv_sample <- function(B_mu, a_0, b_0, B_sigma, a_i, c, d) {
  fsvsample(
    zoo(data[stage == "train", -c("stage", "date")], order.by = data$date),
    
    zeromean = TRUE,
    
    factors = config$mcmc$q, 
    draws = config$mcmc$draws, 
    thin = config$mcmc$thin,
    burnin = config$mcmc$burnin,
    
    priormu = c(0, B_mu),
    
    priorsigmaidi = B_sigma,
    priorsigmafac = B_sigma,
    
    priorphiidi = c(a_0, b_0),
    priorphifac = c(a_0, b_0),
    
    restrict = "none",
    priorfacloadtype = "rowwiseng",
    priorfacload = a_i,
    priorng = c(c, d),
    
    interweaving = 4,
    
    quiet = TRUE
  )
}


#' Compute Effective Sample Size (ESS) of Posterior Covariance Estimates
#'
#' Applies ESSS calculation to posterior draws of covariance matrices.
#'
#' @param cov_fsv_data 3D array of posterior covariance matrices
#' @return A matrix of effective sample sizes with named rows and columns
compute_ess <- function(cov_fsv_data){
  ess <- round(apply(cov_fsv_data, 1:2, coda::effectiveSize))
  colnames(ess) <- config$symbols
  rownames(ess) <- config$symbols
  return(ess)
}


#' Compute Log-Determinant of Covariance Matrices
#'
#' Computes the log-determinant for each series' draw of covariance matrices.
#'
#' @param cov_matrix 4D array of covariance matrices over time
#' @return A numeric vector of log-determinants
compute_log_determinant <- function(cov_matrix) {
  apply(cov_matrix[,,,1], 3, function(x) log(det(x)))
}


#' Predict Future Returns from FSV Sample
#'
#' Generates predictive draws from a fitted model for a specified step ahead.
#'
#' @param days_ahead Integer number of steps ahead to predict
#' @param fsv_sample Fitted MFSV model object
#' @return A matrix of simulated returns for each series
predict_steps_ahead <- function(days_ahead, fsv_sample) {
  predobj <- predcond(fsv_sample, ahead = days_ahead, each = 1)
  means_vec <- as.vector(predobj$means[,,1])
  vols_vec <- as.vector(predobj$vols[,,1])
  t(matrix(rnorm(length(means_vec), mean = means_vec, sd = vols_vec), nrow = 5))
}


#' Generate Multi-Step Predictions from MFSV Model
#'
#' Runs predictions from the MFSV model for a sequence of future days.
#'
#' @param days_ahead Number of prediction steps
#' @param fsv_sample Fitted MFSV model object
#' @return A list of matrices with predictive draws per step
compute_predictions <- function(days_ahead, fsv_sample) {
  lapply(seq_len(days_ahead), predict_steps_ahead, fsv_sample)
}


#' Compute Highest Density Intervals (HDI) for Predictions
#'
#' Converts raw predictive draws into lower and upper bounds of HDI.
#'
#' @param data Full dataset containing test period dates
#' @param pred_draws_data List of predictive draws from `compute_predictions`
#' @return A `data.table` with HDI bounds, series, and associated dates
calculate_hdi_intervals <- function(data, pred_draws_data) {
  test_dates <- data[stage == "test", date]
  num_days <- config$prediction$steps

  total <- num_days * m
  lower <- numeric(total)
  upper <- numeric(total)
  series <- rep(factor(config$symbols), times = num_days)
  date <- rep(test_dates, each = m)
  
  idx <- 1
  for (day in seq_len(num_days)) {
    mat <- pred_draws_data[[day]]
    for (j in seq_len(m)) {
      x <- mat[, j]
      n <- length(x)
      sorted_x <- sort(x)
      interval_idx <- floor(0.95 * n)
      n_intervals <- n - interval_idx
      widths <- sorted_x[(interval_idx + 1):n] - sorted_x[1:n_intervals]
      min_idx <- which.min(widths)
      lower[idx] <- sorted_x[min_idx]
      upper[idx] <- sorted_x[min_idx + interval_idx]
      idx <- idx + 1
    }
  }

  data.table(
    date = date,
    series = series,
    lower = lower,
    upper = upper
  )
}


#' Compare HDI Predictions with Real Returns
#'
#' Merges HDI intervals with actual observed returns for test data.
#'
#' @param data Full dataset including test returns
#' @param pred_draws_data Predictive draws from the MFSV model
#' @return A `data.table` with real values and prediction intervals
compare_predicted_real <- function(data, pred_draws_data) {
  final <- calculate_hdi_intervals(data, pred_draws_data)
  test_data <- data[stage == "test", !"stage"]
  
  real_returns <- melt.data.table(
    test_data,
    id.vars = "date",
    variable.name = "series",
    value.name = "real"
  )
  
  merged <- final[real_returns, on = .(date, series)]
  
  return(merged[])
}


# =============================================================================
# -----------------------------------------------------------------------------
# PLOTS
# -----------------------------------------------------------------------------
# =============================================================================

#' Plot Real Returns and Prediction Bands for Best and Worst Configurations
#'
#' Overlays prediction intervals and real values for both best and worst models.
#'
#' @param realpred_best Output of `compare_predicted_real` for best model
#' @param realpred_worst Output of `compare_predicted_real` for worst model
#' @return A ggplot object showing prediction bands and actual values
realpred_best_worst <- function(realpred_best, realpred_worst) {
  colnames(realpred_worst) <- c("date", "series", "lowerworst", "upperworst", "real")
  colnames(realpred_best) <- c("date", "series", "lowerbest", "upperbest", "real")
  
  all <- cbind(
    realpred_worst[, c("date", "series", "lowerworst", "upperworst")], 
    realpred_best[, -c("date", "series")]
  )
  
  ggplot(all, aes(x = date)) +
    geom_ribbon(aes(ymin = lowerworst, ymax = upperworst), fill = "red", 
                linetype = "dashed", 
                color = "red", linewidth = 0.8, alpha = 0.1) +
    geom_ribbon(aes(ymin = lowerbest, ymax = upperbest), fill = "blue", 
                linetype = "dashed", 
                color = "blue", linewidth = 0.8, alpha = 0.1) +
    geom_point(aes(y = real), size = 2) +
    geom_line(aes(y = real)) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~series, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.5),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(1, "cm"),
          axis.title.y = element_text(vjust = -15))
}


#' Plot Log Determinants for Best and Worst Configurations
#'
#' Visual comparison of log-determinant time series from best and worst FSV outputs.
#'
#' @param logdet_best Numeric vector of log determinants (best config)
#' @param logdet_worst Numeric vector of log determinants (worst config)
#' @return A ggplot object with overlaid log determinant series
logdet_best_worst <- function(logdet_best, logdet_worst) {
  x <- data.table(best = logdet_best, worst = logdet_worst)
  x[, index := .I]
  x_long <- melt(x, id.vars = "index")
  selected_rows <- x_long[index %% 10 == 1]
  ggplot(selected_rows, aes(x = index, y = value, color = factor(variable))) +
    geom_line(linewidth = 0.1, alpha = 0.3) +
    scale_color_manual(values=c('blue', 'red')) +
    labs(x = "Index", y = "Log Determinant") +
    theme(legend.position = "none")
}


#' Plot ACF for Best and Worst Log Determinant Sequences
#'
#' Compares autocorrelation of log determinants between best and worst configs.
#'
#' @param log_data_best Numeric vector of log determinants (best)
#' @param log_data_worst Numeric vector of log determinants (worst)
#' @return A ggplot object with ACF bar plots
acf_best_worst <- function(log_data_best, log_data_worst) {
  draws <- config$mcmc$draws
  thin <- config$mcmc$thin
  ci_value <- qnorm(0.975) / sqrt(draws / thin)
  
  compute_acf_dt <- function(log_data, label) {
    acf_res <- acf(log_data, plot = FALSE)
    data.table(lag = acf_res$lag[, , 1], acf = acf_res$acf[, , 1], group = label)
  }
  
  acf_long <- rbind(
    compute_acf_dt(log_data_best, "Best"),
    compute_acf_dt(log_data_worst, "Worst")
  )
  
  ggplot(acf_long, aes(x = lag, y = acf, fill = group)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_bar(position = "dodge", stat = "identity", alpha = 0.3) +
    labs(x = "Lag", y = "ACF", fill = NULL) +
    scale_fill_manual(values = c("Best" = "blue", "Worst" = "red")) +
    geom_hline(yintercept = c(-ci_value, ci_value), linetype = "dashed")
}


#' Extract and Prepare Factor Loadings for Pair Plot
#'
#' Samples factor loadings across MCMC draws and reshapes them for plotting.
#'
#' @param fsv_data Fitted FSV model object
#' @param maxpoints Max number of posterior draws to sample for plotting
#' @param alpha Transparency level for points
#' @return A tidy `data.table` with factor 1 and factor 2 loadings
replicate_facpairplot <- function(fsv_data, maxpoints = 500, alpha = 0.2){
  draws <- config$mcmc$draws
  q <- config$mcmc$q
  symbols <- config$symbols
  
  n <- nrow(fsv_data$y)
  
  plotthese <- sample.int(draws, min(draws, maxpoints))
  means <- apply(fsv_data$facload, 1:2, mean)
  whiches <- matrix((1:(2 * q) - 1)%%q + 1, nrow = 2)
  
  oldpal <- palette(rainbow(m))
  colas <- apply(
    sapply(palette(), col2rgb)/255, 
    2, 
    function(cc,alpha) rgb(cc[1], cc[2], cc[3], alpha = alpha), alpha)
  
  tmp <- aperm(fsv_data$facload[, whiches[, 1], plotthese], c(1, 3, 2))
  
  myxlims <- quantile(fsv_data$facload[, whiches[1, 1], plotthese], c(0.1/m, 1 - 0.1/m))
  myylims <- quantile(fsv_data$facload[, whiches[2, 1], plotthese], c(0.1/m, 1 - 0.1/m))
  
  x <- t(tmp[,,1])
  colnames(x) <- symbols
  x_long <- melt(data.table(x), id.vars = as.integer())
  x_long
  
  colnames(x_long) <- c("series", "x")
  
  y <- t(tmp[,,2])
  colnames(y) <- config$symbols
  y_long <- melt(data.table(y), id.vars = as.integer())
  colnames(y_long) <- c("series", "y")
  tmp <- data.table(cbind(x_long, y_long[,-"series"]))
  
  return(tmp)
}


#' Plot Pairwise Factor Loadings for Best and Worst Models
#'
#' Visualizes posterior means of first two factor loadings for comparison.
#'
#' @param fsv_data_best FSV model object (best)
#' @param fsv_data_worst FSV model object (worst)
#' @return A faceted ggplot of factor loadings with series labels
pairplot_best_worst <- function(fsv_data_best, fsv_data_worst){
  worst <- replicate_facpairplot(fsv_data_worst)
  worst[, config := factor("Worst")]
  best <- replicate_facpairplot(fsv_data_best)
  best[, config := factor("Best")]
  
  all <- rbind(best, worst)
  
  myxlims <- range(
    quantile(all[config == "Worst", x], c(0.1/m, 1 - 0.1/m)), 
    quantile(all[config == "Best", x], c(0.1/m, 1 - 0.1/m))
  )
  
  myylims <- range(
    quantile(all[config == "Worst", y], c(0.1/m, 1 - 0.1/m)), 
    quantile(all[config == "Best", y], c(0.1/m, 1 - 0.1/m))
  )
  
  means_best <- apply(fsv_data_best$facload, 1:2, mean)
  means_worst <- apply(fsv_data_worst$facload, 1:2, mean)
  
  labels_best <- data.table(
    x = means_best[, 1], 
    y = means_best[, 2], 
    series = config$symbols,
    config = "Best"
  )
  
  labels_worst <- data.table(
    x = means_worst[, 1], 
    y = means_worst[, 2], 
    series = config$symbols,
    config = "Worst"
  )
  
  labels_all <- rbind(labels_best, labels_worst)
  
  ggplot(all, aes(x = x, y = y, color = series)) +
    geom_point(alpha = 0.4, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Loadings on Factor 1", y = "Loadings on Factor 2") +
    facet_wrap(~ config) +
    xlim(myxlims) +
    ylim(myylims) +
    geom_text(data = labels_all, 
              aes(x = x, y = y, label = series), 
              vjust = -0.5, 
              color = "black") +
    theme(legend.position = "none", panel.spacing.x = unit(1.5, "lines"))
}


#' Process Marginal Volatility Estimates from MFSV Model Output
#'
#' Extracts posterior mean volatilities and attaches model configuration label.
#'
#' @param fsv_data List containing MFSV model results (from `fsvsample`)
#' @param config_name Character string indicating the model configuration label 
#' (e.g. "Best", "Worst")
#' @return A long-format `data.table` with columns: date, series, value, config
process_volatility <- function(fsv_data, config_name) {
  symbols <- config$symbols
  dates <- index(fsv_data$y)
  vols <- as.data.table(fsv_data$runningstore$vol[, , "mean"])

  if (length(dates) != nrow(vols)) 
    stop(paste("Mismatch between dates and volatility data for", config_name))
  if (ncol(vols) != length(symbols)) 
    stop(paste("Mismatch in column names for", config_name))
  
  setnames(vols, symbols)
  vols[, date := dates]
  
  melt(
    vols, 
    id.vars = "date", 
    variable.name = "series", 
    value.name = "value")[, config := factor(config_name)]
}


#' Plot Posterior Mean Volatility for Best and Worst Configurations
#'
#' Combines volatilities from two model runs and compares via line plots by 
#' series.
#'
#' @param fsv_data_best FSV model output for best configuration
#' @param fsv_data_worst FSV model output for worst configuration
#' @return A ggplot object with faceted volatility time series
volatility_best_worst <- function(fsv_data_best, fsv_data_worst) {
  all <- rbindlist(list(
    process_volatility(fsv_data_best, "Best"),
    process_volatility(fsv_data_worst, "Worst")
  ))
  
  ggplot(all, aes(x = date, y = value, col = series)) +
    geom_line(linewidth = 0.8) + 
    labs(
      x = NULL, 
      y = "Volatilities",
      title = "Posterior Means of Daily Marginal Volatilities",
      color = NULL
    ) +
    facet_wrap(~config)
}


#' Process Log Variance Estimates from MFSV Model Output
#'
#' Extracts posterior log variance means and ±2 SD bounds (for CI ribbons).
#'
#' @param fsv_data FSV model results (from `fsvsample`)
#' @param labels Factor vector for labeling each variance series
#' @param config_name String indicating model configuration label
#' @return Long-format `data.table` with mean, lower, upper, date, var, and 
#' config
process_logvar <- function(fsv_data, labels, config_name) {
  logvar <- fsv_data$runningstore$logvar
  data.table(
    mean = as.vector(logvar[, , "mean"]),
    lower = as.vector(logvar[, , "mean"] - 2 * logvar[, , "sd"]),
    upper = as.vector(logvar[, , "mean"] + 2 * logvar[, , "sd"]),
    dates = rep(index(fsv_data$y), ncol(logvar)),
    var = rep(labels, each = nrow(logvar)),
    config = factor(config_name)
  )
}


#' Plot Log Variance with Confidence Intervals for Best/Worst Configs
#'
#' Combines log variance estimates from two configurations and visualizes them
#' with confidence ribbons and line plots.
#'
#' @param fsv_data_best MFSV model output for best configuration
#' @param fsv_data_worst MFSV model output for worst configuration
#' @return A faceted ggplot comparing mean log variances and uncertainty bands
logvars_best_worst <- function(fsv_data_best, fsv_data_worst) {
  q <- config$mcmc$q
  
  labels <- factor(c(
    sprintf("Series %d", 1:m),
    sprintf("Factor %d", (m + 1):(m + q))
  ))
  labels <- factor(labels, levels = labels)
  
  all <- rbindlist(list(
    process_logvar(fsv_data_best, labels, "Best"),
    process_logvar(fsv_data_worst, labels, "Worst")
  ))
  
  ggplot(all, aes(dates)) +
    geom_line(aes(y = mean)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    facet_grid(var ~ config, scales = "free") +
    labs(x = "Date", y = "Mean")
}

