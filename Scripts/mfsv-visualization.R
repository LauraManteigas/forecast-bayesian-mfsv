check_convergence <- function() {
  repeat {
    cat("\nWhich plot (Best VERSUS Worst) do you want to see?\n")
    cat("1: Trace plot of the log determinant.\n")
    cat("2: Autocorrelation function of the log determinant.\n")
    cat("E: Exit\n")
    
    # Read input and convert to upper case (to handle 'e' or 'E')
    choice <- toupper(trimws(readline("Select 1, 2, or E for exit: ")))
    
    if (choice == "E") {
      cat("Exiting...\n")
      break
    }
    
    if (choice == 1) {
      print(logdet_best_worst(fsv_results$best$log_det_fsv_data, fsv_results$worst$log_det_fsv_data))
    } else if (choice == 2) {
      print(acf_best_worst(fsv_results$best$log_det_fsv_data, fsv_results$worst$log_det_fsv_data))
    } else {
      cat("Invalid selection. Please choose 1, 2, or E.\n")
    }
    
    dev.flush() 
  }
}

plot_test <- function() {
  repeat {
    cat("\nWhich plot (Best VERSUS Worst) do you want to see?\n")
    cat("1: Bivariate marginal posterior distributions of factor loadings.\n")
    cat("2: Series-specific volatilities over time.\n")
    cat("3: Log-variances over time.\n")
    cat("4: Real vs Predicted.\n")
    cat("E: Exit\n")
    
    # Read input and convert to upper case (to handle 'e' or 'E')
    choice <- toupper(trimws(readline("Select 1, 2, 3, 4, or E for exit: ")))
    
    if (choice == "E") {
      cat("Exiting...\n")
      break
    }
    
    if (choice == "1") {
      print(pairplot_best_worst(fsv_results$best$fsv_data, fsv_results$worst$fsv_data))
    } else if (choice == "2") {
      print(volatility_best_worst(fsv_results$best$fsv_data, fsv_results$worst$fsv_data))
    } else if (choice == "3") {
      print(logvars_best_worst(fsv_results$best$fsv_data, fsv_results$worst$fsv_data))
    } else if (choice == "4") {
      print(realpred_best_worst(prediction_results$best$real_and_hdi, prediction_results$worst$real_and_hdi))
    } else {
      cat("Invalid selection. Please choose 1, 2, 3, 4, or E for exit.\n")
    }
    
    dev.flush() 
  }
}
