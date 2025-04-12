# =====================
# SETUP SCRIPT
# =====================

# Clear environment
rm(list = ls())

# Load required packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  ggplot2, tidyr, dplyr, data.table, zoo, factorstochvol, stringr,
  IntroCompFinR, quadprog, tidyverse, bayestestR, gridExtra,
  e1071, ParBayesianOptimization, tseries, mgcv
)

# Set ggplot theme
theme_set(theme_minimal() + theme(legend.position = 'bottom'))

# Set random seed
set.seed(1)

# Function to source multiple R scripts
source_scripts <- function(paths) {
  invisible(lapply(paths, source))
}

source_scripts(c(
  "Scripts/config.R",
  "Scripts/00-data-loading.R",
  "Scripts/01-data-visualization.R",
  "Scripts/02-mfsv.R",
  "Scripts/03-bayesian-optimization.R",
  "Scripts/04-save-results.R",
  "Scripts/05-mfsv-visualization.R"
))
