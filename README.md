# Bayesian-tuned MFSV forecasting
This code accompanies the article "Bayesian-Tuned Multivariate Factor Stochastic Volatility Model: Forecasting Amid Geopolitical Crisis".

It integrates Bayesian optimization into a Multivariate Factor Stochastic Volatility (MFSV) model to improve 50-day-ahead forecasts for five energy futures:
+ Brent
+ WTI
+ Dubai Crude
+ Gasoline
+ Heating Oil (HO)

The framework fine-tunes hyperparameters using an objective function based on Highest Density Interval (HDI) widths. Applied during a geopolitical crisis, the model achieves stable covariance predictions with coverage near the nominal 95% level. However, sensitivity analyses reveal that not all parameters significantly impact predictive accuracy. While the model captures overall volatility trends, wide prediction intervals limit precision in extreme market conditions.


## Project Structure
```bash
├── Data/                              # Input data files (CSV format)                    
├── Results/                           # Model outputs and diagnostics
│   ├── MFSV/                          # Outputs specific to MFSV
├── Scripts/
│   ├── config.R                       # Model configuration and parameters               
│   ├── setup.R                        # Package loading and environment setup          
│   ├── 00-data-loading.R              # Data reading and preprocessing
│   ├── 01-data-visualization.R        # Data plot highlighting train/test
│   ├── 02-mfsv.R                      # MFSV model estimation  
│   ├── 03-bayesian-optimization.R     # Hyperparameter tuning via Bayesian Optimization
│   ├── 04-save-results.R              # Save results and intermediate outputs   
│   └── 05-mfsv-visualization.R        # Forecast and volatility plots
├── main.R                             # Main workflow script                   
└── README.md     
```

## Notes
+ Hyperparameters, forecast horizon, and model settings can be adjusted in `Scripts/config.R`.
+ The random seed is fixed for reproducibility.
+ The `Results/` folder contains the forecast outputs.
  + If result files already exist, the scripts will load them automatically.
  + If not, the model will generate and save them into this folder during execution.
+ The `Results/` directory is created after the first run of the code. The same applies to `Results/MFSV/`.
