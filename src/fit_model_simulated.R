# This script runs the simple bayes model on the data from 
# Simonsen et al and saves the output.
# Assumes the presence of a Stan model file called "GCM_categorisation.stan".
# Reads in the simulated data files created by simulate_data.R.
# Fits the GCM Stan model to the simulated data and runs the Bayesian inference process.

# create a model fit folder
dir.create("fits")

# Ensuring the cmdstanr packages are loaded
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages("cmdstanr")
}

library(pacman)

pacman::p_load(cmdstanr,
               tidyverse)

# Defining the sequence of 'c' parameter values for which the model should be run
c_values <- seq(0.1, 1, 0.2)


# Looping over each 'c' value to fit the model on the respective simulated dataset
for (c in c_values) {
  # Read the simulated data from the appropriate RDS file
  data <- readRDS(paste0("data/c_", c, ".rds"))
  
  # -- Commenting out for now, doing it below directly in fit code -- 
  
  # Extract the number of trials, the stimulus attributes, categories, and choices
  #n_trials <- nrow(data)
  #attributes <- as.matrix(data %>% select(Var1, Var2, Var3, Var4, Var5))
  #categories <- data$danger
  #choices <- data$choice_danger
  
  # Load the Stan model
  model <- cmdstan_model("GCM_categorisation.stan")
  
  # Run the Stan model using the cmdstanr 'sample' method
  fit <- model$sample(
    data = list(
      # Not modeling nutritious right now, just doing danger
      ntrials = nrow(data),       # number of stimuli/trials
      nfeatures = 5,              # number of features per stimulus
      cat_one = data$danger,      # binary values indicating dangerous (1) or not dangerous (0)
      y = data$choice_danger,     # decision outcomes from participants
      obs = as.matrix(data %>% select(Var1,
                                       Var2,
                                       Var3,
                                       Var4,
                                       Var5)), # Features matrix
      b = 0.5, # Initial bias for cat one over two, adjust maybe?
      w_prior_values = rep(1,5), # uniform prior for the feature weights for now
      c_prior_values = c(0,1) # m and sd for the scaling parameter
    ),
    
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    refresh = 0  # Set to 0 to not print progress
  )
  
  # Save the posterior samples from the Stan model fit
  fit$save_object(file = paste0("fits/model_fit_c_", c, ".rds"))
  
  # Save a summary of the fit to CSV
  fit_summary <- fit$summary()
  write.csv(fit_summary, file = paste0("fits/fit_summary_c_", c, ".csv"))
  
  # Print a message indicating completion of this iteration
  cat("Finished fitting model for c =", c, "\n")
}
