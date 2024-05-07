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
source("simulation_params.R") # loads the c values and lists of weights (so that we do not have to change in all scripts)


pacman::p_load(cmdstanr,
               tidyverse)


skip_fitted <- F

n_trials = "96_trials"


# create output folder
output_dir <- paste0("fits/", n_trials)
cmdst_output_dir <- paste0("fits/", n_trials, "/cmdstan")
dir.create(output_dir, showWarnings = FALSE)
dir.create(cmdst_output_dir, showWarnings = FALSE)

# Load the Stan model
Sys.setenv(CXXFLAGS = "-flarge-source-files -Ofast")
model <- cmdstan_model(
    "GCM_categorisation.stan",
    stanc_options = list(
        "O1"
    ),
    cpp_options = list(stan_opencl = TRUE),
    compile_model_methods = FALSE
)

# Looping over each 'c' value to fit the model on the respective simulated dataset
for (c in c_values) {
  # Read the simulated data from the appropriate RDS file
  for (w in list_of_weights) {
    output_basename <- paste0("model_fit_c_", c, "_w_", paste0(w, collapse = "_"))
    fit_file <- paste0(output_dir, "/", output_basename, ".rds")
    
    if (skip_fitted && file.exists(fit_file)) {
      cat("Model fit for c =", c, "and w =", w, "already exists, skipping...\n")
      next
    }

    data <- readRDS(paste0("data/", n_trials, "/c_", c, "_w_", paste0(w, collapse = "_"), ".rds"))
  
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
      output_dir = cmdst_output_dir,
      output_basename = output_basename,
      chains = 4,
      parallel_chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      refresh = 0  # Set to 0 to not print progress
    )
    
    # Save the posterior samples from the Stan model fit
    fit$save_object(file = fit_file)
    fit$save_data_file(dir = cmdst_output_dir, basename = output_basename)

    cat("Calculating LOO for c =", c, "and w =", w, "\n")
    loo_result <- fit$loo(save_psis = TRUE, cores=4, is_method="psis")
    # save the loo result
    saveRDS(loo_result, file = paste0(output_dir, "/", output_basename, "_loo.rds"))
    
    # Print a message indicating completion of this iteration
    cat("Finished fitting model for c =", c, "\n", "and w = ", w, "\n")

  }
}
