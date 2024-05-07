library(pacman)
pacman::p_load(tidyverse, cmdstanr)


skip_fitted <- TRUE
exp_session <- 1

extract_stimulus_features <- function(stimulus) {
  # takes a stimulus string (xxxxx.jpg) and returns a vector of the features

  # remove the file extension
  stimulus <- gsub(".jpg", "", stimulus)

  # remove pt
  stimulus <- gsub("pt", "", stimulus)

  # split the string into a vector of characters
  stimulus <- strsplit(stimulus, "")[[1]]

  # convert the characters to integers
  stimulus <- as.integer(stimulus)

  return(stimulus)
}

extract_danger_response <- function(response) {
  ## takes a response int (1, 2, 3, or 4) and returns a binary value indicating dangerous (1) or not dangerous (0)
  # CHECK HOW THIS SHOULD BE!!!! Which numbers are actually dangerous?
  responses <- c()

  for (i in 1:length(response)) {
    if (response[i] == 3 || response[i] == 4) {
      responses <- c(responses, 1)
    } else {
      responses <- c(responses, 0)
    }
  }
  return(responses)
}

if (!file.exists("data/empirical_data_updated_subj_ids.rds")) {
  # load data from AlienData.txt
  data <- read.table("data/AlienData.txt", sep = ",", header = TRUE)

  # only keep data from session 1 (so we have the same rule for danger and nutritious as in the simulated data)
  data <- data %>% filter(session == exp_session)
  max_cond_1_subj_id <- max(data[data$condition == 1, ]$subject)
  # subject ids are repeated between condition 1 and condition 2
  # but in condition 1 they are dyads, and in condition 2 they are individuals
  # so let's make the subject ids continue from condition 1 to condition 2
  data[data$condition == 2, ]$subject <- data[data$condition == 2, ]$subject + max_cond_1_subj_id

  # save the data
  saveRDS(data, file = "data/empirical_data_updated_subj_ids.rds")
} else {
  data <- readRDS("data/empirical_data_updated_subj_ids.rds")
}

# just as a test fit a model on the first participant's data
subjects <- unique(data$subject)

# Load the Stan model
Sys.setenv(CXXFLAGS = "-flarge-source-files -Ofast")
model <- cmdstan_model(
  "GCM_categorisation.stan",
  stanc_options = list(
    "O1"
  ),
  # opencl isn't faster now that the number of trials is halved (more time spent on copying data to GPU than on computation)
  # cpp_options = list(stan_opencl = TRUE),
  compile_model_methods = TRUE
)

output_dir <- paste0("fits/empirical_session_", exp_session)
cmdst_output_dir <- paste0(output_dir, "/cmdstan")
dir.create(output_dir, showWarnings = FALSE)
dir.create(cmdst_output_dir, showWarnings = FALSE)

for (s in subjects) {
  output_basename <- paste0("participant_", s)
  fit_file <- paste0(output_dir, "/", output_basename, "_fit.rds")
  fit_summary_csv <- paste0(output_dir, "/", output_basename, "_fit_summary.csv")


  if (skip_fitted && file.exists(fit_file)) {
    cat("Model fit for participant =", s, "already exists, skipping...\n")
    next
  }

  subject_data <- data %>% filter(subject == s)
  category <- subject_data$dangerous

  # extract the features from the stimuli strings
  features <- subject_data$stimulus %>% purrr::map(extract_stimulus_features)
  features <- do.call(rbind, features) # convert the list of vectors to a matrix

  # Run the Stan model using the cmdstanr 'sample' method
  fit <- model$sample(
    data = list(
      ntrials = nrow(subject_data), # number of stimuli/trials
      nfeatures = 5, # number of features per stimulus
      cat_one = subject_data$dangerous, # binary values indicating dangerous (1) or not dangerous (0)
      y = extract_danger_response(subject_data$response), # decision outcomes from participants
      obs = features, # Features matrix
      b = 0.5, # Initial bias for cat one over two, adjust maybe?
      w_prior_values = rep(1, 5), # uniform prior for the feature weights for now
      c_prior_values = c(0, 1) # m and sd for the scaling parameter
    ),
    output_dir = cmdst_output_dir,
    output_basename = output_basename,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    refresh = 0, # Set to 0 to not print progress
    sig_figs = 18 # set to max to avoid simplex not summing to 1 error
  )

  # Save the posterior samples from the Stan model fit
  fit$save_object(file = fit_file)

  # Save a summary of the fit to CSV
  fit_summary <- fit$summary()
  write.csv(fit_summary, file = fit_summary_csv)

  cat("Calculating LOO for participant =", s, "\n")
  loo_output <- fit$loo(save_psis = TRUE, cores = 4, is_method = "psis")
  # save the loo output to a file
  saveRDS(loo_output, file = paste0(output_dir, "/", output_basename, "_loo.rds"))

  # Print a message indicating completion of this iteration
  cat("Finished fitting model for participant =", s, "\n")
}
