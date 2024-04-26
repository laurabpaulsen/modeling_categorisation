library(pacman)
pacman::p_load(tidyverse, cmdstanr)

extract_stimulus_features <- function(stimulus) {
    ## takes a stimulus string (xxxxx.jpg) and returns a vector of the features
    
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
    return (responses)
}

# load data from AlienData.txt
data <- read.table("data/AlienData.txt", sep = ",", header = TRUE)

# only keep data from session 1 (so we have the same rule for danger and nutritious as in the simulated data)
data <- data %>% filter(session == 1)

# just as a test fit a model on the first participant's data
subjects <- unique(data$subject)


for (s in subjects) {
    subject_data <- data %>% filter(subject == s)
    category <- subject_data$dangerous
  
    # extract the features from the stimuli strings
    features <- subject_data$stimulus %>% map(extract_stimulus_features) 
    features <- do.call(rbind, features)   # convert the list of vectors to a matrix
  
    # Load the Stan model
    model <- cmdstan_model("GCM_categorisation.stan")
    
    # Run the Stan model using the cmdstanr 'sample' method
    fit <- model$sample(
        data = list(
            ntrials = nrow(subject_data),                       # number of stimuli/trials
            nfeatures = 5,                                      # number of features per stimulus
            cat_one = subject_data$dangerous,                   # binary values indicating dangerous (1) or not dangerous (0)
            y = extract_danger_response(subject_data$response), # decision outcomes from participants
            obs = features,                                     # Features matrix
            b = 0.5,                                            # Initial bias for cat one over two, adjust maybe?
            w_prior_values = rep(1,5),                          # uniform prior for the feature weights for now
            c_prior_values = c(0,1)                             # m and sd for the scaling parameter
        ),
        
        chains = 4,
        parallel_chains = 4,
        iter_warmup = 1000,
        iter_sampling = 1000,
        refresh = 0  # Set to 0 to not print progress
    )
    
    # Save the posterior samples from the Stan model fit
    fit$save_object(file = paste0("fits/model_fit_participant_", s, ".rds"))
    
    # Save a summary of the fit to CSV
    fit_summary <- fit$summary()
    write.csv(fit_summary, file = paste0("fits/fit_summary_participant_", s, ".csv"))
    
    # Print a message indicating completion of this iteration
    cat("Finished fitting model for participant =", s, "\n")
}
