# This script runs the simple bayes model on the data from 
# Simonsen et al and saves the output

pacman::p_load(cmdstanr, tidyverse)

c_values <- seq(0.1, 1, 0.2)

for (c in c_values){
    # setup experimental design
    data <- readRDS(paste0("data/c_", c, ".rds"))

    n_trials <- nrow(data)
    attributes <- data %>% select(c("Var1", "Var2", "Var3", "Var4", "Var5"))
    categories <- data$danger
    choices <- data$choice_danger

    model <- cmdstan_model("GCM_categorisation.stan")

    fit <- model$sample(
        data = list(
            N = n_trials,
            attributes = as.matrix(attributes),
            category = categories,
            choices = choices
        ),
        chains = 4,
        iter_warmup = 1000,
        iter_sampling = 1000
    )

}


"""



first_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
group_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)
second_rating_matrix <- matrix(NA, nrow = n_trials, ncol = n_subjects)

# Fill in the matrices with ratings
for (i in 1:length(unique_subjects)) {
  subject_data <- data[data$ID == unique_subjects[i], ]
  first_rating_matrix[, i] <- subject_data$FirstRating
  group_rating_matrix[, i] <- subject_data$GroupRating
  second_rating_matrix[, i] <- subject_data$SecondRating
}


simple_betabayes <- cmdstan_model()

fit <- simple_betabayes$sample(
  data = list(N = n_trials,
              N_subj = n_subjects,
              lower_bound = 1,
              upper_bound = 8,
              first_rating = first_rating_matrix,
              group_rating = group_rating_matrix,
              second_rating = second_rating_matrix
  ),
  chains = 4,
  thin = 2,
  iter_warmup = 1000
)

# save the posterior
fit$save_object(file = )

"""