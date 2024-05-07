
library(pacman)
pacman::p_load(posterior, ggplot2, gridExtra, tidyverse, ggridges, loo)

dir.create("fig", showWarnings = FALSE)
source("simulation_params.R") # loads the c values and lists of weights (so that we do not have to change in all scripts)

fit <- readRDS("fits/96_trials/model_fit_c_1_w_0.2_0.2_0.2_0.2_0.2.rds")
data <- readRDS("data/96_trials/c_1_w_0.2_0.2_0.2_0.2_0.2.rds")
fit$loo()
# Extract the relevant parameters from the fit object
# For this model, you need 'w' and 'logit_c'
w <- fit$draws("w")  # Attention weights
logit_c <- fit$draws("logit_c")  # Logit of the scaling parameter
logit_c <- matrix(logit_c, nrow = nrow(logit_c), ncol = ncol(logit_c)[1])

# Extract the observed data
y <- data$choice_danger  # Binary decisions on a trial-by-trial basis
obs <- as.matrix(data %>% select(Var1,
                                        Var2,
                                        Var3,
                                        Var4,
                                        Var5))  # Stimuli features

# Define the number of trials and features
ntrials <- nrow(obs)
nfeatures <- ncol(obs)

# Initialize log likelihood vector
log_likelihood <- numeric(nrow(lp))
print(dim(w))
print(dim(obs))
# Loop over each iteration and chain of the MCMC
# Loop over each iteration and chain of the MCMC
for (chain in 1:1000) {
  for (iter in 1:4) {
    # Calculate r for each trial
    r <- rep(0, ntrials)
    for (j in 1:ntrials) {
      # Calculate exemplar similarities
      exemplar_sim <- rep(0, j - 1)
      for (e in 1:(j - 1)) {
        tmp_dist <- rep(0, nfeatures)
        for (k in 1:nfeatures) {
          print(c(iter, chain, k))  # Print the current indices for debugging
          print(w[chain, iter, k])
          if (k <= ncol(obs)) {
            print(obs[e, k])
            print(obs[j, k])
            tmp_dist[k] <- w[chain, iter, k] * abs(obs[e, k] - obs[j, k])
          }
        }
        exemplar_sim[e] <- exp(-inv_logit(logit_c[iter, chain]) * sum(tmp_dist))
      }
      
      # Calculate r[i] based on exemplar similarities
      if (sum(y[1:(j - 1)]) == 0 || sum(y[1:(j - 1)]) == (j - 1)) {
        r[j] <- 0.5
      } else {
        cat_one_idx <- which(y[1:(j - 1)] == 1)
        cat_two_idx <- which(y[1:(j - 1)] == 0)
        similarities <- c(mean(exemplar_sim[cat_one_idx]), mean(exemplar_sim[cat_two_idx]))
        rr <- (fit$data$b * similarities[1]) / (fit$data$b * similarities[1] + (1 - fit$data$b) * similarities[2])
        rr <- pmax(pmin(rr, 0.9999), 0.0001)  # Ensure rr stays within [0.0001, 0.9999] range
        r[j] <- rr
      }
    }
    
    # Compute the log likelihood using the Bernoulli likelihood function
    log_likelihood[(chain - 1) * fit$iter() + iter] <- sum(log(ifelse(y == 1, r, 1 - r)))
  }
}

# Total log likelihood across all iterations
total_log_likelihood <- sum(log_likelihood)













trials <- "320_trials"
dataframe <- tibble()

for (c in c_values) {
  for (w in list_of_weights) {
      # Read the simulated data from the appropriate rds file
      fit <- readRDS(paste0("fits/", trials,"/model_fit_c_", c, "_w_", paste0(w, collapse = "_"), ".rds"))

      # extract the posterior
      posterior <- fit$draws()

      fit_df <- as_draws_df(posterior)

      fit_df <- fit_df %>%
        rename("posterior_w1" = "posterior_w[1]",
               "posterior_w2" = "posterior_w[2]",
               "posterior_w3" = "posterior_w[3]",
               "posterior_w4" = "posterior_w[4]",
               "posterior_w5" = "posterior_w[5]")
      
      fit_df$c <- c
      fit_df$c <- as.factor(fit_df$c)
      fit_df$w1 <- w[1]
      fit_df$w2 <- w[2]
      fit_df$w3 <- w[3]
      fit_df$w4 <- w[4]
      fit_df$w5 <- w[5]
      fit_df$log_lik <- log_lik

      # Columns to convert to factors
      columns <- c("c", "w1", "w2", "w3", "w4", "w5")

      # Convert columns to factors using a loop
      for (col in columns) {
        fit_df[[col]] <- as.factor(fit_df[[col]])
      }
      
      fit_df$weights_string <- paste(w[1], w[2], w[3], w[4], w[5])

      dataframe <- bind_rows(dataframe, fit_df)
  }
}


ggplot(dataframe) +
  geom_density_ridges(aes(x = posterior_c, y = c, fill = c), alpha = 0.5) +
  # TODO add a line for the value of true c in each plot in the same fill color
  #geom_vline(xintercept = c_values, linetype = "dashed", color = "black", alpha = 0.5) +
  facet_wrap(~weights_string) +
  labs(title = "Posterior density for c",
        x = "Estimated c",
        y = "True c") +
  xlim(0, 5) +
  theme_bw()
  
ggsave(paste0("fig/", trials, "_posterior_density_c.png")) 



for (w in 1:5) {
  column_x = paste0("posterior_w", w)
  #print(dataframe$column_x)

  ggplot(dataframe) +
    geom_density_ridges(aes(x = !!sym(column_x), y = c, fill = c), alpha = 0.5) +
    # TODO add a line for the value of true w in each plot in the same fill color
    facet_wrap(~weights_string) +
    labs(title = paste0("Posterior density for w", w),
         x = paste0("Estimated w", w),
         y = "True w") + 
    theme_bw()
    ggsave(paste0("fig/", trials, "_posterior_density_w", w, ".png"))
}


simulated_data <- tibble()

for (c in c_values) {
  for (w in list_of_weights) {
    # Read the simulated data from the appropriate rds file
    data <- readRDS(paste0("data/", trials, "/c_", c, "_w_", paste0(w, collapse = "_"), ".rds"))
    data$c <- c
    data$c <- as.factor(data$c)
    data$trial <- 1:nrow(data)
    data$weights_string <- paste(w[1], w[2], w[3], w[4], w[5])
    data$correct <- ifelse(data$danger == data$choice, 1, 0)

    # make accumulated accuracy
    data$accumulated_accuracy <- cumsum(data$correct) / 1:nrow(data)
    
    
    simulated_data <- bind_rows(simulated_data, data)
  }
}

## plotting the accumulated accuracy
ggplot(simulated_data) +
  geom_line(aes(x = trial, y = accumulated_accuracy, color = c), alpha = 0.5) +
  facet_wrap(~weights_string) +
  labs(title = "Accumulated accuracy",
       x = "Trial",
       y = "Accuracy") +
  ylim(0, 1) +
  theme_bw()

ggsave(paste0("fig/", trials, "_accumulated_accuracy.png"))


