
library(pacman)
pacman::p_load(posterior, ggplot2, gridExtra, tidyverse, ggridges)

dir.create("fig", showWarnings = FALSE)
source("simulation_params.R") # loads the c values and lists of weights (so that we do not have to change in all scripts)


dataframe <- tibble()

for (c in c_values) {
  for (w in list_of_weights) {
      # Read the simulated data from the appropriate rds file
      fit <- readRDS(paste0("fits/model_fit_c_", c, "_w_", paste0(w, collapse = "_"), ".rds"))

      # extract the posterior
      posterior <- fit$draws()

      fit_df <- as_draws_df(posterior)

      fit_df$c <- c
      fit_df$w1 <- w[1]
      fit_df$w2 <- w[2]
      fit_df$w3 <- w[3]
      fit_df$w4 <- w[4]
      fit_df$w5 <- w[5]

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
  
ggsave("fig/recovery_of_c.png")



for (w in 1:5) {
  ggplot(dataframe) +
    
    geom_density_ridges(aes(x = `paste0("posterior_w[", w, "]")`, y = paste0("w", w), fill = c), alpha = 0.5) +
    facet_wrap(~c) +
    labs(title = "Posterior density for w1",
         x = "Estimated w1",
         y = "True w1") +
    xlim(0, 1) +
    theme_bw()
    ggsave(paste0("fig/recovery_of_w", w, ".png"))
}


