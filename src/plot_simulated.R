
library(pacman)
pacman::p_load(posterior, ggplot2, gridExtra, tidyverse, ggridges)

dir.create("fig", showWarnings = FALSE)
source("simulation_params.R") # loads the c values and lists of weights (so that we do not have to change in all scripts)



trials <- "96_trials"
dataframe <- tibble()

for (c in c_values) {
  for (w in list_of_weights) {
    print(w)
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


