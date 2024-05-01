
library(pacman)
pacman::p_load(posterior, ggplot2, gridExtra, tidyverse, ggridges)

dir.create("fig", showWarnings = FALSE)


c_values <- seq(0.1, 5, 0.5)

list_of_weights <- list(
  c(1/5, 1/5, 1/5, 1/5, 1/5),
  c(0.9, 0.1/4, 0.1/4, 0.1/4, 0.1/4),
  c(0.5, 0.4, 0.1, 0.1, 0.1)
)



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


      fit_df$c <- as.factor(fit_df$c)
      fit_df$weights_string <- paste(w[1], w[2], w[3], w[4], w[5])

      dataframe <- bind_rows(dataframe, fit_df)
  }
}


ggplot(dataframe) +
  geom_density_ridges(aes(x = posterior_c, y = c), alpha = 0.5) +
  # add a line for the value of true c in each plot
  facet_wrap(~weights_string) +
  labs(title = "Posterior density for c",
        x = "Estimated c",
        y = "True c") +
  xlim(0, 5) +
  theme_bw()
  
ggsave("fig/recovery_of_c.png")
