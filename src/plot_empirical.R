
library(pacman)
pacman::p_load(posterior, ggplot2, gridExtra, tidyverse, ggridges)

dir.create("fig", showWarnings = FALSE)

dataframe <- tibble()



  
for (participant in 1:27) {
    # Read the fit
    fit <- readRDS(paste0("fits/model_fit_participant_", participant, ".rds"))

    # extract the posterior
    posterior <- fit$draws()

    fit_df <- as_draws_df(posterior)

    fit_df <- fit_df %>%
        rename("posterior_w1" = "posterior_w[1]",
               "posterior_w2" = "posterior_w[2]",
               "posterior_w3" = "posterior_w[3]",
               "posterior_w4" = "posterior_w[4]",
               "posterior_w5" = "posterior_w[5]")
      
    fit_df$participant <- participant
    fit_df$participant <- as.factor(fit_df$participant)

    dataframe <- bind_rows(dataframe, fit_df)

}


ggplot(dataframe) +
  geom_density_ridges(aes(x = posterior_c, y = participant, fill=participant), alpha = 0.5) +

  labs(title = "Posterior density for c",
        x = "Estimated c",
        y = "Participant") +
  xlim(0, 5) +
  theme_bw()
  
ggsave("fig/estimated_c.png")

for (w in 1:5) {
  column_x = paste0("posterior_w", w)

  ggplot(dataframe) +
    geom_density_ridges(aes(x = !!sym(column_x), y = participant, fill = participant), alpha = 0.5) +
    labs(title = paste0("Posterior density for w", w),
         x = paste0("Estimated w", w)) + 
    theme_bw()
    ggsave(paste0("fig/estimated_w", w, ".png"))
}


