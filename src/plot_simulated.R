
library(pacman)
pacman::p_load(posterior, ggplot2, gridExtra, tidyverse)

dir.create("fig", showWarnings = FALSE)


c_values <- seq(0.1, 5, 0.5)

list_of_weights <- list(
  c(1/5, 1/5, 1/5, 1/5, 1/5),
  c(0.9, 0.1/4, 0.1/4, 0.1/4, 0.1/4),
  c(0.5, 0.4, 0.1, 0.1, 0.1)
)


for (c in c_values) {

  for (w in list_of_weights) {
      # Read the simulated data from the appropriate rds file
      fit <- readRDS(paste0("fits/model_fit_c_", c, "_w_", paste0(w, collapse = "_"), ".rds"))

      # plot posterior distribution
      # extract the posterior
      posterior <- fit$draws()

      fit_df <- as_draws_df(posterior)
      

      ggplot(fit_df) +
        # plot the prior density
        geom_density(aes(x = prior_c), fill = "skyblue", alpha = 0.5) +
        
        # plot the posterior density
        geom_density(aes(x = posterior_c), fill = "red", alpha = 0.5) +

        # add a vertical line for the true value of c
        geom_vline(xintercept = c, linetype = "dashed") +
        
        labs(title = paste0("Prior and posterior density for c = ", c),
              x = "c",
              y = "Density") +

        xlim(0, 5) +

        theme_bw()
    
      # save the plot
      ggsave(paste0("fig/posteriorc_c", c, "_w_", paste0(w, collapse = "_"), ".png"))

      plot1 <- fit_df %>% ggplot() +
        geom_density(aes(x = `prior_w[1]`), fill = "skyblue", alpha = 0.5) +
        geom_density(aes(x = `posterior_w[1]`), fill = "red", alpha = 0.5) +
        geom_vline(xintercept = w[1], linetype = "dashed") +
        labs(x = "weight 1",
            y = "Density") +
        theme_bw()

      plot2 <- fit_df %>% ggplot() +
        geom_density(aes(x = `prior_w[2]`), fill = "skyblue", alpha = 0.5) +
        geom_density(aes(x = `posterior_w[2]`), fill = "red", alpha = 0.5) +
        geom_vline(xintercept = w[2], linetype = "dashed") +
        labs(x = "weight 2",
            y = "Density") +
        theme_bw()

      plot3 <- fit_df %>% ggplot() +
        geom_density(aes(x = `prior_w[3]`), fill = "skyblue", alpha = 0.5) +
        geom_density(aes(x = `posterior_w[3]`), fill = "red", alpha = 0.5) + 
        geom_vline(xintercept = w[3], linetype = "dashed") +
        labs(x = "weight 3",
            y = "Density") +
        theme_bw()
      
      plot4 <- fit_df %>% ggplot() +
        geom_density(aes(x = `prior_w[4]`), fill = "skyblue", alpha = 0.5) +
        geom_density(aes(x = `posterior_w[4]`), fill = "red", alpha = 0.5) + 
        geom_vline(xintercept = w[4], linetype = "dashed") +
        labs(x = "weight 4",
            y = "Density") +
        theme_bw()
      
      plot5 <- fit_df %>% ggplot() +
        geom_density(aes(x = `prior_w[5]`), fill = "skyblue", alpha = 0.5) +
        geom_density(aes(x = `posterior_w[5]`), fill = "red", alpha = 0.5) + 
        labs(x = "weight 5",
            y = "Density") +
        theme_bw()


      combined <- grid.arrange(plot1, plot2, plot3, plot4, plot5,  ncol = 3, nrow = 2)

      # save the plot
      ggsave(paste0("fig/posteriorsweights_c_", c, "_w_", paste0(w, collapse = "_"), ".png"), plot = combined)

  }
}