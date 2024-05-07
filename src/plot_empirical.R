library(pacman)
pacman::p_load(posterior, ggplot2, gridExtra, tidyverse, ggridges)

dir.create("fig", showWarnings = FALSE)

dataframe <- tibble()




for (participant in 1:27) {
  # Read the fit
  fit <- readRDS(paste0("fits/empirical_session_1/participant_", participant, "_fit.rds"))

  # extract the posterior
  posterior <- fit$draws()

  fit_df <- as_draws_df(posterior)

  fit_df <- fit_df %>%
    rename(
      "posterior_w1" = "posterior_w[1]",
      "posterior_w2" = "posterior_w[2]",
      "posterior_w3" = "posterior_w[3]",
      "posterior_w4" = "posterior_w[4]",
      "posterior_w5" = "posterior_w[5]"
    )

  fit_df$participant <- participant
  fit_df$participant <- as.factor(fit_df$participant)

  dataframe <- bind_rows(dataframe, fit_df)
}


ggplot(dataframe) +
  geom_density_ridges(aes(x = posterior_c, y = participant, fill = participant), alpha = 0.5) +
  labs(
    title = "Posterior density for c",
    x = "Estimated c",
    y = "Participant"
  ) +
  xlim(0, 5) +
  theme_bw()

ggsave("fig/estimated_c.png")

for (w in 1:5) {
  column_x <- paste0("posterior_w", w)

  ggplot(dataframe) +
    geom_density_ridges(aes(x = !!sym(column_x), y = participant, fill = participant), alpha = 0.5) +
    labs(
      title = paste0("Posterior density for w", w),
      x = paste0("Estimated w", w)
    ) +
    theme_bw()
  ggsave(paste0("fig/estimated_w", w, ".png"))
}


# load real data
data <- read.table("data/AlienData.txt", sep = ",", header = TRUE)
# only get data from session 1
data <- data %>% filter(session == 1)
# compare outcomes of each trial's y_pred[trial_num] distribution with the actual "dangerous" value
# for each participant

dataframe_preds <- tibble()
subjects <- unique(data$subject)
for (participant in subjects) {
  # Read the fit
  cat("Participant: ", participant, "\n")
  fit <- readRDS(paste0("fits/empirical_session_1/participant_", participant, "_fit.rds"))

  # extract the posterior
  fit_df <- fit$draws("y_pred", format = "df")


  fit_df$participant <- participant
  #fit_df$participant <- as.factor(fit_df$participant)

  # break out y_pred[n] into a column with trial number and a column with the value
  fit_df <- fit_df %>%
    pivot_longer(cols = starts_with("y_pred"), names_to = "trial", values_to = "y_pred") %>%
    mutate(trial = as.numeric(str_remove(str_remove(trial, "\\]"), "y_pred\\[")))
  # add "response" value for each trial from the data (matching "trial" to the trial number in the data)
  # only keeping the "response" column from the data
  cat("Adding response values for participant: ", participant, "\n")
  fit_df <- left_join(fit_df, data %>% select(subject, trial, response), by = c("participant" = "subject", "trial" = "trial"), keep = FALSE)


  dataframe_preds <- bind_rows(dataframe_preds, fit_df)
}

# plot mode y_pred vs actual for each participant for each trial
dataframe_preds %>% group_by(participant, trial) %>% summarise(y_pred = mode(y_pred)) %>% 
  ggplot(aes(x = y_pred, y = dangerous)) + geom_point() + facet_wrap(~participant) + theme_bw()
