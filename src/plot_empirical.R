library(pacman)
pacman::p_load(posterior, ggplot2, gridExtra, tidyverse, ggridges)

dir.create("fig", showWarnings = FALSE)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
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

dataframe <- tibble()

# load real data
data <- readRDS(file = "data/empirical_data_updated_subj_ids.rds")

# get subjects
subjects <- unique(data$subject)


for (participant in subjects) {
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
  fit_df <- left_join(fit_df, data %>% select(subject, trial, response), by = c("participant" = "subject", "trial" = "trial"))


  dataframe_preds <- bind_rows(dataframe_preds, fit_df)
}

# plot
preds_vs_real <- dataframe_preds %>% group_by(participant, trial) %>% summarise(response=extract_danger_response(first(response)), trial=first(trial), participant=first(participant), y_pred = Mode(y_pred)) %>% ungroup()
preds_vs_real$pred_correct <- preds_vs_real$response == preds_vs_real$y_pred

preds_vs_real$participant <- as.factor(preds_vs_real$participant)
preds_vs_real$trial <- as.integer(preds_vs_real$trial)
preds_vs_real$pred_correct <- as.integer(preds_vs_real$pred_correct)

# boxplot of correct predictions by participant
preds_vs_real %>%
  ggplot(aes(x = trial, y = pred_correct, color = participant)) +
    geom_jitter() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(
      title = "Correct predictions by participant",
      x = "Trial",
      y = "Correct prediction"
    ) +
    theme_bw()

ggsave("fig/y_pred_vs_actual_empirical.png")

# plot log likelihood of the data for each participant
dataframe_log_lik <- tibble()
for (participant in subjects) {
  # Read the fit
  cat("Participant: ", participant, "\n")
  fit <- readRDS(paste0("fits/empirical_session_1/participant_", participant, "_fit.rds"))

  # extract the posterior
  fit_df <- fit$draws("log_lik", format = "df")

  fit_df$participant <- participant
  #fit_df$participant <- as.factor(fit_df$participant)

  dataframe_log_lik <- bind_rows(dataframe_log_lik, fit_df)
}

dataframe_log_lik$participant <- as.factor(dataframe_log_lik$participant)

ggplot(dataframe_log_lik, aes(x = log_lik, y = participant, fill = participant)) +
  geom_density_ridges(alpha = 0.5) +
  labs(
    title = "Log likelihood of the data for each participant",
    x = "Log likelihood",
    y = "Density"
  ) +
  theme_bw()

ggsave("fig/log_lik_empirical.png")
=======
# load data from AlienData.txt
data <- read.table("data/AlienData.txt", sep = ",", header = TRUE)

# only keep data from session 1 (so we have the same rule for danger and nutritious as in the simulated data)
data <- data %>% filter(session == 1)

# create a new participant column depending both on the subject and the condition
data$participant <- paste0(data$subject, "_", data$condition)
data$participant <- as.factor(data$participant)
unique_participants <- unique(data$participant)

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


tmp_data <- tibble()

for (s in unique_participants) {
    subject_data <- data %>% filter(participant == s)

    subject_data$trial <- 1:nrow(subject_data)
    subject_data$correct <- as.integer(extract_danger_response(subject_data$response) == subject_data$dangerous)

    # cumulative correct
    subject_data$cumulative_accuracy <- cumsum(subject_data$correct) / 1:nrow(subject_data)

    print(subject_data)

    tmp_data <- rbind(tmp_data, subject_data)
}

ggplot(tmp_data) +
  geom_line(aes(x = trial, y = cumulative_accuracy, color = participant), alpha = 0.5) +
  #facet_grid(~participant) +
  labs(title = "Accumulated accuracy",
       x = "Trial",
       y = "Accuracy") +
  ylim(0, 1) +
  theme_bw()

ggsave(paste0("fig/accumulated_accuracy_empirical.png"))

