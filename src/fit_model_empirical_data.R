library(pacman)
pacman::p_load(tidyverse)

# load data from AlienData.txt
data <- read.table("data/AlienData.txt", sep = ",", header = TRUE)

# only keep data from session 1 (so we have the same rule for danger and nutritious as in the simulated data)
data <- data %>% filter(session == 1)


