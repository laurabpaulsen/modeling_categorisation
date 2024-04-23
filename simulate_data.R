library(tidyverse)
source("simulation_functions.R")

# setup experimental design
experiment <- full_experiment()

# extract the attributes of the stimuli
stimuli <- experiment %>% select("Var1", "Var2", "Var3", "Var4", "Var5")

# make choices according to the GCM
agent_choices <- gcm(
    w = c(1,1,1,1,1),
    c = 1,
    stimuli = stimuli,
    category = experiment$danger
    )

# add the choices to the dataframe
experiment$choice <- agent_choices

print(experiment)
  