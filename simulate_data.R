library(tidyverse)
source("simulation_functions.R")

# create a data folder
dir.create("data")


# loop over values of c
c_values <- seq(0.1, 1, 0.2)


for (c in c_values){
  # setup experimental design
  experiment <- full_experiment()
  
  # extract the attributes of the stimuli
  stimuli <- experiment %>% select("Var1", "Var2", "Var3", "Var4", "Var5")
  
  # make choices according to the GCM
  agent_choices <- gcm(
    w = c(1,1,1,1,1),
    c = c,
    stimuli = stimuli,
    category = experiment$danger
  )
  
  # add the choices to the dataframe
  experiment$choice_danger <- agent_choices

  # save the results
    saveRDS(experiment, file = paste0("data/c_", c, ".rds"))

}
