library(tidyverse)
source("simulation_functions.R")

# create a data folder
dir.create("data", showWarnings = FALSE)


# loop over values of c
c_values <- seq(0.1, 5, 0.5)


list_of_weights <- list(
  c(1/5, 1/5, 1/5, 1/5, 1/5),
  c(0.9, 0.1/4, 0.1/4, 0.1/4, 0.1/4),
  c(0.5, 0.4, 0.1, 0.1, 0.1)
)


for (c in c_values){
  # setup experimental design
  experiment <- full_experiment()
  
  # extract the attributes of the stimuli
  stimuli <- experiment %>% select("Var1", "Var2", "Var3", "Var4", "Var5")

  for(w in list_of_weights){
    # make choices according to the GCM
    agent_choices <- gcm(
      w = w,
      c = c,
      stimuli = stimuli,
      category = experiment$danger
    )
    
    # add the choices to the dataframe
    experiment$choice_danger <- agent_choices

    for (i in 1:5) {
      experiment[[paste0("w", i)]] <- w[i]
    }
    

    # save the results
    saveRDS(experiment, file = paste0("data/c_", c, "_w_", paste0(w, collapse = "_"), ".rds"))
  
  }
}
