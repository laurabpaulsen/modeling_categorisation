library(tidyverse)
source("simulation_functions.R")
source("simulation_params.R") # loads the c values and lists of weights (so that we do not have to change in all scripts)

# create a data folder
dir.create("data", showWarnings = FALSE)

for (n_repeats in c(3)){ #, 10)){

  if (n_repeats == 3){
    output_dir <- "data/96_trials"
  } else {
    output_dir <- "data/320_trials"
  }

  dir.create(output_dir, showWarnings = FALSE)


  for (c in c_values){
    print(paste0("Simulating data for c = ", c))
    # setup experimental design
    experiment <- full_experiment(n_repeats = n_repeats)
    
    # extract the attributes of the stimuli
    stimuli <- experiment %>% select("Var1", "Var2", "Var3", "Var4", "Var5")

    for(w in list_of_weights){
      print(paste0("Finished simulating data for c = ", c, " and w = ", paste(w, sep=", ")))
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
      saveRDS(experiment, file = paste0(output_dir, "/c_", c, "_w_", paste0(w, collapse = "_"), ".rds"))

    }
  }

}
