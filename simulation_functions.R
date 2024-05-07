# this script contains the functions used for simulation


# FUNCTIONS TO GENERATE STIMULI
# NOTES:
# - 32 different types of stimuli
# - each stimuli is presented 3 times in a randomised order (all 32 are randomised and presented, then they are randomised again and presented, and so on)


generate_stimuli <- function(){
  stimuli <- expand.grid(replicate(5, c(0, 1), simplify = FALSE))
  return(stimuli)

}

determine_danger_nutri <- function(df){
  # if the first and second variable is one, then the stimuli is dangerous
  df <- df %>% mutate(danger = ifelse(Var1 == 1 & Var2 == 1, 1, 0),
                     nutri = ifelse(Var4 == 1, 1, 0))
  return(df)
}


full_experiment <- function(n_repeats = 3){

  stimuli <- generate_stimuli()
  stimuli <- determine_danger_nutri(stimuli)

  # create a dataframe to store the results
  df <- data.frame()
  
  for(i in 1:n_repeats){
    # shuffle the stimuli
    shuffled_stimuli <- stimuli[sample(nrow(stimuli)),]

    # add to dataframe
    df <- rbind(df, shuffled_stimuli)
  }
  
  return(df)
}
  
# FUNCTIONS TO SIMULATE RESPONSES
weighted_distance <- function(v1, v2, w){
  return(sum(w * abs(v1 - v2)))
}

similarity <- function(distance, c){
  return(exp(-c * distance))
}

calculate_all_similiarities <- function(prev_stim, current_stim, w, c){
  similarities <- c()
  
  for(i in 1:nrow(prev_stim)){
    tmp_dist <- weighted_distance(prev_stim[i, ], current_stim, w)
    tmp_sim <- similarity(tmp_dist, c)
    
    similarities <- c(similarities, tmp_sim)
    
  }
  return(similarities)
}


gcm <- function(
    w, # weights for each of the stimuli attributes
    c, # context-depencey parameter
    stimuli, # stimuli shown
    category # the category of the stimuli (e.g., dangerous)
    ){
  
  
  choice_prob <- c()
  
  n_trials <- nrow(stimuli)

  category0 <- tibble()
  category1 <- tibble()
  
  
  for(t in 1:n_trials){
    tmp_stim <- stimuli[t,]
    
    
    # check whether there is data in all categories
    if (nrow(category0)==0 || nrow(category1)==0){
      choice_prob <- c(choice_prob, 0.5)
    
    }
    
    
    # if each category has at least one observation
    else {
      # get the similiarity for the current stimuli to each of the observations in each of the categories
      similarites_category0 <- calculate_all_similiarities(category0, tmp_stim, w, c)
      similarites_category1 <- calculate_all_similiarities(category1, tmp_stim, w, c)


      # QUESTION? DOES THIS STANDARDISE SOME HOW? DOES IT NOT MATTER HOW MANY OF EACH EXAMPLE YOU HAVE SEEN IN EACH CATEGORY WHEN TAKING THE SUM?????
      # Answer from ricc: Should be mean (changes have been made to the code)
      numerator <- 0.5 * mean(similarites_category0)
      denominator <- 0.5 * mean(similarites_category0) + 0.5 * mean(similarites_category1)
      
      tmp_choice_prop <- numerator / denominator
  
      
      choice_prob <- c(choice_prob, tmp_choice_prop)
      
    }
  
    # Add to the correct category dataframe
    if (category[t] == 0){
      category0 <- rbind(category0, tmp_stim)
    }
    else{
      category1 <- rbind(category1, tmp_stim)
    }
    
    
  }
  return(rbinom(n_trials, 1, choice_prob))
  
}
