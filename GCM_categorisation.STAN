data {
    int<lower=0> N;                                 // number of trials
    array[N, 5] int<lower=0, upper = 1> attributes; // attributes of the stimuli for each trial
    array[N] int<lower=0, upper = 1> category;   // true categories of each of the trials
    array[N] int<lower=0, upper = 1> choices;       // responses of the participant (pertaining to the category)  
}

parameters {
    real log_c;   
}

