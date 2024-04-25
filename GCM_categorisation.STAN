data {
    int<lower=0> N;                                 // number of trials
    array[N, 5] int<lower=0, upper = 1> attributes; // attributes of the stimuli for each trial
    array[N] int<lower=0, upper = 1> category;   // true categories of each of the trials
    array[N] int<lower=0, upper = 1> choices;       // responses of the participant (pertaining to the category)  
}


transformed data {
    array[N] real<lower=0, upper=1> choice_prob; // probability of choosing category 1
    
    int n_cat0_prev = 0;
    int n_cat1_prev = 0;

    for (n in 1:N){
        // current stimuli == attributes[n]
    

        // if atleast one of the categories are empty, set the probability to 0.5
        if (n_cat0_prev == 0 || n_cat1_prev == 0){
            choice_prob[n] = 0.5;
        } 
        
        // otherwise calculate the probability of choosing category using the CGM
       else {
            // insert one million things here
            real dist_cat0 = 0;
            real dist_cat1 = 0;

            real similarity_cat0 = 0;
            real similarity_cat1 = 0;


            // start by calculating the distance to all previous exemplars
            for (e in 1:N-1){
                if (category[e] == 0){
                    // do something
                } 
                else {
                    // do the same thing but for category 1
                }
            }




        }


        // update the count of the previous exemplars shown
        if (category[n] == 0){
            n_cat0_prev += 1;
        } 
        else {
            n_cat1_prev += 1;
        }


    }

}

parameters {
    // array[5] real w; maybe at some point implement a weight for each of the features
    real log_c;   
}


transformed parameters {
    real c;

    // scale the parameter to be between 0 and 2??? Ricc did this but is it really constrained between 0 and 2?
    // Ricc:  "model is difficult to fit if you assume it can be higher than two
    // hard boundaries not always nice, but makes the model run sooooo <333"
    c = inv_logit(log_c)*2; 
}




model {
    // setting a prior for c
    target += normal_lpdf(log_c | 0, 1);
}



