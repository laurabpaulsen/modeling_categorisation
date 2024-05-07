// GENERALISED CONTEXT MODEL 
// ---------------------------------
// GCM stores every category exemplar encountered in memory according to a geometric representation
// GCM adds each exemplar to the pile of relevant category (as per feedback)
// GCM has thus built two sets of exemplars, one for each category, and all subsequent stimuli are classified by referring to those memorised ensembles

// TEST PHASE: 
// New stimulus
// Calculates distance between rep of test stim and each of learned exemplars, applying pre-given weights to each feature
// Calculate sim between stimuli using exp decay function
// Identify similarity to a category using average
// Identify probability of attributing to category using sim1/sim1+sim2

// We want to calculate sim of each stim to exemplars of each cat using an exp decay based on weighted distance measures. Then we use these similarities to calculate the probability of a response belonging to a given category

data {
  int<lower=1> ntrials; // number of trials
  int<lower=1> nfeatures; // number of predefined relevant features
  array[ntrials] int<lower=0, upper=1> cat_one; // true responses on a trial by trial basis
  array[ntrials] int<lower=0, upper=1> y; // decisions on a trial by trial basis
  matrix[ntrials, nfeatures] obs; // stimuli as vectors of features
  real<lower=0, upper=1> b; // initial bias for category one over two

  // priors
  vector[nfeatures] w_prior_values; // concentration parameters for dirichlet distribution
  vector[2] c_prior_values; // mean and variance for logit-normal distribution
}

transformed data {
  array[ntrials] int<lower=0, upper=1> cat_two; // dummy var for cat two over cat 1
  array[sum(cat_one)] int<lower=1, upper=ntrials> cat_one_idx; // array of which stim are cat 1
  array[ntrials-sum(cat_one)] int<lower=1, upper=ntrials> cat_two_idx; // array of which stimuli are cat 2
  int idx_one = 1; // initializing
  int idx_two = 1; 
  
  for (i in 1:ntrials) {
    cat_two[i] = abs(cat_one[i]-1);
    
    if (cat_one[i]==1){
      cat_one_idx[idx_one] = i;
      idx_one +=1;
      
    } else {
      cat_two_idx[idx_two] = i;
      idx_two += 1;
    }
    
  }
}

parameters {
  simplex[nfeatures] w; // simplex means sum(w)=1
  real logit_c;
}

transformed parameters {
  real<lower=0, upper=5> c = inv_logit(logit_c) * 5;
  
  array[ntrials] real<lower=0.0001, upper=0.9999> r;
  vector[ntrials] rr;
  
  // trials
  for (i in 1:ntrials){
    // if it's the first trial, we have no prior information to base our decision on
    if (i == 1 || sum(cat_one[:(i-1)])==0 || sum(cat_two[:(i-1)])==0){
      r[i] = 0.5;
    } else {
      matrix[i-1, nfeatures] tmp_dist = rep_matrix(w', i-1) .* abs(obs[1:(i-1),:] - rep_matrix(obs[i,:], i-1));
      vector[i-1] exemplar_sim;
      for (j in 1:(i-1)){
        exemplar_sim[j] = exp(-c*sum(tmp_dist[j,:]));
      }
      
      array[sum(cat_one[:(i-1)])] int tmp_idx_one = cat_one_idx[:sum(cat_one[:(i-1)])];
      array[sum(cat_two[:(i-1)])] int tmp_idx_two = cat_two_idx[:sum(cat_two[:(i-1)])];

      rr[i] = (b*mean(exemplar_sim[tmp_idx_one]))/(b*mean(exemplar_sim[tmp_idx_one])+(1-b)*mean(exemplar_sim[tmp_idx_two]));
      // clamp the value
      r[i] = fmax(0.0001, fmin(rr[i], 0.9999));
    }
  }
}

model {
  // Defining the likelihood of each observed choice given the prob r[i] which is calculated in the transformed parameters block as the probability of resp being category 1. 
  // Likelihood is computed using a bernoulli prob mass function which is appropriate for binary choice data
  
  // Priors
  target += dirichlet_lpdf(w | w_prior_values); // prior on the attention weights
  target += normal_lpdf(logit_c | c_prior_values[1], c_prior_values[2]); // prior on the logit of the scaling parameter
  
  // try a beta distribution
  // target += beta_lpdf(logit_c | c_prior_values[1], c_prior_values[2]);

  // Decision Data
  // Vectorized decision data likelihood
  target += bernoulli_lpmf(y | r);

}



generated quantities {
  real posterior_c;
  posterior_c = inv_logit(logit_c)*5;

  real prior_c;
  prior_c = inv_logit(normal_rng(c_prior_values[1], c_prior_values[2]))*5;

  array[nfeatures] real posterior_w;
  for (i in 1:nfeatures){
    posterior_w[i] = w[i];
  }

  array[nfeatures] real prior_w;
  for (i in 1:nfeatures){
    prior_w[i] = dirichlet_rng(w_prior_values)[i];
  }


  real log_lik = 0;
  array[ntrials] int y_pred;
  for (i in 1:ntrials){
    log_lik += bernoulli_lpmf(y[i] | r[i]);
    y_pred[i] = bernoulli_rng(r[i]);
  }
}
