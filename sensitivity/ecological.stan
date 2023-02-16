functions{ 
  real loglikeco_log(vector y, int numcounties, int numeffects, matrix adata, int[] states, vector pars, vector rand){ //real
    vector[numeffects] alphac = pars[1:numeffects]; // area-level covariate effects
    vector[numcounties] N = adata[,2];
    vector[numcounties] q;
    vector[numcounties] p;
    vector[numcounties] loglik;
    
    // add group level covariate effect
    q = adata[,3:(numeffects+2)] * alphac; 
  
    for (i in 1:num_elements(y)){
        p[i] = logistic_cdf(q[i]+ rand[states[i]],0,1);
    }
    
    // end up with vector of probabilities for binomial, one for each FIPS 
    // calculate loglikelihood
    for (i in 1:num_elements(y)){
      loglik[i] = y[i]*log(p[i]) + (N[i]-y[i])*log(1-p[i]);//binomial_lpmf(y[i] | N[i], p[i]); - not working b/c require ints not reals.
    }
    return sum(loglik);
  }
}

data{ // how to make generalizable?
  int numcounties;
  int numeffects;
  vector[numcounties] y; // deaths
  matrix[numcounties,2 + numeffects] adata; 
  int states[numcounties];
} 

parameters{
  vector[numeffects] pars; 
  real mu;
  real<lower=0,upper=1> sigma;
  vector[49] rand;
}

model{
  mu ~ normal(-8,5); // prior for intercept 
  rand ~ normal(mu, sigma);
  pars ~ normal(0, sqrt(0.68)); // priors for remaining 
  y ~ loglikeco(numcounties, numeffects, adata, states, pars, rand); // log likelihood
}
