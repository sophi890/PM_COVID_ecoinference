functions{ 
  real loglikeco_log(vector y, int numcounties, int numstrata, int[] numeffects, matrix[] covlist, matrix adata, int[] states, vector cateffs, vector pars, vector rand){ //real
    vector[numeffects[1]] alphac = pars[1:(numeffects[1])]; // area-level covariate effects
    vector[numeffects[2]] beta; // normal covariate effects
    vector[numcounties] N = adata[,2]; // population each county
    vector[numcounties] q; 
    real c2 = 0.34584297349917959319510856775587; //  c = 16*sqrt(3) / (15 * pi()); 
    vector[numcounties] denom = rep_vector(1.0, numcounties); 
    vector[numcounties] normvec = rep_vector(0.0, numcounties); 
    vector[numcounties] p = rep_vector(0.0, numcounties);
    
    // add group level covariate effect
    q = adata[,3:(numeffects[1]+2)] * alphac; // (3082 x 15)  x (15 x 1)
    
    // add "normal" covariate effects
   if (numeffects[2]>0){
      beta = pars[(numeffects[1]+1):(numeffects[1]+numeffects[2])]; // individual-level normal covariate effects
      for (i in 1:numcounties){
        denom[i] = sqrt(1+c2*beta'*covlist[i]*beta); // incorporates covariance 
      }
    normvec = adata[,(numeffects[1]+3+numstrata):(numeffects[1]+2+numstrata+numeffects[2])] * beta; 
    }

  for (j in 1:numstrata){ // progress along cols
    p += adata[,(numeffects[1]+2+j)] ./ (1.0 + exp(-(cateffs[j] + q + normvec + rand[states]) ./ denom));
  }
    // end up with vector of probabilities for binomial, one for each FIPS 
  
    // calculate loglikelihood
    return sum(y .* log(p) + (N-y) .* log(1-p));
  }
}

data{ // how to make generalizable?
  int numcounties;
  int numstrata;
  vector[numcounties] y; // deaths
  vector[numstrata] cateffs;
  int numeffects[2];   // # group level, # categorical covariates, # normal covariates in that order
  matrix[numeffects[2],numeffects[2]] covlist[numcounties]; // covariance list for normal covariates
  matrix[numcounties,numeffects[1]+numeffects[2]+numstrata+2] adata; // population + grouplevel covariates + normal covariates + percent ppl in each strata
  int states[numcounties];
} 

parameters{
  vector[numeffects[1]+numeffects[2]] pars; // use bounds? eg <lower=-3,upper=3>
  real<lower=0,upper=1> sigma; // sd of random state effects
  vector[49] rand;
}

model{
  rand ~ normal(0, sigma);
  pars[1] ~ normal(0,0.5); // prior for intercept. Using fixed offsets so this should now be centered at 0.
  pars[2:] ~ normal(0, sqrt(0.68)); // priors for remaining //corresponds to 95 percent odds ratio between 1/5 and 5
  y ~ loglikeco(numcounties, numstrata, numeffects, covlist, adata, states, cateffs, pars, rand); // log likelihood
}

