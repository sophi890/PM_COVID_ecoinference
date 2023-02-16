library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# Cluster usage is not necessary here (shorter run time)

load('../dataverse/processed_dat/ecoreg_add1.RData')

## Run MCMC
fit_add1 = stan(file = '../sensitivity/ecological.stan', 
                     data = list(y=adata[,1], 
                                 numcounties=3089, 
                                 numeffects=24, 
                                 adata=adata, 
                                 states=states))

save(fit_add1, file = '../dataverse/results/fit_add1.Rdata')
