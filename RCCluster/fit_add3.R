library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# Cluster usage is not necessary here (shorter run time)

load('../dataverse/processed_dat/dat_add3.RData')

## Run MCMC
fit_add3 = stan(file = '../sensitivity/ecological.stan', 
                data = list(y=adata[,1], 
                            numcounties=3089, 
                            numeffects=24, 
                            adata=adata, 
                            states=states))

save(fit_add3, file = '../dataverse/results/fit_add3.Rdata')