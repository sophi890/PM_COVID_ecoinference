library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

load("../dataverse/processed_dat/ecoreg_1.RData")

fit1 = stan(file = '../sensitivity/ecoreg_random_fixedoffset.stan', 
            data = list(y=adata[,1], 
                        numcounties = 3082, 
                        numstrata = 96, 
                        numeffects = c(13, 3), 
                        covlist=covlist.pm25, 
                        adata=adata, 
                        states=states, 
                        cateffs = gamma_s),
            iter = 4000)

save(fit1, file=paste0("../dataverse/results/fit1.Rdata"))
