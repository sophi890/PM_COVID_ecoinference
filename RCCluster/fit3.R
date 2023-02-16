library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

load("../dataverse/processed_dat/ecoreg_3.RData")

fit3 = stan(file = '../sensitivity/ecoreg_random_fixedoffset.stan', 
            data = list(y=adata[,1], 
                        numcounties = 3082, 
                        numstrata = 192, 
                        numeffects = c(13, 3), 
                        covlist=covlist.pm25, 
                        adata=adata, 
                        states=states, 
                        cateffs = gamma_s),
            iter = 4000)

save(fit3, file=paste0("../dataverse/results/fit3.Rdata"))
