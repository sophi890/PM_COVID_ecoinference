library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

load("../dataverse/processed_dat/ecoreg_7.RData")

fit7 = stan(file = '../sensitivity/ecoreg_random_fixedoffset.stan', 
            data = list(y=adata[,1], 
                        numcounties = 3082, 
                        numstrata = 192, 
                        numeffects = c(14, 2), 
                        covlist=covlist, 
                        adata=adata, 
                        states=states, 
                        cateffs = gamma_s),
            iter = 4000)

save(fit7,file=paste0("../dataverse/results/fit7.Rdata"))
