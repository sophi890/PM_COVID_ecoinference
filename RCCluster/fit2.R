library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

load("../dataverse/processed_dat/ecoreg_2.RData")

fit2 = stan(file = '../sensitivity/ecoreg_2.stan', 
                   data = list(y=adata[,1], 
                               numcounties = 3082, 
                               numeffects = c(13, 6, 3), 
                               numcats = c(2,2,2,4,2,3), 
                               covlist=covlist.pm25, 
                               adata=adata, 
                               whicha=whicha192, 
                               states=states), iter = 4000)


save(fit2,file=paste0("../dataverse/results/fit2.Rdata"))
