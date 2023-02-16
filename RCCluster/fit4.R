library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

load("../dataverse/processed_dat/dat_4.RData")

fit4 = stan(file = '../sensitivity/hierbayes_4.stan', 
                   data = list(y=adata[,1], 
                               numcounties = 3082, 
                               numeffects = c(14, 6, 2), 
                               numcats = c(2,2,2,2,2,3), 
                               covlist=covlist, 
                               adata=adata, 
                               whicha=whicha, 
                               states=states), iter = 4000)


save(fit4, file=paste0("../dataverse/results/fit4.Rdata"))


