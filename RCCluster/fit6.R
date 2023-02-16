library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

load("../dataverse/processed_dat/dat_6.RData")

fit6 = stan(file = '../sensitivity/hierbayes_6.stan', 
                   data = list(y=adata[,1], 
                               numcounties = 3082, 
                               numeffects = c(14, 6, 2), 
                               numcats = c(2,2,2,4,2,3), 
                               covlist=covlist, 
                               adata=adata, 
                               whicha=whicha192, 
                               states=states), iter = 4000)


save(fit6,file=paste0("../dataverse/results/fit6.Rdata"))
