library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

load("../dataverse/processed_dat/dat_add2.RData")

fit_add2 = stan(file = '../sensitivity/hierbayes_add2.stan', 
                       data = list(y=adata[,1], 
                                   numcounties = 3082, 
                                   numeffects = c(13, 6, 3), 
                                   numcats = c(2,2,2,2,2,3), 
                                   covlist=covlist.pm25, 
                                   adata=adata, 
                                   whicha=whicha, 
                                   states=states), iter = 4000)


save(fit_add2, file=paste0("../dataverse/results/fit_add2.Rdata"))


