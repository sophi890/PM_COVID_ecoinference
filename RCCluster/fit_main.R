library('rstan')
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

load("../ecoreg_main.RData")

fit_main = stan(file = '../ecoreg_random.stan', 
            data = list(y=adata[,1], 
                        numcounties = 3082, 
                        numeffects = c(14, 6, 3), 
                        numcats = c(2,2,2,2,2,3), 
                        covlist=covlist.pm25, 
                        adata=adata, 
                        whicha=whicha, 
                        states=states), iter = 6000)


save(fit_main, file=paste0("../results/fit_main.Rdata"))
