library('rstan')
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Full results for Main Analysis
load('../dataverse/results/fit_main.Rdata')
for (i in 1:23){
  print(noquote(paste(signif(exp(mean(rstan::extract(fit_main)$pars[,i])),4), 
                      ' & ','(', signif(exp(mean(rstan::extract(fit_main)$pars[,i])-1.96*sd(rstan::extract(fit_main)$pars[,i])),4), 
                      ', ', signif(exp(mean(rstan::extract(fit_main)$pars[,i])+1.96*sd(rstan::extract(fit_main)$pars[,i])),4), ')', 
                      sep = '')))
}

# PM2.5 only
# Main analysis
print(noquote(paste(signif(exp(mean(rstan::extract(fit_main)$pars[,23])),4), 
                    ' & ','(', signif(exp(mean(rstan::extract(fit_main)$pars[,23])-1.96*sd(rstan::extract(fit_main)$pars[,23])),4), 
                    ', ', signif(exp(mean(rstan::extract(fit_main)$pars[,23])+1.96*sd(rstan::extract(fit_main)$pars[,23])),4), ')', 
                    sep = '')))

# Additional Analysis 1: Ecological 12/1/2020
load('../dataverse/results/fit_add1.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit_add1)$pars[,2])),4), 
                      ' & ','(', signif(exp(mean(rstan::extract(fit_add1)$pars[,2])-1.96*sd(rstan::extract(fit_add1)$pars[,2])),4), 
                      ', ', signif(exp(mean(rstan::extract(fit_add1)$pars[,2])+1.96*sd(rstan::extract(fit_add1)$pars[,2])),4), ')', 
                      sep = '')))

# Additional Analysis 2: Hierarchical 6/18/2020
load('../dataverse/results/fit_add2.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit_add2)$pars[,23])),4), 
                      ' & ','(', signif(exp(mean(rstan::extract(fit_add2)$pars[,23])-1.96*sd(rstan::extract(fit_add2)$pars[,23])),4), 
                      ', ', signif(exp(mean(rstan::extract(fit_add2)$pars[,23])+1.96*sd(rstan::extract(fit_add2)$pars[,23])),4), ')', 
                      sep = '')))

# Additional Analysis 3: Ecological 6/18/2020
load('../dataverse/results/fit_add3.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit_add3)$pars[,2])),4), 
                    ' & ','(', signif(exp(mean(rstan::extract(fit_add3)$pars[,2])-1.96*sd(rstan::extract(fit_add3)$pars[,2])),4), 
                    ', ', signif(exp(mean(rstan::extract(fit_add3)$pars[,2])+1.96*sd(rstan::extract(fit_add3)$pars[,2])),4), ')', 
                    sep = '')))

# SENSITIVITY ANALYSES
load('../dataverse/results/fit1.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit1)$pars[,17])),4), 
                    ' & ','(', signif(exp(mean(rstan::extract(fit1)$pars[,17])-1.96*sd(rstan::extract(fit1)$pars[,17])),4), 
                    ', ', signif(exp(mean(rstan::extract(fit1)$pars[,17])+1.96*sd(rstan::extract(fit1)$pars[,17])),4), ')', 
                    sep = '')))

load('../dataverse/results/fit2.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit2)$pars[,25])),4), 
                    ' & ','(', signif(exp(mean(rstan::extract(fit2)$pars[,25])-1.96*sd(rstan::extract(fit2)$pars[,25])),4), 
                    ', ', signif(exp(mean(rstan::extract(fit2)$pars[,25])+1.96*sd(rstan::extract(fit2)$pars[,25])),4), ')', 
                    sep = '')))

load('../dataverse/results/fit3.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit3)$pars[,17])),4), 
                    ' & ','(', signif(exp(mean(rstan::extract(fit3)$pars[,17])-1.96*sd(rstan::extract(fit3)$pars[,17])),4), 
                    ', ', signif(exp(mean(rstan::extract(fit3)$pars[,17])+1.96*sd(rstan::extract(fit3)$pars[,17])),4), ')', 
                    sep = '')))

load('../dataverse/results/fit4.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit4)$pars[,1])),4), 
                    ' & ','(', signif(exp(mean(rstan::extract(fit4)$pars[,1])-1.96*sd(rstan::extract(fit4)$pars[,1])),4), 
                    ', ', signif(exp(mean(rstan::extract(fit4)$pars[,1])+1.96*sd(rstan::extract(fit4)$pars[,1])),4), ')', 
                    sep = '')))

load('../dataverse/results/fit5.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit5)$pars[,2])),4), 
                    ' & ','(', signif(exp(mean(rstan::extract(fit5)$pars[,2])-1.96*sd(rstan::extract(fit5)$pars[,2])),4), 
                    ', ', signif(exp(mean(rstan::extract(fit5)$pars[,2])+1.96*sd(rstan::extract(fit5)$pars[,2])),4), ')', 
                    sep = '')))

load('../dataverse/results/fit6.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit6)$pars[,1])),4), 
                    ' & ','(', signif(exp(mean(rstan::extract(fit6)$pars[,1])-1.96*sd(rstan::extract(fit6)$pars[,1])),4), 
                    ', ', signif(exp(mean(rstan::extract(fit6)$pars[,1])+1.96*sd(rstan::extract(fit6)$pars[,1])),4), ')', 
                    sep = '')))

load('../dataverse/results/fit7.Rdata')
print(noquote(paste(signif(exp(mean(rstan::extract(fit7)$pars[,2])),4), 
                    ' & ','(', signif(exp(mean(rstan::extract(fit7)$pars[,2])-1.96*sd(rstan::extract(fit7)$pars[,2])),4), 
                    ', ', signif(exp(mean(rstan::extract(fit7)$pars[,2])+1.96*sd(rstan::extract(fit7)$pars[,2])),4), ')', 
                    sep = '')))