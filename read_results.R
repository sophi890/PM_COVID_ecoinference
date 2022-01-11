library('rstan')
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

load('results/fit_withoutsex.Rdata')
print(fit_withoutsex)
for (i in 1:23){
  print(noquote(paste(signif(exp(mean(extract(fit_withoutsex)$pars[,i])),4), ' & ','(', signif(exp(mean(extract(fit_withoutsex)$pars[,i])-1.96*sd(extract(fit_withoutsex)$pars[,i])),4), ', ', signif(exp(mean(extract(fit_withoutsex)$pars[,i])+1.96*sd(extract(fit_withoutsex)$pars[,i])),4), ')', sep = '')))
}

load('results/fit_sexonly.Rdata')
print(fit_sexonly)
for (i in 1:23){
  print(noquote(paste(signif(exp(mean(extract(fit_withoutsex)$pars[,i])),4), ' & ','(', signif(exp(mean(extract(fit_withoutsex)$pars[,i])-1.96*sd(extract(fit_withoutsex)$pars[,i])),4), ', ', signif(exp(mean(extract(fit_withoutsex)$pars[,i])+1.96*sd(extract(fit_withoutsex)$pars[,i])),4), ')', sep = '')))
}
