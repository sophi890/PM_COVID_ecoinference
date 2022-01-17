library('rstan')
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Main Analysis
load('./results/fit6.Rdata')
print(fit6)
for (i in 1:24){
  print(noquote(paste(signif(exp(mean(extract(fit6)$pars[,i])),4), ' & ','(', signif(exp(mean(extract(fit6)$pars[,i])-1.96*sd(extract(fit6)$pars[,i])),4), ', ', signif(exp(mean(extract(fit6)$pars[,i])+1.96*sd(extract(fit6)$pars[,i])),4), ')', sep = '')))
}

load('./results/fit_fakesex.Rdata')
print(fit_fakesex)
for (i in 1:24){
  print(noquote(paste(signif(exp(mean(extract(fit_fakesex)$pars[,i])),4), ' & ','(', signif(exp(mean(extract(fit_fakesex)$pars[,i])-1.96*sd(extract(fit_fakesex)$pars[,i])),4), ', ', signif(exp(mean(extract(fit_fakesex)$pars[,i])+1.96*sd(extract(fit_fakesex)$pars[,i])),4), ')', sep = '')))
}

load('./results/fit_sexonly.Rdata')
print(fit_sexonly)
for (i in 1:18){
  print(noquote(paste(signif(exp(mean(extract(fit_sexonly)$pars[,i])),4), ' & ','(', signif(exp(mean(extract(fit_sexonly)$pars[,i])-1.96*sd(extract(fit_sexonly)$pars[,i])),4), ', ', signif(exp(mean(extract(fit_sexonly)$pars[,i])+1.96*sd(extract(fit_sexonly)$pars[,i])),4), ')', sep = '')))
}

load('./results/fit_withoutsex.Rdata')
print(fit_withoutsex)
for (i in 1:23){
  print(noquote(paste(signif(exp(mean(extract(fit_withoutsex)$pars[,i])),4), ' & ','(', signif(exp(mean(extract(fit_withoutsex)$pars[,i])-1.96*sd(extract(fit_withoutsex)$pars[,i])),4), ', ', signif(exp(mean(extract(fit_withoutsex)$pars[,i])+1.96*sd(extract(fit_withoutsex)$pars[,i])),4), ')', sep = '')))
}
