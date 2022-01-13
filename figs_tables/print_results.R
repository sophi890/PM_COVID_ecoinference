library('rstan')
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Main Analysis
load('./results/fit6.Rdata')
print(fit6)
for (i in 1:24){
  print(noquote(paste(signif(exp(mean(extract(fit6)$pars[,i])),4), ' & ','(', signif(exp(mean(extract(fit6)$pars[,i])-1.96*sd(extract(fit6)$pars[,i])),4), ', ', signif(exp(mean(extract(fit6)$pars[,i])+1.96*sd(extract(fit6)$pars[,i])),4), ')', sep = '')))
}