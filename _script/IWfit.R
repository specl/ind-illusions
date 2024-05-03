source("../_logistics/funs.R")

dat = read.csv("../_data/clean_data.csv")
dat$S = as.numeric(factor(dat$sub))
dat$J = as.numeric(factor(dat$task))
dat$JV = (dat$J-1)*2+dat$version
vars = tapply(dat$y, list(dat$S, dat$JV), var)
div = sqrt(apply(vars, 2, mean))
dat$y_coerced = dat$y/div[dat$JV]

data_list = list(
  "y" = dat$y_coerced,
  "J" = 10,
  "task" = dat$JV,
  "sub" = dat$S,
  "N" = nrow(dat),
  "I" = max(dat$S),
  "diagJ" = diag(max(dat$JV)),
  "tuneT" = 1
)

pars = c("pSig2", "theta")

fit.IW = runJags(modIWJags, data_list, pars)

saveRDS(fit.IW, "../_data/JAGS-IW-fit.RDS")


pSig2 = fit.IW$BUGSoutput$sims.list$pSig2
theta = fit.IW$BUGSoutput$sims.list$theta

ind_corr = popThetaCors(theta)
pop_corr = popSigCors(pSig2, p = T)


samp_cor = cor(tapply(dat$y_coerced, list(dat$S, dat$JV), mean))
mod_ind = ind_corr$avg_cor
mod_pop = pop_corr$avg_cor

out = list(
  "samp_cor" = cor(tapply(dat$y_coerced, list(dat$S, dat$JV), mean)),
  "mod_ind_cor_avg" = ind_corr$avg_cor,
  "mod_ind_cor_post" = ind_corr$post_cor,
  "mod_pop_cor_avg" = pop_corr$avg_cor,
  "mod_pop_cor_post" = pop_corr$post_cor
)

saveRDS(out, "../data/correlations.RDS")
