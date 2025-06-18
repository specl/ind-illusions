
source("../_logistics/funs.R")
source("../_logistics/mods.R")

require(abind)
require(infinitefactor)
require(rlist)
dat = read.csv("../_data/clean_data.csv")
dat$S = as.numeric(factor(dat$sub))
dat$J = as.numeric(factor(dat$task))
dat$JV = (dat$J-1)*2+dat$version

v1 = tapply(dat$y, list(dat$S, dat$JV), var)
divisor = sqrt(apply(v1, 2, mean))
dat$yc = dat$y / divisor[dat$JV]

fit = readRDS("../_results/bhfm_mod.rds")


lambda = fit$BUGSoutput$sims.list$lambda
lamb_aligned = align(lambda[,,6:7], I =length(unique(dat$S)))
lamb_aligned_eigen = f2Eigenmax(lamb_aligned)
lambda[,,6:7] = makePositive(lamb_aligned_eigen, I = length(unique(dat$S)))
del2 = fit$BUGSoutput$sims.list$del2
samples = fit$BUGSoutput$sims.list
samples$lambda = lambda
std_out = standardize(samples)
tab = makeTable(std_out$lambda, std_out$del2)
write.csv(tab, "../_results/bhfm_lambda.csv")