
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


if (file.exists("../_results/bhfm_mod.rds")) {
  fit = readRDS("../_results/bhfm_mod.rds")
} else {
  constraint_matrix_zero = matrix(0, nrow = 10, ncol = 7)
  for (i in 1:5){constraint_matrix_zero[(i*2-1):(i*2),i] = 1}
  constraint_matrix_zero[,6:7] = 1
  constraint_matrix_pos = matrix(0, nrow = 10, ncol = 7)
  for (i in 1:5){constraint_matrix_pos[(i*2-1),i] = 1}
  
  data_list = list(
    y = dat$yc,
    task = dat$JV,
    sub = dat$S,             
    I = length(unique(dat$S)),
    J = length(unique(dat$JV)),
    N = nrow(dat),
    D = 7,                     
    constraint_zero = constraint_matrix_zero,
    constraint_pos = constraint_matrix_pos,
    mu.m = 2,
    mu.s = 2,
    tuneDelta = 1,
    tuneLambda = 1
  )
  parameters = c("lambda", "theta", "mu", "del2", "pTau2", "Sigma")
  fit = runJags(mod_bhfm, data_list, parameters)
  saveRDS(fit, "../_results/bhfm_mod.rds")
}

