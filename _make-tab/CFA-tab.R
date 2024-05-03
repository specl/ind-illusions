source("../_logistics/funs.R")
dat = read.csv("../_data/clean_data.csv")
dat$S = as.numeric(factor(dat$sub))
dat$J = as.numeric(factor(dat$task))
dat$JV = (dat$J-1)*2+dat$version

vars = tapply(dat$y, list(dat$S, dat$JV), var)
div = sqrt(apply(vars, 2, mean))
dat$y_coerced = dat$y/div[dat$JV]

scores = tapply(dat$y_coerced, list(dat$S, dat$JV), mean)


suffixes = c("1", "2")
tasks = c("br", "eb", "pog", "pz", "zol")
tasks_vs = sapply(tasks, function(x) paste0(x, suffixes))
new_names = as.list(as.vector(tasks_vs))


require(lavaan)

z_score = apply(scores,2,scale)
colnames(z_score) = new_names
z_score = as.data.frame(z_score)

fit = efa(data = z_score, 
          nfactors = 1:6,
          rotation="varimax")

ev = eigen(cor(z_score))$value

tab_out = cbind(1:6,ev[1:6]/sum(ev),t(fitMeasures(fit,c("aic","bic","cfi"))))
colnames(tab_out) = c("# Factors","Prop Var","AIC","BIC","CFI")
rownames(tab_out) = NULL

write.csv(tab_out, "../_results/EFA_fit_measures.csv")


tab_out2 = fit$loadings$nf5
o = order(apply(tab_out2, 2, function(x) sum(x^2)), decreasing = T)
tab_out2 = tab_out2[,o]

suffixes = c(" V1", " V2")
tasks = c("Brentano", "Ebbinghaus", "Poggendorf", "Ponzo", "Zoellner")
tasks_vs = sapply(tasks, function(x) paste0(x, suffixes))
new_names = as.list(as.vector(tasks_vs))
rownames(tab_out2) = new_names
colnames(tab_out2) = 1:5


write.csv(round(tab_out2,3), "../_results/EFA_factor_loading.csv")


select_task = c(1, 4, 5, 7, 9)
ind = which(dat$JV %in% select_task)
tdat = dat[ind, ]
tdat$JV = as.numeric(factor(tdat$JV))

vars = tapply(tdat$y, list(tdat$S, tdat$JV), var)
div = sqrt(apply(vars, 2, mean))
tdat$y_coerced = tdat$y/div[tdat$JV]

tnew_names = new_names[select_task]

scores = tapply(tdat$y_coerced, list(tdat$S, tdat$JV), mean)
z_score = apply(scores,2,scale)
colnames(z_score) = LETTERS[1:5]
z_score = as.data.frame(z_score)


fit = efa(data = z_score, 
          nfactors = 1:2,
          rotation="varimax")

ev = eigen(cor(z_score))$value

tab_out = cbind(1:2,ev[1:2]/sum(ev),t(fitMeasures(fit,c("aic","bic","cfi"))))
colnames(tab_out) = c("# Factors","Prop Var","AIC","BIC","CFI")
rownames(tab_out) = NULL


tab_out2 = fit$loadings$nf2
o = order(apply(tab_out2, 2, function(x) sum(x^2)), decreasing = T)
tab_out2 = tab_out2[,o]
rownames(tab_out2) = tnew_names
colnames(tab_out2) = 1:5

write.csv(round(tab_out,2), "../_results/EFA_fit_measures_no_version.csv")
write.csv(round(tab_out2,3), "../_results/EFA_factor_loading_no_version.csv")








