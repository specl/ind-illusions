results = readRDS("../_results/correlations.RDS")
pop_cors = results$mod_ind_cor_post

pdf("../_figs/cor-CI.pdf",
    width = 6,
    height = 5)

avg_cor = apply(pop_cors, 2:3, mean)
avg_cor = avg_cor[upper.tri(avg_cor)]
CI_cor_l = apply(pop_cors, 2:3, function (x) quantile(x, prob = .025))
CI_cor_u = apply(pop_cors, 2:3, function (x) quantile(x, prob = .975))
CI_cor_l = CI_cor_l[upper.tri(CI_cor_l)]
CI_cor_u = CI_cor_u[upper.tri(CI_cor_u)]
o = order(avg_cor)
avg_cor = avg_cor[o]
CI_cor_l = CI_cor_l[o]
CI_cor_u = CI_cor_u[o]

ind = matrix(1, 10, 10)
mat = matrix(
  data = 1:10,
  nrow = 2
)
ind[cbind(mat[1, 1:5], mat[2, 1:5])] = 2
ind = ind[upper.tri(ind)]
ind = ind[o]

cols = readRDS("plot-cols.RDS")

p_cols = c(cols[[1]][4], cols[[2]][[4]])
a_cols = c(cols[[1]][5], cols[[2]][[5]])

a_cols_list = lapply(ind, function(x) a_cols[x])
p_cols_list = lapply(ind, function(x) p_cols[x])


myPoints = function(x,y,col){
  points(x,y, pch = 16, cex = 1.3, col = col)
}
myArrows = function(x,y1,y2,col){
  arrows(x0 = x, x1 = x, y0 = y1, y1 = y2, code = 3, angle = 90, length = .05, col = col, lwd = 2)
}


source("~/Git/ctx-pca/presentations/MathPsych2023/aux.R")
setwd("~/Git/ctx-pca/presentations/MathPsych2023")

res = gammaPlotJags(run = F)
corrs = res$theta_cor


ll = quantile(corrs, prob = .025)
ul = quantile(corrs, prob = .975)
mc = mean(corrs)

CI_cor_l[length(CI_cor_l)+1] = ll
CI_cor_u[length(CI_cor_u)+1] = ul
avg_cor[length(avg_cor)+1] = mc
p_cols_list[[length(p_cols_list)+1]] = "palevioletred2"
a_cols_list[[length(a_cols_list)+1]] = "palevioletred2"
# pdf("../_manuscript/_figs/cor-CI.pdf",
#         width = 6,
#         height = 5)
plot(NA,
     NA,
     xlim = c(1,46),
     ylim = c(-.2,1),
     axes = F,
     xlab = "",
     ylab = ""
)
axis(1, at = c(1, 46), labels = NA)
axis(2, las = 1)
mtext("Correlation Coefficients", side = 2, line = 2.5, cex = 1)
mtext("Task-Version Correlations", side = 1, line = 1.5, cex = 1)
mapply(myArrows, 1:46, CI_cor_l, CI_cor_u, a_cols_list)
mapply(myPoints, 1:46, avg_cor, p_cols_list)

legend(
  "top",
  fill = unlist(unique(p_cols_list)), 
  legend = c("Inter-version Illusion", "Intra-version Illusions", "Stroop and flanker"),
  cex = 1,          
  bty = "o",     
  box.col = "cornsilk",  
  bg = "cornsilk",  
  border = "black", 
  title = "Model-Based Correlations"
)


dev.off()
