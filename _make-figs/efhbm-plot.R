source("../_logistics/funs.R")
source("../_logistics/mods.R")


fit = readRDS("../_results/bhfm_mod.rds")



lambda = fit$BUGSoutput$sims.list$lambda
lamb_aligned = align(lambda[,,6:7], I =length(unique(dat$S)))
lamb_aligned_eigen = f2Eigenmax(lamb_aligned)
lambda[,,6:7] = makePositive(lamb_aligned_eigen, I = length(unique(dat$S)))
del2 = fit$BUGSoutput$sims.list$del2
samples = fit$BUGSoutput$sims.list
samples$lambda = lambda
std_out = standardize(samples)


dims = dim(lambda)
M = dims[1]  
J = dims[2]  
D = dims[3]  
lam_sum = matrix(0, J, D)
uni_prop_task_mat = matrix(0, M, J)
fac_prop_total_mat = matrix(0, M, D)
uni_prop_total_vec = numeric(M)

for (m in 1:M) {
  tlam = lambda[m,,]
  tdel = del2[m,]
  
  tot_com_task = rowSums(tlam^2)
  tot_task_j = tot_com_task + tdel
  tot_com_fac = colSums(tlam^2)
  tot_uni = sum(tdel)
  tot_var = sum(tot_task_j)
  
  lam_sum = lam_sum + tlam
  uni_prop_task_mat[m,] = tdel / tot_task_j
  fac_prop_total_mat[m,] = tot_com_fac / tot_var
  uni_prop_total_vec[m] = tot_uni / tot_var
}

lam_mean = round(lam_sum / M, 3)


task_names = c(
  "Brentano 1", "Brentano 2",
  "Ebbinghaus 1", "Ebbinghaus 2",
  "Poggendorf 1", "Poggendorf 2",
  "Ponzo 1", "Ponzo 2",
  "Zöllner 1", "Zöllner 2"
)

task_names2 = c(
  "Brentano",
  "Ebbinghaus",
  "Poggendorf",
  "Ponzo",
  "Zöllner"
)


base_colors = RColorBrewer::brewer.pal(5, "Set1")
cols = unlist(lapply(base_colors, function(col) {
  c(adjustcolor(col, alpha.f = 0.6), col)
}))


pdf("../_figs/efhbm.pdf",
    width = 6,
    height = 3)
par(mfrow = c(1, 3), mar = c(5.2, 1.5, 2.5, .5))

## Plot 1
plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, xlab = "", ylab = "")
grid(lwd = 2)
box()
abline(h = 0, v = 0, lwd = 2)
for (i in 1:nrow(lam_mean)) {
  segments(0, 0, lam_mean[i, 6], lam_mean[i, 7], col = cols[i], lwd = 2)
}
points(lam_mean[, 6], lam_mean[, 7], col = cols, pch = 19, cex = 1.2, lwd = 2)
legend("bottomleft", legend = task_names, col = cols, pch = 19, cex = .7, bg = "white")
mtext("(A)", side = 3, line = .35, cex = 0.75, font = 2, adj = 0)
mtext("General Factor 1", side = 1, line = .5, cex = .5)
mtext("General Factor 2", side = 2, line = .5, cex = .5)


## Plot 2
plot(NA, xlim = c(0, 1), ylim = c(0, 50), axes = FALSE, xlab = "", ylab = "")
axis(1); box()
tcols = cols[c(1,3,5,7,9)]
for (j in 1:5) {
  dens = density(fac_prop_total_mat[, j])
  polygon(dens, col = adjustcolor(tcols[j], 0.4), border = tcols[j], lwd = 2)
}
legend("topright", legend = task_names2, fill = adjustcolor(tcols, 0.4), border = tcols, cex = 1)
mtext("(B)", side = 3, line = .35, cex = 0.75, font = 2, adj = 0)
mtext("Proportion of Variance \nExplained by Nuisance Factors", side = 1, line = 2.75, cex = .5)
mtext("Posterior Probability", side = 2, line = .5, cex = .5)

## Plot 3
plot(NA, xlim = c(0, 1), ylim = c(0, 50), axes = FALSE, xlab = "", ylab = "")
axis(1); box()
tcols2 = c("black", "gray80", "brown")
for (d in 6:7) {
  dens = density(fac_prop_total_mat[, d])
  polygon(dens, col = adjustcolor(tcols2[d-5], 0.4), border = tcols2[d-5], lwd = 2)
}
dens = density(uni_prop_total_vec)
polygon(dens, col = adjustcolor(tcols2[3], 0.4), border = tcols2[3], lwd = 2)

legend("topright", legend = c("General Factor 1", "General Factor 2", "Unique Variance"),
       fill = adjustcolor(tcols2[1:3], 0.4), border = tcols2[1:3], cex = 1)
mtext("(C)", side = 3, line = .35, cex = 0.75, font = 2, adj = 0)
mtext("Proportion of Variance Explained \nby General Factors and Residuals", side = 1, line = 2.75, cex = .5)
mtext("Posterior Probability", side = 2, line = .5, cex = .5)

dev.off()
# 
# 
# 

# y_labs = rep(c(T, F, F, F), 3)[1:12]
# x_labs = c(rep(F, 8), rep(T, 4))
# top = c(rep(1.5, 4), rep(0, 8))
# # g_pos = list(
# #   "1" = c(35,5),
# #   "2" = c(35,8-1-2/6),
# #   "3" = c(35,3.5),
# #   "4" = c(35,6-1-1/6),
# #   "5" = c(35,8-1-2/6),
# #   "6" = c(35,6-1-2/6),
# #   "7" = c(35,9-1-3/6),
# #   "8" = c(100,-1),
# #   "9" = c(35,9-1-4/6),
# #   "10" = c(35,8-1-3/6),
# #   "11" = c(70,1.5),
# #   "12" = c(70,1.5)
# # )
# 
# g_pos = lapply(1:10, function(x) c(5,7,5,5.75))
# g_pos[[11]] = g_pos[[12]] = c(10,.85,10,.65)
# 
# layout_matrix = matrix(1:12, nrow = 3, byrow = TRUE)
# layout(layout_matrix)
# layout(layout_matrix, heights = c(.9, .8, .8), widths = c(rep(c(.95, rep(.8,3)),3)))
# mapply(plotTasks, scores_list, post_theta, new_names, y_labs, x_labs, top, F, g_list, exp_rel_list, g_pos)
# # mapply(plotHalves, score_list, CI_95_list_co, new_names, y_labs, x_labs, top, F)
# dev.off()
# 
# # plotTasks(scores_list[[3]], scores_se_list[[3]], new_names[[3]], T, T, top[1], F, g_list[[1]])
