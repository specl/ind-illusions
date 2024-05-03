

results = readRDS("../_results/correlations.RDS")

cor_mat = matrix(1, 10, 10)
cor_mat[upper.tri(cor_mat)] = results$samp_cor[upper.tri(cor_mat)]
cor_mat[lower.tri(cor_mat)] = results$mod_ind_cor_avg[lower.tri(cor_mat)]
cor_mat = round(cor_mat, 2)

suffixes = c(" v1", " v2")
tasks = c("Br", "Eb", "Pog", "Pz", "Zol")
tasks_vs = sapply(tasks, function(x) paste0(x, suffixes))
colnames(cor_mat) = rownames(cor_mat) = tasks_vs



pdf("../_manuscript/_figs/cor.pdf",
    width = 6,
    height = 6)
current_mar = c(5.1, 4.1, 4.1, 4.1)
t_current_mar = current_mar + c(0, 0, 0, 2)
par(mar = t_current_mar)
corrplot::corrplot(
  cor_mat,
  method="color",
  diag = F,
  cl.pos='b',
  tl.col='black',
  tl.srt = 45,
  col.lim=c(0,1),
  cl.ratio = .2,
  cl.length= 2,
  addgrid.col = 'black',
  addCoef.col = "black",
  cl.cex = 1.5,
  tl.cex = 1.2
)
dev.off()

#
pop_cors = results$mod_ind_cor_post

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
  points(x,y, pch = 16, cex = 1.2, col = col)
}
myArrows = function(x,y1,y2,col){
  arrows(x0 = x, x1 = x, y0 = y1, y1 = y2, code = 3, angle = 90, length = .05, col = col)
}



pdf("../_manuscript/_figs/cor-CI.pdf",
        width = 6,
        height = 5)
plot(NA,
     NA,
     xlim = c(1,45),
     ylim = c(-.2,1),
     axes = F,
     xlab = "",
     ylab = ""
)
axis(1, at = c(1, 45), labels = NA)
axis(2, las = 1)
mtext("Correlation Coefficients", side = 2, line = 2.5)
mtext("Task-Version Correlations", side = 1, line = 1.5)
mapply(myArrows, 1:45, CI_cor_l, CI_cor_u, a_cols_list)
mapply(myPoints, 1:45, avg_cor, p_cols_list)

dev.off()

pdf("../_manuscript/_figs/cor-rt.pdf", width = 6, height = 4)

dat = read.csv("../_data/clean_data.csv")
dat$S = as.numeric(factor(dat$sub))
dat$J = as.numeric(factor(dat$task))
dat$JV = (dat$J-1)*2+dat$version

scores = tapply(dat$y, list(dat$S, dat$JV), mean)
RTs = tapply(dat$rt, list(dat$S, dat$JV), mean)
y_rts = cbind(scores, log(1/RTs))
cor_y_rts = cor(y_rts)

current_mar = c(5.1, 4.1, 4.1, 4.1)
t_current_mar = current_mar + c(0, 4, 0, 2)
n = ncol(cor_y_rts)
outer_margin = c(1, 1, 1, .5)
layout_matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
layout(layout_matrix, widths=c(1, 0.85))
par(oma = outer_margin)
corrplot(cor_y_rts, method="color", diag=FALSE, tl.pos="n", cl.pos='b', col.lim=c(-1,1), cl.ratio=.2, cl.length=2, cl.cex=1)

corrplot(cor_y_rts, method="color", diag=TRUE, tl.pos="lt", tl.col="white", tl.srt=45, tl.cex=1, cl.pos='n', col.lim=c(-1,1), cl.ratio=.2, cl.length=2, cl.cex=1.5, add=TRUE)

i <- nrow(cor_y_rts)
j <- ncol(cor_y_rts)
rect(j - 9.5, i - 9.5, j + 0.5, i + 0.5, border="black", lwd=3)
mtext("Response Times", side = 2, line = 4, at = 5)
mtext("Response Times", side = 3, line = 4, at = 15.5)
mtext("Adjustments", side = 3, line = 4, at = 5)
mtext("Adjustments", side = 2, line = 4, at = 15.5)

corrplot(cor_y_rts[1:10,11:20], method="color", diag=TRUE, addCoef.col = 'black', tl.pos="lt", tl.col="white", tl.srt=45, tl.cex=.01, cl.pos='n', col.lim=c(-1,1), cl.ratio=.2, cl.length=2, cl.cex=1.5, number.cex=.4)

rect(j - 9.5, i - 9.5, j - 19.5, i -19.5, border="black", lwd=3)

dev.off()



mean(avg_cor[ind != 2])^2
mean(avg_cor[ind != 2])


x = c(.534, .606, .580, .423, .739, .641,.916)
sum(x)/length(x)
