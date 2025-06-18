
col = readRDS("plot-cols.RDS")

library(mvtnorm)
set.seed(123)
nu = rep(0, 2)
sig = matrix(c(1,.9,.9,1), nrow = 2)

I = 50
mu = rmvnorm(I, nu, sig)

I2 = 500
L = 50
J = 2
sub = rep(1:I, each = J*L)
task = rep(rep(1:J, each = L),I)
trials = rep(rep(1:L, J), I)
dat = data.frame("sub" = sub,
                 "task" = task,
                 "trial" = trials)
subtask = cbind(dat$sub, dat$task)
mus = mu[subtask]
dat$y1 = rnorm(length(mus), mus, 7)

scores = tapply(dat$y1, list(dat$sub, dat$task), mean)



sub2 = rep(1:I2, each = J*L)
mu2 = rmvnorm(I2, nu, sig)
dat2 = data.frame("sub" = sub2,
                 "task" = task,
                 "trial" = trials)
subtask = cbind(dat2$sub, dat2$task)
mus2 = mu2[subtask]
dat2$y1 = rnorm(length(mus2), mus2, 7)

scores2 = tapply(dat2$y1, list(dat2$sub, dat2$task), mean)

a = -10
b = 3.5
e = 2
d = 4

pdf("../_figs/intro-plot.pdf",
    width = 7,
    height = 4)

layout_matrix = matrix(1:4, nrow = 2, byrow = TRUE)
layout(layout_matrix)
current_mar = c(5.1, 4.1, 4.1, 2.1)
t_current_mar = current_mar 
par(mar = t_current_mar)
xlim = c(-5, 5)
ylim = c(-5, 5)
col1 = rgb(t(col2rgb(col$Thistle[5]) / 255), alpha = 0.7)
lt = 3



t_current_mar = current_mar - c(4, 0, 2, 2)
par(mar = t_current_mar)

plot(NA, NA, 
     xlim = xlim,
     ylim = ylim,
     pch = 19,
     axes = F,
     asp = 1, 
     xlab = "",
     ylab = "",
     main = "True Scores")
box(lty = lt)

# mtext("Task 1", side = 1, line = 1, cex = 1.2)
mtext("Task 2", side = 2, line = 1, cex = 1.2)

abline(v = 0, lty = lt)
abline(h = 0, lty = lt)
points(mu, pch = 19, col = col1, cex = .8)
text_expression = bquote(italic(r) == .(round(cor(mu)[1,2],2)))
text(a, b, text_expression, cex = 1.1, pos = d)
text_expression = bquote(n == .(round(I,2)))
text(a, e, text_expression, cex = 1.1, pos = d)


t_current_mar = current_mar - c(4, 2, 2, 0)
par(mar = t_current_mar)

plot(NA, NA, 
     xlim = xlim,
     ylim = ylim,
     pch = 19,
     axes = F,
     asp = 1, 
     xlab = "",
     ylab = "",
     main = "Observed Scores")
box(lty = lt)
# mtext("Task 1", side = 1, line = 1, cex = 1.2)
# mtext("Task 2", side = 2, line = 1, cex = 1.2)
abline(v = 0, lty = lt)
abline(h = 0, lty = lt)
points(scores, pch = 19, col = col1, cex = .8)
text_expression = bquote(italic(r) == .(round(cor(scores)[1,2],2)))
text(a, b, text_expression, cex = 1.1, pos = d)
text_expression = bquote(n == .(round(I,2)))
text(a, e, text_expression, cex = 1.1, pos = d)


t_current_mar = current_mar - c(3, 0, 3, 2)
par(mar = t_current_mar)


plot(NA, NA, 
     xlim = xlim,
     ylim = ylim,
     pch = 19,
     axes = F,
     asp = 1, 
     xlab = "",
     ylab = "",
     main = "")
box(lty = lt)

mtext("Task 1", side = 1, line = 1, cex = 1.2)
mtext("Task 2", side = 2, line = 1, cex = 1.2)

abline(v = 0, lty = lt)
abline(h = 0, lty = lt)
points(mu2, pch = 19, col = col1, cex = .8)
text_expression = bquote(italic(r) == .(round(cor(mu2)[1,2],2)))
text(a, b, text_expression, cex = 1.1, pos = d)
text_expression = bquote(n == .(round(I2,2)))
text(a, e, text_expression, cex = 1.1, pos = d)

t_current_mar = current_mar - c(3, 2, 3, 0)
par(mar = t_current_mar)

plot(NA, NA, 
     xlim = xlim,
     ylim = ylim,
     pch = 19,
     axes = F,
     asp = 1, 
     xlab = "",
     ylab = "",
     main = "")
box(lty = lt)
mtext("Task 1", side = 1, line = 1, cex = 1.2)
# mtext("Task 2", side = 2, line = 1, cex = 1.2)
abline(v = 0, lty = lt)
abline(h = 0, lty = lt)
points(scores2, pch = 19, col = col1, cex = .8)
text_expression = bquote(italic(r) == .(round(cor(scores2)[1,2],2)))
text(a, b, text_expression, cex = 1.1, pos = d)
text_expression = bquote(n == .(round(I2,2)))
text(a, e, text_expression, cex = 1.1, pos = d)
dev.off()