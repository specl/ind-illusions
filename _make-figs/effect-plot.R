source("../_logistics/funs.R")
dat = read.csv("../_data/clean_data.csv")
dat$S = as.numeric(factor(dat$sub))
dat$J = as.numeric(factor(dat$task))
dat$JV = (dat$J-1)*2+dat$version


suffixes = c(" V1", " V2")
tasks = c("Brentano", "Ebbinghaus", "Poggendorf", "Ponzo", "Zoellner")
tasks_vs = sapply(tasks, function(x) paste0(x, suffixes))
new_names = as.list(as.vector(tasks_vs))


vars = tapply(dat$y, list(dat$S, dat$JV), var)
div = sqrt(vars)
dat$y_fined = dat$y/div[cbind(dat$S, dat$JV)]
col = readRDS("plot-cols.RDS")
col1 = col$Lavender[[1]]
effect_size = tapply(dat$y_fined, list(dat$S, dat$JV), mean)


plot1 = function(){
  hist(effect_size,
       main = "",
       axes = F,
       xlab = "",
       ylab = "",
       col = adjustcolor(col1, alpha.f = .5),
       border = col1,
       breaks = 30,
       xlim = c(-1, 12),
       ylim = c(0, 200),
       cex.lab = 1.2)
  mtext("Effect Size", side = 1, line = 3, cex = 1.2)
  mtext("Frequency", side = 2, line = 3.5, cex = 1.2)
  mtext("t-value", side = 3, line = 2.5, cex = 1.2)
  axis(1, at = seq(0, 12, 3), labels = seq(0, 12, 3))
  axis(2, at = seq(0, 200, length.out = 5), 
       labels = seq(0, 200, length.out = 5), 
       line = .5,
       las = 2)
  axis(3, at = seq(0, 40, 20)/sqrt(15), labels = seq(0, 40, 20), line = .5)
  lines(x = c(.8, .8), y = c(0, 200), lty=2, lwd=2, col = "firebrick4")
  lines(x = rep(qt(.975,14)/sqrt(15), 2), y = c(0, 200), lty=2, lwd=2, col = "darkcyan")
}


myLine = function(y, col){lines(ecdf(y),do.points=F,col=col)}
col2 =   col$Lavender[[4]]
plot2 = function(){
  plot(NA, NA,
       main = "",
       axes = F,
       xlab = "",
       ylab = "",
       xlim = c(-1, 12),
       ylim = c(0, 1),
       cex.lab = 1.2
  )
  mtext("Effect Size", side = 1, line = 3, cex = 1.2)
  mtext("Cumulative Proportion", side = 2, line = 3.5, cex = 1.2)
  mtext("t-value", side = 3, line = 2.5, cex = 1.2)
  axis(1, at = seq(0, 12, 3), labels = seq(0, 12, 3))
  axis(2, at = seq(0, 1, length.out = 5), 
       labels = seq(0, 1, length.out = 5), 
       line = .5,
       las = 2)
  axis(3, at = seq(0, 40, 20)/sqrt(15), labels = seq(0, 40, 20), line = .5)
  score_list = lapply(1:10, function(i) effect_size[,i])
  mapply(myLine, score_list, col2)
  lines(x = c(.8, .8), y = c(0, 1), lty=2, lwd=2, col = "firebrick4")
  lines(x = rep(qt(.975,14)/sqrt(15), 2), y = c(0, 1), lty=2, lwd=2, col = "darkcyan")
}

pdf("../_manuscript/_figs/effect-plot.pdf",
    width = 7,
    height = 5)
layout_matrix = matrix(1:2, nrow = 1, byrow = TRUE)
layout(layout_matrix)
current_mar = par("mar")
t_current_mar = current_mar - c(0, -1, 0 ,2)
par(mar = t_current_mar)
plot1()
t_current_mar = current_mar - c(0, 0, 0 ,1)
par(mar = t_current_mar )
plot2()
dev.off()
