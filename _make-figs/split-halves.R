source("../_logistics/funs.R")
dat = read.csv("../_data/clean_data.csv")
dat$S = as.numeric(factor(dat$sub))
dat$J = as.numeric(factor(dat$task))

addTrialNum = function(x){
  x$S = as.numeric(factor(x$sub))
  ref = table(x$S, x$cond)
  x$trial = NA
  x$half = NA
  for (i in 1:max(x$S)) {
    for (u in 1:2){
      ind = which(x$S == i & x$cond == u)
      if (length(ind) != ref[i,u]){print("stop")}
      x$trial[ind] = 1:ref[i, u]
      x$half[ind] = ifelse(x$trial[ind] > ceiling(ref[i, u]/2), 2, 1)
    }
  }
  return(x)
}
stroop = addTrialNum(readRMStroopI())
flank = addTrialNum(readRMFlankI())

dat$half = ifelse(dat$trial<8,1,2)
scores = tapply(dat$y, list(dat$S,  dat$half, dat$version, dat$J), mean)
suffixes = c(" v1", " v2")
tasks = sort(unique(dat$task))
tasks = c("Brentano", "Ebbinghaus", "Poggendorf", "Ponzo", "Zoellner")
tasks_vs = sapply(tasks, function(x) paste0(x, suffixes))
new_names = as.list(as.vector(tasks_vs))
tscores = array(scores, dim = c(nrow(scores), 2, 10))
score_list = list()

scores_stroop_mu = tapply(stroop$y, list(stroop$S, stroop$cond, stroop$half), mean)
scores_stroop_sd = tapply(stroop$y, list(stroop$S, stroop$cond, stroop$half), sd)
tscores_stroop_mu = scores_stroop_mu[,2,] - scores_stroop_mu[,1,]
# tscores_stroop_sd = apply(scores_stroop_sd, 1, mean)

scores_flank_mu = tapply(flank$y, list(flank$S, flank$cond, flank$half), mean)
scores_flank_sd = tapply(flank$y, list(flank$S, flank$cond, flank$half), sd)
tscores_flank_mu = scores_flank_mu[,2,] - scores_flank_mu[,1,]


for (i in 1:10){score_list[[i]] = tscores[,,i]}

score_list[[11]] = tscores_stroop_mu
new_names[[11]] = "Stroop"
score_list[[12]] = tscores_flank_mu
new_names[[12]] = "Flanker"
cols = readRDS("plot-cols.RDS")


col1 = cols$Lavender[3]
col2 = cols$Lavender[5]
plotHalves = function(tscores, task_name, y_labs, x_labs, top = 0){
  left = c(2, 4)
  bottom = c(4, 4)
  par(mar = c(bottom[x_labs+1], left[y_labs+1], top, 1))
  tscores = scale(tscores)
  cor.val = round(cor(tscores), 2)
  # xlims = c(min(tscores[,1:2]), max(tscores[,1:2]))
  # ylims = c(min(tscores[,1:2]), max(tscores[,1:2]))
  xlims = c(-4.2,4.3)
  ylims = c(-4.2,4.3)
  plot(NA,
       NA,
       xlim = xlims,
       ylim = ylims,
       axes = F,
       xlab = "",
       ylab = "",
       main = "",
       asp = 1,
       cex.main = 1.5
  )
  mtext(task_name, side = 3, line = 0.2, cex = 1)
  if (x_labs == T){
    axis(1, at = seq(-4, 4, 2), labels = seq(-4, 4, 2), cex.axis = 1, line = -.18)
    mtext("First Half", side = 1, line = 2.5, cex = .8)
  } else{
    axis(1, at = seq(-4, 4, 2), labels = NA, cex.axis = 1.2, line = -.18)
  }
  if (y_labs == T){
    axis(2, 
         at = seq(-4, 4, 2), 
         labels = seq(-4, 4, 2), 
         las = 2, 
         cex.axis = 1,
         line = -.05)
    mtext("Second Half", side = 2, line = 2.5, cex = .8)
  } else{
    axis(2, 
         at = seq(-4, 4, 2), 
         labels = NA, 
         las = 2, 
         cex.axis = 1.2,
         line = -.05)
  }
  text_expression = bquote(italic(r) == .(cor.val[1, 2]))
  text(-2, 4, text_expression)
  
  points(tscores, col = col1, pch = 16, cex = .8)
  lines(c(-4.1,4.1), c(-4.1,4.1), col = col2, lwd = 1.5, lty = 5)
  # lines(c(-4,-4), c(-4.4,4.4))
  # lines(c(-4.4,4.4), c(-4,-4))
}

pdf("../_manuscript/_figs/split-halves.pdf",
# pdf("test.pdf",
    width = 6,
    height = 5)
y_labs = rep(c(T, F, F, F), 3)[1:12]
x_labs = c(rep(F, 8), rep(T, 4))
top = c(rep(1.5, 4), rep(0, 8))
layout_matrix = matrix(1:12, nrow = 3, byrow = TRUE)
layout(layout_matrix)
layout(layout_matrix, heights = c(.9, .8, .8), widths = c(rep(c(.95, rep(.8,3)),3)))
mapply(plotHalves, score_list, new_names, y_labs, x_labs, top)
dev.off()



