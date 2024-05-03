source("../_logistics/funs.R")
dat = read.csv("../_data/clean_data.csv")
dat$S = as.numeric(factor(dat$sub))
dat$J = as.numeric(factor(dat$task))
dat$JV = (dat$J-1)*2+dat$version

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


suffixes = c(" v1", " v2")
tasks = c("Brentano", "Ebbinghaus", "Poggendorf", "Ponzo", "Zoellner")
tasks_vs = sapply(tasks, function(x) paste0(x, suffixes))
new_names = as.list(as.vector(tasks_vs))


vars = tapply(dat$y, list(dat$S, dat$JV), var)
div = sqrt(apply(vars, 2, mean))
dat$y_coerced = dat$y/div[dat$JV]

scores = tapply(dat$y_coerced, list(dat$S, dat$JV), mean)
scores_se = tapply(dat$y_coerced, 
                   list(dat$S, dat$JV), 
                   function(x) sd(x)/sqrt(length(x)))

scores_list = lapply(1:10, function(i) scores[,i])
crit = qt(.975,14)
scores_CI_list = lapply(1:10, function(i) scores_se[,i]*crit)

vars_stroop = tapply(stroop$y, list(stroop$S, stroop$cond), var)
div = sqrt(mean(vars_stroop))
stroop$y_coerced = stroop$y/div

scores_stroop = tapply(stroop$y_coerced, list(stroop$S, stroop$cond), mean)
scores_stroop = scores_stroop[,2] - scores_stroop[,1]
scores_se_stroop = tapply(stroop$y_coerced, 
                          list(stroop$S), 
                          function(x) sd(x)/sqrt(length(x)))

scores_list[[11]] = scores_stroop
scores_CI_list[[11]] = scores_se_stroop*1.96

vars_flank = tapply(flank$y, list(flank$S, flank$cond), var)
div = sqrt(mean(vars_flank))
flank$y_coerced = flank$y/div

scores_flank = tapply(flank$y_coerced, list(flank$S, flank$cond), mean)
scores_flank = scores_flank[,2] - scores_flank[,1]
scores_se_flank = tapply(flank$y_coerced, 
                          list(flank$S), 
                          function(x) sd(x)/sqrt(length(x)))

scores_list[[12]] = scores_flank
scores_CI_list[[12]] = scores_se_flank*1.96

new_names[[11]] = "Stroop"
new_names[[12]] = "Flanker"

g_list = NULL
exp_rel_list = NULL
k= 1
for (i in 1:5){
  for (j in 1:2){
    ind = which(dat$J == i & dat$version == j)
    g_list[[k]] = sampEstG(dat[ind,])^2
    exp_rel_list[[k]] = g_list[[k]]/(g_list[[k]]+1/15)
    k = k + 1
  }
}
g_list[[11]] = sampEstG(stroop, contrast = T)$g_bar_theta^2
exp_rel_list[[11]] = g_list[[11]]/(g_list[[11]]+2/mean(tapply(stroop$trial, list(stroop$S), max)))
g_list[[12]] = sampEstG(flank, contrast = T)$g_bar_theta^2
exp_rel_list[[12]] = g_list[[12]]/(g_list[[12]]+2/mean(tapply(flank$trial, list(flank$S), max)))


cols = readRDS("plot-cols.RDS")
col1 = cols$Thistle[2]
col2 = cols$Thistle[5]
plotTasks = function(scores, CI,  task_name, y_labs, x_labs, top = 0, scale = F, g_est = NA, exp_rel_est = NA, g_pos = NA){
  
  left = c(2, 4)
  bottom = c(4, 4)
  par(mar = c(bottom[x_labs+1], left[y_labs+1], top, 1))
  
  
  o = order(scores)
  tscores = scores[o]
  
  tCI_95 = CI[o]
  I = length(o)
  
  ylims = c(floor(min(tscores) - tCI_95[1]), 
            ceiling(max(tscores) + tCI_95[length(o)]))
  xlims = c(0,I+1)
  # ylims = c(min(scores), max(scores))
  # ylims[1] = min(ylims[1], 0)
  if (g_est<.025){ylims = c(-1,2)}
  ylims = c(-1, 8)
  if (scale==T){ylims = c(-4.2, 4.2)}
  plot(NA,
       NA,
       xlim = xlims,
       ylim = ylims,
       axes = F,
       xlab = "",
       ylab = "",
       main = "",
       cex.main = 1.5
  )
  polygon(c(1:I, rev(1:I)), c(tscores - tCI_95, rev(tscores + tCI_95)), col = col1, 
          border = col2)
  # points(1:I, tscores, pch = 16, col = col2, cex = .3, lty = 6)
  
  
  if (scale==T){
    ylim1 = -4
    ylim2 = 4
    mtext(task_name, side = 3, line = 0.2, cex = 1)
    if (x_labs == T){
      axis(1, at = c(1,I), label = c("Low","High"))
      mtext("Individuals", side = 1, line = 2.5, cex = .8)
    } else{
      axis(1, at = c(1,I), label = NA)
    }
    
    
    if (y_labs == T){
      axis(2,
           at = seq(ylim1, ylim2, 2),
           las = 2, 
           cex.axis = 1,
           line = -.05)
      mtext("Scaled Scores", side = 2, line = 2.5, cex = .8)
    } else{
      axis(2, 
           las = 2,
           at = seq(ylim1, ylim2, 2),
           label = NA,
           cex.axis = 1,
           line = -.05)
    }
  } else{
    mtext(task_name, side = 3, line = 0.2, cex = 1)
    if (x_labs == T){
      axis(1, at = c(1,I), label = c("Low","High"))
      mtext("Individuals", side = 1, line = 2.5, cex = .8)
    } else{
      axis(1, at = c(1,I), label = c("Low","High"))
    }
    if (ylims[2] > .1){
      if (ylims[2] > 1){
        ylim2 = round(ylims[2],1)
      } else{
        ylim2 = round(ylims[2], 2)
      }
    } else{
      ylim2 = round(ylims[2], 3)
    }
    
    if (ylims[2] > .1){
      if (ylims[2] > 1){
        ylim1 = round(ylims[1], 1)
      } else{
        ylim1 = round(ylims[1], 2)
      }
    } else{
      ylim1 = round(ylims[1], 3)
    }
    
    if (y_labs == T){
      axis(2,
           at = c(ylim1, ylim2),
           las = 2, 
           cex.axis = 1,
           line = -.05)
      mtext("Scaled Scores", side = 2, line = 2.5, cex = .8)
    } else{
      axis(2, 
           las = 2,
           at = c(ylim1, ylim2),
           cex.axis = 1,
           line = -.05)
    }
  }
  text_expression = bquote(gamma^2 == .(round(g_est,3)))
  text(g_pos[1], g_pos[2], text_expression, adj = 0)
  text_expression = bquote(italic(r) == .(round(exp_rel_est,3)))
  text(g_pos[3], g_pos[4], text_expression, adj = 0)
  
  
  
  # lines(c(-4,-4), c(-4.4,4.4))
  # lines(c(-4.4,4.4), c(-4,-4))
}
# 
pdf("../_manuscript/_figs/task_plots.pdf",
# # pdf("test.pdf",
    width = 6,
    height = 5)
y_labs = rep(c(T, F, F, F), 3)[1:12]
x_labs = c(rep(F, 8), rep(T, 4))
top = c(rep(1.5, 4), rep(0, 8))
# g_pos = list(
#   "1" = c(35,5),
#   "2" = c(35,8-1-2/6),
#   "3" = c(35,3.5),
#   "4" = c(35,6-1-1/6),
#   "5" = c(35,8-1-2/6),
#   "6" = c(35,6-1-2/6),
#   "7" = c(35,9-1-3/6),
#   "8" = c(100,-1),
#   "9" = c(35,9-1-4/6),
#   "10" = c(35,8-1-3/6),
#   "11" = c(70,1.5),
#   "12" = c(70,1.5)
# )

g_pos = lapply(1:10, function(x) c(5,7,5,5.75))
g_pos[[11]] = g_pos[[12]] = c(20,7,20,5.75)

layout_matrix = matrix(1:12, nrow = 3, byrow = TRUE)
layout(layout_matrix)
layout(layout_matrix, heights = c(.9, .8, .8), widths = c(rep(c(.95, rep(.8,3)),3)))
mapply(plotTasks, scores_list, scores_CI_list, new_names, y_labs, x_labs, top, F, g_list, exp_rel_list, g_pos)
# mapply(plotHalves, score_list, CI_95_list_co, new_names, y_labs, x_labs, top, F)
dev.off()

# plotTasks(scores_list[[3]], scores_se_list[[3]], new_names[[3]], T, T, top[1], F, g_list[[1]])
