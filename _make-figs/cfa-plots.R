
drawEllipse = function(x_center, y_center, radius_x = 0.2, radius_y = 0.15, l = 2){
  theta = seq(0, 2 * pi, length.out = 100)
  x = radius_x * cos(theta) + x_center
  y = radius_y * sin(theta) + y_center
  polygon(x, y, border = "black", col = "white", lwd = l)
}
curveFunction = function(start, end, distance, mid_y=0) {
  if (mid_y == 0){
    mid_y = (start[2] + end[2]) / 2
  }
  control_points = rbind(start, c(start[1] + distance, mid_y), end)
  
  bezier_curve = function(t, control_points) {
    (1 - t)^2 * control_points[1,] + 2 * (1 - t) * t * control_points[2,] + t^2 * control_points[3,]
  }
  
  t_values = seq(0, 1, length.out = 100)
  curve_points = t(sapply(t_values, function(t) bezier_curve(t, control_points)))
  
  lines(curve_points, col = "gray", lwd = 2)
}


current_mar = par("mar")
suffixes = c(" v1", " v2")
tasks = c("Br", "Eb", "Pog", "Pz", "Zol")
tasks_vs = sapply(tasks, function(x) paste0(x, suffixes))

plotCFA1 = function(){
  par(mar = c(0,1,1,0))
  plot(NA,
       NA,
       xlim = c(-.1,1),
       ylim = c(0,2.1),
       xlab = "",
       ylab = "",
       main = "",
       asp = 1,
       axes = F
  )
  mtext("(A)", side = 3, line = -2, adj = -.01, cex = 1.5)
  n.task = 10
  task_pos.y = seq(.1, 1.9, length.out = n.task)
  task_pos.x = rep(-0.1, n.task)
  task.length = c(.2, .08)
  n.factor_versions = 5
  version_factor.length = .3
  version_factor_pos.y = mapply(
    function(x) (task_pos.y[x+1] - task_pos.y[x])/2 + task_pos.y[x],
    seq(1,10,2)
  )
  version_factor_pos.x = rep(.5, n.factor_versions)
  rect(task_pos.x+task.length[1],
       task_pos.y+task.length[2],
       task_pos.x-task.length[1],
       task_pos.y-task.length[2],
       lwd = 2
  )
  text(task_pos.x, task_pos.y, rev(tasks_vs), cex = 2)
  
  a = mapply(
    function(x) drawEllipse(version_factor_pos.x[x], version_factor_pos.y[x]), 
    1:n.factor_versions
  )
  radius_x = 0.2
  text(version_factor_pos.x, version_factor_pos.y, rev(tasks), cex = 2)
  
  first_con = matrix(
    c(task_pos.x + task.length[1], 
      task_pos.y), 
    ncol = 2 
  )
  
  second_con = matrix(
    c(rep(version_factor_pos.x - radius_x, each = 2), 
      rep(version_factor_pos.y, each = 2)), 
    ncol = 2 
  )
  
  b = mapply(
    function(x) lines(c(first_con[x,1], second_con[x,1]), 
                      c(first_con[x,2], second_con[x,2]),
                      lwd = 2),
    1:10
  )
  dis = rnorm(n.factor_versions^2, 1, .015)
  jit = rnorm(n.factor_versions^2, 0, .025)
  count = 1.5
  for (i in 1:n.factor_versions){
    for (j in 1:n.factor_versions){
      if (i > j){
        curveFunction(
          c(version_factor_pos.x[i] + radius_x, version_factor_pos.y[i]),
          c(version_factor_pos.x[j] + radius_x, version_factor_pos.y[j]),
          1.2
        )
        count = count + 1
      }
      
    }
  }
}

plotCFA2 = function(){
  par(mar = c(0,1,1,0))
  plot(NA,
       NA,
       xlim = c(-0.1,1),
       ylim = c(0,2.1),
       xlab = "",
       ylab = "",
       main = "",
       asp = 1,
       axes = F
  )
  mtext("(B)", side = 3, line = -2, adj = -.01, cex = 1.5)
  n.task = 10
  task_pos.y = seq(.1, 1.9, length.out = n.task)
  task_pos.x = rep(-0.1, n.task)
  task.length = c(.2, .08)
  n.factor_versions = 5
  version_factor.length = .3
  version_factor_pos.y = mapply(
    function(x) (task_pos.y[x+1] - task_pos.y[x])/2 + task_pos.y[x],
    seq(1,10,2)
  )
  version_factor_pos.x = rep(.5, n.factor_versions)
  rect(task_pos.x+task.length[1],
       task_pos.y+task.length[2],
       task_pos.x-task.length[1],
       task_pos.y-task.length[2],
       lwd = 2
  )
  text(task_pos.x, task_pos.y, rev(tasks_vs), cex = 2)
  
  a = mapply(
    function(x) drawEllipse(version_factor_pos.x[x], version_factor_pos.y[x]), 
    1:n.factor_versions
  )
  radius_x = 0.2
  text(version_factor_pos.x, version_factor_pos.y, rev(tasks), cex = 2)
  
  first_con = matrix(
    c(task_pos.x + task.length[1], 
      task_pos.y), 
    ncol = 2 
  )
  
  second_con = matrix(
    c(rep(version_factor_pos.x - radius_x, each = 2), 
      rep(version_factor_pos.y, each = 2)), 
    ncol = 2 
  )
  
  b = mapply(
    function(x) lines(c(first_con[x,1], second_con[x,1]), 
                      c(first_con[x,2], second_con[x,2]),
                      lwd = 2),
    1:10
  )
  gf_pos.x = 1.1
  gf_pos.y = 1
  gf.length = c(.1,.1)
  
  third_con = matrix(
    c(version_factor_pos.x + radius_x, version_factor_pos.y), 
    ncol = 2 
  )
  
  fourth_con = matrix(
    rep(c(gf_pos.x - gf.length[1], gf_pos.y), 5), 
    ncol = 2 
  )
  
  c = mapply(
    function(x) lines(c(third_con[x,1], fourth_con[x,1]), 
                      c(third_con[x,2], fourth_con[x,2]),
                      lwd = 2),
    1:5
  )
  drawEllipse(gf_pos.x, gf_pos.y)
  
  text(gf_pos.x, gf_pos.y, "I", cex = 2)
  
}


plotCFA3 = function(){
  par(mar = c(0,1,1,0))
  plot(NA,
       NA,
       xlim = c(-.1,1),
       ylim = c(0,2.1),
       xlab = "",
       ylab = "",
       main = "",
       asp = 1,
       axes = F
  )
  n.task = 10
  task_pos.y = seq(.1, 1.9, length.out = n.task)
  task_pos.x = rep(-0.1, n.task)
  task.length = c(.2, .08)
  n.factor_versions = 5
  version_factor.length = .3
  version_factor_pos.y = mapply(
    function(x) (task_pos.y[x+1] - task_pos.y[x])/2 + task_pos.y[x],
    seq(1,10,2)
  )
  version_factor_pos.x = rep(.5, n.factor_versions)
  rect(task_pos.x+task.length[1],
       task_pos.y+task.length[2],
       task_pos.x-task.length[1],
       task_pos.y-task.length[2],
       lwd = 2
  )
  text(task_pos.x, task_pos.y, rev(tasks_vs), cex = 2)
  
  
  radius_x = 0.2
  
  first_con = matrix(
    c(task_pos.x + task.length[1], 
      task_pos.y), 
    ncol = 2 
  )
  
  second_con = matrix(
    c(rep(version_factor_pos.x - radius_x, each = 2), 
      rep(version_factor_pos.y, each = 2)), 
    ncol = 2 
  )
  mtext("(C)", side = 3, line = -2, adj = -.01, cex = 1.5)
  
  
  dis = rnorm(n.factor_versions^2, 1, .015)
  jit = rnorm(n.factor_versions^2, 0, .025)
  count = 1.5
  gf_pos.x = 1.1
  gf_pos.y = 1
  gf.length = c(.1,.1)
  for (i in 1:n.task){
    curveFunction(
      c(task_pos.x[i] + radius_x, task_pos.y[i]),
      c(gf_pos.x, gf_pos.y),
      1.8,
      mid_y = 1
    )
  }
  a = mapply(
    function(x) drawEllipse(version_factor_pos.x[x], version_factor_pos.y[x]), 
    1:n.factor_versions
  )
  text(version_factor_pos.x, version_factor_pos.y, rev(tasks), cex = 2)
  b = mapply(
    function(x) lines(c(first_con[x,1], second_con[x,1]), 
                      c(first_con[x,2], second_con[x,2]),
                      lwd = 2),
    1:10
  )
  
  
  
  third_con = matrix(
    c(version_factor_pos.x + radius_x, version_factor_pos.y), 
    ncol = 2 
  )
  
  fourth_con = matrix(
    rep(c(gf_pos.x - gf.length[1], gf_pos.y), 5), 
    ncol = 2 
  )
  
  c = mapply(
    function(x) lines(c(third_con[x,1], fourth_con[x,1]), 
                      c(third_con[x,2], fourth_con[x,2]),
                      lwd = 2),
    1:5
  )
  drawEllipse(gf_pos.x, gf_pos.y)
  
  text(gf_pos.x, gf_pos.y, "I", cex = 2)
  
}




pdf("../_manuscript/_figs/factor_cfa.pdf",
    width = 8,
    height = 10)
layout(
  matrix(c(1,1,2,2,0,3,3,0), nrow = 2, byrow = TRUE), 
  widths = c(.5, .5, .5, .5)
)
plotCFA1()
plotCFA2()
plotCFA3()

dev.off()
