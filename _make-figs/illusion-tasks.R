dat = read.csv("../_data/clean_data.csv")

# tapply((dat$y), list(dat$J, dat$version), median)


illPlots = function(){
  centerX = rep(c(.25,.75), 5)
  centerY = rev(rep(seq(.12 ,.82,length=3), each = 2))
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,typ='n',axes=F)
  centerX = centerX[1:6]
  centerY = centerY[1:6]
  
  
  rect(centerX-.2,centerY-.14,centerX+.2,centerY+.14,col='antiquewhite')
  text(centerX[1]-.19,centerY[1]+.17,"(A) Brentano v1", cex = 2, adj = 1, pos=4, offset=0)
  text(centerX[2]-.19,centerY[2]+.17,"(B) Brentano v2", cex = 2, adj = 1, pos=4, offset=0)
  text(centerX[3]-.19,centerY[3]+.17,"(C) Ebbinghaus", cex = 2, adj = 1, pos=4, offset=0)
  text(centerX[4]-.19,centerY[4]+.17,"(D) Poggendorf", cex = 2, adj = 1, pos=4, offset=0)
  text(centerX[5]-.19,centerY[5]+.17,"(E) Ponzo", cex = 2, adj = 1, pos=4, offset=0)
  text(centerX[6]-.19,centerY[6]+.17,"(F) Zoellner", cex = 2, adj = 1, pos=4, offset=0)
  
  set_lwd = 2
  segments(centerX[1]+.01,centerY[1],centerX[1]+.09,centerY[1], lwd = set_lwd, col = "red")
  segments(centerX[1]-.07,centerY[1],centerX[1]+.01,centerY[1], lwd = set_lwd, col = "blue")
  segments(centerX[1]-.1,centerY[1]+.02,centerX[1]-.07,centerY[1], lwd = set_lwd)
  segments(centerX[1]-.1,centerY[1]-.02,centerX[1]-.07,centerY[1], lwd = set_lwd)
  segments(centerX[1]+.06,centerY[1]+.02,centerX[1]+.09,centerY[1], lwd = set_lwd)
  segments(centerX[1]+.06,centerY[1]-.02,centerX[1]+.09,centerY[1], lwd = set_lwd)
  segments(centerX[1]+.04,centerY[1]+.02,centerX[1]+.01,centerY[1], col = "black", lwd = set_lwd)
  segments(centerX[1]+.04,centerY[1]-.02,centerX[1]+.01,centerY[1], col = "black", lwd = set_lwd)
  # segments(centerX[1]+.01, centerY[1]+.04, centerX[1]+.01, centerY[1]-.04, lty = 2, lwd = set_lwd/2, col = "black")


  # set_lwd = 4
  segments(centerX[2]-.01,centerY[2],centerX[2]+.07,centerY[2], lwd = set_lwd, col = "red")
  segments(centerX[2]-.09,centerY[2],centerX[2]-.01,centerY[2], lwd = set_lwd, col = "blue")
  segments(centerX[2]-.06,centerY[2]+.02,centerX[2]-.09,centerY[2], lwd = set_lwd)
  segments(centerX[2]-.06,centerY[2]-.02,centerX[2]-.09,centerY[2], lwd = set_lwd)
  segments(centerX[2]+.10,centerY[2]+.02,centerX[2]+.07,centerY[2], lwd = set_lwd)
  segments(centerX[2]+.10,centerY[2]-.02,centerX[2]+.07,centerY[2], lwd = set_lwd)
  segments(centerX[2]-.04,centerY[2]+.02,centerX[2]-.01,centerY[2], col = "black", lwd = set_lwd)
  segments(centerX[2]-.04,centerY[2]-.02,centerX[2]-.01,centerY[2], col = "black", lwd = set_lwd)
  # segments(centerX[2]+.01, centerY[2]+.04, centerX[2]+.01, centerY[2]-.04, lty = 2, lwd = set_lwd/2, col = "black")
  
  
  
  radiusLarge = .025
  radiusSmall = .01
  theta = seq(0, 2*pi, length.out = 100)

  xLarge = centerX[3]+.1 + (radiusLarge+.002) * cos(theta)
  yLarge = centerY[3]-.015 + (radiusLarge+.002) * sin(theta)
  polygon(xLarge, yLarge , border = "black", lwd = 2)

  orbitRadius = radiusLarge + 0.015
  numSmallCircles = 12
  for (i in 1:numSmallCircles) {
    angle = (2 * pi / numSmallCircles) * i
    xSmall = centerX[3]+.1 + orbitRadius * cos(angle)
    ySmall = centerY[3]-.015 + orbitRadius * sin(angle)

    x = xSmall + radiusSmall * cos(theta)
    y = ySmall + radiusSmall * sin(theta)
    polygon(x, y, border = "black", lwd = 2)
  }


  radiusSmall = .025
  radiusLarge = .035
  theta = seq(0, 2*pi, length.out = 100)

  xSmall = centerX[3]-.08 + (radiusSmall+.002) * cos(theta)
  ySmall = centerY[3]-.015 + (radiusSmall+.002) * sin(theta)
  polygon(xSmall, ySmall , border = "black", lwd = 2)

  orbitRadius = radiusSmall + radiusLarge + 0.002

  numLargeCircles = 5

  for (i in 1:numLargeCircles) {
    angle = (2 * pi / numLargeCircles) * i
    xLarge = centerX[3]-.08 + orbitRadius * cos(angle)
    yLarge = centerY[3]-.015 + orbitRadius * sin(angle)

    x = xLarge + radiusLarge * cos(theta)
    y = yLarge + radiusLarge * sin(theta)
    polygon(x, y, border = "black", lwd = 2)
  }

  segments(centerX[4]-.04,centerY[4]-.08,centerX[4]-.04,centerY[4]+.05, lwd = set_lwd)
  segments(centerX[4]+.04,centerY[4]-.08,centerX[4]+.04,centerY[4]+.05, lwd = set_lwd)
  segments(centerX[4]-.08,centerY[4]-.066,centerX[4]-.04,centerY[4]-.036, lwd = set_lwd)
  # segments(centerX[4]-.04,centerY[4]-.036,centerX[4]+.04,centerY[4]+.02, lwd = set_lwd)
  segments(centerX[4]+.04,centerY[4]+.02,centerX[4]+.08,centerY[4]+.05, lwd = set_lwd)

  
  img = png::readPNG("../_media/train.png")
  rasterImage(img, centerX[5] - .2, centerY[5] - .14, centerX[5] + .2, centerY[5] + .14)
  segments(centerX[5]-.13,centerY[5]-.08,centerX[5]-.13,centerY[5]-.02, col = "red", lwd = set_lwd)
  segments(centerX[5]-.019,centerY[5]+.005,centerX[5]-.019,centerY[5]+.055, lwd = set_lwd,  col = "red")
  # text(centerX[5]-.19,centerY[5]+.1,"(F) Ponzo", cex = 2, adj = 1, pos=4, offset=0, col = "coral")
  

  ys = c(.08, .05, .02, -.01) - .025
  y2s = c(1, -1, 1, -1) 
  xs = seq(-.09,.09, length.out = 10)
  jits = seq(-.002, .002, length.out = 10)
  for (i in 1:5){
    # if (i %% 2){
    #   segments(centerX[6]-.105,centerY[6]-ys[i]+.0025,centerX[6]+.1,centerY[6]-ys[i]-.0005,
    #            col = "black", lwd = set_lwd)
    # } else{
    #   segments(centerX[6]-.105,centerY[6]-ys[i]+.0025,centerX[6]+.1,centerY[6]-ys[i]+.0055,
    #            col = "black", lwd = set_lwd)
    # }
    segments(centerX[6]-.105,centerY[6]-ys[i],centerX[6]+.1,centerY[6]-ys[i],
             col = "black", lwd = set_lwd)
    for (i2 in 1:10){
      jits = sample(jits, size = 10)
      if (i %% 2){
        segments(centerX[6]-xs[i2]-.015,centerY[6]-ys[i]+.01+jits[i2],
                 centerX[6]-xs[i2]+.015,centerY[6]-ys[i]-.01+jits[i2], lwd = set_lwd)
      } else {
        segments(centerX[6]-xs[i2]-.015,centerY[6]-ys[i]-.01+jits[i2],
                 centerX[6]-xs[i2]+.015,centerY[6]-ys[i]+.01+jits[i2], lwd = set_lwd)
      }
    }
  }
}

pdf("../_manuscript/_figs/ill-tasks.pdf",
    width = 8,
    height = 8)
illPlots()
dev.off()



# 
# illPlots1 = function(){
#   centerX = rep(c(.25,.75), 5)
#   centerY = rev(rep(seq(.12 ,.88,length=3), each = 2))
#   par(mar=c(0,0,0,0))
#   plot(0:1,0:1,typ='n',axes=F)
#   centerX = centerX[1:6]
#   centerY = centerY[1:6]
#   
  # rect(centerX-.2,centerY-.14,centerX+.2,centerY+.14,col='antiquewhite')
  # text(centerX[1]-.19,centerY[1]+.1,"(A) Brentano V1", cex = 2, adj = 1, pos=4, offset=0)
  # text(centerX[2]-.19,centerY[2]+.1,"(B) Brentano V2", cex = 2, adj = 1, pos=4, offset=0)
  # text(centerX[3]-.19,centerY[3]+.1,"(C) Ebbinghaus V1", cex = 2, adj = 1, pos=4, offset=0)
  # text(centerX[4]-.19,centerY[4]+.1,"(D) Ebbinghaus V2", cex = 2, adj = 1, pos=4, offset=0)
  # text(centerX[5]-.19,centerY[5]+.1,"(E) Poggendorf V1", cex = 2, adj = 1, pos=4, offset=0)
  # text(centerX[6]-.19,centerY[6]+.1,"(F) Poggendorf V2", cex = 2, adj = 1, pos=4, offset=0)
#   
#   
#   
#   set_lwd = 4
#   segments(centerX[1]-.001,centerY[1],centerX[1]+.09,centerY[1], lwd = set_lwd, col = "red")
#   segments(centerX[1]-.07,centerY[1],centerX[1]-.001,centerY[1], lwd = set_lwd, col = "blue")
#   segments(centerX[1]-.1,centerY[1]+.02,centerX[1]-.07,centerY[1], lwd = set_lwd)
#   segments(centerX[1]-.1,centerY[1]-.02,centerX[1]-.07,centerY[1], lwd = set_lwd)
#   segments(centerX[1]+.06,centerY[1]+.02,centerX[1]+.09,centerY[1], lwd = set_lwd)
#   segments(centerX[1]+.06,centerY[1]-.02,centerX[1]+.09,centerY[1], lwd = set_lwd)
#   segments(centerX[1]+.031,centerY[1]+.02,centerX[1]-.001,centerY[1], col = "black", lwd = set_lwd)
#   segments(centerX[1]+.031,centerY[1]-.02,centerX[1]-.001,centerY[1], col = "black", lwd = set_lwd)
#   segments(centerX[1]+.01, centerY[1]+.04, centerX[1]+.01, centerY[1]-.04, lty = 2, lwd = set_lwd/2, col = "black")
#   
#   
#   segments(centerX[2]-.09,centerY[2],centerX[2]+.0035,centerY[2], lwd = set_lwd, col = "red")
#   segments(centerX[2]+.0035,centerY[2],centerX[2]+.07,centerY[2], lwd = set_lwd, col = "blue")
#   segments(centerX[2]-.07,centerY[2]+.02,centerX[2]-.1,centerY[2], lwd = set_lwd)
#   segments(centerX[2]-.07,centerY[2]-.02,centerX[2]-.1,centerY[2], lwd = set_lwd)
#   segments(centerX[2]+.1,centerY[2]+.02,centerX[2]+.07,centerY[2], lwd = set_lwd)
#   segments(centerX[2]+.1,centerY[2]-.02,centerX[2]+.07,centerY[2], lwd = set_lwd)
#   segments(centerX[2]-.0335,centerY[2]+.02,centerX[2]+.0035,centerY[2], col = "black", lwd = set_lwd)
#   segments(centerX[2]-.0335,centerY[2]-.02,centerX[2]+.0035,centerY[2], col = "black", lwd = set_lwd)
#   segments(centerX[2]-.01, centerY[2]+.04, centerX[2]-.01, centerY[2]-.04, lty = 2, lwd = set_lwd/2, col = "black")
#   
#   
#   
#   
#   
#   
#   radiusLarge = .025
#   radiusSmall = .01 
#   theta = seq(0, 2*pi, length.out = 100)
#   
#   xLarge = centerX[3]+.1 + (radiusLarge+.002) * cos(theta)
#   yLarge = centerY[3]-.015 + (radiusLarge+.002) * sin(theta)
#   polygon(xLarge, yLarge , border = "black", lwd = 2)
#   
#   orbitRadius = radiusLarge + 0.015  
#   numSmallCircles = 12
#   for (i in 1:numSmallCircles) {
#     angle = (2 * pi / numSmallCircles) * i
#     xSmall = centerX[3]+.1 + orbitRadius * cos(angle)
#     ySmall = centerY[3]-.015 + orbitRadius * sin(angle)
#     
#     x = xSmall + radiusSmall * cos(theta)
#     y = ySmall + radiusSmall * sin(theta)
#     polygon(x, y, border = "black", lwd = 2)
#   }
#   
#   
#   radiusSmall = .025*(1-0.10077010)
#   radiusLarge = .035
#   theta = seq(0, 2*pi, length.out = 100)
#   
#   xSmall = centerX[3]-.08 + (radiusSmall+.002) * cos(theta)
#   ySmall = centerY[3]-.015 + (radiusSmall+.002) * sin(theta)
#   polygon(xSmall, ySmall , border = "black", lwd = 2)
#   
#   orbitRadius = radiusSmall + radiusLarge + 0.002
#   
#   numLargeCircles = 5
#   
#   for (i in 1:numLargeCircles) {
#     angle = (2 * pi / numLargeCircles) * i
#     xLarge = centerX[3]-.08 + orbitRadius * cos(angle)
#     yLarge = centerY[3]-.015 + orbitRadius * sin(angle)
#     
#     x = xLarge + radiusLarge * cos(theta)
#     y = yLarge + radiusLarge * sin(theta)
#     polygon(x, y, border = "black", lwd = 2)
#   }
#   
#   
#   
#   radiusLarge = .025*(1+0.17999854)
#   radiusSmall = .01 
#   theta = seq(0, 2*pi, length.out = 100)
#   
#   xLarge = centerX[4]+.1 + (radiusLarge+.002) * cos(theta)
#   yLarge = centerY[4]-.015 + (radiusLarge+.002) * sin(theta)
#   polygon(xLarge, yLarge , border = "black", lwd = 2)
#   
#   orbitRadius = radiusLarge + 0.015  
#   numSmallCircles = 12
#   for (i in 1:numSmallCircles) {
#     angle = (2 * pi / numSmallCircles) * i
#     xSmall = centerX[4]+.1 + orbitRadius * cos(angle)
#     ySmall = centerY[4]-.015 + orbitRadius * sin(angle)
#     
#     x = xSmall + radiusSmall * cos(theta)
#     y = ySmall + radiusSmall * sin(theta)
#     polygon(x, y, border = "black", lwd = 2)
#   }
#   
#   
#   radiusSmall = .025
#   radiusLarge = .035
#   theta = seq(0, 2*pi, length.out = 100)
#   
#   xSmall = centerX[4]-.08 + (radiusSmall+.002) * cos(theta)
#   ySmall = centerY[4]-.015 + (radiusSmall+.002) * sin(theta)
#   polygon(xSmall, ySmall , border = "black", lwd = 2)
#   
#   orbitRadius = radiusSmall + radiusLarge + 0.002
#   
#   numLargeCircles = 5
#   
#   for (i in 1:numLargeCircles) {
#     angle = (2 * pi / numLargeCircles) * i
#     xLarge = centerX[4]-.08 + orbitRadius * cos(angle)
#     yLarge = centerY[4]-.015 + orbitRadius * sin(angle)
#     
#     x = xLarge + radiusLarge * cos(theta)
#     y = yLarge + radiusLarge * sin(theta)
#     polygon(x, y, border = "black", lwd = 2)
#   }
#   
#   
#   segments(centerX[5]-.04,centerY[5]-.08,centerX[5]-.04,centerY[5]+.05, lwd = set_lwd)
#   segments(centerX[5]+.04,centerY[5]-.08,centerX[5]+.04,centerY[5]+.05, lwd = set_lwd)
#   segments(centerX[5]-.1,centerY[5]-.05,centerX[5]-.04,centerY[5]-.016, lwd = set_lwd)
#   segments(centerX[5]+.1,centerY[5]-.016 + 0.04533334 - .015 + 0.04533334*0.75,
#            centerX[5]+.04,centerY[5] - .016 + 0.04533334 - .015, col = "black", lwd = set_lwd)
#   
#   
#   
#   segments(centerX[6]-.04,centerY[6]-.08,centerX[6]-.04,centerY[6]+.05, lwd = set_lwd)
#   segments(centerX[6]+.04,centerY[6]-.08,centerX[6]+.04,centerY[6]+.05, lwd = set_lwd)
#   segments(centerX[6] + .1, centerY[6] - .05, centerX[6] + .04, centerY[6] - .016, lwd = set_lwd)
#   segments(centerX[6] - .1, centerY[6] - .016 + 0.04533334 - .015 + 0.04533334 * 0.75,
#            centerX[6] - .04, centerY[6] - .016 + 0.04533334 - .015, col = "black", lwd = set_lwd)
# }
# 
# 
# 
# illPlots2 = function(){
#   set_lwd = 4
#   
#   centerX = rep(c(.25,.75), 5)
#   centerY = rev(rep(seq(.12 ,.88,length=3), each = 2))
#   par(mar=c(0,0,0,0))
#   plot(0:1,0:1,typ='n',axes=F)
#   centerX = centerX[1:4]
#   centerY = centerY[1:4]
#   
#   rect(centerX-.2,centerY-.14,centerX+.2,centerY+.14,col='antiquewhite')
#   
#   img = png::readPNG("../_media/train.png")
#   
#   rasterImage(img, centerX[1] - .2, centerY[1] - .14, centerX[1] + .2, centerY[1] + .14)
#   segments(centerX[1]-.13,centerY[1]-.08,centerX[1]-.13,centerY[1]-.01, col = "red", lwd = set_lwd)
#   segments(centerX[1]-.019,centerY[1]+.005,centerX[1]-.019,centerY[1]+.025, lwd = set_lwd,  col = "red")
#   
#   
#   rasterImage(img, centerX[2] - .2, centerY[2] - .14, centerX[2] + .2, centerY[2] + .14)
#   segments(centerX[2]-.13,centerY[2]-.08,centerX[2]-.13,centerY[2]-.033, col = "red", lwd = set_lwd)
#   segments(centerX[2]-.019,centerY[2]+.005,centerX[2]-.019,centerY[2]+.05, lwd = set_lwd,  col = "red")
#   
#   
#   ys = c(.08, .05, .02, -.01) - .025
#   y2s = c(1, -1, 1, -1) 
#   xs = seq(-.09,.09, length.out = 10)
#   jits = seq(-.002, .002, length.out = 10)
#   for (i in 1:5){
#     if (i %% 2){
#       segments(centerX[4]-.105,centerY[4]-ys[i]+.0025,centerX[4]+.1,centerY[4]-ys[i]-.0005,
#                col = "black", lwd = set_lwd)
#     } else{
#       segments(centerX[4]-.105,centerY[4]-ys[i]+.0025,centerX[4]+.1,centerY[4]-ys[i]+.0055,
#                col = "black", lwd = set_lwd)
#     }
#     for (i2 in 1:10){
#       jits = sample(jits, size = 10)
#       if (i %% 2){
#         segments(centerX[4]-xs[i2]-.01,centerY[4]-ys[i]+.01+jits[i2],
#                  centerX[4]-xs[i2],centerY[4]-ys[i]-.01+jits[i2], lwd = set_lwd)
#       } else {
#         segments(centerX[4]-xs[i2]-.01,centerY[4]-ys[i]-.01+jits[i2],
#                  centerX[4]-xs[i2],centerY[4]-ys[i]+.01+jits[i2], lwd = set_lwd)
#       }
#     }
#   }
#   
#   
#   for (i in 1:5){
#     if (i %% 2){
#       segments(centerX[3]-.105,centerY[3]-ys[i]-.0005,centerX[3]+.1,centerY[3]-ys[i]+.0025,
#                col = "black", lwd = set_lwd)
#     } else{
#       segments(centerX[3]-.105,centerY[3]-ys[i]+.0055,centerX[3]+.1,centerY[3]-ys[i]+.0025,
#                col = "black", lwd = set_lwd)
#     }
#     for (i2 in 1:10){
#       if (i %% 2){
#         jits = sample(jits, size = 10)
#         segments(centerX[3]-xs[i2]-.01,centerY[3]-ys[i]+.01+jits[i2],
#                  centerX[3]-xs[i2],centerY[3]-ys[i]-.01+jits[i2], lwd = set_lwd)
#       } else {
#         segments(centerX[3]-xs[i2]-.01,centerY[3]-ys[i]-.01+jits[i2],
#                  centerX[3]-xs[i2],centerY[3]-ys[i]+.01+jits[i2], lwd = set_lwd)
#       }
#     }
#   }
#   
#   
#   text(centerX[1]-.19,centerY[1]+.1,"(G) Ponzo V1", cex = 2, adj = 1, pos=4, offset=0, col = "coral")
#   text(centerX[2]-.19,centerY[2]+.1,"(H) Ponzo V2", cex = 2, adj = 1, pos=4, offset=0, col = "coral")
#   text(centerX[3]-.19,centerY[3]+.1,"(I) Zoellner V1", cex = 2, adj = 1, pos=4, offset=0)
#   text(centerX[4]-.19,centerY[4]+.1,"(J) Zoellner V2", cex = 2, adj = 1, pos=4, offset=0)
# }
# pdf("../_manuscript/_figs/ill-tasks-1.pdf",
#     width = 8,
#     height = 8)
# illPlots1()
# dev.off()
# 
# pdf("../_manuscript/_figs/ill-tasks-2.pdf",
#     width = 8,
#     height = 8)
# illPlots2()
# dev.off()
