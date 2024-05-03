
library(R2jags)

runJags = function(mod, dat, pars, nchains = 2, niter = 3000, nburnin = 1000){
  result = jags(model.file = textConnection(mod),
                data = dat,
                n.chains = nchains,
                n.iter = niter,
                n.burnin = nburnin,
                n.thin = 1,
                parameters.to.save = pars)
  return(result)
}

rmCleanData=function(dat,
                     top=1.995,
                     hiTrlRT=1.995,
                     loTrlRT=.275,
                     loTop=.98,
                     loAcc=.90,
                     loRT=.99){
  excludeSub=c(132,164,368,402,429)  #origial author exclusions
  #exclusions
  bad0=dat$sub %in% excludeSub
  bad1=dat$block == 'practice'
  bad2=dat$trialType != 'exp'
  bad3=!(dat$acc %in% c(0,1)) #there are some strange things
  dat=dat[!(bad0 | bad1 | bad2 | bad3),]
  sub=tapply(dat$sub,dat$sub,mean)
  topBySub=tapply(dat$rt<top,dat$sub,mean)
  accBySub=(tapply(dat$acc,dat$sub,mean))
  loBySub=tapply(dat$rt>loTrlRT,dat$sub,mean)
  bad4=dat$sub %in% sub[
    accBySub<loAcc |
      topBySub<loTop |
      loBySub<loRT]
  bad5 = dat$rt<loTrlRT | dat$rt>min(hiTrlRT,top) | dat$acc==0
  dat=dat[!(bad4 | bad5),]
  return(dat)
}

readRMStroopI=function(){
  numStroop=read.table("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/ReyMermetJEPLMC2018/merged/numStroop.dat",head=T)
  a3=rmCleanData(numStroop)
  numStroop=a3[a3$acc==1,]
  numStroop$cond=as.integer(as.factor(numStroop$cond))
  dat=numStroop[numStroop$cond %in% 1:2,]
  dat$y = dat$rt
  return(dat)
}

readRMFlankI=function(){
  letFlanker=read.table("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/inhibitionTasks/ReyMermetJEPLMC2018/merged/letFlanker.dat",head=T)
  a3=rmCleanData(letFlanker)
  letFlanker=a3[a3$acc==1,]
  letFlanker$cond=as.integer(as.factor(letFlanker$cond))
  dat=letFlanker[letFlanker$cond %in% 1:2,]
  dat$y = dat$rt
  return(dat)
}


sampEstG = function(tdat, contrast = F){
  if (contrast == F){
    
    nt = nrow(tdat)/length(unique(tdat$sub))
    res = aov(y~as.factor(sub),data=tdat)$residuals
    sig = sd(res)
    mrt = tapply(tdat$y,list(tdat$sub),mean)
    delta = var(mrt)
    g2_bar = delta/sig^2 - 1/nt
    g_bar = sqrt(g2_bar)
    
    return(g_bar)
  }
  if (contrast == T){
    
    nt = nrow(tdat)/length(unique(tdat$sub))
    res = aov(y~as.factor(sub)+as.factor(cond),data=tdat)$residuals
    sig = sd(res)
    mrt = tapply(tdat$y,list(tdat$sub, tdat$cond),mean)
    delta.a = var((mrt[,2]+mrt[,1])/2)
    delta.t = var(mrt[,2]-mrt[,1])
    g2_bar_theta = delta.t/sig^2 - 2/(nt/2)
    g_bar_theta = sqrt(g2_bar_theta)
    g2_bar_alpha = delta.a/sig^2 - 1/nt
    g_bar_alpha = sqrt(g2_bar_alpha)
    return(list(g_bar_alpha = g_bar_alpha, g_bar_theta = g_bar_theta))
  }
}


popThetaCors = function(theta){
  M = dim(theta)[1]
  J = dim(theta)[3]
  theta_cors = array(NA, dim = c(nrow(theta), J, J))
  for (m in 1:M){
    theta_cors[m,,] = cor(theta[m,,])
  }
  mean_theta_cors = apply(theta_cors, 2:3, myMean)
  return(list("avg_cor" = mean_theta_cors, "post_cor" = theta_cors))
}

popSigCors = function(Sig, p = F){
  M = dim(Sig)[1]
  J = dim(Sig)[3]
  corVal = array(dim=c(M,J,J))
  for (m in 1:M){
    Cov = Sig[m,,]
    if (p == T){
      Cov = solve(Cov)
    }
    corVal[m,,]=cov2cor(Cov)
  }
  return(list("avg_cor" = apply(corVal,c(2,3),mean), "post_cor" = corVal))
}

myMean = function(x){
  mean(x, na.rm = T)
}

