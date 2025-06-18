
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


makePositive=function(samples,I=0){
  lambda = samples
  dimMean=apply(lambda,3,mean)
  D=length(dimMean)
  for (d in 1:D) {
    lambda[,,d]=sign(dimMean[d])*samples[,,d]
    return(lambda)}
}



align=function(lambda,I=0){
  M=dim(lambda)[1]
  numFactors=dim(lambda)[3]
  eta=array(0, dim = c(M, I, numFactors))
  lambdaList=lapply(1:M,function(x) lambda[x,,])
  etaList=lapply(1:M,function(x) eta[x,,])
  aligned=jointRot(lambda = lambdaList, eta = etaList)
  out=aperm(abind(aligned$lambda, along=3),c(3,1,2))
  return(out)
}


f2Eigenmax=function(lambda){
  
  euler2=function(theta) 
    matrix(ncol=2,byrow=T,c(
      cos(theta),-sin(theta),
      sin(theta),cos(theta)))
  
  obj2=function(theta,b){
    r=b%*%euler2(theta)
    apply(r^2,2,sum)[1]}
  
  M=dim(lambda)[1]
  pLam=apply(lambda,2:3,mean)
  out=lambda
  ang=optimize(obj2,c(-pi,pi),maximum=T,b=pLam)$maximum
  for (m in 1:M) {
    out[m,,]=lambda[m,,]%*%euler2(ang)
  }
  return(out)}



makeTable = function(lambda, del2) {
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
  uni_prop_task = round(colMeans(uni_prop_task_mat), 3)
  fac_prop_total = round(colMeans(fac_prop_total_mat), 3)
  uni_prop_total = round(mean(uni_prop_total_vec), 3)
  
  out = cbind(lam_mean, uni_prop_task)
  out = rbind(out, c(fac_prop_total, uni_prop_total))
  
  longNames = c("Brentano 1","Brentano 2","Ebbinghaus 1","Ebbinghaus 2",
                "Poggendorf 1","Poggendorf 2","Ponzo 1","Ponzo 2",
                "Zöllner 1","Zöllner 2")
  rownames(out) = c(longNames, "Prop. Var.")
  colnames(out) = c(paste0("F", 1:D), "Unique Var.")
  
  return(out)
}

standardize=function(samples){
  M=dim(samples$lambda)[1]
  J=dim(samples$lambda)[2]
  sigma=array(dim=c(M,J))
  lambda=samples$lambda
  delta2=samples$pDelta2
  Sigma=rho=array(dim=c(M,J,J))
  for (m in 1:M){
    Sigma[m,,]=crossprod(t(samples$lambda[m,,]))+diag(samples$del2[m,])
    rho[m,,]=cov2cor(Sigma[m,,])
    sigma[m,]=sqrt(diag(Sigma[m,,]))
    lambda[m,,]=samples$lambda[m,,]/sigma[m,]
    delta2[m,]=(1/samples$pDelta2[m,])/sigma[m,]^2}
  samples$lambda=lambda
  samples$delta2=delta2
  samples$Sigma=Sigma
  samples$rho=rho
  return(samples)}




