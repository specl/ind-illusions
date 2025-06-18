
modIWJags = "
model{
  # Priors
  for (i in 1:I){
    theta[i, 1:J] ~ dmnorm(mu, pSig2)
  }
  
  for (j in 1:J){
    mu[j] ~ dnorm(0, pow(2, -2))
    pTau2[j] ~ dgamma(.5,.5)
  }
  
  pSig2 ~ dwish(diagJ*tuneT^2, J+1)
  
  
  # Likelihood
  for (n in 1:N){
    center[n] = theta[sub[n], task[n]]
    y[n] ~ dnorm(center[n], pTau2[task[n]])
  }
}
"


modUW = "
model{
  # Priors
  for (i in 1:I){
    theta[i] ~ dmnorm(mu, pSig2)
  }
  
  mu ~ dnorm(0, pow(2, -2))
  pTau2 ~ dgamma(.5,.5)
  pSig2 ~ dgamma(.5,.5)
  
  # Likelihood
  for (n in 1:N){
    y[n] ~ dnorm(theta[sub[n]], pTau2)
  }
}
"

modUW_Contrast = "
model{
  # Priors
  for (i in 1:I){
    theta[i] ~ dmnorm(mu, pSig2)
    alpha[i] ~ dmnorm(nu, pDel2)
  }
  
  mu ~ dnorm(0, pow(2, -2))
  nu ~ dnorm(0, pow(2, -2))
  pTau2 ~ dgamma(.5,.5)
  pSig2 ~ dgamma(.5,.5)
  pDel2 ~ dgamma(.5,.5)
  
  # Likelihood
  for (n in 1:N){
    center[n] = alpha[sub[n]]+(cond[n]-1.5)*theta[sub[n]]
    y[n] ~ dnorm(center[n], pTau2)
  }
}
"

mod_bhfm = "
model {
  # Priors
  for (j in 1:J) {
    pTau2[j] ~ dgamma(0.5, 0.5)
    mu[j] ~ dnorm(mu.m, pow(mu.s, -2))
    pDel2[j] ~ dgamma(0.5, 0.5 * pow(tuneDelta, 2))
    del2[j] = 1 / pDel2[j]
  }
  

  for (j in 1:J) {
    for (d in 1:D) {
      lambda_pos[j,d] ~ dnorm(0, pow(tuneLambda, -2)) T(0,)
      lambda_free[j,d] ~ dnorm(0, pow(tuneLambda, -2))
      
      aux_pos[j,d] = constraint_zero[j,d] - constraint_pos[j,d]
  
      lambda[j,d] = constraint_pos[j,d]*lambda_pos[j,d] + aux_pos[j,d]*lambda_free[j,d]
    }
  }
    
  
  for (jx in 1:J) {
    for (jy in 1:J) {
      diag_Del2[jx, jy] = equals(jx, jy) * del2[jx]
    }
  }

  cross_product_lambda = lambda %*% t(lambda)
  for (j1 in 1:J) {
    for (j2 in 1:J) {
      Sigma[j1, j2] = cross_product_lambda[j1, j2] + diag_Del2[j1, j2]
    }
  }

  for (i in 1:I) {
    theta[i, 1:J] ~ dmnorm.vcov(mu[], Sigma[,])
  }


  # Likelihood
  for (n in 1:N) {
    center[n] = theta[sub[n], task[n]]
    y[n] ~ dnorm(center[n], pTau2[task[n]])
  }
}
"

