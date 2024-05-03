
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
