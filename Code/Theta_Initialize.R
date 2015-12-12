Theta.Initialize<-function(m0,C0, sigma2, t.T){
  K = dim(m0)[1]
  Theta = matrix(nrow = K, ncol = t.T)
  for(t in 1:t.T){
    for(k in 1:K) Theta[k,t] = m0[k] + rnorm(1,0,sqrt(sigma2[k]) )
  }
  
  return(Theta)
}