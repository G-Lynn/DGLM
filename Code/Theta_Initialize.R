Theta.Initialize<-function(m0,C0, sigma, t.T){
  K = dim(m0)[1]
  Theta = matrix(nrow = K, ncol = t.T)
  for(t in 1:t.T){
    Theta[,t] = m0 + rnorm(K,0,sigma)
  }
  
  return(Theta)
}