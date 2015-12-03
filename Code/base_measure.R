base.measure<-function(K,tau.0,PI.0){
  m0 = log( PI.0 / (1-PI.0 ) )
  beta = (5/10)
  sigma2 = rep(NA,K)
  for(k in 1:K){
    delta.m = (m0[k+1]-m0[k])
    if(k==K) delta.m = (m0[k]-m0[k-1])/2
    sigma2[k] = -delta.m^2/(8*log(beta))
  }
  tau.0 = which(abs(m0-(-4)) == min(abs(m0-(-4) ) ) ) 
  
  weights = rep(NA,K)
  for(k in 1:K){
    if(k>tau.0){
      weights[k] = exp( -1.5*abs( m0[k]-m0[tau.0] ) )
    }else{
      weights[k] = exp( -1.5*abs( m0[k]-m0[tau.0] ))
    }
  }
  
  PI.G.0 = weights / sum(weights)
  return( PI.G.0 )
}