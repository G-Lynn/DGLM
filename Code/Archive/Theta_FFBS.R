Theta.FFBS<-function(n,m0,C0,Gamma,FF,N,V,Z){
  
  # store m[[t]]
  # store C[[t]]
  t.T = max(which(n>0))
  t.1 = min(which(n>0))
  K = dim(m0)[1]
  
  delta.0 = .9
  delta = rep(.95,t.T)
  y.hat = mu.hat = index = list()
  
  for(t in t.1:t.T){
    if(n[t]==0){next}
    y.hat[[t]] = rep(NA, n[t])
    mu.hat[[t]] = rep(NA,n[t])
    names(y.hat[[t]]) = names(mu.hat[[t]]) = names(Z[[t]])
    index[[t]] = order(Gamma[[t]])
  }
  
  
  #These quantities will have fixed dimension throughout regardless of Gamma assignment
  Theta = matrix(nrow = K, ncol = t.T)
  m     = matrix(nrow = K, ncol = t.T)
  a     = matrix(nrow = K, ncol = t.T) 
  
  
  #These quantities will have fixed dimension but are matrix valued.  
  f = list()
  C = list()
  A = list()
  Q = list()
  R = list()
  R.inv = list()
  
  
  for(t in t.1:t.T){# Forward Filtering
    Players.t = names(Z[[t]])
    if( n[t] == 0 ){
      if(t==t.1){
        m[,t] = m0
        a[,t] = m0
        C[[t]] = C0
        R[[t]] = (1/delta.0) * C0
      }else{
        m[,t] = m[,t-1]
        a[,t] = m[,t-1]
        C[[t]] = C[[t-1]]
        R[[t]] = (1/delta.0) * C[[t-1]]
      }
      next
    }else{
      
      
      if(t==t.1){
        a[,t] =  m0
        C.star = C0
      }else{
        a[,t] = m[,t-1]
        C.star = C[[t-1]]
      }
      #Now tune for delta
      C.inv = solve(C.star)
      V.inv = (1/V)*diag(n[t])
      
      n.Groups = sapply(1:K, function(k) sum(Gamma[[t]]==k))
      FF.index = matrix( FF[[t]][ index[[t]], ], nrow = n[t])
      rownames(FF.index) = Players.t[ index[[t]] ]
      
      f[[t]] = FF.index%*%a[,t]
      
      #delta[t] = delta.tune(3, FF.index, C.inv, V.inv, Z[[t]][ index[[t]] ], f[[t]] )
      R[[t]] = (1/delta[t])*C.star
      R.inv[[t]] = delta[t]*C.inv
      Q[[t]] = FF.index%*%R[[t]]%*%t(FF.index) + V*diag(n[t])
      
      # Woodbury Matrix Identity
      Q.inv= V.inv - V.inv%*%FF.index%*% solve(delta[t]*C.inv + t(FF.index)%*%V.inv%*%FF.index) %*%t(FF.index)%*%V.inv
      
      
      A[[t]] = R[[t]]%*%t( FF.index )%*%Q.inv
      C[[t]] = R[[t]] - A[[t]]%*%Q[[t]]%*%t( A[[t]] )
      
      
      
      m[,t] = a[,t] + A[[t]]%*%( Z[[t]][ index[[t]] ] - f[[t]] )
      
      
      theta.t = matrix(mvrnorm(1,mu = a[,t], Sigma = R[[t]]), ncol = 1)
      
      eta.t = FF[[t]]%*%theta.t
      mu.t = exp(eta.t)/(1+exp(eta.t))
      y.hat[[t]][Players.t] = rbinom(n[t],N[[t]],mu.t)
      mu.hat[[t]][Players.t] = mu.t
      
    } #End of logic for empty group
  }# End of Forward Filtering
  
  for(t in t.T:t.1){# Backward Sampling
    if(t == t.T){
      Theta[,t] = mvrnorm(1, mu = m[,t], Sigma = C[[t]])
    }else{ 
      b = m[,t] + C[[t]]%*%R.inv[[t+1]]%*%(Theta[,t+1] - a[,t+1])
      B = C[[t]] - C[[t]]%*%R.inv[[t+1]]%*%C[[t]]
      Theta[,t] = mvrnorm(1, b, B)
    }
  }# End of Backward Sampling
  
  return(list(Theta, y.hat, mu.hat,delta) )
}# End of Theta.FFBS function

