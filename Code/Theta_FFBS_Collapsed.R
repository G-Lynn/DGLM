Theta.FFBS.collapsed<-function(n,m0,C0,W,FF,N,Omega,Kappa){
  
  # store m[[t]]
  # store C[[t]]
  t.T = max(which(n>0))
  t.1 = min(which(n>0))
  K = dim(m0)[1]
  
  y.hat = mu.hat = index = list()
  
  for(t in t.1:t.T){
    if(n[t]==0){next}
    y.hat[[t]] = rep(NA, n[t])
    mu.hat[[t]] = rep(NA,n[t])
    names(y.hat[[t]]) = names(mu.hat[[t]]) = names(Kappa[[t]])
  }
  
  
  #These quantities will have fixed dimension throughout regardless of Gamma assignment
  Theta = matrix(nrow = K, ncol = t.T)
  Theta[K,] = -1
  m     = matrix(nrow = K, ncol = t.T)
  a     = matrix(nrow = K, ncol = t.T) 
  
  
  #These quantities will have fixed dimension but are matrix valued.  
  C = list()
  R = list()
  R.inv = list()
  
  
  for(t in t.1:t.T){# Forward Filtering
    Players.t = names(Kappa[[t]])
    if( n[t] == 0 ){
      if(t==t.1){
        m[,t] = m0
        a[,t] = m0
        C[[t]] = C0
        R[[t]] = C0 + W
      }else{
        m[,t] = m[,t-1]
        a[,t] = m[,t-1]
        C[[t]] = C[[t-1]]
        R[[t]] = C[[t-1]] + W
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
      
      C.inv = solve(C.star)
    
      
      R[[t]] = C.star + W
      R.inv[[t]] = solve(R[[t]])
  
      C[[t]] =solve( t( FF[[t]] ) %*% diag(Omega[[t]]) %*% FF[[t]] + R.inv[[t]] )     
      m[,t] = C[[t]]%*%( t(FF[[t]])%*%Kappa[[t]] + R.inv[[t]]%*%a[,t] )
      #Note that diag(Omega[[t]])%*%alpha[[t]] = Kappa[[t]] So I omit the matrix multiplication
      
      theta.t = matrix(mvrnorm(1,mu = a[,t], Sigma = R[[t]]), ncol = 1)
      
      eta.t = FF[[t]]%*%theta.t
      mu.t = exp(eta.t)/(1+exp(eta.t))
      y.hat[[t]][Players.t] = rbinom(n[t],N[[t]],mu.t)
      mu.hat[[t]][Players.t] = mu.t
      
    } #End of logic for empty group
  }# End of Forward Filtering
  
  for(t in t.T:t.1){# Backward Sampling
    if(t == t.T){
      #while(Theta[K,t]<.3) Theta[,t] = mvrnorm(1, mu = m[,t], Sigma = C[[t]])
      Theta[,t] = mvrnorm(1, mu = m[,t], Sigma = C[[t]])
    }else{ 
      b = m[,t] + C[[t]]%*%R.inv[[t+1]]%*%(Theta[,t+1] - a[,t+1]) #this is equivalent to the form in the paper
      B = C[[t]] - C[[t]]%*%R.inv[[t+1]]%*%C[[t]] ## this holds by Woodbury matrix identity
      #while(Theta[K,t]<.3) Theta[,t] = mvrnorm(1, b, B)
      Theta[,t] = mvrnorm(1, b, B)
    }
  }# End of Backward Sampling
  Theta[1:(K-1),] = apply(Theta[1:(K-1),],2,sort)  
  return(list(Theta, y.hat, mu.hat) )
}# End of Theta.FFBS function

