MCMC.collapsed<-function(init,nSamples,N.MC,Thin.Rate,m0,C0,sigma2.theta,t.T,n,K,PI.G_Z.0,Q.gamma_zeta,N,y,V,Theta.Truth,Gamma.Truth,Zeta.Truth){

  Theta = Theta.Initialize(m0,C0,sqrt(sigma2.theta),t.T)
  Gamma = Gamma.Initialize(m0,K,N,y)
  Zeta = Zeta.Initialize(n,0,y)
  FF = F_construct(n,K,t.T,Gamma,Zeta)
  Omega = Omega.step.collapsed(FF,Theta,N,n,t.T)
  Z = Z_construct(N,Omega,y)

  Theta.Samples = list()
  Gamma.Samples = list()
  Zeta.Samples = list()
  for(t in 1:t.T){
    Theta.Samples[[t]] = matrix(nrow = nSamples*(K+1), ncol = 2)
    Gamma.Samples[[t]] = matrix(nrow = nSamples*n[t], ncol = 2)
    Zeta.Samples[[t]] = matrix(nrow = nSamples*n[t], ncol = 2)
  }
  
  for(m in 1:N.MC){
    Omega = Omega.step.collapsed(FF,Theta,N,n,t.T)
    Z = Z_construct(N,Omega,y)
    
    TY = Theta.FFBS.collapsed(n,m0,C0,FF,N,Omega,Kappa)
    Theta = TY[[1]]
    
    GammaZeta = GammaZeta.FFBS.collapsed(n,K,PI.G_Z.0,Q.gamma_zeta,Theta,N,Omega,Z)
    Gamma = GammaZeta[[1]]
    Zeta = GammaZeta[[2]]
    
    FF = F_construct(n,K,t.T,Gamma,Zeta)
    
    if(m>B & m%%Thin.Rate==0){
      for(t in 1:t.T){
        
        index = (m-B)/Thin.Rate

        Theta.Samples[[t]][( (index-1)*(K+1)+1):( (index)*(K+1) ),1] =  1:(K+1) 
        Theta.Samples[[t]][( (index-1)*(K+1)+1):( (index)*(K+1) ),2] = Theta[,t] 
        
        Gamma.Samples[[t]][( (index-1)*n[t]+1):( (index)*n[t] ),1] = as.numeric( names(Gamma[[t]]) )
        Gamma.Samples[[t]][( (index-1)*n[t]+1):( (index)*n[t] ),2] = as.numeric( Gamma[[t]] )
        
        Zeta.Samples[[t]][( (index-1)*n[t]+1):( (index)*n[t] ),1] = as.numeric( names(Zeta[[t]]) )
        Zeta.Samples[[t]][( (index-1)*n[t]+1):( (index)*n[t] ),2] = as.numeric( Zeta[[t]] )
        
      }
    } 
  }
  
  Post.Mean.Theta = Post.Err.Theta = list()
  Post.Mean.Gamma = Post.Err.Gamma = list()
  Post.Mean.Zeta = Post.Err.Zeta = list()
  for(t in 1:t.T){

    Post.Mean.Theta[[t]] = sapply(1:(K+1), function(k) mean( Theta.Samples[[t]][Theta.Samples[[t]][,1]==k,2] ) )
    Post.Err.Theta[[t]] = Theta.Truth[,t] - Post.Mean.Theta[[t]]
    
    index = order(Post.Mean.Theta[[t]])
    Post.Mean.Theta[[t]] = sort(Post.Mean.Theta[[t]])
    
    for(k in 1:K){
      subset = which(Gamma.Samples[[t]][,2]==k)
      n.k = length(subset)
      Gamma.Samples[[t]][subset,2] = rep(index[k],n.k)
    }
    
    Post.Mean.Gamma[[t]] = sapply(1:n[t], function(i) mean ( Gamma.Samples[[t]][Gamma.Samples[[t]][,1]==i,2] ) )
    Post.Err.Gamma[[t]] = Gamma.Truth[[t]] - Post.Mean.Gamma[[t]]
    
    Post.Mean.Zeta[[t]] = sapply(1:n[t], function(i) mean(Zeta.Samples[[t]][Zeta.Samples[[t]][,1]==i,2] ) )
    Post.Err.Zeta[[t]] = Zeta.Truth[[t]] - Post.Mean.Zeta[[t]]
  }
  nPlayers = max(n)
  save(file = paste("~/DGLM/Reproducibility/Init_",init,".RData",sep=""), Post.Mean.Theta, Post.Err.Theta, Post.Mean.Gamma, Post.Err.Gamma, Post.Mean.Zeta, Post.Err.Zeta )
}