GammaZeta.FFBS.collapsed<-function(n,K,PI.G_Z.0, Q.gamma_zeta, Theta, N, Omega, Z){
  t.T = max(which(n>0))
  t.1 = min(which(n>0))
  p.GammaZeta = list()
  
  # Re-label the prior based on the Theta specific ordering.  
  #order.index = apply(Theta[1:K,],2,order)
  #for(t in 1:t.T){
  #  PI.G_Z.0[t,1:K] = PI.G_Z.0[t,order.index[,t]]
  #  PI.G_Z.0[t,(K+1):(2*K)] = PI.G_Z.0[t,(K+order.index[,t])]
    
  #  Q.gamma_zeta[[t]][1:K,1:K] = Q.gamma_zeta[[t]][order.index[,t],order.index[,t]]
  #  Q.gamma_zeta[[t]][1:K,(K+1):(2*K)] = Q.gamma_zeta[[t]][order.index[,t],(K+order.index[,t])]
  #  Q.gamma_zeta[[t]][(K+1):(2*K),1:K] = Q.gamma_zeta[[t]][(K+order.index[,t]),order.index[,t]]
  #  Q.gamma_zeta[[t]][(K+1):(2*K),(K+1):(2*K)] = Q.gamma_zeta[[t]][(K+order.index[,t]),(K+order.index[,t])]
  #}
  
  for(t in t.1:t.T){# Begin Forward Filtering
    if(n[t]==0){
      if(t==t.1){
        p.GammaZeta[[t]] = PI.G_Z.0[t,]
      }else{
        p.GammaZeta[[t]] = p.GammaZeta[[t-1]]
      }
      
      Players.tm1 = ""
      next
    }else{
      
      Players.t = names(Z[[t]])
      p.GammaZeta[[t]] = matrix(nrow = n[t], ncol = 2*K)
      rownames(p.GammaZeta[[t]]) = Players.t
      
      for(k in 1:(2*K)){
        if(k<=K){
          p.GammaZeta[[t]][,k] = dnorm( Z[[t]], mean =  F_kzt(Players.t,K,k,0) %*% Theta[,t] , sd = sqrt( 1/Omega[[t]] ), log=T )
        }else{
          p.GammaZeta[[t]][,k] = dnorm( Z[[t]], mean = F_kzt(Players.t,K,k-K,1) %*% Theta[,t] , sd = sqrt( 1/Omega[[t]] ), log=T ) 
        }
      }     
      
      if( length(Players.t) == 1){
        p.GammaZeta[[t]] = matrix(p.GammaZeta[[t]], ncol = 2*K)
        rownames(p.GammaZeta[[t]]) = Players.t
      }
      
      if(t==t.1){
        p.GammaZeta[[t]] = matrix(rep(log( as.numeric( PI.G_Z.0[t,]%*%Q.gamma_zeta[[t]]) ), n[t]), nrow = n[t], byrow=T) + p.GammaZeta[[t]]

      }else{
        Ret.Players.t = Players.t[is.element(Players.t, Players.tm1)] 
        New.Players.t = Players.t[!is.element(Players.t, Players.tm1)]
        nNew.t = length(New.Players.t)
        nRet.t = length(Ret.Players.t)
        if(nNew.t > 0){
          #I should really sample from the marginal not the prior for new players given their age
          p.GammaZeta[[t]][New.Players.t, ] = matrix( rep(log( as.numeric( PI.G_Z.0[t,]%*%Q.gamma_zeta[[t]]) ), nNew.t ), nrow = nNew.t, byrow=T)  +  p.GammaZeta[[t]][New.Players.t, ]
        }
        
        if( nRet.t > 0){
          p.GammaZeta[[t]][Ret.Players.t, ] = log(p.GammaZeta[[t-1]][Ret.Players.t, ]%*%Q.gamma_zeta[[t]])+ p.GammaZeta[[t]][Ret.Players.t, ]
        }
        
      }
      
      a = apply(p.GammaZeta[[t]],1,max)
      p.GammaZeta[[t]] = p.GammaZeta[[t]] - a - log( apply( exp(p.GammaZeta[[t]]-a),1,sum) )
      p.GammaZeta[[t]] = exp(p.GammaZeta[[t]])
      
      NA.index = which( is.nan(apply(p.GammaZeta[[t]],1,sum)) )
      
      if(length(NA.index>0)){stop(paste("NA in probability for:",Players.t[NA.index],"at",t, ":", as.character( p.GammaZeta[[t]][NA.index,] ), sep=" ") )}
      Players.tm1 = Players.t
    }
  }# End Forward Filtering
  
  GammaZeta = list()
  Gamma = list()
  Zeta = list()
  
  for(t in t.T:t.1){# Begin Backward sampling
    
    if(n[t]==0){
      Players.tp1 = ""
      next
    }
    
    Players.t = rownames(p.GammaZeta[[t]])
    GammaZeta[[t]] = rep(NA,n[t])
    names(GammaZeta[[t]]) = Players.t
    
    if(t == t.T){
      GammaZeta[[t]][Players.t] = sapply(1:n[t], function(i) sample(1:(2*K), 1, prob = p.GammaZeta[[t]][i,]))
    }else{
      Ret.Players.t = Players.t[is.element(Players.t, Players.tp1)] 
      New.Players.t = Players.t[!is.element(Players.t, Players.tp1)]
      nNew.t = length(New.Players.t)
      nRet.t = length(Ret.Players.t)
      
      if( nNew.t > 0){
        GammaZeta[[t]][New.Players.t] = sapply(1:nNew.t, function(i) sample(1:(2*K), 1, prob = p.GammaZeta[[t]][New.Players.t[i],]))
      }
      
      if( nRet.t > 0){
        p.BS = p.GammaZeta[[t]][Ret.Players.t, ]*t( Q.gamma_zeta[[t]][,GammaZeta[[t+1]][Ret.Players.t] ] )
        if(nRet.t==1) p.BS = matrix(p.BS, ncol = (2*K) )
        p.BS = p.BS/apply(p.BS, 1, sum)
        rownames(p.BS) = Ret.Players.t
        GammaZeta[[t]][Ret.Players.t] = sapply(1:nRet.t, function(i) sample(1:(2*K), 1, prob = p.BS[Ret.Players.t[i], ] ) )
      }
      
    }
    
    Gamma[[t]] = GammaZeta[[t]]
    Zeta[[t]] = rep(0, n[t]); names(Zeta[[t]]) = Players.t
    Zeta[[t]][Gamma[[t]]>K] = 1
    Gamma[[t]][ Zeta[[t]] == 1 ] = Gamma[[t]][ Zeta[[t]] == 1 ] - K
    
    Players.tp1 = Players.t
  }# End of Backward sampling
  GammaZeta = list(Gamma,Zeta)
  return(GammaZeta)
} #End of Gamma Zeta FFBS

