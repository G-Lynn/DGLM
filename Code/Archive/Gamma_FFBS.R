Gamma.FFBS<-function(n,pi.G0,Q.gamma,Theta,FF,N,V,Z){
  t.T = max(which(n>0))
  t.1 = min(which(n>0))
  p.Gamma = list()
  for(t in t.1:t.T){# Begin Forward Filtering
    if(n[t]==0){
      if(t==t.1){
        p.Gamma[[t]] = pi.G0
      }else{
        p.Gamma[[t]] = p.Gamma[[t-1]]
      }
      
      Players.tm1 = ""
      next
    }
    
    Players.t = names(Z[[t]])
    p.Gamma[[t]] = matrix(nrow = n[t], ncol = K)
    rownames(p.Gamma[[t]]) = Players.t
    
    p.Gamma[[t]] = sapply(1:K, function(k) dnorm( Z[[t]], mean = F_kt(FF[[t]],k) %*% Theta[,t] , sd = sqrt( V ) ) )  
    
    if( length(Players.t) == 1){
      p.Gamma[[t]] = matrix(p.Gamma[[t]], ncol = K)
      rownames(p.Gamma[[t]]) = Players.t
    }
    
    if(t==t.1){
      p.Gamma[[t]] = as.numeric( pi.G0%*%Q.gamma[[t]]) * p.Gamma[[t]]
    }else{
      Ret.Players.t = Players.t[is.element(Players.t, Players.tm1)] 
      New.Players.t = Players.t[!is.element(Players.t, Players.tm1)]
      
      if( length(New.Players.t) > 0){
        p.Gamma[[t]][New.Players.t, ] = as.numeric( pi.G0%*%Q.gamma[[t]])  * p.Gamma[[t]][New.Players.t, ]
      }
      
      if( length(Ret.Players.t) > 0){
        p.Gamma[[t]][Ret.Players.t, ] = (p.Gamma[[t-1]][Ret.Players.t, ]%*%Q.gamma[[t]]) * p.Gamma[[t]][Ret.Players.t, ]
      }
      
    }
    
    p.Gamma[[t]] = p.Gamma[[t]]/apply(p.Gamma[[t]],1,sum)
    Players.tm1 = Players.t
  }# End Forward Filtering
  
  Gamma = list()
  for(t in t.T:t.1){# Begin Backward sampling
    
    if(n[t]==0){
      Players.tp1 = ""
      next
    }
    
    Players.t = rownames(p.Gamma[[t]])
    Gamma[[t]] = rep(NA,n[t])
    names(Gamma[[t]]) = Players.t
    
    if(t == t.T){
      Gamma[[t]][Players.t] = sapply(1:n[t], function(i) sample(1:K, 1, prob = p.Gamma[[t]][i,]))
    }else{
      Ret.Players.t = Players.t[is.element(Players.t, Players.tp1)] 
      New.Players.t = Players.t[!is.element(Players.t, Players.tp1)]
      nNew.t = length(New.Players.t)
      nRet.t = length(Ret.Players.t)
      
      if( nNew.t > 0){
        Gamma[[t]][New.Players.t] = sapply(1:nNew.t, function(i) sample(1:K, 1, prob = p.Gamma[[t]][New.Players.t[i],]))
      }
      
      if( nRet.t > 0){
        p.BS = p.Gamma[[t]][Ret.Players.t, ]*t( Q.gamma[[t]][, Gamma[[t+1]][Ret.Players.t] ] )
        if(nRet.t==1) p.BS = matrix(p.BS, ncol = K)
        p.BS = p.BS/apply(p.BS, 1, sum)
        rownames(p.BS) = Ret.Players.t
        Gamma[[t]][Ret.Players.t] = sapply(1:nRet.t, function(i) sample(1:K, 1, prob = p.BS[Ret.Players.t[i], ] ) )
      }
      
    }
    
    Players.tp1 = Players.t
  }# End of Backward sampling
  return(Gamma)
} #End of Gamma_FFBS function