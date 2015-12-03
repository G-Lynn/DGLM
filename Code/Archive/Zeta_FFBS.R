Zeta.FFBS<-function(n,pi.Z0,Q.zeta,Theta,FF,N,V,Z,Data){
  t.T = max(which(n>0))
  t.1 = min(which(n>0))
  
  p.Zeta = list()
  FF.z = list(FF.zero = F_zt(FF,Zeta.Initialize(n,0,Data,"") ), FF.one = F_zt(FF,Zeta.Initialize(n,1,Data,"") ) )
  
  for(t in t.1:t.T){# Begin Forward Filtering
    if(n[t]==0){
      
      if(t==t.1){
        p.Zeta[[t]] = pi.Z0
      }else{
        p.Zeta[[t]] = p.Zeta[[t-1]]
      }
      
      Players.tm1 = ""
      next
      
    }
    Players.t = names(Z[[t]])
    
    p.Zeta[[t]] = sapply(1:2, function(z) dnorm(Z[[t]], mean = FF.z[[z]][[t]] %*% Theta[,t] , sd = sqrt(V[[t]] ) ) )  
    p.Zeta[[t]] = matrix(p.Zeta[[t]], nrow = n[t], ncol= 2)
    rownames(p.Zeta[[t]]) = Players.t
    colnames(p.Zeta[[t]]) = c("Zero", "One")
    
    if(t==t.1){
      p.Zeta[[t]] = as.numeric( pi.Z0%*%Q.zeta) * p.Zeta[[t]]
    }else{
      Ret.Players.t = Players.t[is.element(Players.t, Players.tm1)] 
      New.Players.t = Players.t[!is.element(Players.t, Players.tm1)]
      
      if( length(New.Players.t) > 0){
        p.Zeta[[t]][New.Players.t, ] = as.numeric( pi.Z0%*%Q.zeta)  * p.Zeta[[t]][New.Players.t, ]
      }
      
      if( length(Ret.Players.t) > 0){
        p.Zeta[[t]][Ret.Players.t, ] = (p.Zeta[[t-1]][Ret.Players.t, ]%*%Q.zeta) * p.Zeta[[t]][Ret.Players.t, ]
      }
      
    }
    
    p.Zeta[[t]] = p.Zeta[[t]]/apply(p.Zeta[[t]],1,sum)
    Players.tm1 = Players.t
  }# End Forward Filtering
  
  Zeta = list()
  for(t in n.T:t.1){# Begin Backward sampling
    
    if(n[t]==0){
      Players.tp1 = ""
      next
    }
    
    Players.t = rownames(p.Zeta[[t]])
    Zeta[[t]] = rep(NA,n[t])
    names(Zeta[[t]]) = Players.t
    
    if(t == t.T){
      Zeta[[t]][Players.t] = sapply(1:n[t], function(i) sample(0:1, 1, prob = p.Zeta[[t]][i,]))
    }else{
      Ret.Players.t = Players.t[is.element(Players.t, Players.tp1)] 
      New.Players.t = Players.t[!is.element(Players.t, Players.tp1)]
      nNew.t = length(New.Players.t)
      nRet.t = length(Ret.Players.t)
      
      if( nNew.t > 0){
        Zeta[[t]][New.Players.t] = sapply(1:nNew.t, function(i) sample(0:1, 1, prob = p.Zeta[[t]][New.Players.t[i],]))
      }
      
      if( nRet.t > 0){
        p.BS = p.Zeta[[t]][Ret.Players.t, ]*t( Q.zeta[, Zeta[[t+1]][Ret.Players.t]+1 ] )
        if(nRet.t==1) p.BS = matrix(p.BS, ncol = 2)
        p.BS = p.BS/apply(p.BS, 1, sum)
        rownames(p.BS) = Ret.Players.t
        Zeta[[t]][Ret.Players.t] = sapply(1:nRet.t, function(i) sample(0:1, 1, prob = p.BS[Ret.Players.t[i], ] ) )
      }
      
    }
    Players.tp1 = Players.t
  }# End of Backward sampling
  
  return(Zeta)
}# End Zeta FFBS function