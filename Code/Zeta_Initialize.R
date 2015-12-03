Zeta.Initialize<-function(n,zeta,Data){
  
  Zeta = list()
  
  for(t in 1:t.T){
    if(n[t]==0){next}
    
    Players.t = names(Data[[t]])
    Zeta[[t]] = rep(zeta, n[t])
    names(Zeta[[t]]) = Players.t  
    
  }  
  return(Zeta)
}