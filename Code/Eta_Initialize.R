Eta.Initialize<-function(n,Theta,FF,sigma,t.T){
  K = dim(Theta)[1]
  Eta = list()
  for(t in 1:t.T){
    Players.t = rownames(FF[[t]])
    Eta[[t]] = as.numeric( FF[[t]]%*%Theta[,t] ) + rnorm(n[t],0,sigma)
    names(Eta[[t]]) = Players.t
  }
  return(Eta)
}