Eta.step <-function(Theta, FF, Kappa, sigma2.V, Omega, nPlayers, t.T){
  Eta = list()
  for(t in 1:t.T){
    Players.t = rownames(FF[[t]])
    mean.eta = as.numeric( FF[[t]]%*%Theta[,t] )
    sigma2.post = (Omega[[t]] + 1/sigma2.V)^(-1)
    mu.post = sigma2.post*(Kappa[[t]] + mean.eta/sigma2.V)
    Eta[[t]] = mvrnorm(1, mu.post, sigma2.post*diag(nPlayers[t]))
    names(Eta[[t]]) = Players.t
  }
  return(Eta)
} #End of Eta Step


