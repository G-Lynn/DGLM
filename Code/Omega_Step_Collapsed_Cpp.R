Omega.step.collapsed <-function(FF, Theta, N, n, t.T){
  PG = list()
  for(t in 1:t.T){
    Players.t = names(N[[t]])
    PG[[t]] = rpgApproxCpp(n[t], N[[t]], FF[[t]]%*%Theta[,t] )
    names(PG[[t]]) = Players.t
  }
  return(PG)
} #End of Omega Step
