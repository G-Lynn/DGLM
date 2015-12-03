Omega.step <-function(Eta, N, nPlayers, t.T){
  PG = list()
  for(t in 1:t.T){
    Players.t = names(Eta[[t]])
    PG[[t]] = rpg.devroye(num = nPlayers[t], n = N[[t]], z = Eta[[t]] )
    names(PG[[t]]) = Players.t
  }
  return(PG)
} #End of Omega Step

