F_construct <-function(n,K,t.T,Gamma,Zeta){
  FF = list()
  for(t in 1:t.T){
    if(n[t]==0){next}
    FF[[t]] = matrix(nrow = n[t], ncol = (K+1) )
    Players.t = names(Gamma[[t]])
    rownames(FF[[t]]) = Players.t
    
    for(i in 1:n[t]){
      player = Players.t[i]
      FF[[t]][player,1:K] = rep(0,K)
      FF[[t]][player,Gamma[[t]][player] ] = 1
      FF[[t]][player,K+1] = Zeta[[t]][player]
    }
  }
  return(FF)
}