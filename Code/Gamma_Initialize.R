Gamma.Initialize<-function(m0,K,N,y){
  PI.0 = 1/(1+exp(-m0[1:K]) )
  Gamma = list()
  
  for(t in 1:t.T){
    Gamma[[t]] = sapply(1:n[t], function(i) which( abs(y[[t]][i]-PI.0*N[[t]][i]) == min(abs(y[[t]][i]-PI.0*N[[t]][i]) ) )[1] )
    names(Gamma[[t]]) = names(y[[t]])
  }
  return(Gamma)
}