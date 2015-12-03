Z_construct<-function(N,Omega,Y){
  Z = list()
  for(t in 1:t.T){
      Z[[t]] = (Y[[t]] - .5*N[[t]])/Omega[[t]]
  }
  return(Z)
}