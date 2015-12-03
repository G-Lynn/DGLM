F_kzt <-function(Players.t,K,k,z){
  t.T = length(Players.t)
  if(t.T==0){
    return(NULL)
  }else{
    FF.t = matrix(0, nrow = t.T, ncol = K+1)
    FF.t[,k] = 1
    FF.t[,(K+1)] = z
    return(FF.t)
  }
}