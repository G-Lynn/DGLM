Q.PED <- function(stickiness,rho){
  Q = matrix(rep(0,4),ncol=2,nrow=2)
  Q[1,1] = 1-rho; Q[1,2] = rho
  Q[2,1] = 1-stickiness; Q[2,2] = stickiness
  return(Q)
}