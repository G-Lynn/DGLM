rm(list=ls())
source("~/Dropbox/Surya_Group/Baseball/Simulation_Study/DLM_functions.R")
source("~/Dropbox/Surya_Group/Baseball/Simulation_Study/DLM_FFBS.R")


t.T = 10
Age = 18+(1:t.T)

n = c(10, 100, 500,1000)
K = ceiling( log(n) )
lambda = 400
ALPHA = ALPHA.decay = 3.5

PI.Z.0 = matrix(c(1,0), nrow = 1)
stickiness_zeta = 8/9  # 8 players out of every 9 stay steroid users.  
rho = 1/36             # 1 player out of every 36 becomes a steroid user
Q.zeta = Q.PED(stickiness_zeta,rho)
V = 1e-3

for(j in 1:4){
  m0 = -5+(0:(K[j]-1))/(K[j]-1)*2.5
  m0.PED = .4
  
  y.1 = y.2 = Gamma = Zeta = N = Q.gamma = Q.gamma_zeta = eta.1 = eta.2 = list()
  Players.t = as.character(1:n[j])
  vec = rep(NA, n[j]); names(vec) = Players.t
  
  for(t in 1:t.T) y.1[[t]] = y.2[[t]] = Gamma[[t]] = Zeta[[t]] = N[[t]] = eta.1[[t]] = eta.2[[t]] = vec
  Theta = matrix(nrow = K[j]+1, ncol = t.T)
  
  weights = 1/(1:K[j])^2
  tau.0 = round(.5*K[j])
  PI.G.0 = rep(NA,K[j])
  PI.G.0[tau.0] = weights[1]
  
  for(tau in 1:K[j]){
    if((tau.0-tau)>0) PI.G.0[tau.0-tau] = weights[1+tau]
    if((tau.0+tau)<=K[j]) PI.G.0[tau.0+tau] = weights[1+tau]
  }
  PI.G.0 = PI.G.0/sum(PI.G.0)
  
  PI.G_Z.0 = matrix(rep(0,2*K[j]), nrow = 1)
  PI.G_Z.0[1:K[j]] = PI.G.0 * PI.Z.0[,1]
  PI.G_Z.0[(K[j]+1):(2*K[j])] = PI.G.0 * PI.Z.0[,2]
  
    
  gamma.0 = sample(1:K[j], size = n[j], prob = PI.G.0, replace=T)
    
  for(t in 1:t.T){
    Q.gamma[[t]] = Q.age(Age[t],alpha=ALPHA,decay.rate = ALPHA.decay, K[j])
    Q.gamma_zeta[[t]] = matrix(nrow = 2*K[j], ncol = 2*K[j])
    Q.gamma_zeta[[t]][1:K[j],1:K[j]] = Q.gamma[[t]]*Q.zeta[1,1]
    Q.gamma_zeta[[t]][1:K[j],(K[j]+1):(2*K[j])] = Q.gamma[[t]]*Q.zeta[1,2]
    Q.gamma_zeta[[t]][(K[j]+1):(2*K[j]),1:K[j]] = Q.gamma[[t]]*Q.zeta[2,1]
    Q.gamma_zeta[[t]][(K[j]+1):(2*K[j]),(K[j]+1):(2*K[j])] = Q.gamma[[t]]*Q.zeta[2,2]
    
    
    N[[t]] = rpois(n[j],lambda)
    
    if(t==1){
      Gamma[[t]] = sapply(1:n[j], function(i) sample(1:K[j], size=1, prob = Q.gamma[[t]][gamma.0[i],], replace=T ) )
      Zeta[[t]] =  sapply(1:n[j], function(i) sample(0:1, size = 1, prob = Q.zeta[1,], replace=T) ) 
    }else{
      Gamma[[t]] = sapply(1:n[j], function(i) sample(1:K[j], size=1, prob = Q.gamma[[t]][Gamma[[t-1]][i],], replace=T ) )
      Zeta[[t]] =  sapply(1:n[j], function(i) sample(0:1, size = 1, prob = Q.zeta[(Zeta[[t-1]][i]+1),], replace=T) ) 
    }
    
    theta1 = sapply(1:n[j], function(i) m0[ Gamma[[t]][i] ] + m0.PED*Zeta[[t]][i] ) 
    theta2 = sapply(1:n[j], function(i) m0[ Gamma[[t]][i] ]+ (1/K[j])*(m0[2]-m0[1]) + m0.PED*Zeta[[t]][i] )
    Theta[1:K[j],t] = m0
    Theta[(K[j]+1),t] = m0.PED
    
    eta.1[[t]] = theta1 + m0.PED*Zeta[[t]] #+ rnorm(n[j],0,sqrt(V) )
    eta.2[[t]] = theta2 + m0.PED*Zeta[[t]] #+ rnorm(n[j],0,sqrt(V) )
    
    mu1 = exp(eta.1[[t]])/(1+exp(eta.1[[t]]))
    mu2 = exp(eta.2[[t]])/(1+exp(eta.2[[t]]))
    
    y.1[[t]] = rbinom(n[j],N[[t]],mu1)
    y.2[[t]] = rbinom(n[j],N[[t]],mu2)
    names(y.1[[t]]) = names(y.2[[t]]) = names(eta.1[[t]]) = names(eta.2[[t]]) = names(Gamma[[t]]) = names(Zeta[[t]]) = Players.t
  }
  
save(file = paste("~/Dropbox/Surya_Group/DGLM/SimStudy/p", n[j],"/DGP1/DGP1.RData",sep=""),t.T, Age, Q.gamma_zeta, Q.gamma, Q.zeta, PI.G_Z.0, PI.G.0, PI.Z.0, y.1,N,eta.1, Theta, Gamma, Zeta, V)
save(file = paste("~/Dropbox/Surya_Group/DGLM/SimStudy/p", n[j],"/DGP2/DGP2.RData",sep=""),t.T, Age, Q.gamma_zeta, Q.gamma, Q.zeta, PI.G_Z.0, PI.G.0, PI.Z.0, y.2,N,eta.2, Theta, Gamma, Zeta, V)
}
