rm(list=ls())
source("~/Dropbox/Surya_Group/Baseball/Simulation_Study/DLM_functions.R")
source("~/Dropbox/Surya_Group/Baseball/Simulation_Study/DLM_FFBS.R")
load("~/DGLM/Data/AgeAlignment.RData")

t.T = length(Age.Alignment)
Age = 18+(1:t.T)

n = rep(NA,t.T)
for(t in 1:t.T) n[t] = length(Age.Alignment[[t]])
t.min = min(which(n>=100))
t.max = max(which(n>=100))
K = 10
Age = Age[t.min:t.max]
t.T = length(t.min:t.max)

y = N = list()
for(t in 1:t.T){
  tt = t.min + t-1
  y[[t]] = sapply(1:n[tt], function(i) Age.Alignment[[tt]][[i]]$Response$HR )
  N[[t]] = sapply(1:n[tt], function(i) Age.Alignment[[tt]][[i]]$Response$AB )
  names(y[[t]]) = names(N[[t]]) = names(Age.Alignment[[tt]])
}

n = n[t.min:t.max]
ALPHA = 2
ALPHA.decay = 5

PI.Z.0 = matrix(c(1,0), nrow = 1)
stickiness_zeta = 8/9  # 8 players out of every 9 stay steroid users.  
rho = 1/36             # 1 player out of every 36 becomes a steroid user
Q.zeta = Q.PED(stickiness_zeta,rho)

PI.0 = seq(from = .005, to = .10, len = K) 
m0 = log( PI.0 / (1-PI.0 ) )
m0.PED = .4
m0 = matrix(c(m0, m0.PED), ncol = 1)


beta = (8/10)
sigma2 = rep(NA,K+1)
for(k in 1:K){
  delta.m = (m0[k+1]-m0[k])
  if(k==K) delta.m = (m0[k]-m0[k-2])/2
  sigma2[k] = -delta.m^2/(8*log(beta))
}
sigma2[K+1] = .02
C0 = sigma2*diag(K+1)



tau.0 = which(abs(m0-(-4)) == min(abs(m0-(-4) ) ) ) 
weights = rep(NA,K)
for(k in 1:K){
  if(k<tau.0){
	weights[k] = exp(-1*abs(m0[k]-m0[tau.0]) )
  }else{
	weights[k] = exp(-2*abs(m0[k]-m0[tau.0]) )
  }
}

PI.G.0 = weights/sum(weights)


PI.G_Z.0 = matrix(rep(0,2*K), nrow = 1)
PI.G_Z.0[1:K] = PI.G.0 * PI.Z.0[,1]
PI.G_Z.0[(K+1):(2*K)] = PI.G.0 * PI.Z.0[,2]

Q.gamma = Q.gamma_zeta = list()
for(t in 1:t.T){
  Q.gamma[[t]] = Q.age(Age[t],alpha=ALPHA,decay.rate = ALPHA.decay, K)
  Q.gamma_zeta[[t]] = matrix(nrow = 2*K, ncol = 2*K)
  Q.gamma_zeta[[t]][1:K,1:K] = Q.gamma[[t]]*Q.zeta[1,1]
  Q.gamma_zeta[[t]][1:K,(K+1):(2*K)] = Q.gamma[[t]]*Q.zeta[1,2]
  Q.gamma_zeta[[t]][(K+1):(2*K),1:K] = Q.gamma[[t]]*Q.zeta[2,1]
  Q.gamma_zeta[[t]][(K+1):(2*K),(K+1):(2*K)] = Q.gamma[[t]]*Q.zeta[2,2]
}
  
save(file = "~/DGLM/Data/Raw_Data.RData",t.T,n,Age,K,Q.gamma_zeta,Q.gamma,Q.zeta,PI.G_Z.0,PI.G.0,PI.Z.0,y,N,m0,C0)

