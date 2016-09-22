rm(list=ls())
#source("~/Dropbox/Surya_Group/Baseball/Simulation_Study/DLM_functions.R")
#source("~/Dropbox/Surya_Group/Baseball/Simulation_Study/DLM_FFBS.R")
source("~/DGLM/Code/Q_Age.R")
source("~/DGLM/Code/Q_PED.R")
#load("~/DGLM/Data/AgeAlignmentSmall.RData")
load("~/DGLM/Data/AgeAlignment2005.RData")

t.T = length(Age.Alignment)
Age = 18+(1:t.T)

n = rep(NA,t.T)
for(t in 1:t.T) n[t] = length(Age.Alignment[[t]])
t.min = min(which(n>=10)) #50
t.max = max(which(n>=10)) #50
Age = Age[t.min:t.max]
print(Age)
t.T = length(t.min:t.max)
print(t.T)
y = N = list()
for(t in 1:t.T){
  tt = t.min + t-1
  y[[t]] = sapply(1:n[tt], function(i) Age.Alignment[[tt]][[i]]$Response$HR )
  N[[t]] = sapply(1:n[tt], function(i) Age.Alignment[[tt]][[i]]$Response$AB )
  names(y[[t]]) = names(N[[t]]) = names(Age.Alignment[[tt]])
}

n = n[t.min:t.max]
alpha = 5
decay.rate = 2


PI.Z.0 = matrix(c(1,0), nrow = 1)
stickiness_zeta = 2/3 #8/9  # 8 players out of every 9 stay steroid users.  
rho = 1/50  #1/36           # 1 player out of every 36 becomes a steroid user
Q.zeta = Q.PED(stickiness_zeta,rho)

K = 15
m0 = seq(from = -4.5, to = -2.25, len = K)
PI.0 = 1/(1+exp(-m0))

m0.PED = .4
sigma2.PED = .001
m0 = matrix(c(m0, m0.PED), ncol = 1)


beta = 2/10 #1/100
sigma2 = rep(NA,K+1)
for(k in 1:K){
  delta.m = (m0[k+1]-m0[k])
  if(k==K) delta.m =(m0[k]-m0[k-1])
  sigma2[k] = -delta.m^2/(8*log(beta))
}

sigma2[K+1] = sigma2.PED
C0 = sigma2*diag(K+1)
#print(sigma2)
W = .5*C0 #.5*C0 works well.

#print(W)

#PI.G.0 = c(1, rep(0,K-1))
PI.G.0 = rep(NA,K)
#PI.G.0 = c(1,rep(0,K-1))
k.0 = 7
for(k in 1:K) PI.G.0[k] = exp(-1/(2*6)*(k.0-k)^2)
PI.G.0 = PI.G.0 / sum(PI.G.0)

load("~/DGLM/Data/Gamma_Zeta_marginal_time.RData")
PI.G_Z.0 = PI.G_Z.0[is.element(rownames(PI.G_Z.0),as.character(Age)),]

Q.gamma = Q.gamma_zeta = list()
for(t in 1:t.T){
  Q.gamma[[t]] = Q.age(Age[t],alpha=alpha,decay.rate = decay.rate, K, m0[1:K])
  Q.gamma_zeta[[t]] = matrix(nrow = 2*K, ncol = 2*K)
  Q.gamma_zeta[[t]][1:K,1:K] = Q.gamma[[t]]*Q.zeta[1,1]
  Q.gamma_zeta[[t]][1:K,(K+1):(2*K)] = Q.gamma[[t]]*Q.zeta[1,2]
  Q.gamma_zeta[[t]][(K+1):(2*K),1:K] = Q.gamma[[t]]*Q.zeta[2,1]
  Q.gamma_zeta[[t]][(K+1):(2*K),(K+1):(2*K)] = Q.gamma[[t]]*Q.zeta[2,2]
}

#save(file = "~/DGLM/Data/Q_Matrices.RData", Q.gamma, Q.zeta)
#print(PI.G_Z.0)
apply(PI.G_Z.0,1,sum)
save(file = "~/DGLM/Data/Raw_Data.RData",t.T,n,Age,K,Q.gamma_zeta,Q.gamma,Q.zeta,PI.G_Z.0,PI.G.0,PI.Z.0,y,N,m0,C0,W)
#save(file = "~/DGLM/Data/Raw_Data_Prediction.RData",t.T,n,Age,K,Q.gamma_zeta,Q.gamma,Q.zeta,PI.G_Z.0,PI.G.0,PI.Z.0,y,N,m0,C0,W)

