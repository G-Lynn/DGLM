rm(list=ls())
library(MASS)
library(parallel)
library(BayesLogit)
load("~/Dropbox/Surya_Group/DGLM/SimStudy/p1000/DGP1/DGP1.RData")
source("~/DGLM/Code/Theta_FFBS_Collapsed.R")
source("~/DGLM/Code/Gamma_Zeta_FFBS_Collapsed.R")
source("~/DGLM/Code/Omega_Step_Collapsed.R")
source("~/DGLM/Code/F_Construct.R")
source("~/DGLM/Code/F_kzt.R")
source("~/DGLM/Code/Z_Construct.R")
source("~/DGLM/Code/Zeta_Initialize.R")
source("~/DGLM/Code/Gamma_Initialize.R")
source("~/DGLM/Code/Theta_Initialize.R")
source("~/DGLM/Code/Collapsed_MCMC.R")
#The goal here is to determine if conditioning on the true values of Theta and Eta
#is it possible for me to recover the true Gamma values in a reproducible way

nPlayers = 1000
n = rep(nPlayers, t.T)
Theta.Truth = Theta
Gamma.Truth = Gamma
Zeta.Truth = Zeta
Eta.Truth = eta.1
Eta = eta.1
rm(Theta, Gamma, eta.1)


K = ceiling(log(nPlayers))
#PI.0 = seq(from = .001, to = .08, len = K)
#m0 = log( PI.0 / (1-PI.0 ) )
m0 = -5+(0:(K-1))/(K-1)*2.5

#the m0 specification is a big problem I think.  
# needs to be bunched up to provide more granularity closer to -2.5
m0.PED = .4

m0 = matrix(c(m0,m0.PED),ncol =1)
beta = (2/4)
#beta = 1/4 worked well.  

delta.m = m0[2]-m0[1]
sigma2 = -delta.m^2/(8*log(beta))


C0 = sigma2*diag(K+1)
C0[K+1,K+1] = .02

Kappa = list()
for(t in 1:t.T) Kappa[[t]] = y.1[[t]] - .5*N[[t]]


Post.Mean.Theta = Post.Err.Theta = list()
Post.Mean.Gamma = Post.Err.Gamma = list()

#for 10 players
#B = 3e5
#Thin.Rate = 5
#nSamples = 1e5

#for 100 players
#B =2.5e4
#Thin.Rate=1
#nSamples = 2.5e4

#for 500 players
#B = 2.5e4
#Thin.Rate = 5
#nSamples = 1e4

#for 1000 players
B = 2.5e4
Thin.Rate = 5
nSamples = 1e4



N.MC = B+Thin.Rate*nSamples
sigma2.init = rep( seq(from = .01, to = sigma2, length = 3), each = 2)
y = y.1

mclapply(1:6, function(init) MCMC.collapsed(init,nSamples,N.MC,Thin.Rate,m0,C0,sigma2.theta = sigma2.init[init],t.T,n,K,PI.G_Z.0,Q.gamma_zeta,N,y,V,Theta.Truth,Gamma.Truth,Zeta.Truth), mc.cores = 6 )



