rm(list=ls())
library(MASS)
library(parallel)
library(BayesLogit)
load("~/DGLM/Data/Raw_Data.RData")
source("~/DGLM/Code/Theta_FFBS_Collapsed.R")
source("~/DGLM/Code/Gamma_Zeta_FFBS_Collapsed.R")
source("~/DGLM/Code/Omega_Step_Collapsed.R")
source("~/DGLM/Code/F_Construct.R")
source("~/DGLM/Code/F_kzt.R")
source("~/DGLM/Code/Z_Construct.R")
source("~/DGLM/Code/Zeta_Initialize.R")
source("~/DGLM/Code/Gamma_Initialize.R")
source("~/DGLM/Code/Theta_Initialize.R")
source("~/DGLM/Code/Collapsed_MCMC_Production.R")
#The goal here is to determine if conditioning on the true values of Theta and Eta
#is it possible for me to recover the true Gamma values in a reproducible way
Kappa = list()
for(t in 1:t.T) Kappa[[t]] = y[[t]] - .5*N[[t]]


Post.Mean.Theta = Post.Err.Theta = list()
Post.Mean.Gamma = Post.Err.Gamma = list()

#for 10 players
#B = 3e5
#Thin.Rate = 5
#nSamples = 1e5

#for 100 players
B =5e4
Thin.Rate=10
nSamples = 5e4

#for 500 players
#B = 2.5e4
#Thin.Rate = 5
#nSamples = 1e4

#for 1000 players
#B = 2.5e4
#Thin.Rate = 5
#nSamples = 1e4


N.MC = B+Thin.Rate*nSamples
sigma2.init = rep( seq(from = .01, to = .1, length = 3), each = 2)

mclapply(1:6, function(init) MCMC.collapsed(init,nSamples,N.MC,Thin.Rate,m0,C0,sigma2.theta = sigma2.init[init],t.T,n,K,PI.G_Z.0,Q.gamma_zeta,N,y), mc.cores = 6 )



