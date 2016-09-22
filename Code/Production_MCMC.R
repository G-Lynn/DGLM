rm(list=ls())
library(MASS)
library(parallel)
library(BayesLogit)
library(Rcpp)
#load("~/DGLM/Data/Raw_Data_Prediction.RData")
load("~/DGLM/Data/Raw_Data.RData")
source("~/DGLM/Code/Theta_FFBS_Collapsed.R")
source("~/DGLM/Code/Gamma_Zeta_FFBS_Collapsed.R")
#source("~/DGLM/Code/Omega_Step_Collapsed.R")
source("~/DGLM/Code/Omega_Step_Collapsed_Cpp.R")
sourceCpp("~/DGLM/Code/rpgApprox.cpp")
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

B = 1000 #1000 #20000
Thin.Rate = 1 #10
nSamples = 1000 #1000 #10000
INIT_base = 110
inits = INIT_base + 1 
print(inits)

N.MC = B+Thin.Rate*nSamples
nCores = 8
sigma2.init = 2*diag(W)
ptm = proc.time()[3]
mclapply(inits, function(init) MCMC.collapsed(init,nSamples,N.MC,Thin.Rate,m0,C0,W,sigma2.theta = sigma2.init,t.T,n,K,PI.G_Z.0,Q.gamma_zeta,N,y), mc.cores = nCores )
proc.time()[3]-ptm


