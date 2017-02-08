rm(list=ls())
library(MASS)
library(parallel)
library(BayesLogit)
library(Rcpp)
dir = "~/sDGLM-master/"

load(paste(dir,"Data/MCMC_Data.RData",sep=""))
source(paste(dir,"Code/Theta_FFBS_Collapsed.R",sep=""))
source(paste(dir,"Code/Gamma_Zeta_FFBS_Collapsed.R",sep=""))
#source(paste(dir,"Code/Omega_Step_Collapsed.R",sep=""))
source(paste(dir,"Code/Omega_Step_Collapsed_Cpp.R",sep=""))
sourceCpp(paste(dir,"Code/rpgApprox.cpp",sep=""))
source(paste(dir,"Code/F_Construct.R",sep=""))
source(paste(dir,"Code/F_kzt.R",sep=""))
source(paste(dir,"Code/Z_Construct.R",sep=""))
source(paste(dir,"Code/Zeta_Initialize.R",sep=""))
source(paste(dir,"Code/Gamma_Initialize.R",sep=""))
source(paste(dir,"Code/Theta_Initialize.R",sep=""))
source(paste(dir,"Code/MCMC_function.R",sep=""))

Kappa = list()
for(t in 1:t.T) Kappa[[t]] = y[[t]] - .5*N[[t]]


Post.Mean.Theta = Post.Err.Theta = list()
Post.Mean.Gamma = Post.Err.Gamma = list()

B = 10 #1000 #20000
Thin.Rate = 1 #10
nSamples = 10 #1000 #10000
INIT_base = 0
inits = INIT_base + 1 
print(inits)

N.MC = B+Thin.Rate*nSamples
nCores = 1
sigma2.init = 2*diag(W)
ptm = proc.time()[3]
mclapply(inits, function(init) MCMC.collapsed(init,dir,nSamples,N.MC,Thin.Rate,m0,C0,W,sigma2.theta = sigma2.init,t.T,n,K,PI.G_Z.0,Q.gamma_zeta,N,y), mc.cores = nCores )
proc.time()[3]-ptm


