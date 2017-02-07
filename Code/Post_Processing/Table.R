rm(list=ls())
library(ggplot2)
library(reshape2)
t.T = 20 #19 in full sample t.T = 19
#if prediction t.T = 18
t = 1
K = 15
p = K+1
B = 0
N.MC = 1000
INIT_base = 100
nSims = 8
Age = 20 + (1:t.T)

init = INIT_base + 1
Table_strong = list()
Table_moderate = list()
Table_weak = list()
Table_spec = list()
for(t in 1:t.T){
    Names = read.csv(paste("~/sDGLM/Reproducibility/Init_",init,"/Gamma_",t,"_colnames.csv",sep=""),header=F, stringsAsFactors=F)
    zeta = read.csv(paste("~/sDGLM/Reproducibility/Init_",init,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
    
    
    Names = Names[,-1]
    zeta = zeta[,-1]
    zeta = zeta[(B+1):N.MC,]
    zeta_mean = apply(zeta,2,mean)
    if(sum(zeta_mean>=.95)>0) Table_strong[[t]] = Names[zeta_mean>=.95]
    if(sum(zeta_mean<.95 & zeta_mean>=.85)) Table_moderate[[t]] = Names[zeta_mean<.95 & zeta_mean>=.85]
    if(sum(zeta_mean<.85 & zeta_mean>=.65)) Table_weak[[t]] = Names[zeta_mean<.85 & zeta_mean>=.65]
    if(sum(zeta_mean<.65 & zeta_mean>=.4)) Table_spec[[t]] = Names[zeta_mean<.65 & zeta_mean>=.4] 
} 
   



