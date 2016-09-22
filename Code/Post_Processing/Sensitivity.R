rm(list=ls())
library(ggplot2)
t.T = 19 #19 in full sample t.T = 19
#if prediction t.T = 18
t = 1
B = 0
N.MC = 1000
Age = 22 + (1:t.T)
Zeta.Mean_15 = Zeta.Mean_13 = Diff = list()

init_15 = 101
init_13 = 151

for(t in 1:t.T){
    Names = read.csv(paste("~/DGLM/Reproducibility/Init_",init_15,"/Gamma_",t,"_colnames.csv",sep=""),header=F, stringsAsFactors=F)
    zeta_15 = read.csv(paste("~/DGLM/Reproducibility/Init_",init_15,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
    zeta_13 = read.csv(paste("~/DGLM/Reproducibility/Init_",init_13,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
    Names = Names[,-1]
    zeta_15 = zeta_15[,-1]
    zeta_13 = zeta_13[,-1]
    
    zeta_15 = zeta_15[(B+1):N.MC,]
    zeta_13 = zeta_13[(B+1):N.MC,]
    
    colnames(zeta_15) = colnames(zeta_13) = Names
    Zeta.Mean_15[[t]] = apply(zeta_15,2,mean)
    Zeta.Mean_13[[t]] = apply(zeta_13,2,mean)
    Diff[[t]] = Zeta.Mean_15[[t]] - Zeta.Mean_13[[t]]
}



Zeta.Mean_15[[1]][1:5]
Zeta.Mean_13[[1]][1:5]

Z.diff = NULL
for(t in 1:t.T){
  n.t = length(Zeta.Mean_15[[t]])
  tmp1 = cbind(Diff[[t]],rep(Age[t], n.t))
  Z.diff = rbind(Z.diff,tmp1) 
}

Z.diff = data.frame(Zeta = Z.diff[,1], Age = Z.diff[,2])
names(Z.diff) = c("Zeta", "Age")
Z.diff$Age = factor(Z.diff$Age)

pdf("~/Dropbox/GT_2015/Figures/K_17_Sensitivity.pdf")
g <- ggplot(Z.diff, aes(Age, Zeta))
g + geom_boxplot(fill="gray") + ylim(-1,1)+
  xlab("Age")+
  ylab("Zeta")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()



