rm(list=ls())
dir = "~/sDGLM-master/"
load(paste(dir,"Data/AgeAlignment_modern.RData",sep=""))
n.T = length(Age.Alignment)

Mu.obs = list()
ABs = HRs = NULL
Ages = NULL
n = rep(NA,n.T)
for(t in 1:n.T){
  n[t] = length(Age.Alignment[[t]])
  Ages = c(Ages,rep(Age.Alignment[[t]][[1]]$Age, n[t] ) )
  for(i in 1:n[t]){
    HRs = c(HRs,as.numeric(Age.Alignment[[t]][[i]]$Response$HR)  )
    ABs = c(ABs, as.numeric(Age.Alignment[[t]][[i]]$Response$AB) )
  }
}

Data.df = data.frame(ABs, HRs, Rate = HRs/ABs, Age = factor(Ages) )

pdf("~/sDGLM-master/Figures/Marginal_Y_Obs.pdf")
p <- ggplot(Data.df, aes(Age, HRs))
p + geom_boxplot(fill="gray") + ylim(0,100)+
  xlab("Age")+
  ylab("Home runs")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))+
  scale_x_discrete(breaks = c(20,25,30,35,40,45))
dev.off()


pdf("~/sDGLM-master/Figures/Marginal_Rate_Obs.pdf")
p <- ggplot(Data.df, aes(Age, Rate))
p + geom_boxplot(fill="gray") + ylim(0,.3)+
  xlab("Age")+
  ylab("Probability")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))+
  scale_x_discrete(breaks = c(20,25,30,35,40,45))
dev.off()


  






