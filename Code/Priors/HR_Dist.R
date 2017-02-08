Mu.obs = list()
ABs = HRs = NULL
Ages = NULL
for(t in 1:n.T){
  nn = length(Age.Alignment[[t]])
  Ages = c(Ages,rep(Age[t], nn ) )
  for(i in 1:length(Age.Alignment[[t]]) ){
    HRs = c(HRs,as.numeric(Age.Alignment[[t]][[i]][[2]][2])  )
    ABs = c(ABs, as.numeric(Age.Alignment[[t]][[i]][[2]][1]) )
  }
}

Data.df = data.frame(ABs, HRs, Rate = HRs/ABs, Age = factor(Ages) )

pdf("~/sDGLM-master/Figures/Marginal_Y_Obs.pdf")
p <- ggplot(Data.df, aes(Age, HRs))
p + geom_boxplot(fill="gray") + ylim(0,100)+
  xlab("Age")+
  ylab("Home runs")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


pdf("~/sDGLM-master/Figures/Marginal_Rate_Obs.pdf")
p <- ggplot(Data.df, aes(Age, Rate))
p + geom_boxplot(fill="gray") + ylim(0,.3)+
  xlab("Age")+
  ylab("Probability")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()
  






