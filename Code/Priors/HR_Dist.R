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

pdf(paste(dir,"Figures/Marginal_Y_Obs.pdf",sep=""))
p <- ggplot(Data.df, aes(Age, HRs))
p + geom_boxplot(fill="gray") + ylim(0,100)+
  xlab("Age")+
  ylab("Home runs")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))+
  scale_x_discrete(breaks = c(20,25,30,35,40,45))
dev.off()


pdf(paste(dir,"Figures/Marginal_Rate_Obs.pdf",sep=""))
p <- ggplot(Data.df, aes(Age, Rate))
p + geom_boxplot(fill="gray") + ylim(0,.3)+
  xlab("Age")+
  ylab("Probability")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))+
  scale_x_discrete(breaks = c(20,25,30,35,40,45))
dev.off()

#now load in the data 
Data_modern = read.csv(paste(dir,"Lahman/Data_modern.csv",sep=""),header=T,stringsAsFactors=T)
Data_WW2 = read.csv(paste(dir,"Lahman/Data_WW2.csv",sep=""),header=T,stringsAsFactors=T)
Data_modern = Data_modern[Data_modern$AB>=40,]
Data_WW2 = Data_WW2[Data_WW2$AB>=40,]
Data = rbind(Data_WW2, Data_modern)

years = 1950:2016
nYears = length(years)
pt95 = pt75 = pt5 = m = pt25 = rep(NA,nYears)
for(i in 1:nYears){
  HRs = Data$HR[Data$yearID == years[i]]
  pt95[i] = quantile(HRs,.95)
  pt75[i] = quantile(HRs,.75)
  pt5[i] = quantile(HRs,.5)
  m[i] = mean(HRs)
  pt25[i] = quantile(HRs,.25)
}

df = data.frame(years,pt95,pt75,pt5,m,pt25)
pdf(paste(dir,"Figures/Distribution.pdf", sep = ""))
p <- ggplot(df, aes(years))+
  geom_line(aes(years,pt95, color = "95th %") )+
  geom_line(aes(years,pt75, color = "75th %") )+
  geom_line(aes(years,pt5, color = "median") )+
  geom_line(aes(years,m, color = "mean") )+
  geom_line(aes(years,pt25, color = "25th %") )+
  geom_vline(xintercept=1990)+
  scale_colour_manual(values=c("95th %" = "purple","75th %" = "orange","median" = "blue", "mean" = "black", "25th %" = "red"))+
  ylab("Home run totals") +
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
print(p)
dev.off()

Age = unique(Ages)
df = data.frame(Age,Players = n)
pdf(paste(dir,"Figures/nPlayers_Age.pdf",sep=""))
ggplot(df, aes(Age,n)) + geom_point() + geom_line()
dev.off()

