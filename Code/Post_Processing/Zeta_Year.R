rm(list = ls())
dir = "~/sDGLM-master/"
t.T = 21 # 21 years in 20:40
t = 1
K = 15
B = 0
N.MC = 1000
INIT_base = 0
nSims = 8
Age = 20 + (0:(t.T-1))
init = 1 + INIT_base 

Data = read.csv(paste(dir,"Lahman/DLM_data.csv",sep=""))
Zeta_Year = list()
Year_Index = 1990:2013
nYears = length(Year_Index)
#initialize the first element to zero and then remove.  necessary for concatenating vectors.  
for(i in 1:nYears){
  Zeta_Year[[i]] = 0
}

# Age threshold.  The idea is that we know the least about the youngest players.  
summary(Data$yearID[is.element(Data$playerID,Cohort)])
for(t in 1:t.T){
  Names = read.csv(paste(dir,"Reproducibility/Init_",init,"/Gamma_",t,"_colnames.csv",sep=""),header=F, stringsAsFactors=F)
  zeta = read.csv(paste(dir,"Reproducibility/Init_",init,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
  
  Names = Names[,-1]
  zeta = zeta[,-1]
  
  zeta = zeta[(B+1):N.MC,]
  colnames(zeta) = Names
  
  nPlayers = length(Names)
  Years = rep(NA,nPlayers)
  for(i in 1:nPlayers){
    Years[i] = Data$yearID[Data$playerID == as.character(Names[i]) & Data$Age == Age[t]][1]
  }
  
  for(i in 1:nPlayers){
    Zeta_Year[[ which(Years[i]==Year_Index) ]] = c(Zeta_Year[[which(Years[i]==Year_Index)]], zeta[,i])
  }
}

Zeta_Prob = matrix(nrow = 3, ncol = nYears)
Pop_Prob.MC = matrix(nrow = N.MC, ncol = nYears)
nCohort = rep(NA,nYears)
for(i in 1:nYears){
  #Now remove the first zero in every list element.
  Zeta_Year[[i]] = Zeta_Year[[i]][-1]
  nPlayers = length(Zeta_Year[[i]])/N.MC
  nCohort[i] = nPlayers
  index = rep(1:N.MC, times = nPlayers)
  for(ii in 1:N.MC){
    Pop_Prob.MC[ii,i] = mean(Zeta_Year[[i]][ index == ii ])
  }
  
}

#Pop_Prob.MC = Pop_Prob.MC - Pop_Prob.MC[,1]
m.1 = apply(Pop_Prob.MC,2,mean, na.rm = T)
up.1 = apply(Pop_Prob.MC,2,quantile,.975, na.rm = T)
lw.1 = apply(Pop_Prob.MC,2,quantile,.025, na.rm = T)

traj.1 = Pop_Prob.MC[sample(size = 1, 1:N.MC),]
traj.2 = Pop_Prob.MC[sample(size = 1, 1:N.MC),]

df = data.frame(Year = Year_Index,  m.1 = m.1, up.1 = up.1, lw.1 = lw.1, traj.1 = traj.1, traj.2 = traj.2 )
png(paste(dir,"Figures/Population_Pct_Year.png",sep=""))
p1 <- ggplot(df, aes(Year, m.1))+
  geom_point(color = "blue")+
  geom_line(data=df, color = "blue")+
  geom_ribbon(data=df,aes(ymin=lw.1,ymax=up.1),alpha=0.3, color = "blue", fill = "blue")+ 
  ylab("Population %")+
  #ylim(2.5,10)+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
print(p1)   
dev.off()
