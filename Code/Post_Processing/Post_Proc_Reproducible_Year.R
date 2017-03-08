rm(list=ls())
library(ggplot2)
library(reshape2)
dir = "~/sDGLM-master/"
t.T = 21 #number of years from 20:40
#if prediction t.T = 18
t = 1
K = 15
p = K+1
B = 0
N.MC = 1000
INIT_base = 0
nSims = 6
Age = 20 + (0:(t.T-1))
players = c(
  "pujolal01",
  "sosasa01",
  "bondsba01",
  "ortizda01",
  "jeterde01",
  "suzukic01",
  "griffke02",
  "mcgwima01",
  "thomeji01",
  "thomafr04", 
  "rodrial01", 
  "boonebr01", 
  "seguida01",
  "anderbr01",
  "ramirma02",
  "tejadmi01",
  "braunry02",
  "caminke01"
  )

nPlayers = length(players)

Theta.init = list()
Gamma.init = list()
Zeta.init = list()
Gamma_Players = Zeta_Players = Zeta_Players_13 = Zeta_Players_17 = Eta_Players = list()
for(i in 1:nPlayers) Gamma_Players[[i]] = Zeta_Players[[i]] = Zeta_Players_13[[i]] = Zeta_Players_17[[i]] = Eta_Players[[i]] = matrix(nrow=N.MC, ncol=t.T)


for(jj in 1:nSims){
  init = jj+INIT_base
  thetaNames = read.csv(paste(dir,"Reproducibility/Init_",init,"/Theta_colnames.csv",sep=""), header=F, stringsAsFactors=F)
  theta = read.csv(paste(dir,"Reproducibility/Init_",init,"/Theta.csv",sep=""), header=F, stringsAsFactors=F)
  Theta.Mean = matrix(nrow = p, ncol = t.T)
  Theta.CI = list()
  Mu.CI = list()
  print(jj) 
  for(j in 1:p){
    theta_j = theta[theta[,1]==j,]
    print(dim(theta_j))
    theta_j = theta_j[,-1]
    theta_j = theta_j[(B+1):N.MC,]
    colnames(theta_j) = thetaNames[,2]
    mu_j = 1/(1+exp(-theta_j))
    Theta.Mean[j,] = apply(theta_j,2,mean)
  
    Theta.CI[[j]] = Mu.CI[[j]] = matrix(nrow = 3, ncol = t.T)
    Theta.CI[[j]][2,] = Theta.Mean[j,]
    Theta.CI[[j]][1,] = apply(theta_j,2,quantile,prob=.025)
    Theta.CI[[j]][3,] = apply(theta_j,2,quantile,prob=.975)
    Mu.CI[[j]][2,] = apply(mu_j,2,mean)
    Mu.CI[[j]][1,] = apply(mu_j,2,quantile,prob=.025)
    Mu.CI[[j]][3,] = apply(mu_j,2,quantile,prob=.975)
    rm(theta_j,mu_j)
  }
  

  Gamma.Mean = Zeta.Mean = Zeta.Mean_13 = Zeta.Mean_17 = list()
  Gamma.Prob = matrix(nrow = K, ncol = t.T)
  Zeta.Prob = matrix(nrow = N.MC, ncol = t.T)
  
  for(t in 1:t.T){
    
    Names = read.csv(paste(dir,"Reproducibility/Init_",init,"/Gamma_",t,"_colnames.csv",sep=""),header=F, stringsAsFactors=F)
    gamma = read.csv(paste(dir,"Reproducibility/Init_",init,"/Gamma_",t,".csv",sep=""), header=F, stringsAsFactors=F)
    zeta = read.csv(paste(dir,"Reproducibility/Init_",init,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
    
    if(jj==1){
      zeta_13 = read.csv(paste(dir,"Reproducibility/Init_",141,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
      zeta_17 = read.csv(paste(dir,"Reproducibility/Init_",151,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
    }
      
    Names = Names[,-1]
    gamma = gamma[,-1]
    zeta = zeta[,-1]

  
    gamma = gamma[(B+1):N.MC,]
    zeta = zeta[(B+1):N.MC,]
    
    if(jj==1){    
      zeta_13 = zeta_13[,-1]
      zeta_17 = zeta_17[,-1]
      zeta_13 = zeta_13[(B+1):N.MC,]
      zeta_17 = zeta_17[(B+1):N.MC,]
      colnames(zeta_13) = colnames(zeta_17) = Names
    }
  
    colnames(gamma) = Names
    colnames(zeta) = Names
  
    gamma_Agg = c(as.matrix(gamma))
    zeta_Agg = c(as.matrix(zeta))
    Zeta.Prob[,t] = apply(zeta,1,mean)
    for(k in 1:K) Gamma.Prob[k,t] = sum(gamma_Agg==k)/length(gamma_Agg)
    
    #Compute the mean
    Gamma.Mean[[t]] = matrix(nrow = 3, ncol = dim(gamma)[2])
    colnames(Gamma.Mean[[t]]) = Names
  
    Gamma.Mean[[t]][2,] = apply(gamma,2,mean)
    Gamma.Mean[[t]][1,] = apply(gamma,2,quantile,prob=.025)
    Gamma.Mean[[t]][3,] = apply(gamma,2,quantile,prob=.975)
  
    Zeta.Mean[[t]] = apply(zeta,2,mean)
    if(jj==1){
      Zeta.Mean_13[[t]] = apply(zeta_13,2,mean)
      Zeta.Mean_17[[t]] = apply(zeta_17,2,mean)
    }
  
      theta.t = matrix(nrow = N.MC, ncol = p)
      for(j in 1:p) theta.t[,j] = tmp = theta[theta[,1]==j,t+1]
                                        
      for(ii in 1:nPlayers){
        if(!is.element(players[[ii]],colnames(gamma))) next
        Gamma_Players[[ii]][,t] = gamma[,players[ii]]
        Zeta_Players[[ii]][,t] =  zeta[,players[ii]]
        
        if(jj==1){
          Zeta_Players_13[[ii]][,t] = zeta_13[, players[[ii]] ]
          Zeta_Players_17[[ii]][,t] = zeta_17[, players[[ii]] ]
        }
        
        Eta_Players[[ii]][,t] = sapply(1:N.MC, function(s) theta.t[s,Gamma_Players[[ii]][s,t]] + theta.t[s,p]*Zeta_Players[[ii]][s,t])
    }
  }
  
  Theta.init[[jj]] = Theta.Mean 
  Gamma.init[[jj]] = Gamma.Mean
  Zeta.init[[jj]] = Zeta.Mean
}

print(Theta.init[[1]])
ThetaMax = matrix(nrow = p, ncol = t.T)
Theta.max = list()
for(j in 1:p){
  Theta.max[[j]] = matrix(nrow = (nSims-1), ncol = t.T)
  for(i in 2:nSims){
    Theta.max[[j]][(i-1),] = abs( Theta.init[[i]][j,] - Theta.init[[1]][j,])
  }
  ThetaMax[j,] = apply(Theta.max[[j]],2,max)
}


colnames(ThetaMax) = as.character(Age)

sa <- stack(as.data.frame(ThetaMax) )
names(sa) = c("Theta", "Age")
pdf(paste(dir,"Figures/Theta_max.pdf",sep=""))
g <- ggplot(sa, aes(Age, Theta))
g + geom_boxplot(fill="gray") + ylim(0,1)+
  xlab("Age")+
  ylab("Theta")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

Zeta.max = Gamma.max = list()
for(t in 1:t.T){
  n = length(Zeta.init[[1]][[t]])
  Zeta.diff = matrix(nrow = n , ncol = 4)
  Gamma.diff = matrix(nrow = n, ncol = 4)
  for(i in 1:n){
    for(j in 2:5){
      Zeta.diff[i,(j-1)] = abs( Zeta.init[[1]][[t]][i] - Zeta.init[[j]][[t]][i] )
      Gamma.diff[i,(j-1)] = abs( Gamma.init[[1]][[t]][i] - Gamma.init[[j]][[t]][i] )
    }
  }
  Zeta.max[[t]] = apply(Zeta.diff,1,max)
  Gamma.max[[t]] = apply(Gamma.diff,1,max)
}

Z.max = G.max = NULL
for(t in 1:t.T){
  n.t = length(Zeta.max[[t]])
  tmp1 = cbind(Zeta.max[[t]],rep(Age[t], n.t))
  Z.max = rbind(Z.max,tmp1)
  
  tmp2 = cbind(Gamma.max[[t]], rep(Age[t], n.t))
  G.max = rbind(G.max,tmp2)
}

Z.max = as.data.frame(Z.max)
G.max = as.data.frame(G.max)
names(Z.max) = c("Zeta", "Age")
names(G.max) = c("Gamma", "Age")
Z.max$Age = factor(Z.max$Age)
G.max$Age = factor(G.max$Age)

pdf(paste(dir,"Figures/Zeta_max.pdf",sep=""))
g <- ggplot(Z.max, aes(Age, Zeta))
g + geom_boxplot(fill="gray") + ylim(0,1)+
  xlab("Age")+
  ylab("Zeta")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

pdf(paste(dir,"Figures/Gamma_max.pdf",sep=""))
g <- ggplot(G.max, aes(Age, Gamma))
g + geom_boxplot(fill="gray") + ylim(0,K)+
  xlab("Age")+
  ylab("Gamma")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


POI_Zeta = POI_Gamma = POI_Gamma_pt025 = POI_Gamma_pt975 = matrix(nrow = nPlayers, ncol = t.T)
rownames(POI_Zeta) = rownames(POI_Gamma) = players

for(t in 1:t.T){
  POI_Zeta[,t] = Zeta.init[[1]][[t]][players]
  POI_Gamma[,t] = Gamma.init[[1]][[t]][2,][players]
  POI_Gamma_pt025[,t] = Gamma.init[[1]][[t]][1,][players]
  POI_Gamma_pt975[,t] = Gamma.init[[1]][[t]][3,][players]
}

#load Data
Data = read.csv(paste(dir,"Lahman/Data_modern.csv",sep=""),header=T,stringsAsFactors=F)
for(i in 1:nPlayers){
  Age_i = unique(Data[Data$playerID == players[i] & Data$Age>20 & Data$Age<=40 ,c("Age")])
  Year_i = unique(Data[Data$playerID == players[i] & Data$Age>20 & Data$Age<=40,c("yearID")])
  Year = rep(NA,t.T)
  Year[is.element(Age,Age_i)] = Year_i 
  
  tmp = data.frame(Gamma = POI_Gamma[i,], Year = Year, pt025 = POI_Gamma_pt025[i,], pt975 = POI_Gamma_pt975[i,])
  pdf(paste(dir,"Figures/Gamma_",players[i],".pdf",sep="") )
  g = ggplot(data = tmp, aes(x=Year,y=Gamma,size=1.5 ))+
    geom_point(na.rm=T)+
    geom_line(aes(Year,pt025), linetype=2, size = .5)+
    geom_line(aes(Year,pt975), linetype=2, size = .5)+
    xlab("Age")+
    ylab("Gamma")+
    ylim(1,K)+
    theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"),legend.position = "none")
  print(g)
  dev.off()
  
  tmp13 = apply(Zeta_Players_13[[i]],2,mean)
  tmp17 = apply(Zeta_Players_17[[i]],2,mean)
  KK = rep(c(15,13,17), each = t.T)
  tmp = data.frame(Zeta = c(POI_Zeta[i,], tmp13, tmp17), Year = rep(Year,times=3), K = factor(KK) ) 
  pdf(paste(dir,"Figures/Zeta_",players[i],".pdf",sep="") )
  g=ggplot(data = tmp, aes(x=Year,y=Zeta, colour = K ))+
    geom_point(size = 3, na.rm=T)+
    geom_line()+
    xlab("Age")+
    ylab("P[zeta=1]")+
    ylim(0,1)+
    theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"))
  print(g)
  dev.off()
  
}

for(i in 1:nPlayers){
player = players[i]
Mu = 1/(1+exp(-Eta_Players[[i]]))
Mean = apply(Mu, 2, mean)
Quant = apply(Mu,2, quantile, c(.025, .975), na.rm = T)

df = data.frame(Age, Mean, pt025 = Quant[1,], pt975 = Quant[2,])
pdf(paste(dir,"Figures/Ability_Curve_",player, ".pdf", sep="") )
p<-ggplot(data = df, aes(Age,Mean))  + geom_line(size = 2) + geom_line(aes(Age,pt025), linetype=2) + geom_line(aes(Age,pt975), linetype=2) + ylim(0,.15) + ylab("Probability") + theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"))
print(p)
dev.off()
}

#Need posterior summaries for Theta_t 
#what I want here are plot of the mean for Theta.CI over time for each Theta
#Then with a specific Theta, I will plot the uncertainty bands. 

#Need posterior summaries for Gamma_t

df = melt(t(Gamma.Prob))
names(df) = c("Age", "Class", "Probability")
df$Class = factor(df$Class)
df$Age = Age
pdf(paste(dir,"Figures/Posterior_Prob_Gamma.pdf",sep=""))
ggplot(data=df, aes(x=Age, y=Probability, colour=Class) ) +
  geom_line(size = 2)+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

#Need posterior summaries for marginal Zeta_t
lw.1 = apply(Zeta.Prob,2,quantile,.025)
up.1 = apply(Zeta.Prob,2,quantile,.975)
m.1 = apply(Zeta.Prob,2,mean)

df = data.frame(Age = Age, m.1, up.1, lw.1)
pdf(paste(dir,"Figures/Posterior_Prob_Zeta.pdf",sep=""))
p1 <- ggplot(df, aes(Age, m.1))+
  geom_point(color = "blue")+
  geom_line(data=df, color = "blue")+
  geom_ribbon(data=df,aes(ymin=lw.1,ymax=up.1),alpha=0.3, color = "blue", fill = "blue")+ 
  ylab("Probability")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
print(p1)   
dev.off()

z = NULL
z.Age = NULL
for(t in 1:t.T){ 
  z = c(z,Zeta.init[[1]][[t]])
  z.Age = c(z.Age, rep(Age[t], length(Zeta.init[[1]][[t]])))
}

df = data.frame(Zeta = z, Age = as.factor(z.Age) )
pdf(paste(dir,"Figures/Zeta_Age_Box.pdf",sep=""))
g <- ggplot(df, aes(Age, Zeta))
g + geom_boxplot(fill="gray") + ylim(0,1)+ 
  xlab("Age")+
 # geom_jitter(width = 0.05, height= 0.1)+
  ylab("E[Zeta_i | y]")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()
#Last thing to do: Dynamics of Theta

Theta.Dynamic = Theta.Dynamic.025 = Theta.Dynamic.975 = matrix(nrow = K, ncol = t.T)
Mu.Dynamic = Mu.Dynamic.025 = Mu.Dynamic.975 = matrix(nrow =K, ncol = t.T)
for(k in 1:K){
  Theta.Dynamic[k,] = Theta.CI[[k]][2,]
  Theta.Dynamic.025[k,] = Theta.CI[[k]][1,]
  Theta.Dynamic.975[k,] = Theta.CI[[k]][3,]
  
  Mu.Dynamic[k,] = Mu.CI[[k]][2,]
  Mu.Dynamic.025[k,] = Mu.CI[[k]][1,]
  Mu.Dynamic.975[k,] = Mu.CI[[k]][3,]
}

df = melt(t(Theta.Dynamic))
df.025 = melt(t(Theta.Dynamic.025))
df.975 = melt(t(Theta.Dynamic.975))

names(df) = names(df.025) = names(df.975) = c("Age", "Class", "Theta_k")
df$Class = df.025$Class = df.975$Class = factor(df$Class)
df$Age = df.025$Age = df.975$Age = Age
pdf(paste(dir,"Figures/Posterior_Theta_k.pdf",sep=""))
ggplot(data=df, aes(x=Age, y=Theta_k, colour=Class) ) +
  geom_line(size = 2)+
  geom_line(data = df.025, aes(x=Age,y=Theta_k, colour=Class), linetype=2)+
  geom_line(data = df.975, aes(x=Age,y=Theta_k, colour=Class), linetype=2)+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

df = melt(t(Mu.Dynamic))
df.025 = melt(t(Mu.Dynamic.025))
df.975 = melt(t(Mu.Dynamic.975))

names(df) = names(df.025) = names(df.975) = c("Age", "Class", "Mu_k")
df$Class = df.025$Class = df.975$Class = factor(df$Class)
df$Age = df.025$Age = df.975$Age = Age
pdf(paste(dir,"Figures/Posterior_Mu_k.pdf",sep=""))
ggplot(data=df, aes(x=Age, y=Mu_k, colour=Class) ) +
  geom_line(size = 2)+
  geom_line(data = df.025, aes(x=Age,y=Mu_k, colour=Class), linetype=2)+
  geom_line(data = df.975, aes(x=Age,y=Mu_k, colour=Class), linetype=2)+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

pdf(paste(dir,"Figures/AP_Effect_Log.pdf",sep=""))
df = data.frame(Age = Age, t(Theta.CI[[K+1]]))
ggplot(data = df, aes(x=Age, y = X2))+
  geom_line(size=2)+
  geom_line(aes(x=Age,y=X1), linetype=2)+
  geom_line(aes(x=Age,y=X3), linetype=2)+
  ylab("Increase to log odds")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

tmp = Theta.CI[[K+1]]-2.94
benchmark = rep(-2.94,t.T)

AP.Effect = 1/(1+exp(-tmp)) - 1/(1+exp(-benchmark))
df = data.frame(Age = Age, t(AP.Effect))

pdf(paste(dir,"Figures/AP_Effect_Probability.pdf",sep=""))
ggplot(data = df, aes(x=Age, y = X2))+
  geom_line(size=2)+
  geom_line(aes(x=Age,y=X1), linetype=2)+
  geom_line(aes(x=Age,y=X3), linetype=2)+
  ylab("Probability")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

########################################
## Generate proportion by year
Zeta_Year = list()
Year_Index = 1990:2016
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


##########################################
#Generate list
init = INIT_base + 1
Table_strong = list()
Table_moderate = list()
Table_weak = list()
Table_spec = list()
for(t in 1:t.T){
  Names = read.csv(paste(dir,"Reproducibility/Init_",init,"/Gamma_",t,"_colnames.csv",sep=""),header=F, stringsAsFactors=F)
  zeta = read.csv(paste(dir,"Reproducibility/Init_",init,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
  
  
  Names = Names[,-1]
  zeta = zeta[,-1]
  zeta = zeta[(B+1):N.MC,]
  zeta_mean = apply(zeta,2,mean)
  if(sum(zeta_mean>=.95)>0) Table_strong[[t]] = Names[zeta_mean>=.95]
  if(sum(zeta_mean<.95 & zeta_mean>=.85)) Table_moderate[[t]] = Names[zeta_mean<.95 & zeta_mean>=.85]
  if(sum(zeta_mean<.85 & zeta_mean>=.65)) Table_weak[[t]] = Names[zeta_mean<.85 & zeta_mean>=.65]
  if(sum(zeta_mean<.65 & zeta_mean>=.4)) Table_spec[[t]] = Names[zeta_mean<.65 & zeta_mean>=.4] 
} 



