#First get all of the relevant players active in 2005.  
#Check the alignment to make sure that all is proper.  

rm(list=ls())
load("~/DGLM/Data/AgeAlignment2005.RData")
load("~/DGLM/Data/Raw_Data.RData")
library(ggplot2)

t.T = length(Age.Alignment)
Age = 18+(1:t.T)
K = 15
B = 0
N.MC = 1000
sigma2 = diag(W)

n = rep(NA,t.T)
for(t in 1:t.T) n[t] = length(Age.Alignment[[t]])
t.min = min(which(n>=10))
t.max = max(which(n>=10))
Age = Age[t.min:t.max]

print(Age)
t.T = length(t.min:t.max)
print(t.T)
y = N = list()
for(t in 1:(t.T+1)){
  tt = t.min + t-1
  y[[t]] = sapply(1:n[tt], function(i) Age.Alignment[[tt]][[i]]$Response$HR )
  N[[t]] = sapply(1:n[tt], function(i) Age.Alignment[[tt]][[i]]$Response$AB )
  names(y[[t]]) = names(N[[t]]) = names(Age.Alignment[[tt]])
}

n = n[t.min:t.max]

Data = read.csv("~/Dropbox/Baseball/Lahman/DLM_data.csv", header=T,stringsAsFactor=F)
#So the age alignment is correct.  

#How do I identify player ages in 2006

players2005 = Data$playerID[Data$yearID==2005 & Data$AB>40 ]
players2006 = Data$playerID[Data$yearID==2006 & Data$AB>40 ]

library(xlsx)
Jensen_118 = read.xlsx("~/Dropbox/Baseball/Lahman/Jensen_118.xlsx", sheetIndex=1)
Jensen_118 = as.character(Jensen_118[,1])
Jensen05 = intersect(players2005,Jensen_118)
Jensen06 = intersect(players2006,Jensen_118)

print(paste("Jensen Overlap 2005= ", length(Jensen05), sep=""))
print(paste("Jensen Overlap 2006= ", length(Jensen06), sep=""))


players = intersect(players2005, players2006)
players = c(players, "baldero01")

nPlayers = length(players)
Players.0506 = data.frame(players, Age.05 = rep(NA, nPlayers), HR.05 = rep(NA, nPlayers), HR.06 = rep(NA, nPlayers), AB.06 = rep(NA,nPlayers), HR.hat = rep(NA,nPlayers),HR.hat.025 = rep(NA,nPlayers) ,HR.hat.975 = rep(NA,nPlayers) )
for(i in 1:nPlayers){ 
  Players.0506[i,2] = Data$Age[Data$playerID==players[i] & Data$yearID==2005][1]
  Players.0506[i,3] = Data$HR[Data$playerID==players[i] & Data$yearID==2005][1]
  Players.0506[i,4] = Data$HR[Data$playerID==players[i] & Data$yearID==2006][1]
  Players.0506[i,5] = Data$AB[Data$playerID==players[i] & Data$yearID==2006][1]
}  


init = 111
thetaNames = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Theta_colnames.csv",sep=""), header=F, stringsAsFactors=F)
theta = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Theta.csv",sep=""), header=F, stringsAsFactors=F)
THETA = THETA_forecast = list()
print(t.T)
print(N.MC)
for(j in 1:(K+1)){ 
  THETA[[j]] = theta[theta[,1]==j,]
  THETA[[j]] = THETA[[j]][,-1]
  colnames(THETA[[j]]) = Age
  THETA_forecast[[j]] = THETA[[j]] + matrix( rnorm(N.MC*t.T, 0, sqrt(sigma2[j]) ), nrow = N.MC, ncol = t.T)
}


Player_Year_05 = AB_06 = gamma_05 = gamma_06_forecast = zeta_05 = zeta_06_forecast = Y_06 = Y_06_forecast = Y.hat = list()
N = RMSE.06 = rep(NA,t.T)

for(t in 1:t.T){
  
  Player_Year_05[[t]] = Players.0506[Players.0506[,2]==Age[t],1]
  
  if(length(Player_Year_05[[t]]) == 0) next
  AB_06[[t]] = Players.0506[Players.0506[,2]==Age[t],5]
  Y_06[[t]] = Players.0506[Players.0506[,2]==Age[t],4]
  N[t] = length(Player_Year_05[[t]])
  Y.hat[[t]] = matrix(nrow = 3, ncol = N[t])
  colnames(Y.hat[[t]]) = as.character(Player_Year_05[[t]])
  gamma_06_forecast[[t]] = zeta_06_forecast[[t]] = Y_06_forecast[[t]] = matrix(nrow = N.MC, ncol = N[t])
  Names = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Gamma_",t,"_colnames.csv",sep=""),header=F, stringsAsFactors=F)
  gamma = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Gamma_",t,".csv",sep=""), header=F, stringsAsFactors=F)
  zeta = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
    
  Names = Names[,-1]
  gamma = gamma[,-1]
  zeta = zeta[,-1]
    
  gamma = gamma[(B+1):N.MC,]
  zeta = zeta[(B+1):N.MC,]
    
  colnames(gamma) = Names
  colnames(zeta) = Names
  index = intersect(as.character(Player_Year_05[[t]]), names(gamma))
  gamma_05[[t]] = gamma[, index ]
  zeta_05[[t]] = zeta[, index ]
  gamma_05[[t]] = zeta_05[[t]] = matrix(nrow = N.MC, ncol = N[t])
  colnames(gamma_05[[t]]) = names(zeta_05[[t]]) = as.character(Player_Year_05[[t]])
  for(i in 1:N[t]){
    if(!is.element(as.character(Player_Year_05[[t]][i]),names(gamma) ) ){
      next
    }else{
      gamma_05[[t]][,i] = gamma[,as.character(Player_Year_05[[t]][i])]
      zeta_05[[t]][,i] = zeta[, as.character(Player_Year_05[[t]][i]) ]
      for(j in 1:N.MC){
        g.06 = sample(1:K, size=1, prob = Q.gamma[[t]][gamma_05[[t]][j,i],] )
        z.06 = sample(0:1, size=1, prob = Q.zeta[(zeta_05[[t]][j,i]+1),] )
        gamma_06_forecast[[t]][j,i] = g.06
        zeta_06_forecast[[t]][j,i] = z.06
        eta.06 = THETA_forecast[[g.06]][j,t] + z.06*THETA_forecast[[K+1]][j,t]
        mu.06 = 1/(1+exp(-eta.06))
        Y_06_forecast[[t]][j,i] = rbinom(1, AB_06[[t]][i], mu.06)
      }
      Y.hat[[t]][1,i] = mean(Y_06_forecast[[t]][,i])
      Y.hat[[t]][2:3,i] = quantile(Y_06_forecast[[t]][,i], c(.025, .975))
      Players.0506[Players.0506[,1]== Player_Year_05[[t]][i] ,6:8] = Y.hat[[t]][,i]
    }
  }
  RMSE.06[t] = sqrt( mean( (Y.hat[[t]][1,] - Y_06[[t]])^2, na.rm=T ) )
}

df = data.frame(Age, RMSE = RMSE.06)
pdf("~/Dropbox/GT_2015/Figures/Prediction_Age.pdf")
ggplot(df, aes(Age,RMSE)) + geom_line() + xlab("Age")+
  ylab("RMSE") +
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

RMSE = sqrt( mean( (Players.0506$HR.06 - Players.0506$HR.hat)^2, na.rm=T ) )

library(xlsx)
Jensen_118 = read.xlsx("~/Dropbox/Baseball/Lahman/Jensen_118.xlsx", sheetIndex=1)
Jensen_118 = as.character(Jensen_118[,1])
Jensen = intersect(players, Jensen_118)

print(paste("Jensen Overlap= ", length(Jensen), sep=""))

rownames(Players.0506) = Players.0506[,1]
Jensen.Players = Players.0506[Jensen,]

#gz.bonds = sample(1:(2*K), 1000, replace=T, prob = PI.G_Z.0[20,] )
gz.baldelli = sample(1:(2*K),1000,replace=T,prob = PI.G_Z.0[5,])

y.baldelli = y.bonds = rep(NA,1000)
#AB.Bonds = Jensen.Players["bondsba01",5]
AB.Baldelli = Jensen.Players["baldero01",5]
for(i in 1:1000){
  #if(gz.bonds[i]<=K){
  #  gamma.bonds = gz.bonds[i]
  #  zeta.bonds = 0
  #}else{
  #  gamma.bonds = gz.bonds[i] - K
  #  zeta.bonds = 1
  #}
  
  if(gz.baldelli[i]<=K){
    gamma.baldelli = gz.baldelli[i]
    zeta.baldelli = 0
  }else{
    gamma.baldelli = gz.baldelli[i] - K
    zeta.baldelli = 1
  }
  
  theta = rnorm((K+1),m0, sqrt(sigma2) )
  #eta.bonds = theta[gamma.bonds] + zeta.bonds*theta[K+1]
  eta.baldelli = theta[gamma.baldelli] + zeta.baldelli*theta[K+1]
  
  #mu.bonds = 1/(1+exp(-eta.bonds))
  mu.baldelli = 1/(1+exp(-eta.baldelli))
  #y.bonds[i] = rbinom(1,AB.Bonds,mu.bonds)
  y.baldelli[i] = rbinom(1,AB.Baldelli, mu.baldelli)
}

#Jensen.Players["bondsba01","HR.hat"] = mean(y.bonds)
#Jensen.Players["bondsba01","HR.hat.025"] = quantile(y.bonds,prob=.025)
#Jensen.Players["bondsba01","HR.hat.975"] = quantile(y.bonds,prob=.9755)

Jensen.Players["baldero01","HR.hat"] = mean(y.baldelli)
Jensen.Players["baldero01","HR.hat.025"] = quantile(y.baldelli,prob=.025)
Jensen.Players["baldero01","HR.hat.975"] = quantile(y.baldelli,prob=.9755)

RMSE.Jensen = sqrt( mean( (Jensen.Players$HR.06 - Jensen.Players$HR.hat)^2 ) )
RMSE.strawman = sqrt( mean( (Jensen.Players$HR.06 - Jensen.Players$HR.05)^2, na.rm =T ) )
LAD.Jensen = mean(abs(Jensen.Players$HR.06 - Jensen.Players$HR.hat) )

plot( Jensen.Players[,"HR.hat"] - Jensen.Players[,"HR.06"] )
strawman = sqrt( mean( (Players.0506$HR.06 - Players.0506$HR.05)^2, na.rm =T ) )
library(xtable)
xtable( data.frame(Full_Sample = c(5.3, RMSE), Top = c(7.3, RMSE.Jensen)) )
