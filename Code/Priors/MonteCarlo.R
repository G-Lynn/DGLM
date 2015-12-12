# EDA for Prior. 
rm(list=ls())
load("~/DGLM/Data/AgeAlignment.RData")
library(MASS)
library(ggplot2)
library(reshape2)
#source("~/Dropbox/Baseball/Particle Filter/PF_Mixture/PF_Functions.R")
source("~/DGLM/Code/Q_Age.R")
source("~/DGLM/Code/Q_PED.R")
n.T = length(Age.Alignment)
n = rep(NA,n.T)
nSamples = 10000

AB = list()
for(t in 1:n.T){
  AB[[t]] = rep(NA, length(Age.Alignment[[t]]) )
  for(i in 1:length(Age.Alignment[[t]]) ){
    AB[[t]][i] = as.numeric( Age.Alignment[[t]][[i]][[2]][1] )
  }
}

ABs = unlist(AB)

for(t in 1:n.T) n[t] = length(Age.Alignment[[t]])
Age = 17:(17+n.T-1)

Age.names = as.character(Age)
Players.df = data.frame(Players = n, Age = as.factor(Age.names))

pdf("~/Dropbox/GT_2015/Figures/nPlayers_Age.pdf")
ggplot(data = Players.df, aes(x=Age,y=Players)) + geom_bar(stat="identity") + xlab("Age") + ylab("# Players") + theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


## Priors for individual ability class levels
K = 5
#PI.0 = seq(from = .015, to = .065, len = K)
PI.0 = seq(from = .005, to = .065, len = K)
PI.0 = c(PI.0)
m0 = log( PI.0 / (1-PI.0 ) )

beta = 1/3
#7.5 seems to work well
sigma2 = rep(NA,K)
for(k in 1:K){
  delta.m = (m0[k+1]-m0[k])
  if(k==K) delta.m =(m0[k]-m0[k-1])
    sigma2[k] = -delta.m^2/(8*log(beta))
}


eta = seq(from = -6, to = -1, length = nSamples)
Eta_Norm = matrix(0,nrow = nSamples, ncol = (K+1) )
colnames(Eta_Norm) = c("Eta", as.character(1:K) )
Eta_Norm = as.data.frame(Eta_Norm)
Eta_Norm[,1] = eta 
for(k in 1:K ) Eta_Norm[,(k+1)] = dnorm(eta,m0[k], sd = sqrt(sigma2[k]) )
Eta_Norm_Melted = melt(Eta_Norm, id="Eta")
names(Eta_Norm_Melted)[2:3] = c("Class", "Density")

pdf("~/Dropbox/GT_2015/Figures/Component_Priors.pdf")
ggplot(data=Eta_Norm_Melted, aes(x=Eta, y=Density, colour=Class) ) +
  geom_line(size = 2)+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

#Now do component priors on the probability scale:
mu_0 = Y_0 = matrix(nrow = nSamples, ncol = K)

for(k in 1:K){
  mu_0[,k] = 1/(1+exp(-rnorm(nSamples, m0[k], sqrt(sigma2[k]) ) ) )
  Y_0[,k] = rbinom(nSamples,500,mu_0[,k])
}

colnames(mu_0) = as.character(1:K)
sa <- stack(as.data.frame(mu_0) )
sa$Class <- rep( 1:K, each = nrow(mu_0) )
sa$Class = factor(sa$Class)

 
pdf("~/Dropbox/GT_2015/Figures/Component_Priors_Probability.pdf")
m <-ggplot(data = sa)
m + geom_density(aes(x = values, colour = Class), size = 2 ) +
  xlim (0,.15) +
  xlab("Mu")+
  ylab("Density")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"))
dev.off()

colnames(Y_0) = as.character(1:K)
sa <- stack(as.data.frame(Y_0) )
sa$Class <- rep( 1:K, each = nrow(mu_0) )
sa$Class = factor(sa$Class)

pdf("~/Dropbox/GT_2015/Figures/Component_Priors_Y.pdf")
ggplot(sa, aes(values, fill = Class)) + geom_histogram(alpha = 0.6, aes(y = ..density..), position = 'identity')+
  xlab("Home runs")+
  ylab("Density")+
  xlim(0,70)+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"))
dev.off()

m0_PED = .4
sigma2_PED = .02

Eta_Norm = matrix(0,nrow = nSamples, ncol = (K+1) )
colnames(Eta_Norm) = c("Eta", as.character(1:K) )
Eta_Norm = as.data.frame(Eta_Norm)
Eta_Norm[,1] = eta 
for(k in 1:K ) Eta_Norm[,(k+1)] = dnorm(eta,m0[k]+m0_PED, sd = sqrt(sigma2[k] + sigma2_PED) )
Eta_Norm_Melted = melt(Eta_Norm, id="Eta")
names(Eta_Norm_Melted)[2:3] = c("Class", "Density")

pdf("~/Dropbox/GT_2015/Figures/Component_Priors_PED.pdf")
ggplot(data=Eta_Norm_Melted, aes(x=Eta, y=Density, colour=Class) ) +
  geom_line(size = 2)+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

#Now do component priors on the probability scale:
mu_0 = Y_0 = matrix(nrow = nSamples, ncol = K)

for(k in 1:K){
  mu_0[,k] = 1/(1+exp(-rnorm(nSamples, m0[k]+m0_PED, sqrt(sigma2[k])+sigma2_PED) ) )
  Y_0[,k] = rbinom(nSamples,500,mu_0[,k])
}

colnames(mu_0) = as.character(1:K)
sa <- stack(as.data.frame(mu_0) )
sa$Class <- rep( 1:K, each = nrow(mu_0) )
sa$Class = factor(sa$Class)
pdf("~/Dropbox/GT_2015/Figures/Component_Priors_Probability_PED.pdf")
m <-ggplot(data = sa)
m + geom_density(aes(x = values, colour = Class, group = Class), size = 2 ) +
  xlim (0,.15) +
  xlab("Mu")+
  ylab("Density")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"))
dev.off()

colnames(Y_0) = as.character(1:K)
sa <- stack(as.data.frame(Y_0) )
sa$Class <- rep( 1:K, each = nrow(mu_0) )
sa$Class = factor(sa$Class)

pdf("~/Dropbox/GT_2015/Figures/Component_Priors_Y_PED.pdf")
ggplot(sa, aes(values, fill = Class)) + geom_histogram(alpha = 0.6, aes(y = ..density..), position = 'identity')+
  xlab("Home runs")+
  ylab("Density")+
  xlim(0,70) +
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"))
dev.off()


PED.Effect.random = rnorm(nSamples,m0_PED,sqrt(sigma2_PED))
eta = seq(-6,-2, length = 1000)
mu = exp(eta)/(1+exp(eta))

mu.PED = matrix(nrow = nSamples, ncol = 1000)
mu.PED.summary = matrix(nrow=3,ncol = 1000)
for(i in 1:1000){
  mu.PED[,i] = exp(eta[i]+PED.Effect.random)/(1+exp(eta[i]+PED.Effect.random))
  mu.PED.summary[1,i] = mean(mu.PED[,i])
  mu.PED.summary[2:3,i] = quantile(mu.PED[,i],c(.025,.975))
}

MU.PED = data.frame(Eta = eta, Natural = mu, PED = mu.PED.summary[1,], PED.025 = mu.PED.summary[2,], PED.975 = mu.PED.summary[3,] )

pdf("~/Dropbox/GT_2015/Figures/HR_Rate_PED.pdf")
ggplot(data=MU.PED, aes(x=Eta, colour = Status ) ) +
  geom_line(aes(y=Natural, colour = "Natural"),size = 1)+
  geom_line(aes(y=PED, colour = "PED"), size = 1)+
  geom_line(aes(y=PED.025, colour = "PED"), size=1, linetype=2)+
  geom_line(aes(y=PED.975, colour = "PED"), size=1, linetype=2)+
  xlab("Eta")+
  ylab("Probability")+
  scale_color_manual(values=c("Natural" = "black", "PED" = "red"))+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


PED.Effect = mu.PED.summary[1,] - mu
PED.975 = mu.PED.summary[3,]-mu
PED.025 = mu.PED.summary[2,]-mu

MU.PED = data.frame(Eta = eta, PED.Effect, PED.975, PED.025)
pdf("~/Dropbox/GT_2015/Figures/PED_Effect.pdf")
ggplot(data=MU.PED, aes(x=Eta) ) +
  geom_line(aes(y=PED.Effect), size = 1, color = "red")+
  geom_line(aes(y=PED.025), size=1, linetype=2, color = "red")+
  geom_line(aes(y=PED.975), size=1, linetype=2, color = "red")+
  xlab("Eta")+
  ylab("Probability")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()





pi.0 = matrix(c(1,0),nrow = 1)
rho =  1/36 #1 starting player for every third team becomes a steroid user
stickiness_zeta = 8/9 # 8 starters out of 9 remain steroid users. 
#this combination leads to a stationary distribution of P(PED = 1) = .2.  Nice.  
Q.z = Q.PED(stickiness_zeta,rho)
Pi.PED = matrix(nrow = 2, ncol = n.T)
Q = Q.z

for(t in 1:n.T){
  Pi.PED[,t] = pi.0%*%Q
  Q = Q%*%Q.z
}

PI.PED = data.frame(Age, Probability = Pi.PED[2,])
pdf("~/Dropbox/GT_2015/Figures/PED_Prob.pdf")
ggplot(data = PI.PED, aes(Age)) + 
  geom_line(aes(y=Probability), size = 1, color = "dark blue")+
  xlab("Eta")+
  ylab("Probability")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

m0 = c(m0, m0_PED)  #Adding on the mean for the PED Effect
sigma2 = c(sigma2, sigma2_PED) #Adding the variance for the PED Effect

m0 = matrix(m0,ncol = 1)
C0 = diag(sigma2)

mu = list()
PI.G.0 = c(1, rep(0,K-1))

m0.names = as.character(round( exp(m0[1:K])/(1+exp(m0[1:K])),3 ))
PI.G.df = data.frame(Probability = PI.G.0, Rates = as.factor(m0.names))

pdf("~/Dropbox/GT_2015/Figures/P_Gamma_0.pdf")
ggplot(data = PI.G.df, aes(x=Rates,y=Probability)) + geom_bar(stat="identity") +   theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


#200 worked previously.  
alpha = 175
decay.rate = 1.5

Q = list()
for(t in 1:n.T){
    Q[[t]] = Q.age(Age[t],alpha, decay.rate, K, PI.0) 
}


Gamma = matrix(nrow = nSamples, ncol= n.T)
Theta = list()
theta = matrix(nrow = (K+1), ncol = n.T)
for(k in 1:K) Theta[[k]] = matrix(nSamples)
Zeta = matrix(nrow = nSamples,ncol = n.T)
Mu =Mu.PED = Mu.Clean = matrix(nrow = nSamples, ncol = n.T)
Y = Y.Clean = matrix(nrow = nSamples, ncol = n.T)
gamma.0 = eta.0 = mu.0 = rep(NA, nSamples)

#W = diag(c(.005, .005, .0025, .001, .001))
W = diag(c(.01, .005, .005, .0025, .001, .001))
theta.k = list() 
for(k in 1:K) theta.k[[k]] = matrix(nrow = nSamples, ncol = n.T)

for(m in 1:nSamples){

theta.0 = mvrnorm(1,m0,C0) 
gamma.0[m] = sample(1:K, 1, prob = PI.G.0)
zeta.0 = 0  #assume all players start as non steroid players.  
eta.0[m] = rnorm(1,m0[gamma.0[m]], sqrt(sigma2[gamma.0[m]]) )
mu.0[m] = exp(eta.0[m])/(1+exp(eta.0[m]))
  
  for(t in 1:n.T){
    if(t==1){
      theta[,t] = theta.0 + mvrnorm(1, rep(0,K+1), C0)
      Gamma[m,t] = sample(1:K, 1, prob = Q[[t]][gamma.0[m], ] ) 
      Zeta[m,t] =  sample(0:1, 1, prob = Q.z[1,] ) 
    }else{
      theta[,t] = theta[,t-1] + mvrnorm(1,rep(0,K+1),W)
      Gamma[m,t] = sample(1:K, 1, replace=T, prob = Q[[t]][ Gamma[m,t-1], ] ) 
      Zeta[m,t] =  sample(0:1, 1, prob = Q.z[ Zeta[m,t-1] + 1, ] ) 
    }
      
      for(k in 1:K) theta.k[[k]][m,t] = theta[k,t]

    
      eta = theta[Gamma[m,t],t] + Zeta[m,t]*theta[(K+1),t]
      eta.PED = theta[Gamma[m,t],t] + theta[(K+1),t]
      eta.Clean = theta[Gamma[m,t],t]
      Mu[m,t] = exp(eta)/(1+exp(eta))
      Mu.PED[m,t] = exp(eta.PED)/(1+exp(eta.PED))
      Mu.Clean[m,t] = exp(eta.Clean)/(1+exp(eta.Clean))
      N = sample(AB[[t]], nSamples,replace=T)
      Y[m,t] = rbinom(1,N,Mu[m,t])
      Y.Clean[m,t] = rbinom(1,N,Mu.Clean[m,t])
  }
}

PI.G_Z.0 = matrix(0,nrow = n.T, ncol = 2*K)
rownames(PI.G_Z.0) = Age
for(t in 1:n.T){
  for(z in 1:2){
    for(k in 1:K){
      PI.G_Z.0[t,K*(z-1)+k] = sum( Gamma[,t]==k & Zeta[,t]==(z-1) )/nSamples
    }
  }
}
save(file = "~/DGLM/Data/Gamma_Zeta_marginal_time.RData", PI.G_Z.0)
apply(PI.G_Z.0,1,sum)

Eta.df = data.frame(Eta = eta.0)
pdf("~/Dropbox/GT_2015/Figures/Mixture_Prior_Ability.pdf")
m <-ggplot(data = Eta.df, aes(x = Eta))
m + geom_density(size=1.5) +
  xlim (-10,-1.5) +
  xlab("Eta")+
  ylab("Density")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

Mu.df = data.frame(Mu = mu.0)
pdf("~/Dropbox/GT_2015/Figures/Mixture_Prior_Rate.pdf")
m <-ggplot(data = Mu.df, aes(x = Mu))
m + geom_density(size=1.5) +
  xlim (0,.1) +
  xlab("Eta")+
  ylab("Density")+
  #scale_y_continuous(trans="log10", name="density")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


colnames(Mu) = as.character(Age)
Age.sub = as.character(c(17,seq(from = 20, to = 45, len = 6)) )
Mu.sub = Mu[,Age.sub]

sa <- stack(as.data.frame(Mu.sub) )
sa$Age <- rep( Age.sub, each = nrow(Mu.sub) )
sa$Age = factor(sa$Age)
pdf("~/Dropbox/GT_2015/Figures/Mixture_Prior_Rate_Time.pdf")
m <-ggplot(data = sa)
m + geom_density(aes(x = values, colour = Age, group = Age), size = 1.5) +
  xlim (0,.1) +
  xlab("Mu")+
  ylab("Density")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"))
dev.off()


sa <- stack(as.data.frame(Mu) )
sa$Age <- rep( Age, each = nrow(Mu) )
sa$Age = factor(sa$Age)
pdf("~/Dropbox/GT_2015/Figures/Marginal_Mu.pdf")
p <- ggplot(sa, aes(ind, values))
p + geom_boxplot(fill="gray") + ylim(0,.25) + 
xlab("Age")+
  ylab("Mu")+
  theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


#What are the marginal probabilities of Gamma and Zeta?
Prob.Gamma = matrix(nrow = K, ncol = n.T)
Prob.Zeta = rep(NA,n.T)
for(t in 1:n.T){
  Prob.Zeta[t] = mean(Zeta[,t])
  for(k in 1:K){
    Prob.Gamma[k,t] = sum(Gamma[,t]==k)/nSamples
  }
}

rownames(Prob.Gamma) = as.character(1:K)
sa <- stack(as.data.frame(t(Prob.Gamma)))
names(sa) = c("values", "Class")
sa$Age <- rep( Age, ncol(t(Prob.Gamma)) )

pdf("~/Dropbox/GT_2015/Figures/Prob_Gamma.pdf")
ggplot(data = sa, aes(x=Age))+
  geom_line(aes(Age, y=values, group = Class, colour = Class), size = 2 )+
  xlab("Age")+
  ylab( "Membership Proportions")+
  theme( axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


colnames(Y) = colnames(Y.Clean) = as.character(Age)

pdf("~/Dropbox/GT_2015/Figures/Marginal_Y.pdf")
sa <- stack(as.data.frame(Y) )
names(sa) = c("Homeruns", "Age")
p <- ggplot(sa, aes(Age, Homeruns))
p + geom_boxplot(fill="gray") + ylim(0,100)+
xlab("Age")+
ylab("Home runs")+
theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()


sa <- stack(as.data.frame(Y.Clean) )
names(sa) = c("Homeruns", "Age")

pdf("~/Dropbox/GT_2015/Figures/Marginal_Y_Clean.pdf")
p <- ggplot(sa, aes(Age, Homeruns))
p + geom_boxplot(fill="gray") + ylim(0,90)+
xlab("Age")+
ylab("Home runs")+
theme(axis.text=element_text(size=10, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

MU = MU.PED = MU.Clean = matrix(nrow = 3, ncol = n.T)
MU[1,] = apply(Mu,2,mean)
MU.PED[1,] = apply(Mu.PED,2,mean)
MU.Clean[1,] = apply(Mu.Clean,2,mean)

MU[2:3,] = apply(Mu,2,quantile, probs =c(.025,.975) )
MU.PED[2:3,] = apply(Mu.PED,2,quantile, probs =c(.025,.975) )
MU.Clean[2:3,] = apply(Mu.Clean,2,quantile, probs =c(.025,.975) )

DF = data.frame(Age = Age, Mu = MU[1,], Mu.025 = MU[2,], Mu.975 = MU[3,], Mu.PED = MU.PED[1,], Mu.PED.025 = MU.PED[2,], Mu.PED.975=MU.PED[3,], Mu.Clean = MU.Clean[1,], Mu.Clean.025 = MU.Clean[2,], Mu.Clean.975 = MU.Clean[3,])
pdf("~/Dropbox/GT_2015/Figures/Prior_Age_Curve.pdf")
ggplot(data = DF, aes(x=Age, colour = Status ))+
  geom_line(aes(y=Mu, colour = "Stochastic"), size=1.5)+
  geom_line(aes(y=Mu.025, colour = "Stochastic"), size=1, linetype=2)+
  geom_line(aes(y=Mu.975, colour = "Stochastic"), size=1, linetype = 2)+
  geom_line(aes(y=Mu.PED, colour = "PED"), size=1.5)+
  geom_line(aes(y=Mu.PED.025, colour = "PED"), size=1, linetype=2)+
  geom_line(aes(y=Mu.PED.975, colour = "PED"), size=1, linetype = 2)+
  scale_color_manual(values=c("Stochastic" = "black", "PED" = "red"))+
  xlab("Age")+
  ylab("Mu")+
  theme(axis.text=element_text(size=20, color="black"),axis.title=element_text(size=24,face="bold"), legend.text=element_text(size=20))
dev.off()

#Goals for code:


