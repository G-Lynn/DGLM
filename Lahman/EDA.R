# EDA for Prior. 
rm(list=ls())
load("~/Dropbox/Baseball/Lahman/Age_Alignment_Prior.RData")
source("~/Dropbox/Baseball/Particle Filter/PF_Mixture/PF_Functions.R")
n.T = length(Age.Alignment.Prior)
n = rep(NA,n.T)

for(t in 1:n.T) n[t] = length(Age.Alignment.Prior[[t]])
Age = 17:(17+n.T-1)


#3) Increase in rate as a function of theta.  What is my prior really doing.
#Suppose that that the PED performance increase is approx .5 in log odds

#Priors for m0 C0
pdf("~/Desktop/Prelim/PED_Impact_Prior.pdf")
plot(seq(-.5,1.5,length = 10000), dnorm(seq(-.5,1.5,length = 10000),.4,sqrt(.04)),type="l",lwd=4,col = 2,xlab="Log-odds scale", ylab = "")
axis(1,labels=T)
dev.off()

PED.Effect.random = rnorm(10000,.4,sqrt(.02))
theta = seq(-7,-1, length = 1000)
mu = exp(theta)/(1+exp(theta))
mu.PED = matrix(nrow = 10000, ncol = 1000)
mu.PED.summary = matrix(nrow=3,ncol = 1000)
for(i in 1:1000){
  mu.PED[,i] = exp(theta[i]+PED.Effect.random)/(1+exp(theta[i]+PED.Effect.random))
  mu.PED.summary[1,i] = mean(mu.PED[,i])
  mu.PED.summary[2:3,i] = quantile(mu.PED[,i],c(.025,.975))
}
pdf("~/Desktop/Prelim/HR_Rate_PED.pdf")
plot(theta, mu, type = "l", lwd= 3, lty = 1, ylim = c(0,.4), xlab = "Log Odds", ylab="HR Rate",yaxt="n",xaxt="n")
lines(theta,mu.PED.summary[1,], lwd = 3, lty = 2,col=2)
lines(theta,mu.PED.summary[2,], lwd = 3, lty = 3,col=2)
lines(theta,mu.PED.summary[3,], lwd = 3, lty = 3,col=2)
axis(1,labels=T,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
legend("topleft", legend = c("Clean HR Rate", "PED HR Rate"), lty = c(1,2),text.col=c(1,2))
dev.off()
#4) Estimate AR(1)  #Not sure that this needs to be done.


PED.Effect = mu.PED.summary[1,] - mu
PED.975 = mu.PED.summary[3,]-mu
PED.025 = mu.PED.summary[2,]-mu

pdf("~/Desktop/Prelim/PED_Effect.pdf")
plot(mu, PED.Effect, type = "l", lwd = 3, xlab = "Clean Homerun Rate", ylab="PED Effect",yaxt="n",xaxt="n", ylim = c(min(PED.025),max(PED.975)))
lines(mu,PED.025,lwd=3,lty=3)
lines(mu,PED.975,lwd=3,lty=3)
axis(1,labels=T,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
dev.off()

#5a) Marginal PED probability
N.Players = 30
K = ceiling(sqrt(N.Players))
delta = .95
m0.level = -5.75+(0:(K-1))/(K-1)*4
beta = 8/10
sigma2.k = -2/(log(beta)*(K-1)^2)
sigma2 = rep(sigma2.k,K)


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

pdf("~/Desktop/Prelim/PED_Prob.pdf")
plot(Age,Pi.PED[2,], type = "l", lwd = 2, lty = 1,ylab="", xaxt="n",yaxt="n" )
axis(1,labels=T,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
dev.off()

m0.PED = matrix(c(0,.5),ncol=1)
PED.curve = as.numeric( t(Pi.PED)%*%m0.PED )




#5) Expected Age Curve

m0 = matrix(m0.level,ncol = 1)
C0 = sigma2*diag(K)

mu = list()
weights = 1/(1:K)^4
tau.0 = round(.5*K)
Pi.Gamma.0 = rep(NA,K)
Pi.Gamma.0[tau.0] = weights[1]
for(tau in 1:K){
  if((tau.0-tau)>0) Pi.Gamma.0[tau.0-tau] = weights[1+tau]
  if((tau.0+tau)<=K) Pi.Gamma.0[tau.0+tau] = weights[1+tau]
}

Pi.Gamma.0 = Pi.Gamma.0/sum(Pi.Gamma.0)
m0.names = as.character(round( exp(m0)/(1+exp(m0)),3 ))
pdf("~/Desktop/Prelim/P_Gamma_0.pdf")
barplot(Pi.Gamma.0, names.arg = m0.names, xlab = "HR Rates", ylab = "Probability",yaxt="n")
dev.off()

M = 1000000
Mixture.Prior = rep(NA,M)

for(m in 1:M){
  gamma.draw = sample(1:K, 1, prob = Pi.Gamma.0)
  Mixture.Prior[m] = rnorm(1,m0[gamma.draw],sigma2[gamma.draw])
}

pdf("~/Desktop/Prelim/Mixture_Prior_Ability.pdf")
plot(density(Mixture.Prior), lwd=3, col=1, yaxt="n",xaxt="n",xlab="",main="")
axis(1,labels=T,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
dev.off()

rate.labels = as.character(seq(0,.2, length = 5) )
mu.mix = exp(Mixture.Prior)/(1+exp(Mixture.Prior))
pdf("~/Desktop/Prelim/Mixture_Prior_Rate.pdf")
plot(density(mu.mix), lwd=3, col=1, yaxt="n",xaxt="n",xlab="",main="")
axis(1,rate.labels,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
dev.off()

m = matrix(nrow = K, ncol = n.T)
C = matrix(nrow = K, ncol = n.T)

Pi.Gamma = matrix(nrow = K, ncol = n.T)
  for(k in 1:K){
    p0 = matrix(rep(0,K),nrow = 1)
    p0[,k]=1
    Q = diag(K)
    mu[[k]] = matrix(nrow=3,ncol=n.T)
    
    for(t in 1:n.T){

      age = Age[t]
      Q.t = Q.age(age,alpha=3.5,decay.rate = 3.5, K) 
      #Q.t = Q.RW.age(.95,age,K)
      Q = Q%*%Q.t
      
      if(t==1){
        Pi.Gamma[,t] =Pi.Gamma.0%*%Q.t
      }else{
        Pi.Gamma[,t] = t(Pi.Gamma[,t-1])%*%Q.t
      }
      
      p.gamma = p0%*%Q
      m[k,t] = p.gamma%*%m0
      C[k,t] = delta^(-t)*p.gamma%*%C0%*%t(p.gamma)
      
      theta = rnorm(100000,m[k,t],sqrt(C[k,t]))
      mu[[k]][1,t] = mean( exp(theta)/(1+exp(theta)) )
      mu[[k]][2:3,t] = quantile(exp(theta)/(1+exp(theta)), c(.025,.975))
      
    }
  }

pdf("~/Desktop/Prelim/Prob_Gamma.pdf")
matplot(Age, t(Pi.Gamma), ylab = "",cex=1.5,  yaxt="n",xaxt="n")
axis(1,labels=T,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
dev.off()


for(k in 1:K){
pdf(file = paste("~/Desktop/Prelim/Gamma_0_",k,".pdf",sep=""))
plot(Age,mu[[k]][1,], type="l", lwd = 3, lty=1, xlim = c(15,50), ylim=c(min(mu[[k]][2,]), max(mu[[k]][3,])), ylab="",yaxt="n",xaxt="n")
lines(Age,mu[[k]][2,], lwd=3,lty=2)
lines(Age,mu[[k]][3,], lwd=3,lty=2)
axis(1,labels=T,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
dev.off()
}


J = 100000
mu.curve = mu.curve.PED = mu.curve.PED.fixed = matrix(nrow = J, ncol = n.T)
theta.samples = theta.samples.PED = matrix(nrow = J,ncol = n.T)
for(j in 1:J){
  k = sapply(1:n.T, function(t) sample(1:K, 1, prob = Pi.Gamma[,t]) )
  z = sapply(1:n.T, function(t) sample(0:1,1,prob = Pi.PED[,t]) )
  theta.samples[j,] = sapply(1:n.T, function(t) rnorm(1,m[k[t],t],sqrt(C[k[t],t])) )
  PED.effect =  rnorm(n.T,.5,sqrt(.04))                       
  theta.samples.PED[j,] = theta.samples[j,] + z*PED.effect
  
  mu.curve[j,] = exp(theta.samples[j,])/(1+exp(theta.samples[j,]))
  mu.curve.PED[j,] = exp(theta.samples.PED[j,])/(1+exp(theta.samples.PED[j,]))
  mu.curve.PED.fixed[j,] = exp(theta.samples[j,]+PED.effect)/(1+exp(theta.samples[j,]+PED.effect))
}

 MU = MU.PED = MU.PED.fixed = matrix(nrow = 3, ncol = n.T)
 MU[1,] = apply(mu.curve,2,mean)
 MU.PED[1,] = apply(mu.curve.PED,2,mean)
 MU.PED.fixed[1,] = apply(mu.curve.PED.fixed,2,mean)
 MU[2:3,] = apply(mu.curve,2,quantile,c(.025,.975))
 MU.PED[2:3,] = apply(mu.curve.PED,2,quantile,c(.025,.975))
 MU.PED.fixed[2:3,] = apply(mu.curve.PED.fixed,2,quantile,c(.025,.975))

pdf("~/Desktop/Prelim/Prior_Age_Curve.pdf")
 plot(Age,MU[1,], type = "l", lwd = 2, lty=1, ylim = c(0,max(MU.PED.fixed[3,])),ylab="",xaxt="n",yaxt="n")
 lines(Age,MU[2,], lwd=2, lty=2)
 lines(Age,MU[3,], lwd=2, lty=2)
 lines(Age,MU.PED[1,], lwd = 2, lty = 1, col = "red")
 lines(Age,MU.PED[2,], lwd=2, lty=3, col = "red")
 lines(Age,MU.PED[3,], lwd=2, lty=3, col = "red")
 lines(Age,MU.PED.fixed[1,], lwd = 2, lty = 1, col = "blue")
 lines(Age,MU.PED.fixed[2,], lwd=2, lty=3, col = "blue")
 lines(Age,MU.PED.fixed[3,], lwd=2, lty=3, col = "blue")
 legend("topright",legend = c("Natural","Uncertain PED","Fixed PED"), text.col = c(1,2,4) )
 axis(1,labels=T,cex.axis=1.5)
 axis(2,labels=T,cex.axis=1.5)
dev.off()

# Plot of aggregate homeruns per player
Data.Prior = Data = read.csv("~/Dropbox/Baseball/Lahman/DLM_Prior_data.csv", header=T,stringsAsFactor=F)
Data = read.csv("~/Dropbox/Baseball/Lahman/DLM_data.csv", header=T,stringsAsFactor=F)
Data = rbind(Data.Prior,Data)

HR.per.player = rep(NA,114)
HR.median = rep(NA,114)
HR.max = rep(NA,114)
HR.95 = rep(NA,114)
index = 0
Years = 1900:2013
for(year in Years){
  index = index + 1
  n.Players.t = length(Data$HR[Data$yearID==year])
  HR.per.player[index] = sum(Data$HR[Data$yearID==year])/n.Players.t
  HR.median[index] = median(Data$HR[Data$yearID==year])
  HR.max[index] = max(Data$HR[Data$yearID==year])
  HR.95[index] = quantile(Data$HR[Data$yearID==year], c(.95))
}

pdf("~/Desktop/Prelim/HR_per_player.pdf")
plot(Years,HR.per.player, type = "l", lwd=2,yaxt="n",xaxt="n")
abline(v = 1950 )
abline(v = 1990 )
axis(1,labels=T,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
dev.off()

pdf("~/Desktop/Prelim/HR_median.pdf")
plot(Years,HR.median, type="l", lwd=2,xlab="",ylab="",yaxt="n",xaxt="n")
abline(v = 1950 )
abline(v = 1990 )
axis(1,labels=T,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
dev.off()

pdf("~/Desktop/Prelim/HR_95.pdf")
plot(Years,HR.95, type="l", lwd=2, xlab="",ylab="",yaxt="n",xaxt="n")
abline(v = 1950 )
abline(v = 1990 )
axis(1,labels=T,cex.axis=1.5)
axis(2,labels=T,cex.axis=1.5)
dev.off()



#Griffey = Data[Data$playerID=="griffke02",c("yearID","Age", "HR")]
#Griffey = Griffey[-20,] #get rid of second entry for 2008

#Pujols = Data[Data$playerID=="pujolal01",c("yearID","Age", "HR")]

# Also look at quantiles of HR per year.

#Player.Year = as.data.frame(matrix(nrow = 2, ncol = 24))
#names(Player.Year) = as.character(1990:2013)
#rownames(Player.Year) = c("Griffey","Pujols")
#Player.Year["Griffey",as.character(1990:2010)] = Griffey$HR
#Player.Year["Pujols",as.character(2001:2013)] = Pujols$HR

#Player.Age = as.data.frame(matrix(nrow = 2, ncol = 21))
#names(Player.Age) = as.character(21:41)
#rownames(Player.Age) = c("Griffey","Pujols")

#Player.Age["Griffey",  as.character(Griffey$Age) ] = Griffey$HR
#Player.Age["Pujols",  as.character(Pujols$Age) ] = Pujols$HR

#library(xtable)
#xtable(t(Player.Age) )
#xtable(t(Player.Year) )



