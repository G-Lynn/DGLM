rm(list=ls())
source("~/Dropbox/Surya_Group/Baseball/Simulation_Study/DLM_functions.R")
source("~/Dropbox/Surya_Group/Baseball/Simulation_Study/DLM_FFBS.R")

K = 10
N.samples = 100000
PI.0 = seq(from = .005, to = .10, len = K) 
m0 = log( PI.0 / (1-PI.0 ) )

beta = (8/10)
sigma2 = rep(NA,K)
for(k in 1:K){
  delta.m = (m0[k+1]-m0[k])
  if(k==K) delta.m = (m0[k]-m0[k-2])/2
  sigma2[k] = -delta.m^2/(8*log(beta))
}

x = seq(from = -6, to = -1, length = N.samples)
plot(x, dnorm(x,m0[K], sd = sqrt(sigma2[K]) ), type = "l", lwd = .5, col = 1, xlim = c(-7,-1), ylab = "", xlab = "Log Odds" )
for(k in 1:(K-1) ){
  lines(x, dnorm(x,m0[k], sd = sqrt(sigma2[k]) ), type = "l", lwd=.5, col = k)
}


tau.0 = which(abs(m0-(-4)) == min(abs(m0-(-4) ) ) ) 
PI.G.0 = rep(NA,K)
weights = rep(NA,K)
for(k in 1:K){
  if(k<tau.0){
    weights[k] = exp(-1*abs(m0[k]-m0[tau.0]) )
  }else{
    weights[k] = exp(-2*abs(m0[k]-m0[tau.0]) )
  }
}

PI.G.0 = weights/sum(weights)

barplot(PI.G.0, names.arg = round( 1/(1+exp(-m0)), 3) )

gamma = sample(1:K, size = N.samples, prob = PI.G.0, replace = T )
eta = sapply(1:N.samples, function(i) rnorm(1,m0[gamma[i]], sd = sqrt(sigma2[gamma[i] ] )))
PI = 1/(1+exp(-eta))
plot(density(PI) )
y = rbinom(N.samples, 500,PI)
hist(y)


PI.G_Z.0 = matrix(rep(0,2*K), nrow = 1)
PI.G_Z.0[1:K] = PI.G.0 * PI.Z.0[,1]
PI.G_Z.0[(K+1):(2*K)] = PI.G.0 * PI.Z.0[,2]

Q.gamma = list()
t.T = 18
Age = 22:39
ALPHA = 2
ALPHA.decay = 5
for(t in 1:t.T){
  Q.gamma[[t]] = Q.age(Age[t],alpha=ALPHA,decay.rate = ALPHA.decay, K)
}


Gamma = Eta = Pi = matrix(nrow = N.samples, ncol = t.T)
for(t in 1:t.T){
  if(t==1){
    Gamma[,t] = sapply(1:N.samples, function(i) sample(1:K, 1, prob = Q.gamma[[t]][gamma[i],] ) )
  }else{
    Gamma[,t] = sapply(1:N.samples, function(i) sample(1:K, 1, prob = Q.gamma[[t]][Gamma[i,t-1],] ) )
  }
  
  Eta[,t] = sapply(1:N.samples, function(i) rnorm(1,m0[Gamma[i,t]], sd = sqrt(sigma2[Gamma[i,t] ] )))
  Pi[,t] = 1/(1+exp(-Eta[,t]))
}

E.Pi = apply(Pi,2,mean)
Pi.CI = apply(Pi,2,quantile, prob = c(.025,.975))
plot(Age,E.Pi, ylim = range(Pi.CI) )
lines(Age,Pi.CI[1,])
lines(Age,Pi.CI[2,])


