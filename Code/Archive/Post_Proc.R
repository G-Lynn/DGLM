rm(list=ls())

# REMEMBER Init_1 is conditioned on Eta
# Init_Full_1 is full MCMC
t.T = 10
nPlayers = 1000

n = rep(nPlayers,t.T)
load("~/Dropbox/Surya_Group/DGLM/SimStudy/Function Verification/Init_Full_1.RData")
#Post.Mean.Eta.1 = Post.Mean.Eta
Post.Mean.Theta.1 = Post.Mean.Theta
Post.Mean.Gamma.1 = Post.Mean.Gamma
Post.Mean.Zeta.1 = Post.Mean.Zeta
load("~/Dropbox/Surya_Group/DGLM/SimStudy/Function Verification/Init_Full_2.RData")
#Post.Mean.Eta.2 = Post.Mean.Eta
Post.Mean.Theta.2 = Post.Mean.Theta
Post.Mean.Gamma.2 = Post.Mean.Gamma
Post.Mean.Zeta.2 = Post.Mean.Zeta
load("~/Dropbox/Surya_Group/DGLM/SimStudy/Function Verification/Init_Full_3.RData")
#Post.Mean.Eta.3 = Post.Mean.Eta
Post.Mean.Theta.3 = Post.Mean.Theta
Post.Mean.Gamma.3 = Post.Mean.Gamma
Post.Mean.Zeta.3 = Post.Mean.Zeta

load("~/Dropbox/Surya_Group/DGLM/SimStudy/Function Verification/Init_Full_4.RData")
#Post.Mean.Eta.3 = Post.Mean.Eta
Post.Mean.Theta.4 = Post.Mean.Theta
Post.Mean.Gamma.4 = Post.Mean.Gamma
Post.Mean.Zeta.4= Post.Mean.Zeta

load("~/Dropbox/Surya_Group/DGLM/SimStudy/Function Verification/Init_Full_5.RData")
#Post.Mean.Eta.3 = Post.Mean.Eta
Post.Mean.Theta.5 = Post.Mean.Theta
Post.Mean.Gamma.5 = Post.Mean.Gamma
Post.Mean.Zeta.5 = Post.Mean.Zeta


load("~/Dropbox/Surya_Group/DGLM/SimStudy/Function Verification/Init_Full_6.RData")
#Post.Mean.Eta.3 = Post.Mean.Eta
Post.Mean.Theta.6 = Post.Mean.Theta
Post.Mean.Gamma.6 = Post.Mean.Gamma
Post.Mean.Zeta.6 = Post.Mean.Zeta



Theta.Diff.1 = Theta.Diff.2 = Theta.Diff.3 = Theta.Diff.4 = Theta.Diff.5 = list()
Gamma.Diff.1 = Gamma.Diff.2 = Gamma.Diff.3 = Gamma.Diff.4 = Gamma.Diff.5 = list()
Zeta.Diff.1 = Zeta.Diff.2 = Zeta.Diff.3 = Zeta.Diff.4 = Zeta.Diff.5 = list()

for(t in 1:t.T){
  #Eta.Diff.1[[t]] = Post.Mean.Eta.1[[t]] - Post.Mean.Eta.2[[t]]
  #Eta.Diff.2[[t]] = Post.Mean.Eta.1[[t]] - Post.Mean.Eta.3[[t]]
  
  Theta.Diff.1[[t]] = Post.Mean.Theta.1[[t]] - Post.Mean.Theta.2[[t]]
  Theta.Diff.2[[t]] = Post.Mean.Theta.1[[t]] - Post.Mean.Theta.3[[t]]
  Theta.Diff.3[[t]] = Post.Mean.Theta.1[[t]] - Post.Mean.Theta.4[[t]]
  Theta.Diff.4[[t]] = Post.Mean.Theta.1[[t]] - Post.Mean.Theta.5[[t]]
  Theta.Diff.5[[t]] = Post.Mean.Theta.1[[t]] - Post.Mean.Theta.6[[t]]
  
  Gamma.Diff.1[[t]] = Post.Mean.Gamma.1[[t]] - Post.Mean.Gamma.2[[t]]
  Gamma.Diff.2[[t]] = Post.Mean.Gamma.1[[t]] - Post.Mean.Gamma.3[[t]]
  Gamma.Diff.3[[t]] = Post.Mean.Gamma.1[[t]] - Post.Mean.Gamma.4[[t]]
  Gamma.Diff.4[[t]] = Post.Mean.Gamma.1[[t]] - Post.Mean.Gamma.5[[t]]
  Gamma.Diff.5[[t]] = Post.Mean.Gamma.1[[t]] - Post.Mean.Gamma.6[[t]]
  
  Zeta.Diff.1[[t]] = Post.Mean.Zeta.1[[t]] - Post.Mean.Zeta.2[[t]]
  Zeta.Diff.2[[t]] = Post.Mean.Zeta.1[[t]] - Post.Mean.Zeta.3[[t]]
  Zeta.Diff.3[[t]] = Post.Mean.Zeta.1[[t]] - Post.Mean.Zeta.4[[t]]
  Zeta.Diff.4[[t]] = Post.Mean.Zeta.1[[t]] - Post.Mean.Zeta.5[[t]]
  Zeta.Diff.5[[t]] = Post.Mean.Zeta.1[[t]] - Post.Mean.Zeta.6[[t]]
}

False.Positive.Rate = rep(NA,t.T)
False.Negative.Rate = rep(NA,t.T)

for(t in 1:t.T){
  False.Negative.Rate[t] = sum(Post.Err.Zeta[[t]] > (0.33) )/n[t]
  False.Positive.Rate[t] = sum( Post.Err.Zeta[[t]] < (-0.67) )/n[t]
}
# classify players with less that 1/3 prob of PED as clean. 
# classify players with higher than 2/3 prob of PED as dirty. 
# Remember I assume all players are clean at the beginning of career.

plot(1:t.T,False.Positive.Rate, type="l", col = 1, lwd=2, ylab="", main = "False Negative Rate")
lines(1:t.T, False.Negative.Rate, type = "l", col = 2, lwd=2, ylab="", main = "False Positive Rate")
legend("topright", legend = c("False Positive", "False Negative"), text.col = c(1,2))
abline(h=.05)
