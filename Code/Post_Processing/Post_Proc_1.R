rm(list=ls())
t.T = 18

t = 1
K = 10
p = K+1

Theta.init = list()
Gamma.init = list()
Zeta.init = list()

for(jj in 1:6){
  init = jj
  thetaNames = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Theta_colnames.csv",sep=""), header=F, stringsAsFactors=F)
  theta = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Theta.csv",sep=""), header=F, stringsAsFactors=F)
  Theta.Mean = matrix(nrow = p, ncol = t.T)
  Theta.CI = list()
  
  for(j in 1:p){
    theta_j = theta[theta[,1]==j,]
    theta_j = theta_j[,-1]
    colnames(theta_j) = thetaNames[,2]
    Theta.Mean[j,] = apply(theta_j,2,mean)
    Theta.CI[[j]] = matrix(nrow = 3, ncol = t.T)
    Theta.CI[[j]][2,] = Theta.Mean[j,]
    Theta.CI[[j]][1,] = apply(theta_j,2,quantile,prob=.025)
    Theta.CI[[j]][3,] = apply(theta_j,2,quantile,prob=.975)
    rm(theta_j)
  }
  

  Gamma.Mean = Zeta.Mean = list()
  for(t in 1:t.T){
    Map = order(Theta.Mean[1:K,t])
    Theta.Mean[1:K,t] = Theta.Mean[Map,t]
    
    Names = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Gamma_",t,"_colnames.csv",sep=""),header=F, stringsAsFactors=F)
    gamma = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Gamma_",t,".csv",sep=""), header=F, stringsAsFactors=F)
    zeta = read.csv(paste("~/DGLM/Reproducibility/Init_",init,"/Zeta_",t,".csv",sep=""), header=F, stringsAsFactors=F)
  
    Names = Names[,-1]
    gamma = gamma[,-1]
    zeta = zeta[,-1]
  
  
    colnames(gamma) = Names
    colnames(zeta) = Names
  
    #Now re-map for label switching.  
    for(k in 1:K){
      index= which(gamma==k,arr.ind=T)
      gamma[index] = Map[k]
    }
  
    #Compute the mean
    Gamma.Mean[[t]] = matrix(nrow = 3, ncol = dim(gamma)[2])
    colnames(Gamma.Mean[[t]]) = Names
  
    Gamma.Mean[[t]][2,] = apply(gamma,2,mean)
    Gamma.Mean[[t]][1,] = apply(gamma,2,quantile,prob=.025)
    Gamma.Mean[[t]][3,] = apply(gamma,2,quantile,prob=.975)
  
    Zeta.Mean[[t]] = apply(zeta,2,mean)
    rm(gamma,zeta,Names)
  }
  
  Theta.init[[jj]] = Theta.Mean 
  Gamma.init[[jj]] = Gamma.Mean
  Zeta.init[[jj]] = Zeta.Mean
}

save(file = "~/DGLM/Reproducibility/Rep.RData", Gamma.init, Zeta.init, Theta.init)

Theta.init[[2]] - Theta.init[[5]]
hist(Gamma.init[[1]][[12]][2,] - Gamma.init[[5]][[12]][2,])
hist( Zeta.init[[1]][[12]] - Zeta.init[[6]][[12]] )

players = c(
"sosasa01",
"bondsba01",
"ortizda01",
"jeterde01",
"suzukic01",
"griffke02",
"mcgwima01",
"thomeji01",
"thomafr04")


POI = matrix(nrow = 9, ncol = t.T)
rownames(POI) = players
for(t in 1:t.T){
  POI[,t] = Zeta.init[[1]][[t]][players]
}
