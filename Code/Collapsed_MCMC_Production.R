MCMC.collapsed<-function(init,nSamples,N.MC,Thin.Rate,m0,C0,sigma2.theta,t.T,n,K,PI.G_Z.0,Q.gamma_zeta,N,y){
  unlink(paste("~/DGLM/Reproducibility/Init_",init,sep=""), recursive=T)
  dir.create(paste("~/DGLM/Reproducibility/Init_",init,sep=""))
  #output the names of columns for each of the stored sample files only once at the beginning
  write.table(file = paste("~/DGLM/Reproducibility/Init_",init,"/Theta_colnames.csv", sep=""), x = Age, sep = ",", col.names = F )
  for(t in 1:t.T){
    write.table(file = paste("~/DGLM/Reproducibility/Init_",init,"/Gamma_",t,"_colnames.csv", sep=""), x = t(names(y[[t]])), sep = ",", col.names=F)
    write.table(file = paste("~/DGLM/Reproducibility/Init_",init,"/Zeta_",t,"_colnames.csv", sep=""), x = t(names(y[[t]])), sep = ",", col.names = F)
  }
  
  Theta = Theta.Initialize(m0,C0,sqrt(sigma2.theta),t.T)
  Gamma = Gamma.Initialize(m0,K,N,y)
  Zeta = Zeta.Initialize(n,0,y)
  FF = F_construct(n,K,t.T,Gamma,Zeta)
  Omega = Omega.step.collapsed(FF,Theta,N,n,t.T)
  Z = Z_construct(N,Omega,y)
  
  for(m in 1:N.MC){
    Omega = Omega.step.collapsed(FF,Theta,N,n,t.T)
    Z = Z_construct(N,Omega,y)
    
    TY = Theta.FFBS.collapsed(n,m0,C0,FF,N,Omega,Kappa)
    Theta = TY[[1]]
    
    GammaZeta = GammaZeta.FFBS.collapsed(n,K,PI.G_Z.0,Q.gamma_zeta,Theta,N,Omega,Z)
    Gamma = GammaZeta[[1]]
    Zeta = GammaZeta[[2]]
    
    FF = F_construct(n,K,t.T,Gamma,Zeta)
    
    if(m>B & m%%Thin.Rate==0){
      write.table(file = paste("~/DGLM/Reproducibility/Init_",init,"/Theta.csv", sep=""), x = Theta, sep = ",", append = T, col.names = F )
      for(t in 1:t.T){
        write.table(file = paste("~/DGLM/Reproducibility/Init_",init,"/Gamma_",t,".csv", sep=""), x = t(Gamma[[t]]), sep = ",", append = T, col.names=F)
        write.table(file = paste("~/DGLM/Reproducibility/Init_",init,"/Zeta_",t,".csv", sep=""), x = t(Zeta[[t]]), sep = ",", append = T, col.names = F)
        
      }# end of time loop for writing
      
    }# end of logic for m>B
    
  }# end of MCMC loop

}# end of function