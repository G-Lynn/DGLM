# Subset of Age Alignment
sub.Aligned.Players<-function(Data,sub.Players){
  sub.Alignment = list()
  t.T = length(Data)
  for(t in 1:t.T){
    Players.t = names(Data[[t]])
    index = which( is.element(Players.t,sub.Players) ) 
    sub.Alignment[[t]] = list()
    if(length(index)>0){
      for(i in 1:length(index)) sub.Alignment[[t]][[i]] = Age.Alignment[[t]][[ index[i] ]]
      names(sub.Alignment[[t]]) = names(Age.Alignment[[t]])[index]
    }else{ next }
  }
  return(sub.Alignment)
}





FNY.Time <-function(Data){
  FF = list()
  N  = list()
  Y  = list()
  for(t in 1:t.T){
    if(n[t]==0){next}
    
    Players.t = names(Data[[t]])
    FF[[t]] = t(sapply(1:n[t], function(i) c(rep(0,K+1) ) ) )
    N[[t]] = sapply(1:n[t], function(i) Data[[t]][[i]]$Response$AB)
    Y[[t]] = sapply(1:n[t], function(i) Data[[t]][[i]]$Response$HR)
    rownames(FF[[t]]) = names(N[[t]]) = names(Y[[t]]) = Players.t
  }
  return( list(FF,N,Y) )
}

F_ikzt <-function(n,FF,Gamma,Zeta){
  
  for(t in 1:t.T){
    if(n[t]==0){next}
    Players.t = names(Gamma[[t]])
    for(i in 1:n[t]){
      player = Players.t[i]
      FF[[t]][player,1:(K+1)] = rep(0,K+1)
      FF[[t]][player,Gamma[[t]][player] ] = 1
      FF[[t]][player,K+1] = Zeta[[t]][[player]]
    }
  }
  return(FF)
}






F_kt <-function(FF_t,k){
  nn = dim(FF_t)[1]
  FF_t[,1:K] = matrix(rep(0,K*nn), nrow = nn)
  FF_t[,k] = 1
  return(FF_t)
}

F_zt <-function(FF,Zeta){
  for(t in 1:t.T){
    if(n[t]==0){
      next
    }
    
    FF[[t]][,K+1] = Zeta[[t]]
    
  }
  return(FF)
}

Z.Gamma<-function(N.Gamma,V.Gamma,Y.Gamma){
  Z.Gamma = list()
  for(k in 1:K){
    Z.Gamma[[k]] = list()
    
    for(t in 1:t.T){
      if(length(N.Gamma[[k]][[t]]) == 0){
        Z.Gamma[[k]][[t]] = rep(0,0)
      }else{
        Z.Gamma[[k]][[t]] = V.Gamma[[k]][[t]]%*%(Y.Gamma[[k]][[t]] - .5*N.Gamma[[k]][[t]])  
      }
    }
  }
  return(Z.Gamma)
}


Write.Samples<-function(directory,Theta,Gamma,Zeta,Y.hat,MU.hat){
  for(t in 1:t.T){
    write.table(x = data.frame(Y.hat[[t]]), file = paste(paste(directory,"Yhat_",sep="/"),t,".csv",sep=""), sep="," , row.names=T, col.names=F, append=T)
    write.table(x = data.frame(Gamma[[t]]), file = paste(paste(directory,"Gamma_",sep="/"),t,".csv",sep=""), sep = ",", row.names=T, col.names=F, append=T)
    write.table(x = data.frame(Zeta[[t]]), file = paste(paste(directory,"Zeta_",sep="/"),t,".csv",sep=""), sep = ",", row.names=T, col.names=F, append=T)
    write.table(x = data.frame(MU.hat[[t]]), file = paste(paste(directory,"MUhat_",sep="/"),t,".csv",sep=""), sep="," , row.names=T, col.names=F, append=T)
  }
  write.table(x = data.frame(Theta), file = paste(directory,"Theta.csv",sep="/"), sep = ",", row.names=T, col.names=F, append=T)
  
}







delta.tune<-function(N.Grid, FF.index, C.inv, V.inv,  Z.t, f.t){
  
  nn = length(f.t)
  delta.t = seq(from = 1/2, to = .99, length = N.Grid)
  
  
  cost.star = -1000
  lambda.star = 1
  for(j in 1:N.Grid){
    Q.inv= V.inv - V.inv%*%FF.index%*% solve(delta.t[j]*C.inv + t(FF.index)%*%V.inv%*%FF.index) %*%t(FF.index)%*%V.inv
    
    if(det(Q.inv)<0){next}
    cost = .5*log(det(Q.inv) ) - as.numeric( t(Z.t - f.t )%*%Q.inv%*%(Z.t - f.t ) )
    
    if(j==1){
      lambda.star = delta.t[1]
    }else{
      if(cost>cost.star){
        cost.star = cost
        lambda.star = delta.t[j]
      }
    }
    
  }
  return(lambda.star)
}

Block.Invert <-function(K,Q,n.Groups,epsilon){
  
  Blocks = list()
  Q.inv = matrix(0,nrow = dim(Q)[1], ncol = dim(Q)[1])
  
  for(k in 1:K){
    blk.index.1 = 1+sum(n.Groups[1:k-1])
    blk.index.2 = sum(n.Groups[1:k])
    if(blk.index.2<blk.index.1){ next }
    Blocks[[k]] = Q[blk.index.1:blk.index.2,blk.index.1:blk.index.2] + epsilon*diag(n.Groups[k])
    Q.inv[blk.index.1:blk.index.2,blk.index.1:blk.index.2] = solve(Blocks[[k]])
  }
  
  return(Q.inv)
}