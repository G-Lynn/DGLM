Q.age<-function(age,alpha,decay.rate,K){
  
  Q = matrix(rep(0,K^2),nrow = K, ncol = K)
  for(k in 1:K){
    
    for(tau in 1:K){
      
      if(age<=30){
        if(tau>=k) Q[k,tau] = exp(-alpha*abs(m0[k]-m0[tau]) )
        if(tau<k) Q[k,tau] = exp(-decay.rate*alpha*abs(m0[k]-m0[tau]) )
      }
      if(age>30 & age<=35){
        Q[k,tau] = exp(-alpha*abs(m0[k]-m0[tau]) )
      }
      
      if(age>35){
        if(tau>=k ) Q[k,tau] = exp(-decay.rate*(2/3)*alpha*abs(m0[k]-m0[tau]))
        if( k>tau ) Q[k,tau] = exp(-(2/3)*alpha*abs(m0[k]-m0[tau]) )
      }      
    }
  }
  Q = Q/apply(Q,1,sum)
  return(Q)
}  

