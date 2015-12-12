Q.age<-function(age,alpha,decay.rate,K, PI.0){
  
  Q = matrix(rep(0,K^2),nrow = K, ncol = K)
  for(k in 1:K){
    
    for(tau in 1:K){
      
      if(age<=28){
        if(tau>=k) Q[k,tau] = exp(-alpha*abs(PI.0[k]-PI.0[tau]) ) 
        if(tau<k) Q[k,tau] = exp(-decay.rate*alpha*abs(PI.0[k]-PI.0[tau]) ) 
      }
      if(age>28 & age<=33){
        Q[k,tau] = exp(-alpha*abs(PI.0[k]-PI.0[tau]) )
      }
      
      if(age>33){
        if(tau>=k ) Q[k,tau] = exp(-decay.rate*alpha*abs(PI.0[k]-PI.0[tau]))
        if( k>tau ) Q[k,tau] = exp(-(1/2)*alpha*abs(PI.0[k]-PI.0[tau]) )
      }      
    }
  }
  Q = Q/apply(Q,1,sum)
  return(Q)
}  

