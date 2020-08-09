#lazy controler, does not press any keys
'''
1 = up
3 = down
0 = nothing
'''
key_response = as.data.frame(df.results[,3])

#computing the number of times the PID response matches participants responses
df.lazy = as.data.frame(0)
for (i in 1:44){
  offset = (i-1)*4000
  df.lazy[i,1] =  length(which(key_response[((1+offset):(4000+offset)),1] == 0))
}


#optimizing for PID controller that always wants to move
for (i in 1:44){
  fn <- function( noise ) {
    log(1-(noise*2))*{df.lazy[i,1]}+log(noise)*{4000-df.lazy[i,1]}
  }
  optimisation = optim(noise, fn, method="Brent", lower=0, upper=0.5, control = list(fnscale= -1))
  df.lazy[i,2:3] = optimisation[1:2]
}

colnames (df.lazy) <- c("accuracy_lazy", "noise_value_lazy", "MLE_lazy")

k=1
n = 4000
BIC = as.data.frame(-2*df.lazy[,3]+k*log(n))
colnames(BIC) <- c("BIC_lazy")
df.lazy = cbind (df.lazy, BIC)


mean (BIC[,1])