#PID controller 

#firstly, computing the PID response:

'''
1 = up
3 = down
0 = nothing
'''

relative_right_target = df.results[,2]-df.results[,5]
key_response = df.results[,3]

#compare the location of the right slider with the location of the target region 
up=ifelse (relative_right_target<= 0, 1, 5)
down=ifelse (relative_right_target> 0, 3, 5)
stay = ifelse(relative_right_target > -10, 0, 5) - ifelse(relative_right_target < 10, 0, -5)

up_match = as.data.frame(up)-key_response
down_match = as.data.frame(down)-key_response
stay_match = as.data.frame(stay)-key_response


#computing the number of times the PID response matches participants responses
df.PID = as.data.frame(0)
for (i in 1:44){
  offset = (i-1)*4000
  df.PID[i,1] =  length(which(up_match[(1+offset):(4000+offset),1] == 0)) + length(which(down_match[(1+offset):(4000+offset),1] == 0))
  df.PID[i,2] =  length(which(up_match[(1+offset):(4000+offset),1] == 0)) + length(which(down_match[(1+offset):(4000+offset),1] == 0)) + length(which(stay_match[(1+offset):(4000+offset),1] == 0))
}

noise = 0.00001
log(1-(noise*2))*{df.PID[1,1]}+log(noise)*{4000-df.PID[1,1]}


#optimizing for PID controller that always wants to move
for (i in 1:44){
  fn <- function( noise ) {
    log(1-(noise*2))*{df.PID[i,1]}+(log(noise)*{4000-df.PID[i,1]})
  }
  optimisation = optim(noise, fn, method="Brent", lower=0, upper=0.334, control = list(fnscale= -1))
  df.PID[i,3:4] = optimisation[1:2]
}


#optimizing for PID controller that allows for remaining in the target region 
for (i in 1:44){
  fn <- function( noise ) {
    log(1-(noise*2))*{df.PID[i,2]}+(log(noise)*{4000-df.PID[i,2]})
  }
  optimisation = optim(noise, fn, method="Brent", lower=0, upper=0.334, control = list(fnscale= -1))
  df.PID[i,5:6] = optimisation[1:2]
}

df.PID 
colnames (df.PID) <- c("accuracy_strict_PID", "accuracy_relaxed_PID", "noise_value_strict", "MLE_strict", "noise_value_relaxed","MLE_relaxed")


#compute the BIC score

k=1
n = 4000
BIC = as.data.frame(-2*df.PID[,4]+k*log(n))
colnames(BIC) <- c("BIC_strict")
df.PID = cbind (df.PID, BIC)
mean (BIC[,1])

BIC = as.data.frame(-2*df.PID[,6]+k*log(n))
colnames(BIC) <- c("BIC_relaxed")
df.PID = cbind (df.PID, BIC)
mean (BIC[,1])


#########################
# param removal
#########################
rm(relative_right_target, key_response)
