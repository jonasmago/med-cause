start_softmax = 0.3
start_lazy_sticky = -1
model_selection = 0:2
participant_selection = 1:1
fnscale = -1
################################
# functions
################################
logit <- function (a){
  exp(a)/(exp(a)+1)
}

logit_reverse <- function (y){
  log(y/(1-y))
}

softmax <- function (raw, temperature){
  exp(raw/temperature)/rowSums(exp(raw/temperature)) 
}

lazyness <- function (raw, lazyness_param){
  lazyness_logit = logit (lazyness_param)
  sweep(raw*(1-lazyness_logit), 2, c(0,0,1)*lazyness_logit,"+")
}

stickiness <- function (raw, raw_previous, stickiness_param){
  stickiness_logit = logit(stickiness_param)
  raw*(1-stickiness_logit)+raw_previous*stickiness_logit
}


################################
# setting up df
################################


df.mc_likelihood <- data.frame(matrix("", nrow = (length(df.results_V2[,2])), ncol = 12))
colnames (df.mc_likelihood) <- c("real_up", "real_down","real_stay","PID_up","PID_down","PID_stay","lazy_up","lazy_down","lazy_stay", "bayes_up","bayes_down","bayes_stay")

# intervention: 
# 1 = up, 3=down, 0=stay

df.mc_likelihood[,1] = as.data.frame(ifelse (df.results_V2[,3]==1, 1, 0))
df.mc_likelihood[,2]= as.data.frame(ifelse (df.results_V2[,3]==3, 1, 0))
df.mc_likelihood[,3] = as.data.frame(ifelse (df.results_V2[,3]==0, 1, 0))
df.mc_likelihood[,4] = ifelse ((df.results_V2[,2]-df.results_V2[,5]) < 0, 1,0) #below (press up)
df.mc_likelihood[,5] = ifelse ((df.results_V2[,2]-df.results_V2[,5]) > 0, 1,0) #above (press down)
df.mc_likelihood[,6] = ifelse ((df.results_V2[,2]-df.results_V2[,5]) == 0, 1,0)
df.mc_likelihood[,7] =as.data.frame(rep(0,176000))
df.mc_likelihood[,8] = as.data.frame(rep(0,176000))
df.mc_likelihood[,9] = as.data.frame(rep(1,176000))
df.mc_likelihood[,10] = as.data.frame(df.bayes_V2[,5] * df.bayes_opt_response_V2[,1]+df.bayes_V2[,6] * df.bayes_opt_response_V2[,1]+df.bayes_V2[,7] * df.bayes_opt_response_V2[,4]+df.bayes_V2[,8] * df.bayes_opt_response_V2[,4])
df.mc_likelihood[,11] = as.data.frame(df.bayes_V2[,5] * df.bayes_opt_response_V2[,2]+df.bayes_V2[,6] * df.bayes_opt_response_V2[,2]+df.bayes_V2[,7] * df.bayes_opt_response_V2[,5]+df.bayes_V2[,8] * df.bayes_opt_response_V2[,5])
df.mc_likelihood[,12] = as.data.frame(df.bayes_V2[,5] * df.bayes_opt_response_V2[,3]+df.bayes_V2[,6] * df.bayes_opt_response_V2[,3]+df.bayes_V2[,7] * df.bayes_opt_response_V2[,6]+df.bayes_V2[,8] * df.bayes_opt_response_V2[,6])

#### IMPORTNAT: in the opt_resposne dataframe, moving up and moving down is confused!!!




df.mc_fitted_combined_V2 <- data.frame(matrix("", nrow = 1, ncol = 86))
for (i in 1:86){
  df.mc_fitted_combined_V2[,i] = as.numeric(df.mc_fitted_combined_V2[,i])
}

colnames (df.mc_fitted_combined_V2) <- c("baseline_MLE",
                             "baseline_BIC", 
                             "PID_softmax",
                             "PID_lazyness", 
                             "PID_stickiness",
                             "PID_lazyness_logit",
                             "PID_stickiness_logit",
                             "PID_MLE",
                             "PID_BIC",
                             "lazy_softmax",
                             "lazy_lazyness",
                             "lazy_stickiness",
                             "lazy_lazyness_logit",
                             "lazy_stickiness_logit",
                             "lazy_MLE",
                             "lazy_BIC",
                             "bayes_softmax",
                             "bayes_lazyness",
                             "bayes_stickiness",
                             "bayes_lazyness_logit",
                             "bayes_stickiness_logit",
                             "bayes_MLE",
                             "bayes_BIC",
                             "02_PID_softmax",
                             "02_PID_lazyness", 
                             "02_PID_stickiness",
                             "02_PID_lazyness_logit",
                             "02_PID_stickiness_logit",
                             "02_PID_MLE",
                             "02_PID_BIC",
                             "02_lazy_softmax",
                             "02_lazy_lazyness",
                             "02_lazy_stickiness",
                             "02_lazy_lazyness_logit",
                             "02_lazy_stickiness_logit",
                             "02_lazy_MLE",
                             "02_lazy_BIC",
                             "02_bayes_softmax",
                             "02_bayes_lazyness",
                             "02_bayes_stickiness",
                             "02_bayes_lazyness_logit",
                             "02_bayes_stickiness_logit",
                             "02_bayes_MLE",
                             "02_bayes_BIC",
                             "03_PID_softmax",
                             "03_PID_lazyness", 
                             "03_PID_stickiness",
                             "03_PID_lazyness_logit",
                             "03_PID_stickiness_logit",
                             "03_PID_MLE",
                             "03_PID_BIC",
                             "03_lazy_softmax",
                             "03_lazy_lazyness",
                             "03_lazy_stickiness",
                             "03_lazy_lazyness_logit",
                             "03_lazy_stickiness_logit",
                             "03_lazy_MLE",
                             "03_lazy_BIC",
                             "03_bayes_softmax",
                             "03_bayes_lazyness",
                             "03_bayes_stickiness",
                             "03_bayes_lazyness_logit",
                             "03_bayes_stickiness_logit",
                             "03_bayes_MLE",
                             "03_bayes_BIC",
                             "04_PID_softmax",
                             "04_PID_lazyness", 
                             "04_PID_stickiness",
                             "04_PID_lazyness_logit",
                             "04_PID_stickiness_logit",
                             "04_PID_MLE",
                             "04_PID_BIC",
                             "04_lazy_softmax",
                             "04_lazy_lazyness",
                             "04_lazy_stickiness",
                             "04_lazy_lazyness_logit",
                             "04_lazy_stickiness_logit",
                             "04_lazy_MLE",
                             "04_lazy_BIC",
                             "04_bayes_softmax",
                             "04_bayes_lazyness",
                             "04_bayes_stickiness",
                             "04_bayes_lazyness_logit",
                             "04_bayes_stickiness_logit",
                             "04_bayes_MLE",
                             "04_bayes_BIC")


###########################
#baseline
###########################
df.mc_fitted_combined_V2[,1] = log (1/3)*4000*34
df.mc_fitted_combined_V2[,2] = -2*df.mc_fitted_combined_V2[,1] + log(4000)

###########################
# optimizing the models
###########################

'''
j is the participants, set to 1:44
k is the model 
0 is PID
1 is lazy
2 is Bayes
'''


for (k in model_selection){
  l = (4+k*3): (6+k*3)
  print ("k // the model")
  print (k)
  
  for (j in 1:1){ #participants
    start = 1
    end = length(df.results_V2[,1])
    
    fn <- function(param) {
      data = df.mc_likelihood[start:end, l]
      data_previous = rbind (df.mc_likelihood[start, l],df.mc_likelihood[start:(end-1), l])
      p = softmax (data,param[1])
      p = lazyness (p, param[2])
      p = stickiness(p, data_previous, param[3])
      p = (df.mc_likelihood[start:end, 1:3] * p)
      return (sum (log (rowSums(p))))
    }
    
    fn_02 <- function(param) {
      data = df.mc_likelihood[start:end, l]
      p = softmax (data,param[1])
      p = lazyness (p, param[2])
      p = (df.mc_likelihood[start:end, 1:3] * p)
      return (sum (log (rowSums(p))))
    }
    
    fn_03 <- function(param) {
      data = df.mc_likelihood[start:end, l]
      data_previous = rbind (df.mc_likelihood[start, l],df.mc_likelihood[start:(end-1), l])
      p = softmax (data,param[1])
      p = stickiness(p, data_previous, param[2])
      p = (df.mc_likelihood[start:end, 1:3] * p)
      return (sum (log (rowSums(p))))
    }
    
    fn_04 <- function(param) {
      data = df.mc_likelihood[start:end, l]
      p = softmax (data,param[1])
      p = (df.mc_likelihood[start:end, 1:3] * p)
      return (sum (log (rowSums(p))))
    }
    
    
    opt_param = optim((c(start_softmax,start_lazy_sticky,start_lazy_sticky)), fn, control = list(fnscale=fnscale))
    df.mc_fitted_combined_V2[j,(3+k*7):(5+k*7)] = t(data.frame(opt_param[1]))
    df.mc_fitted_combined_V2[j,(8+k*7)] = opt_param[2] #MLE
    print ("model 1 complete")
    
    #model 2 // softmaz and lazyness
    opt_param = optim((c(start_softmax,start_lazy_sticky)), fn_02, control = list(fnscale=fnscale))
    df.mc_fitted_combined_V2[j,(3+k*7+21):(4+k*7+21)] = t(data.frame(opt_param[1]))
    df.mc_fitted_combined_V2[j,(8+k*7+21)] = opt_param[2] #MLE
    print ("model 2 complete")
    
    #model 3 // softmax and stickyness 
    opt_param = optim((c(start_softmax,start_lazy_sticky)), fn_03, control = list(fnscale=fnscale))
    df.mc_fitted_combined_V2[j,c((3+k*7+42),(5+k*7+42))] = t(data.frame(opt_param[1]))
    df.mc_fitted_combined_V2[j,(8+k*7+42)] = opt_param[2] #MLE
    print ("model 3 complete")
    
    #model 4 // only softmax
    opt_param = optim((c(start_softmax)), fn_04, method="Brent", lower=0, upper=1000, control = list(fnscale=fnscale))
    df.mc_fitted_combined_V2[j,(3+k*7+63)] = t(data.frame(opt_param[1]))
    df.mc_fitted_combined_V2[j,(8+k*7+63)] = opt_param[2] #MLE
    print ("model 4 complete")
    
    
  }
  for (m in 0:3){
    df.mc_fitted_combined_V2[,(6+k*7+21*m):(7+k*7+21*m)] = logit (df.mc_fitted_combined_V2[,(4+k*7+21*m):(5+k*7+21*m)])
  }
}



####################################
# computing BIC score
####################################
for (k in 0:2){
  m= 0; pun = 3; df.mc_fitted_combined_V2[,(9+k*7+21*m)] = -2*df.mc_fitted_combined_V2[,(8+k*7+21*m)]+3*log(4000)
  m= 1; pun = 2; df.mc_fitted_combined_V2[,(9+k*7+21*m)] = -2*df.mc_fitted_combined_V2[,(8+k*7+21*m)]+pun*log(4000)
  m= 2; pun = 2; df.mc_fitted_combined_V2[,(9+k*7+21*m)] = -2*df.mc_fitted_combined_V2[,(8+k*7+21*m)]+pun*log(4000)
  m= 3; pun = 1; df.mc_fitted_combined_V2[,(9+k*7+21*m)] = -2*df.mc_fitted_combined_V2[,(8+k*7+21*m)]+pun*log(4000)
}


####################################
# computing accuracy
# a bit of a weired measure... maybe take out?
####################################


frames = 4000*34
a=1:frames
print (sum (df.mc_likelihood[a,1:3] *df.mc_likelihood[a,4:6])/frames)
print (sum (df.mc_likelihood[a,1:3] *df.mc_likelihood[a,7:9])/frames)
print (sum (df.mc_likelihood[a,1:3] *df.mc_likelihood[a,10:12])/frames)
rm(a, frames)


