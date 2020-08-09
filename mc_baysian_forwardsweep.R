#lazy controler, does not press any keys
'''
1 = up
3 = down
0 = nothing
'''

###########################
# setting up functions for likelihood computation
###########################
model_condition_1 <- function( i ) {
  df.results[i,1]
}
model_condition_2 <- function( i, offset ) {
  p = df.results[i,1]+offset
  if (p>100){
    return (100)
  } else if (p< - 100){
    return (-100)
  } else {
    return (p)
  }
}

model_condition_3 <- function( i, reversal ) {
  p = reversal*df.results[i,1]
  if (p>100){
    return (100)
  } else if (p< - 100){
    return (-100)
  } else {
    return (p)
  }
}


model_condition_4 <- function( i, offset, reversal ) {
  p = reversal*df.results[i,1]+offset
  if (p>100){
    return (100)
  } else if (p< - 100){
    return (-100)
  } else {
    return (p)
  }
}


df.bayes <- data.frame(li_model_1=integer(),
                 li_model_2=integer(),
                 li_model_3=integer(),
                 li_model_4=integer(),
                 post_model_1=integer(),
                 post_model_2=integer(),
                 post_model_3=integer(),
                 post_model_4=integer())


########################################################
# for loop to generate likelihoods
########################################################

#for participants loop

for (j in 1:(length(df.results[,1])/1000)){
  print (j)
  range = ((1+((j-1)*1000)):(1000+((j-1)*1000)))
  prior <- rep(1/4,4)
  for (i in range){
    offset = ((df.results[i,6])*120-60)
    reversal = ((df.results[i,6])*3-1.5)
    prediction <- c(model_condition_1(i), model_condition_2(i, offset), model_condition_3(i, reversal), model_condition_4(i, offset, reversal))
    li <- dnorm((df.results[i,2]-prediction),mean=0,sd=6)
    li <- li / sum(li)
    post.un<-li*prior
    post <- post.un/sum(post.un)
    df.bayes[i,1:4] <- li
    df.bayes[i,5:8] <- post
    prior <- post
    }
}

'''
plot(df.bayes[,5])
plot(df.bayes[,6])
plot(df.bayes[,7])
plot(df.bayes[,8])

'''

##############################
# computing best response for each model NEW
##############################

df.bayes_opt_response_NEW <-data.frame(model_1_2_up=integer(),
                                      model_1_2_down=integer(),
                                      model_1_2_stay=integer(),
                                      model_3_4_up=integer(),
                                      model_3_4_down=integer(),
                                      model_3_4_stay=integer())

df.bayes_opt_response_NEW[length(df.results[,1]),6]=0

relative_loc = df.results[,2] - df.results[,5] 
action_up = ifelse (relative_loc < 0, 1, 0) #go up
action_down = ifelse (relative_loc > 0, 1, 0) #go down 
action_stay = ifelse (relative_loc == 0, 1, 0) #stay

df.bayes_opt_response_NEW[,1] = action_up
df.bayes_opt_response_NEW[,2] = action_down
df.bayes_opt_response_NEW[,3] = action_stay


relative_loc = df.results[,2] - df.results[,5] 
reversal = df.results[,6]-0.5
action_up = ifelse (relative_loc * reversal < 0, 1, 0) #go up
action_down = ifelse (relative_loc * reversal > 0, 1, 0) #go down 
action_stay = ifelse (relative_loc * reversal == 0, 1, 0) #stay

df.bayes_opt_response_NEW[,4] = action_up
df.bayes_opt_response_NEW[,5] = action_down
df.bayes_opt_response_NEW[,6] = action_stay


df.bayes_opt_response 
df.bayes_opt_response_NEW

##############################
# computing best response for each model
##############################
'''
bayes_opt_response_with_reversal <- function (i){
  print (i)
  slider_above = df.results[i,2]-df.results[i,5] <=0
  reversal_region = df.results[i,6] <=0.5
  
  if (slider_above && reversal_region) {
    return (c(1,0,0))
  } else if (slider_above) {
    return (c(0,1,0))
  } else if (reversal_region) {
    return (c(0,1,0))
  } else {
    return (c(1,0,0))
  }
}

bayes_opt_response_no_reversal <- function (i){
  
  slider_above = df.results[i,2]-df.results[i,5] <=0
  reversal_region = df.results[i,6] <=0.5
  
  if (slider_above){
    return (c(0,1,0))
  } else {
    return (c(1,0,0))
  }
}


  df.bayes_opt_response <-data.frame(model_1_2_up=integer(),
                                      model_1_2_down=integer(),
                                      model_1_2_stay=integer(),
                                      model_3_4_up=integer(),
                                      model_3_4_down=integer(),
                                      model_3_4_stay=integer())


for (i in 1:length(df.results[,1])){
  with_reversal =  bayes_opt_response_with_reversal (i)
  no_reversal = bayes_opt_response_no_reversal (i)
  df.bayes_opt_response[i,1:3] = no_reversal
  df.bayes_opt_response[i,4:6] = with_reversal
}
'''