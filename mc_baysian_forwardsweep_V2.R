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
  df.results_V2[i,1]
}
model_condition_2 <- function( i, offset ) {
  p = df.results_V2[i,1]+offset
  if (p>100){
    return (100)
  } else if (p< - 100){
    return (-100)
  } else {
    return (p)
  }
}

model_condition_3 <- function( i, reversal ) {
  p = reversal*df.results_V2[i,1]
  if (p>100){
    return (100)
  } else if (p< - 100){
    return (-100)
  } else {
    return (p)
  }
}


model_condition_4 <- function( i, offset, reversal ) {
  p = reversal*df.results_V2[i,1]+offset
  if (p>100){
    return (100)
  } else if (p< - 100){
    return (-100)
  } else {
    return (p)
  }
}


df.bayes_V2 <- data.frame(li_model_1=integer(),
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

for (j in 1:(length(df.results_V2[,1])/1000)){
  print (j)
  range = ((1+((j-1)*1000)):(1000+((j-1)*1000)))
  prior <- rep(1/4,4)
  for (i in range){
    offset = ((df.results_V2[i,6])*120-60)
    reversal = ((df.results_V2[i,6])*3-1.5)
    prediction <- c(model_condition_1(i), model_condition_2(i, offset), model_condition_3(i, reversal), model_condition_4(i, offset, reversal))
    li <- dnorm((df.results_V2[i,2]-prediction),mean=0,sd=6)
    li <- li / sum(li)
    post.un<-li*prior
    post <- post.un/sum(post.un)
    df.bayes_V2[i,1:4] <- li
    df.bayes_V2[i,5:8] <- post
    prior <- post
    }
}


##############################
# computing best response for each model
##############################
df.bayes_opt_response_V2 <-data.frame(model_1_2_up=integer(),
                                      model_1_2_down=integer(),
                                      model_1_2_stay=integer(),
                                      model_3_4_up=integer(),
                                      model_3_4_down=integer(),
                                      model_3_4_stay=integer())

df.bayes_opt_response_V2[length(df.results_V2[,1]),6]=0

relative_loc = df.results_V2[,2] - df.results_V2[,5] 
action_up = ifelse (relative_loc < 0, 1, 0) #go up
action_down = ifelse (relative_loc > 0, 1, 0) #go down 
action_stay = ifelse (relative_loc == 0, 1, 0) #stay

df.bayes_opt_response_V2[,1] = action_up
df.bayes_opt_response_V2[,2] = action_down
df.bayes_opt_response_V2[,3] = action_stay


relative_loc = df.results_V2[,2] - df.results_V2[,5] 
reversal = df.results_V2[,6]-0.5
action_up = ifelse (relative_loc * reversal < 0, 1, 0) #go up
action_down = ifelse (relative_loc * reversal > 0, 1, 0) #go down 
action_stay = ifelse (relative_loc * reversal == 0, 1, 0) #stay

df.bayes_opt_response_V2[,4] = action_up
df.bayes_opt_response_V2[,5] = action_down
df.bayes_opt_response_V2[,6] = action_stay
