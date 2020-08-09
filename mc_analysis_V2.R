'''
WHAT I WANT TO KNOW:
for each model:
- LogL (MLE)
- BIC
--> note that it is not fitted for entire data but summed across all
- Px
- Accuracy 
- fitted parameters and SD???


'''
########################
# overall fitting the model
########################


########################
# best fit by participant!
########################
colnames(df.mc_fitted_V2)
columns = c(2,9,30,51,72,16,37,58,79,23,44,65,86)

# with all
#columns = c(2,9,30,51,16,37,58,23,44,65) # without just softmax 
#columns = c(2,9,16,23) # only full modles
#columns = c(2,9,30,51,23,44,65) # without just softmax 

p = df.mc_fitted_V2[,columns]
p_min = as.data.frame(apply(p,1,min))
p_dif = p - data.frame(rep(p_min,length(columns)))
d = which((p_dif == 0),arr.ind=TRUE)
data.frame (table (d[,2]))


t(d)

colnames(p)

t(d)[1,]
##############################
# Chi Square test for participant distribution 
##############################
p = c(0, 5, 3, 0, 0, 0, 0, 0, 4, 27, 5, 0, 0)

chisq.test(p)

m= 0; pun = 3; 
-2*-193355.8

####################################
# ANOVA to test for difference between the two models 
####################################

####################################
# test for percentage of accuracy 
# IDEA: multiply actual response with the 
####################################
df.mc_likelihood[1:3]