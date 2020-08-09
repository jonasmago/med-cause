#rm(list = ls())
'''
part1 <- read.csv("CSV/mTurk/v2_01.csv", header=FALSE) #other name is: v2_01
part2 <- read.csv("CSV/mTurk/final.csv", header=FALSE) #other name is: v2_01
part3 = dplyr::bind_rows(part1, part2)
write.csv(part3, file = "v2_01_and_final.csv", row.names = F, col.names = F)
'''

# Libraries  #############
#libraries
library(tidyverse)
library(jsonlite)
library(dplyr)
library(rjson)
library(purrr)
library(tidyr)
library (anytime)
library(ggplot2)
library(grid)
library(gridExtra)
library (rstatix)
library(ggpubr)
library(ez)
library(lme4)
library(nlme)


#avoid null values for the json to df transformation
protect_against_null <- function( x ) {
  if( is.null(x) )
    return( NA ) # Replace with whatever string you'd like.
  else 
    return( x )
}

#import data #######
df <- read.csv("CSV/mTurk/v2_01_and_final.csv", header=T) #other name is: v2_01
names(df) <- c('id','date','subject_str','results_str')

#convert JSON to df
for (a in 1:dim(df)[1])
{
  df[a,3] = str_replace_all(df[a,3], "\n", " ")
  subject <- jsonlite::fromJSON(df[a,3]);
  subject= as.data.frame(subject);
  IP = df[a,1]
  duration = difftime (anytime(c(subject[1,6])), anytime(c(subject[1,5])))
  
  results <- jsonlite::fromJSON(df[a,4]);
  results= as.data.frame(protect_against_null(results));
  
  #get scores and conditions
  timeout = dim(results)[1]/4
  score=c(results[timeout,which(colnames(results)=="score")], results[timeout*2,which(colnames(results)=="score")], results[timeout*3,which(colnames(results)=="score")], results[timeout*4,which(colnames(results)=="score")])
  score_total=sum(score)
  condition=c(results[timeout,which(colnames(results)=="condition")], results[timeout*2,which(colnames(results)=="condition")], results[timeout*3,which(colnames(results)=="condition")], results[timeout*4,which(colnames(results)=="condition")])
  subject [2]=score_total
  names(subject)[2] <- "score_total"
  subject = mutate(subject, duration, score[1], score[2], score[3], score[4], condition[1], condition[2], condition [3], condition [4])
  rm(score, score_total,condition, timeout, duration, IP)
  
  #clean result df
  results = select (results,-c(setup.omega,setup.sigma,setup.theta, setup.order))
  results = mutate (results, participant_id = a)
  
  if (a == 1) {
    results_all = results
    subject_all = subject
  } else {
    subject_all = dplyr::bind_rows(subject_all, subject)
    results_all = dplyr::bind_rows(results_all, results)
  }
  
}

df.results = results_all
df.subject = subject_all
rm (results_all, subject_all, df, a, protect_against_null, results, subject)
#adjust classes
df.subject [,3] = as.numeric(df.subject[,3])
df.subject [,13] = as.integer(df.subject[,13])
df.subject [,15] = as.integer(df.subject[,15])
df.subject [,16] = as.integer(df.subject[,16])
df.results[,11] = as.numeric(df.results[,11])
df.subject$gender = factor(df.subject$gender)

#add in target parameter
in_target[] = c((df.results[,4])[-1],0) -df.results[,4]

for (i in 1:44){
  in_target[i*4000]=in_target[i*4000-1]
}

df.results["in_target"] <- in_target

