#######################################
# Libraries 
#######################################

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

################################
# Data Information 
################################

colnames(df.subject)
colnames(df.results)
summary(df.subject)
summary(df.results)

# participant information 

#gender
paste ("mean age ",  mean (as.numeric(df.subject [,13])),"SD", sqrt(var (as.numeric(df.subject [,13]))))
paste ("male:",length(subset (df.subject, gender=="male")$gender))
paste ("female:",length(subset (df.subject, gender=="female")$gender))
paste ("other:",length(subset (df.subject, gender=="other")$gender))
paste ("noresponse:",length(subset (df.subject, gender=="noresponse")$gender))

#bonus
summary (df.results$bonus)
print ("SD")
sqrt(var((df.subject [,3])))

#duration
mean(df.subject [,17])
sqrt(var((df.subject [,17])))

#engagement
mean(df.subject [,16])
sqrt(var((df.subject [,16])))
#difficulty
mean(df.subject [,15])
sqrt(var((df.subject [,15])))


################################
# Data Visualization 
################################

####################################
#hist_subject_data
####################################


plot1 <- ggplot(data = df.subject, aes(x=as.numeric(bonus)))+
  geom_histogram(binwidth=0.1)+
  labs (title= "Bonus Pay", x="Bonus in $", y="Participants")+
  theme_grey(base_size = 20)
plot2 <- ggplot(data = df.subject, aes(x=duration))+
  geom_histogram(binwidth=0.5)+
  labs (title= "Duration", x="Duraion in Minutes", y="Participants")+
  theme_grey(base_size = 20)
plot3 <- ggplot(data = df.subject, aes(x=as.numeric(engagement)))+
  geom_histogram()+
  labs (title= "Engagement", x="Self-Rated Engagement", y="Participants")+
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  theme_grey(base_size = 20)
plot4 <- ggplot(data = df.subject, aes(x=as.numeric(difficulty)))+
  geom_histogram()+
  labs (title= "Difficulty", x="Self-Rated Difficulty", y="Participants")+
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  theme_grey(base_size = 20)
grid.arrange(plot1, plot2, plot3, plot4); rm(plot1, plot2, plot3, plot4)


####################################
#movement sliders
####################################


a = 0 # 1-9
#a = 36 #10-18
#a = 72 #19-27
#a = 108 #28-36
#a = 144 #37-45

p <- list()
for (i in 1:36)
  p[[i]] <- ggplot(data = df.results[(((i+a)*1000)-999):((i+a)*1000),], aes(x=count))+
  geom_path(aes(y=val_left))+
  geom_line(aes(y=reward_centre), size=1, alpha=0.3)+
  labs (title= paste ("Left, ", "Part.: ", df.results [(i+a)*1000,13]," Con.: ", df.results [(i+a)*1000,9]),x=element_blank(), y=element_blank())
do.call(grid.arrange,p); rm(p, a, i)




####################################
# behavioural characteristics 
# do the sliders move more or less between the various conditions?
####################################



df.movement = data.frame (1, 2, 3, 4, 5)
colnames (df.movement) <- c("stationary_left", "total_movement_left", "total_movement_right", "participant", "condition")

for (i  in 1:176){

  a=(i-1)*1000
  b = (df.results[(1+a):(999+a),1])-df.results[(2+a):(1000+a),1]
  b = as.data.frame(b)
  x=length(which(b == 0)) 
  y=sum(abs(b))
  
  b = (df.results[(1+a):(999+a),2])-df.results[(2+a):(1000+a),2]
  b = as.data.frame(b)
  z=sum(abs(b))
  participant = df.results[(1+a),13]
  condition =df.results[(1+a),9]
  
  df.movement[i,] = data.frame(x,y,z, participant, condition)
}

df.movement$condition = factor(df.movement$condition, levels = c(1:4), labels = c('regular', 'offset','reversal','reversal-offset'))


ggplot(data = df.movement, aes(condition, stationary_left))+
  geom_boxplot()+geom_point()

ggplot(data = df.movement, aes(condition, total_movement_left))+
  geom_boxplot()+geom_point()

ggplot(data = df.movement, aes(condition, total_movement_right))+
  geom_boxplot()+geom_point()

ggplot(data = df.movement, aes(stationary_left))+
  geom_histogram()

#test for difference across conditions: First the ANOVA to test for overall differences, then a pairwise t-test with bonferoni correction to test for differences between any condition
p<-ezANOVA(df.movement, dv = stationary_left, wid=participant, within = condition)
print(p)

pwc <- df.movement %>%
  pairwise_t_test(
    stationary_left ~ condition, paired = TRUE, var.equal=T,
    p.adjust.method = "bonferroni"
  )
pwc

p<-ezANOVA(df.movement, dv = total_movement_left, wid=participant, within = condition)
print(p)

pwc <- df.movement %>%
  pairwise_t_test(
    total_movement_left ~ condition, paired = TRUE, var.equal=T,
    p.adjust.method = "bonferroni"
  )
pwc


p<-ezANOVA(df.movement, dv = total_movement_right, wid=participant, within = condition)
print(p)

pwc <- df.movement %>%
  pairwise_t_test(
    total_movement_right ~ condition, paired = TRUE, var.equal=T,
    p.adjust.method = "bonferroni"
  )
pwc


for (i in c("regular", "reversal","offset", "reversal-offset")){
  print (i)
  print (mean (subset(df.movement, condition == i)$stationary_left))
  print (sqrt(var (subset(df.movement, condition == i)$stationary_left)))
}




####################################
#check if it was in focus 
####################################

print ("phrames during which window was not in focus (out of 4000)")
p <- list()
p<-data.frame()
for (i in 1:dim (df.subject)[1]){
  p[i,1] = as.numeric((dim (subset (df.results, focus == 0 & participant_id == i)) [1]))
}; rm (i)
p

1-mean(df.results$focus)
sqrt(var(df.results$focus))


ggplot(data = p, aes(x=V1))+
  geom_histogram(binwidth=10)+
  labs (title= "focus on task", x="frames during which window is not in focus (out of 4000)", y="participants")

#ggsave("plots/focus_on_task.png")

####################################
#bonus correlation
####################################

plot1<-ggplot(data = df.subject, aes(y=bonus, x=difficulty))+
  geom_jitter(width = 0.1)+
  labs (title= "Difficulty - Bonus Correlation", x="Difficulty (1-10 scale)", y="Bonus")+
  geom_smooth(method = "lm", se = TRUE)+
  theme_grey(base_size = 20)
plot2<-ggplot(data = df.subject, aes(y=bonus, x=engagement))+
  geom_jitter(width = 0.1)+
  labs (title= "Engagement - Bonus Correlation", x="Engagement (1-10 scale)", y="Bonus")+
  geom_smooth(method = "lm", se = TRUE)+
  theme_grey(base_size = 20)
plot3<-ggplot(data = df.subject, aes(y=bonus, x=duration))+
  geom_jitter(width = 0.01)+
  labs (title= "Duration - Bonus Correlation", x="Duration in Minutes", y="Bonus")+
  geom_smooth(method = "lm", se = TRUE)+
  theme_grey(base_size = 20)
plot4 <-ggplot(data = df.subject, aes(y=bonus, x=false_attempts))+
  geom_jitter(width = 0.1)+
  labs (title= "False Attempts - Bonus Correlation", x="False Attempts", y="bonus")+
  geom_smooth(method = "lm", se = TRUE)+
  theme_grey(base_size = 20)
grid.arrange(plot1, plot2, plot3, plot4); rm(plot1, plot2, plot3, plot4)


#linear regression statistics
p <- lm(bonus ~ difficulty, data = df.subject)
summary (p)
p <- lm(bonus ~ engagement, data = df.subject)
summary (p)
p <- lm(bonus ~ duration, data = df.subject)
summary (p)
p <- lm(bonus ~ false_attempts, data = df.subject)
summary (p)
p <- lm(bonus ~ age, data = df.subject)
summary (p)



##############################
#correlation of score across conditions
##############################

df.stack = data.frame (1, 2, 3, 4, 5)
colnames (df.stack) <- c("id", "score", "condition", "condition_count", "participant")
for (i in 1:(length (df.subject[,1])*4)){
  a = i
  b = df.results [i*1000,4] - df.results [(i*1000)-999,4]
  c = df.results [i*1000,9]
  d = df.results [i*1000,8]
  e = df.results [i*1000,13]
  df.stack[i,] = data.frame(a, b, c, d,e)
}; rm(a,b, c,d,e,i)
df.stack$condition = factor(df.stack$condition, levels = c(1:4), labels = c('regular', 'offset','reversal','reversal-offset'))
df.stack$condition_count = factor(df.stack$condition_count)
df.stack$participant = factor(df.stack$participant)

#boxplot
ggplot(data = df.stack, aes(x = factor(condition), y=score))+
  geom_boxplot()+geom_point()+
  #geom_bar(stat="identity")+
  labs (title= "score by condition", x="condition", y="score (6 points are equivalent to $0.01 bonus)")+
  theme_grey(base_size = 24)
#ggsave("plots/score_by_condition.png")

#testing for outliers
df.stack %>%
  group_by(condition) %>%
  identify_outliers(score)

#test for normal distribution (if p<0.5 --> then not normally distributed by Shapiro-Wilk???s test)
df.stack %>%
  group_by(condition) %>%
  shapiro_test(score)

#a within subject ANOVE to test for overall differences
#alternatively, I could use lm(score ~condition, data = df.stack), then I would have to make those comparisons that are of most interest
#bonferoni corrections to account for too many tests (I have as many tests as df)
#Helmholt coding --> 1 agasint 2-4, 2 agasint 3-4,... to make those comparisons that are of most interest

p<-ezANOVA(df.stack, dv = score, wid=participant, within = condition)
print(p)

#as there is a significant difference overall, I can make a comparison of each condition 
pwc <- df.stack %>%
  pairwise_t_test(
    score ~ condition, paired = TRUE, var.equal=T,
    p.adjust.method = "bonferroni"
  )
pwc


# comparing being in target region VS anywhere
# target region is 20 big, whole thing is 200 big --> 0.1
t.test(rep(100,176), score$score, mu=0, paired=T)
mean(score [,2])
sqrt(var((score [,2])))

dim (df.results[-(-length(df.results))],)
dim (df.results)

#change in performance within condition 

data = df.results
data = (subset(df.results, count > 100))
data = (subset(df.results, count > 100 & condition ==4))
p<-lm(in_target~count, data=data)
summary(p)

# linear mixed effect measures --> don't understand this part yet...
p<-lme(in_target ~ count, random = ~ count | condition | participant_id, data=df.results)
p <- lmer(in_target ~ count + (1| participant_id)+ (1| condition), data=df.results)
summary (p)

coeff = 3
p<-ggplot(data = df.results, aes(x = count))+
  geom_smooth(aes(y=in_target))+
  geom_line(aes(y=sin/3+0.3), size=1, alpha=0.2)+
  labs(title= "Change in Performacne Within Condition", x="Frames")+
  scale_y_continuous(
    name = "Proportional Earnings",
    sec.axis = sec_axis(~.*coeff-1.4, name="Moderatig Sin Function"))+
  theme_grey(base_size = 18)
p
p + facet_grid(cols = vars(condition))


####################################
# behavioural evidence for PID control (take this out?)
####################################
PID_behaviour = data.frame()
for (j in 1:176){
  relation = data.frame()
  for (i in 1:1000){
    k = ((j-1)*1000)+i
    a = results [k,2] - results [k,5]
    if (a > 0)
      a = 1
    else 
      a=0
    relation [i,1] = a
  }
  # 1 = right slider is above 
  PID_behaviour[j,1]
  
  PID_behaviour[j,1] = sum (relation [c(1:188,566:1000),1]) /623 # length 623 (it's moving up, we want it below, SMALL)
  PID_behaviour[j,2] = sum (relation [189:565,1]) / 377 # length 377 it's moving down, we want it above (HIGHT)
}
names(PID_behaviour) = c("moving_up", "moving_down")
stacked_PID_behaviour = stack(PID_behaviour)
ggplot(data = stacked_PID_behaviour, aes(x = factor(ind), y=values))+geom_boxplot()+
  labs (title= "Right slider movement dependant on motion of target region", x="movement of target region", y="proportion of time the right slider is above the target region")
  ggsave("plots/right_slider_movement.png")

  
t.test(PID_behaviour$moving_up, PID_behaviour$moving_down, paired=T)

mean(PID_behaviour [,1])
sqrt(var(PID_behaviour [,1]))

mean(PID_behaviour [,2])
sqrt(var(PID_behaviour [,2]))

################################
# ?
################################
p <-cor.test(subject$false_attempts, subject$bonus)
p

####################################
# does bonus depend upon the order of condition? (boxplot)
####################################
plot1 <-  ggplot(data = subset(score, condition==1), aes(x = factor(condition_count), y=score))+geom_boxplot()+geom_point()+labs (title= paste ("bonus depending on order for condition 1"), x="condition_count /order", y="score")
plot2 <-  ggplot(data = subset(score, condition==2), aes(x = factor(condition_count), y=score))+geom_boxplot()+geom_point()+labs (title= paste ("bonus depending on order for condition 2"), x="condition_count /order", y="score")
plot3 <-  ggplot(data = subset(score, condition==3), aes(x = factor(condition_count), y=score))+geom_boxplot()+geom_point()+labs (title= paste ("bonus depending on order for condition 3"), x="condition_count /order", y="score")
plot4 <-  ggplot(data = subset(score, condition==4), aes(x = factor(condition_count), y=score))+geom_boxplot()+geom_point()+labs (title= paste ("bonus depending on order for condition 4"), x="condition_count /order", y="score")
grid.arrange(plot1, plot2, plot3, plot4)

  
