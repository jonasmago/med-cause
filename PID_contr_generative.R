'''
HOW TO:
1) change condition 
2) select the solving strategy by uncommenting the right line at the end of the loop (roughly line 183)
'''
#preparation ###########
library(dplyr)
library(ggplot2)
library(gridExtra)



condition_all = 1:4
participants_all = 1:5
rm (df_all)
#################################
#simulation 
#################################
#PID-controller
reward_width = 10 
inc = 1000
omega = 0.1 #Springiness of OU process
steps = 1000

count = 0
score = 0
cur_val_left = 0
cur_val_right = 0
int_state = 0 



df <- data.frame(val_left = character (), 
                 val_right = character (),
                 score = character (),
                 reward_centre = character (),
                 sin_count = character (),
                 condition = character (),
                 count = character (),
                 bonus = character (),
                 PID_response = character (),
                 participant = character (),
                 stringsAsFactors=FALSE) 

#OU process function 
OU <- function (x0, cause, omega, sigma)
  return (x0 + omega*(cause-x0) + sigma*myNorm())	

#Generates a standard normal using Box-Mueller transformation
myNorm <- function (){
    repeat {
    x1 = 2 * runif(1,0,1) -1
    x2 = 2 * runif(1,0,1) -1
    rad = (x1 * x1) +( x2 * x2)
    if (rad < 1 && !(rad == 0)) {
      break
    }}
    c = sqrt(-2 * log(rad) / rad)
    return (x1 * c);
}

#PID CONTROLLER  
PID <- function (cur_val_right, reward_centre){
  if (cur_val_right < reward_centre-5){
    int_state = 1
  } else if (cur_val_right>reward_centre+5){
    int_state = 3}
  else {
    int_state = 0
  }
  return (int_state)
}

PID_history <- function (cur_val_right, reward_centre, df, count){
  if (count < 15){
    int_state = PID (cur_val_right, reward_centre)
    return (int_state)
  }else {
    history = 0
    for (i in (count-10):count){
      if ((df[i-1,1]>df[i-1,2] && df[i-1,2] > df[i,2]) || (df[i-1,1]<df[i-1,2] && df[i-1,2] <df[i,2]))
      {
        history = history +1
      }}
    if (history > 4){
    reversal_variable = -1
    }else {
      reversal_variable = 1}
    
    if (cur_val_right < reward_centre-5){
      int_state = 2-reversal_variable
    } else if (cur_val_right>reward_centre+5){
      int_state = 2+reversal_variable}
    else {
      int_state = 0
    }
    return (int_state)
}}

PID_plus <- function (cur_val_right, reward_centre, sin_count, condition){
  if (sin_count > 0.5 || condition == 1 || condition == 2){
     if (cur_val_right < reward_centre-5){
      int_state = 1
    } else if (cur_val_right>reward_centre+5){
      int_state = 3}
    else {
      int_state = 0
    }
    }else {
    if (cur_val_right < reward_centre-5){
      int_state = 3
    } else if (cur_val_right>reward_centre+5){
      int_state = 1}
    else {
      int_state = 0
    }}
  return (int_state)
}

random <- sample(c(0,1,3), 1)
random <- function (){
  int_state = sample(c(0,1,3), 1)
  return (int_state)
}


#simulation
for (participant in participants_all){
  for (condition in condition_all){
    for (count in 1:steps){
      
      #setting background color
      sin_count = ((sin(count/30)+1)/2)
      alpha = ((sin_count*2)-1)*1.5
      beta = ((sin_count*2)-1)*50
      target_count = (sin(count/120))
      reward_centre = target_count*80
      reward_min = (reward_centre + reward_width)
      reward_max = (reward_centre - reward_width)
      
      # Create multiple condition statement
      if (int_state==0) {
        step_left = cur_val_left
      } else if (int_state==1) {
        step_left = cur_val_left + 5
      } else if (int_state==3) {
        step_left = cur_val_left - 5
      } else {
        print ("ERROR")
      }
      
      if (step_left>100)
      {
        step_left  = 100;
      } else if (step_left< (-100))
      {
        step_left = -100;
      }
      
      cur_val_left = step_left
      
      if (condition == 1) {
        attractor = (cur_val_left)	
      } else if (condition == 2) {
        attractor = ((cur_val_left + beta));
      } else if (condition == 3){
        attractor = ((alpha*cur_val_left));
      } else if (condition == 4){
        attractor = ((alpha*cur_val_left + beta));
      } 
      
      step_right = OU(cur_val_right,attractor,omega,3)
      
      if (step_right>100)
      {
        step_right  = 100
      } else if (step_right< (-100))
      {
        step_right = -100
      }
      
      cur_val_right = step_right
      
      if (cur_val_right <= (reward_centre + reward_width) && cur_val_right >= (reward_centre - reward_width))
      {		
        score = score +1
      }
      
      bonus = ((round (score / 6))/100);
      
      new_data = data.frame(cur_val_left,cur_val_right,score,reward_centre,sin_count,condition,count,bonus, int_state, participant)
      df  <- rbind(df, new_data)
      
      int_state = 0
      #int_state = PID (cur_val_right, reward_centre)
      #int_state = PID_plus (cur_val_right, reward_centre, sin_count, condition)
      #int_state = PID_history(cur_val_right, reward_centre, df, count)
      #int_state = random()
      #int_state = sample(c(1,3,0),1) #random response baseline
    }
    length = count
    count = 0
    score = 0
    cur_val_left = 0
    cur_val_right = 0
    int_state = 0 
    }}

#cleaning data ############
df_simulated = df   
df_PID = df
df_PID_score = subset (df_PID, count == 1000)
#rm (alpha, attractor, beta, bonus, con, condition_all, count, cur_val_left, cur_val_right, hist_data, inc, int_state, omega, participants_all, reward_centre, reward_max, reward_min, reward_width, score, sin_count, step_left, step_right, steps, target_count, myNorm, OU, PID, PID_history, PID_plus, random, df, condition, length, participant, new_data)


################################
#visualization 
################################
ggplot(data = df_simulated_PID, aes(x=as.numeric(bonus)))+
  geom_histogram(binwidth=0.1)+
  labs (title= "PID // bonus pay", x="bonus in $", y="participants")
#ggsave("plots/simulation/PID_bonus histogram.png")

ggplot(data = df_PID_score, aes(x = factor(condition), y=score))+ geom_boxplot()+geom_point()+
  labs (title= "PID simulation // score by condition", x="condition", y="score (6  points are equivalent to $0.01 bonus)")
#ggsave("plots/simulation/PID_score_by_condition.png")

#PID Behavioural data analysis
PID_PID_behaviour  = data.frame()
for (j in 1:40){
  relation = data.frame()
  for (i in 1:1000){
    k = ((j-1)*1000)+i
    a = df_PID [k,2] - df_PID [k,4]
    if (a > 0)
      a = 1
    else 
      a=0
    relation [i,1] = a
  }
  # 1 = right slider is above 
  PID_PID_behaviour[j,1] = sum (relation [c(1:188,566:1000),1]) /623 # length 623 (it's moving up, we want it below, SMALL)
  PID_PID_behaviour[j,2] = sum (relation [189:565,1]) / 377 # length 377 it's moving down, we want it above (HIGHT)
}
names(PID_PID_behaviour) = c("moving_up", "moving_down")
stacked_PID_behaviour = stack(PID_PID_behaviour)
ggplot(data = stacked_PID_behaviour, aes(x = factor(ind), y=values))+geom_boxplot()+
  labs (title= "Right slider movement dependant on motion of target region", x="movement of target region", y="proportion of time the right slider is above the target region")
#ggsave("plots/simulation/right_slider_movement.png")


###########################
# response profiles 
###########################
#graph movement of left slider
p <- list()
for (i in 1:20){
  id = (ceiling(i/4))
  con = c(1,2,3,4)[i-(ceiling(i/4)-1)*4]
  
  p[[i]] <- 
  ggplot(data = df_PID[(((i)*1000)-999):((i)*1000),], aes(y=cur_val_left, x=count))+
  geom_path()+
  geom_line(aes(y=reward_centre), size=1, alpha=0.3)+
  labs (title= paste ("Simulated Lazy id:",id,"Con:",con), x="count", y="position left slider")
}
do.call(grid.arrange,p)

#graph movement of right slider
p <- list()
for (i in 1:20){
  id = (ceiling(i/4))
  con = c(1,2,3,4)[i-(ceiling(i/4)-1)*4]
  
  p[[i]] <- 
    ggplot(data = df_PID[(((i)*1000)-999):((i)*1000),], aes(y=cur_val_right, x=count))+
    geom_path()+
    geom_line(aes(y=reward_centre), size=1, alpha=0.3)+
    labs (title= paste ("Simulated Random id:",id,"Con:",con), x="count", y="position right slider")
}
do.call(grid.arrange,p)

