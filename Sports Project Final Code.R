library(knitr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
library(corrplot)
library(kableExtra)
library(caret)
library(nnet)
library(MASS)


##Read in the csv file and duplicate

sraw <- read.csv("/Users/caseyb5712/Desktop/STAT_4800_dataset/2021 UVA Football Data-selected/2019 PFF All Plays.csv")
sraw1 <- sraw


#### DATA CLEANING ####

##Remove garbage time and change variable names for ease
sraw1 <- sraw1[sraw1$pff_GARBAGETIME==0,]



sraw1 <- sraw1 %>% 
  rename(quarter = pff_QUARTER,
         down = pff_DOWN,
         fieldpos = pff_FIELDPOSITION,
         endfieldpos = pff_PLAYENDFIELDPOSITION,
         driveendevent= pff_DRIVEENDEVENT,
         passresult = pff_PASSRESULT)


##Keep original columns for field position (precaution)
sraw1$ogfieldpos <- sraw1$fieldpos
sraw1$ogendfieldpos <- sraw1$endfieldpos



##Convert field position to a 0-100 scale
sraw1 <- mutate(sraw1, fieldpos = if_else(fieldpos<0,
                                          fieldpos*-1,100-fieldpos))

sraw1 <- mutate(sraw1, endfieldpos = if_else(endfieldpos<0,
                                          endfieldpos*-1,100-endfieldpos))



#Calculate points scored per drive ending
sraw1$points_scored <- 0

for (i in 1:nrow(sraw1))
  
{
  if (sraw1[i,"driveendevent"] == "TOUCHDOWN") {sraw1[i,"points_scored"] = 7}
  else if (sraw1[i,"driveendevent"] == "FUMBLE-TD") {sraw1[i,"points_scored"] = -7}
  else if (sraw1[i,"driveendevent"] == "SAFETY") {sraw1[i,"points_scored"] = -2}
  else if (sraw1[i,"driveendevent"] == "INTERCEPTION-TD") {sraw1[i,"points_scored"] = -7}
  else if (sraw1[i,"driveendevent"] == "FIELD GOAL") {sraw1[i,"points_scored"] = 3}
  else {0}
}


##Create two state columns
#One for down and distance to first down
#One for entire state including field position
sraw1$downtogo <- paste(sraw1$down, sraw1$pff_DISTANCE, sep =",")
sraw1$state<-paste(sraw1$down, sraw1$pff_DISTANCE, sraw1$fieldpos, sep =",")



##Create bin column for field position
sraw1$fieldposbin <- cut(sraw1$fieldpos,
                              labels = c("1-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50",
                                         "51-55","56-60","61-65","66-70",
                                         "71-75","76-80","81-85","86-90","91-95","96-100"),
                              breaks = seq(0,100,5))



##Create column for red zone vs open field identifier 
sraw1$zone <- NA

for (i in 1:nrow(sraw1))

{
  if (sraw1[i,"fieldpos"] <= 79) {sraw1[i,"zone"] = 'open'}
  else if (sraw1[i,"fieldpos"] > 79) {sraw1[i,"zone"] = 'red'}
}



##Create Yards to go bins

sraw1$disttogo <- NA

for (i in 1:nrow(sraw1))

{
  if (sraw1[i,"pff_DISTANCE"] <= 5) {sraw1[i,"disttogo"] = 'short'}
  else if (5 < sraw1[i,"pff_DISTANCE"] & sraw1[i,"pff_DISTANCE"] <= 10) {sraw1[i,"disttogo"] = 'med'}
  else if (10 < sraw1[i,"pff_DISTANCE"] & sraw1[i,"pff_DISTANCE"] < 20) {sraw1[i,"disttogo"] = 'long'}
  else if (sraw1[i,"pff_DISTANCE"] >= 20) {sraw1[i,"disttogo"] = 'extra long'}
}



#### PLAY TYPE SIMULATOR ####


## Clean data and create column for playtype (Field Goal, Punt, Pass, or Run)
run_pass <- sraw1[sraw1$pff_RUNPASS == 'R' | sraw1$pff_RUNPASS == 'P',]


tofind_1 <- c("MADE", "MISSED","BLOCKED", "RETURNED", "FAIR CATCH", "TOUCHBACK")

punt_fg_1 <- sraw1[grep(paste(tofind_1, collapse = "|"), sraw1$pff_KICKRESULT),]




punt_fg_1 <- punt_fg_1[punt_fg_1$pff_KICKRESULT != "FAKE - BLOCKED",]
punt_fg_1$pff_KICKRESULT<-gsub(" - IL","",as.character(punt_fg_1$pff_KICKRESULT)) 
punt_fg_1$pff_KICKRESULT<-gsub(" - M","",as.character(punt_fg_1$pff_KICKRESULT)) 
punt_fg_1$pff_KICKRESULT<-gsub(" - S","",as.character(punt_fg_1$pff_KICKRESULT)) 
punt_fg_1$pff_KICKRESULT<-gsub(" - IR","",as.character(punt_fg_1$pff_KICKRESULT)) 
punt_fg_1$pff_KICKRESULT<-gsub(" - WR","",as.character(punt_fg_1$pff_KICKRESULT)) 
punt_fg_1$pff_KICKRESULT<-gsub(" - HL","",as.character(punt_fg_1$pff_KICKRESULT)) 
punt_fg_1$pff_KICKRESULT<-gsub(" - WL","",as.character(punt_fg_1$pff_KICKRESULT)) 
punt_fg_1$pff_KICKRESULT<-gsub(" - B","",as.character(punt_fg_1$pff_KICKRESULT)) 
punt_fg_1$pff_KICKRESULT<-gsub(" - HR","",as.character(punt_fg_1$pff_KICKRESULT)) 
punt_fg_1<- punt_fg_1[punt_fg_1$pff_KICKRESULT != "FAKE - TOUCHBACK",]
punt_fg_1 <- punt_fg_1[punt_fg_1$pff_KICKRESULT != "FAKE - RETURNED",]
punt_fg_1<- punt_fg_1[punt_fg_1$pff_KICKRESULT != "FAKE - FAIR CATCH",]

plays <- rbind(run_pass, punt_fg_1)


plays$playtype <- NA


plays$playtype[plays$pff_KICKRESULT == "TOUCHBACK"] <- 'punt'
plays$playtype[plays$pff_KICKRESULT == "RETURNED"] <- 'punt'
plays$playtype[plays$pff_KICKRESULT == "FAIR CATCH"] <- 'punt'
plays$playtype[plays$pff_KICKRESULT == "MADE"] <- 'fg'
plays$playtype[plays$pff_KICKRESULT == "BLOCKED"] <- 'fg'
plays$playtype[plays$pff_KICKRESULT == "MISSED"] <- 'fg'
plays$playtype[plays$pff_RUNPASS == "R"] <- 'run'
plays$playtype[plays$pff_RUNPASS == "P"] <- 'pass'

plays <- plays[plays$down != 0,]


##Subset to plays within 20 yards of own endzone
plays_under_20 <- plays[plays$fieldpos <= 20,]

##Make playtype a factor for classification
plays_under_20$playtype <- as.factor(plays_under_20$playtype)
levels(plays_under_20$playtype)


##Split Data 
index_20 <- createDataPartition(plays_under_20$playtype, p = .70, list = FALSE)
train_20 <- plays_under_20[index_20,]
test_20 <- plays_under_20[-index_20,]

##Set Reference 
train_20$playtype <- relevel(train_20$playtype, ref = "fg")


#Model for 20 yard plays
multinom_model_20 <- multinom(playtype ~ down + fieldpos + pff_DISTANCE, data = plays_under_20)


##Repeat for open field plays
plays_open <- plays[plays$fieldpos > 20 & plays$fieldpos < 80,]

##Make playtype a factor for classification
plays_open$playtype <- as.factor(plays_open$playtype)
levels(plays_open$playtype)

index_open <- createDataPartition(plays_open$playtype, p = .70, list = FALSE)
train_open <- plays_open[index_open,]
test_open <- plays_open[-index_open,]

train_open$playtype <- relevel(train_open$playtype, ref = "fg")


# Model for open field plays
multinom_model_open <- multinom(playtype ~ down + fieldpos + pff_DISTANCE, data = plays_open)


##Repeat for red zone plays
plays_red <- plays[plays$fieldpos >= 80,]

##Make playtype a factor for classification
plays_red$playtype <- as.factor(plays_red$playtype)
levels(plays_red$playtype)


index_red <- createDataPartition(plays_red$playtype, p = .70, list = FALSE)
train_red <- plays_red[index_red,]
test_red <- plays_red[-index_red,]

train_red$playtype <- relevel(train_red$playtype, ref = "fg")


# Model for open field plays
multinom_model_red <- multinom(playtype ~ down + fieldpos + pff_DISTANCE, data = plays_red)





# Predicting the values for train datasets
train_20$play_prediction <- predict(multinom_model_20, newdata = train_20, "class")
train_open$play_prediction <- predict(multinom_model_open, newdata = train_open, "class")
train_red$play_prediction <- predict(multinom_model_red, newdata = train_red, "class")

# Building classification table
tab_20 <- table(train_20$playtype, train_20$play_prediction)
tab_open <- table(train_open$playtype, train_open$play_prediction)
tab_red <- table(train_red$playtype, train_red$play_prediction)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab_20))/sum(tab_red))*100,2)
round((sum(diag(tab_open))/sum(tab_open))*100,2)
round((sum(diag(tab_red))/sum(tab_red))*100,2)



plays_under_20$play_prediction <- predict(multinom_model_20, newdata = plays_under_20, "class")
plays_open$play_prediction <- predict(multinom_model_open, newdata = plays_open, "class")
plays_red$play_prediction <- predict(multinom_model_red, newdata = plays_red, "class")



# Playtype Simulator
playtype_sim <- function(testing.data){
  
  if (testing.data$fieldpos < 80 & testing.data$fieldpos > 20) {
    return(predict(multinom_model_open, newdata = testing.data, "class"))
  } 
  
  if (testing.data$fieldpos <= 20) {
    return(predict(multinom_model_20, newdata = testing.data, "class"))
  } 
  
  if (testing.data$fieldpos >= 80) {
    return(predict(multinom_model_red, newdata = testing.data, "class"))
  } 
}



##### Per Down Yards Gained Mixture Model Building ##### 

# Simulating 32 different normal distributions depending on states like this: (Open, 4th down, Medium)(Red, 1st down, Short)

#### Yards Gained Run Plots

run <- sraw1[sraw1$pff_RUNPASS == 'R',]
pass <- sraw1[sraw1$pff_RUNPASS == 'P',]

run$yardsgained <- run$pff_GAINLOSSNET
pass$yardsgained <- pass$pff_GAINLOSSNET

ggplot(run, aes(x=fieldposbin, y=yardsgained, fill=fieldposbin)) + geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=.5)+ guides(fill=FALSE) + coord_flip()


ggplot(run, aes(x=yardsgained, fill=fieldposbin)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity")



## Red zone vs open field 
zonegraph<- ggplot(run, aes(x=yardsgained, fill=zone)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity")


zonegraph + scale_fill_manual(values = c("blue", "red"))+ scale_color_manual(values = c("blue", "red"))



#### MIXTURE MODEL RUNS OPEN FIELD ####

set.seed(1)
library(mixtools)
runopen <- run[run$zone == 'open',]

plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}


## Open First Down
# Subset the data
run_open_1st_short <- runopen[runopen$down == 1 & runopen$disttogo == 'short',]
run_open_1st_med<- runopen[runopen$down == 1 & runopen$disttogo == 'med',]
run_open_1st_long<- runopen[runopen$down == 1 & runopen$disttogo == 'long',]
run_open_1st_xlong<- runopen[runopen$down == 1 & runopen$disttogo == 'extra long',]

#Normal Mixture Modes with two distributions
run_open_1st_shortgain <- run_open_1st_short$yardsgained
mixmdl_open_1st_short <- normalmixEM(run_open_1st_shortgain, k = 2)

run_open_1st_medgain <- run_open_1st_med$yardsgained
mixmdl_open_1st_med <- normalmixEM(run_open_1st_medgain, k = 2)

run_open_1st_longgain <- run_open_1st_long$yardsgained
mixmdl_open_1st_long <- normalmixEM(run_open_1st_longgain, k = 2)

run_open_1st_xlonggain <- run_open_1st_xlong$yardsgained
mixmdl_open_1st_xlong <- normalmixEM(run_open_1st_xlonggain, k = 2)


#Example mixture model plot for Run, Open Field, 1st down, Short 
data.frame(x = mixmdl_open_1st_short$x) %>%
  ggplot() +
  geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", 
                 fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl_open_1st_short$mu[1], mixmdl_open_1st_short$sigma[1], lam = mixmdl_open_1st_short$lambda[1]),
                colour = "red", lwd = 1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl_open_1st_short$mu[2], mixmdl_open_1st_short$sigma[2], lam = mixmdl_open_1st_short$lambda[2]),
                colour = "blue", lwd = 1.5) +
  ylab("Density")


## Open Second Down
run_open_2nd_short <- runopen[runopen$down == 2 & runopen$disttogo == 'short',]
run_open_2nd_med <- runopen[runopen$down == 2 & runopen$disttogo == 'med',]
run_open_2nd_long<- runopen[runopen$down == 2 & runopen$disttogo == 'long',]
run_open_2nd_xlong<- runopen[runopen$down == 2 & runopen$disttogo == 'extra long',]

run_open_2nd_shortgain <- run_open_2nd_short$yardsgained
mixmdl_open_2nd_short <- normalmixEM(run_open_2nd_shortgain, k = 2)

run_open_2nd_medgain <- run_open_2nd_med$yardsgained
mixmdl_open_2nd_med <- normalmixEM(run_open_2nd_medgain, k = 2)

run_open_2nd_longgain <- run_open_2nd_long$yardsgained
mixmdl_open_2nd_long <- normalmixEM(run_open_2nd_longgain, k = 2)

run_open_2nd_xlonggain <- run_open_2nd_xlong$yardsgained
mixmdl_open_2nd_xlong <- normalmixEM(run_open_2nd_xlonggain, k = 2)

## Open Third Down
run_open_3rd_short<- runopen[runopen$down == 3 & runopen$disttogo == 'short',]
run_open_3rd_med<- runopen[runopen$down == 3 & runopen$disttogo == 'med',]
run_open_3rd_long<- runopen[runopen$down == 3 & runopen$disttogo == 'long',]
run_open_3rd_xlong<- runopen[runopen$down == 3 & runopen$disttogo == 'extra long',]

run_open_3rd_shortgain <- run_open_3rd_short$yardsgained
mixmdl_open_3rd_short <- normalmixEM(run_open_3rd_shortgain, k = 2)

run_open_3rd_medgain <- run_open_3rd_med$yardsgained
mixmdl_open_3rd_med <- normalmixEM(run_open_3rd_medgain, k = 2)

run_open_3rd_longgain <- run_open_3rd_long$yardsgained
mixmdl_open_3rd_long <- normalmixEM(run_open_3rd_longgain, k = 2)

run_open_3rd_xlonggain <- run_open_3rd_xlong$yardsgained
mixmdl_open_3rd_xlong <- normalmixEM(run_open_3rd_xlonggain, k = 2)

## Open Fourth Down
run_open_4th_short<- runopen[runopen$down == 4 & runopen$disttogo == 'short',]
run_open_4th_med<- runopen[runopen$down == 4 & runopen$disttogo == 'med',]
run_open_4th_long<- runopen[runopen$down == 4 & runopen$disttogo == 'long',]
run_open_4th_xlong<- runopen[runopen$down == 4 & runopen$disttogo == 'extra long',]

run_open_4th_shortgain <- run_open_4th_short$yardsgained
mixmdl_open_4th_short <- normalmixEM(run_open_4th_shortgain, k = 2)

run_open_4th_medgain <- run_open_4th_med$yardsgained
mixmdl_open_4th_med <- normalmixEM(run_open_4th_medgain, k = 2)

run_open_4th_longgain <- run_open_4th_long$yardsgained
mixmdl_open_4th_long <- normalmixEM(run_open_4th_longgain, k = 2)

run_open_4th_xlonggain <- run_open_4th_xlong$yardsgained
mixmdl_open_4th_xlong <- normalmixEM(run_open_4th_xlonggain, k = 2)



##Simulators for Runs in Open Field
run_open_1st_short_sim <- rnormmix(1, mixmdl_open_1st_short$lambda, mixmdl_open_1st_short$mu, mixmdl_open_1st_short$sigma)
run_open_1st_med_sim <- rnormmix(1, mixmdl_open_1st_med$lambda, mixmdl_open_1st_med$mu, mixmdl_open_1st_med$sigma)
run_open_1st_long_sim <- rnormmix(1, mixmdl_open_1st_long$lambda, mixmdl_open_1st_long$mu, mixmdl_open_1st_long$sigma)
run_open_1st_xlong_sim <- rnormmix(1, mixmdl_open_1st_xlong$lambda, mixmdl_open_1st_xlong$mu, mixmdl_open_1st_xlong$sigma)

run_open_2nd_short_sim <- rnormmix(1, mixmdl_open_2nd_short$lambda, mixmdl_open_2nd_short$mu, mixmdl_open_2nd_short$sigma)
run_open_2nd_med_sim <- rnormmix(1, mixmdl_open_2nd_med$lambda, mixmdl_open_2nd_med$mu, mixmdl_open_2nd_med$sigma)
run_open_2nd_long_sim <- rnormmix(1, mixmdl_open_2nd_long$lambda, mixmdl_open_2nd_long$mu, mixmdl_open_2nd_long$sigma)
run_open_2nd_xlong_sim <- rnormmix(1, mixmdl_open_2nd_xlong$lambda, mixmdl_open_2nd_xlong$mu, mixmdl_open_2nd_xlong$sigma)

run_open_3rd_short_sim <- rnormmix(1, mixmdl_open_3rd_short$lambda, mixmdl_open_3rd_short$mu, mixmdl_open_3rd_short$sigma)
run_open_3rd_med_sim <- rnormmix(1, mixmdl_open_3rd_med$lambda, mixmdl_open_3rd_med$mu, mixmdl_open_3rd_med$sigma)
run_open_3rd_long_sim <- rnormmix(1, mixmdl_open_3rd_long$lambda, mixmdl_open_3rd_long$mu, mixmdl_open_3rd_long$sigma)
run_open_3rd_xlong_sim <- rnormmix(1, mixmdl_open_3rd_xlong$lambda, mixmdl_open_3rd_xlong$mu, mixmdl_open_3rd_xlong$sigma)

run_open_4th_short_sim <- rnormmix(1, mixmdl_open_4th_short$lambda, mixmdl_open_4th_short$mu, mixmdl_open_4th_short$sigma)
run_open_4th_med_sim <- rnormmix(1, mixmdl_open_4th_med$lambda, mixmdl_open_4th_med$mu, mixmdl_open_4th_med$sigma)
run_open_4th_long_sim <- rnormmix(1, mixmdl_open_4th_long$lambda, mixmdl_open_4th_long$mu, mixmdl_open_4th_long$sigma)
run_open_4th_xlong_sim <- rnormmix(1, mixmdl_open_4th_xlong$lambda, mixmdl_open_4th_xlong$mu, mixmdl_open_4th_xlong$sigma)



#### MIXTURE MODEL RUNS RED ZONE ####

runred <- run[run$zone == 'red',]


## Red Zone First Down
# Subset the data
run_red_1st_short <- runred[runred$down == 1 & runred$disttogo == 'short',]
run_red_1st_med<- runred[runred$down == 1 & runred$disttogo == 'med',]
run_red_1st_long<- runred[runred$down == 1 & runred$disttogo == 'long',]
run_red_1st_xlong<- runred[runred$down == 1 & runred$disttogo == 'extra long',]

#Normal Mixture Modes with two distributions
run_red_1st_shortgain <- run_red_1st_short$yardsgained
mixmdl_red_1st_short <- normalmixEM(run_red_1st_shortgain, k = 2)

run_red_1st_medgain <- run_red_1st_med$yardsgained
mixmdl_red_1st_med <- normalmixEM(run_red_1st_medgain, k = 2)

run_red_1st_longgain <- run_red_1st_long$yardsgained
mixmdl_red_1st_long <- normalmixEM(run_red_1st_longgain, k = 2)

run_red_1st_xlonggain <- run_red_1st_xlong$yardsgained
mixmdl_red_1st_xlong <- normalmixEM(run_red_1st_xlonggain, k = 2)


#Example mixture model plot for Run, Red Zone, 1st down, Short 
data.frame(x = mixmdl_red_1st_short$x) %>%
  ggplot() +
  geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", 
                 fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl_red_1st_short$mu[1], mixmdl_red_1st_short$sigma[1], lam = mixmdl_red_1st_short$lambda[1]),
                colour = "red", lwd = 1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl_red_1st_short$mu[2], mixmdl_red_1st_short$sigma[2], lam = mixmdl_red_1st_short$lambda[2]),
                colour = "blue", lwd = 1.5) +
  ylab("Density")


## Red Zone Second Down
run_red_2nd_short <- runred[runred$down == 2 & runred$disttogo == 'short',]
run_red_2nd_med <- runred[runred$down == 2 & runred$disttogo == 'med',]
run_red_2nd_long<- runred[runred$down == 2 & runred$disttogo == 'long',]
run_red_2nd_xlong<- runred[runred$down == 2 & runred$disttogo == 'extra long',]

run_red_2nd_shortgain <- run_red_2nd_short$yardsgained
mixmdl_red_2nd_short <- normalmixEM(run_red_2nd_shortgain, k = 2)

run_red_2nd_medgain <- run_red_2nd_med$yardsgained
mixmdl_red_2nd_med <- normalmixEM(run_red_2nd_medgain, k = 2)

run_red_2nd_longgain <- run_red_2nd_long$yardsgained
mixmdl_red_2nd_long <- normalmixEM(run_red_2nd_longgain, k = 2)

run_red_2nd_xlonggain <- run_red_2nd_xlong$yardsgained
mixmdl_red_2nd_xlong <- normalmixEM(run_red_2nd_xlonggain, k = 2)

## Red Zone Third Down
run_red_3rd_short<- runred[runred$down == 3 & runred$disttogo == 'short',]
run_red_3rd_med<- runred[runred$down == 3 & runred$disttogo == 'med',]
run_red_3rd_long<- runred[runred$down == 3 & runred$disttogo == 'long',]
run_red_3rd_xlong<- runred[runred$down == 3 & runred$disttogo == 'extra long',]

run_red_3rd_shortgain <- run_red_3rd_short$yardsgained
mixmdl_red_3rd_short <- normalmixEM(run_red_3rd_shortgain, k = 2)

run_red_3rd_medgain <- run_red_3rd_med$yardsgained
mixmdl_red_3rd_med <- normalmixEM(run_red_3rd_medgain, k = 2)

run_red_3rd_longgain <- run_red_3rd_long$yardsgained
mixmdl_red_3rd_long <- normalmixEM(run_red_3rd_longgain, k = 2)

run_red_3rd_xlonggain <- run_red_3rd_xlong$yardsgained
mixmdl_red_3rd_xlong <- normalmixEM(run_red_3rd_xlonggain, k = 2)

## Red Zone Fourth Down
run_red_4th_short<- runred[runred$down == 4 & runred$disttogo == 'short',]
run_red_4th_med<- runred[runred$down == 4 & runred$disttogo == 'med',]
run_red_4th_long<- runred[runred$down == 4 & runred$disttogo == 'long',]
run_red_4th_xlong<- runred[runred$down == 4 & runred$disttogo == 'extra long',]

run_red_4th_shortgain <- run_red_4th_short$yardsgained
mixmdl_red_4th_short <- normalmixEM(run_red_4th_shortgain, k = 2)

run_red_4th_medgain <- run_red_4th_med$yardsgained
mixmdl_red_4th_med <- normalmixEM(run_red_4th_medgain, k = 2)

run_red_4th_longgain <- run_red_4th_long$yardsgained
mixmdl_red_4th_long <- normalmixEM(run_red_4th_longgain, k = 2)

run_red_4th_xlonggain <- run_red_4th_xlong$yardsgained
mixmdl_red_4th_xlong <- normalmixEM(run_red_4th_xlonggain, k = 2)



##Simulators for Runs in Red Zone
run_red_1st_short_sim <- rnormmix(1, mixmdl_red_1st_short$lambda, mixmdl_red_1st_short$mu, mixmdl_red_1st_short$sigma)
run_red_1st_med_sim <- rnormmix(1, mixmdl_red_1st_med$lambda, mixmdl_red_1st_med$mu, mixmdl_red_1st_med$sigma)
run_red_1st_long_sim <- rnormmix(1, mixmdl_red_1st_long$lambda, mixmdl_red_1st_long$mu, mixmdl_red_1st_long$sigma)
run_red_1st_xlong_sim <- rnormmix(1, mixmdl_red_1st_xlong$lambda, mixmdl_red_1st_xlong$mu, mixmdl_red_1st_xlong$sigma)


run_red_2nd_short_sim <- rnormmix(1, mixmdl_red_2nd_short$lambda, mixmdl_red_2nd_short$mu, mixmdl_red_2nd_short$sigma)
run_red_2nd_med_sim <- rnormmix(1, mixmdl_red_2nd_med$lambda, mixmdl_red_2nd_med$mu, mixmdl_red_2nd_med$sigma)
run_red_2nd_long_sim <- rnormmix(1, mixmdl_red_2nd_long$lambda, mixmdl_red_2nd_long$mu, mixmdl_red_2nd_long$sigma)
run_red_2nd_xlong_sim <- rnormmix(1, mixmdl_red_2nd_xlong$lambda, mixmdl_red_2nd_xlong$mu, mixmdl_red_2nd_xlong$sigma)

run_red_3rd_short_sim <- rnormmix(1, mixmdl_red_3rd_short$lambda, mixmdl_red_3rd_short$mu, mixmdl_red_3rd_short$sigma)
run_red_3rd_med_sim <- rnormmix(1, mixmdl_red_3rd_med$lambda, mixmdl_red_3rd_med$mu, mixmdl_red_3rd_med$sigma)
run_red_3rd_long_sim <- rnormmix(1, mixmdl_red_3rd_long$lambda, mixmdl_red_3rd_long$mu, mixmdl_red_3rd_long$sigma)
run_red_3rd_xlong_sim <- rnormmix(1, mixmdl_red_3rd_xlong$lambda, mixmdl_red_3rd_xlong$mu, mixmdl_red_3rd_xlong$sigma)

run_red_4th_short_sim <- rnormmix(1, mixmdl_red_4th_short$lambda, mixmdl_red_4th_short$mu, mixmdl_red_4th_short$sigma)
run_red_4th_med_sim <- rnormmix(1, mixmdl_red_4th_med$lambda, mixmdl_red_4th_med$mu, mixmdl_red_4th_med$sigma)
run_red_4th_long_sim <- rnormmix(1, mixmdl_red_4th_long$lambda, mixmdl_red_4th_long$mu, mixmdl_red_4th_long$sigma)
run_red_4th_xlong_sim <- rnormmix(1, mixmdl_red_4th_xlong$lambda, mixmdl_red_4th_xlong$mu, mixmdl_red_4th_xlong$sigma)



#### RUN SIMULATOR ####
run_sim_yg <- function(fieldpos,down,pff_DISTANCE){
  
  if (testing.data$fieldpos < 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_open_1st_short$lambda, mixmdl_open_1st_short$mu, mixmdl_open_1st_short$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_open_1st_med$lambda, mixmdl_open_1st_med$mu, mixmdl_open_1st_med$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_open_1st_long$lambda, mixmdl_open_1st_long$mu, mixmdl_open_1st_long$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 20 < testing.data$pff_DISTANCE & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_open_1st_xlong$lambda, mixmdl_open_1st_xlong$mu, mixmdl_open_1st_xlong$sigma) )
  } 
  if (testing.data$fieldpos < 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_open_2nd_short$lambda, mixmdl_open_2nd_short$mu, mixmdl_open_2nd_short$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_open_2nd_med$lambda, mixmdl_open_2nd_med$mu, mixmdl_open_2nd_med$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_open_2nd_long$lambda, mixmdl_open_2nd_long$mu, mixmdl_open_2nd_long$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 20 < testing.data$pff_DISTANCE & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_open_2nd_xlong$lambda, mixmdl_open_2nd_xlong$mu, mixmdl_open_2nd_xlong$sigma) )
  } 
  if (testing.data$fieldpos < 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_open_3rd_short$lambda, mixmdl_open_3rd_short$mu, mixmdl_open_3rd_short$sigma))
  } 
  if (testing.data$fieldpos < 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_open_3rd_med$lambda, mixmdl_open_3rd_med$mu, mixmdl_open_3rd_med$sigma))
  } 
  if (testing.data$fieldpos < 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_open_3rd_long$lambda, mixmdl_open_3rd_long$mu, mixmdl_open_3rd_long$sigma))
  } 
  if (testing.data$fieldpos < 80 & 20 < testing.data$pff_DISTANCE & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_open_3rd_xlong$lambda, mixmdl_open_3rd_xlong$mu, mixmdl_open_3rd_xlong$sigma))
  } 
  if (testing.data$fieldpos < 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 4) {
    return(rnormmix(1, mixmdl_open_4th_short$lambda, mixmdl_open_4th_short$mu, mixmdl_open_4th_short$sigma))
  } 
  if (testing.data$fieldpos < 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 4) {
    return(rnormmix(1, mixmdl_open_4th_med$lambda, mixmdl_open_4th_med$mu, mixmdl_open_4th_med$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 4) {
    return( rnormmix(1, mixmdl_open_4th_long$lambda, mixmdl_open_4th_long$mu, mixmdl_open_4th_long$sigma))
  } 
  if (testing.data$fieldpos >= 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_red_1st_short$lambda, mixmdl_red_1st_short$mu, mixmdl_red_1st_short$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_red_1st_med$lambda, mixmdl_red_1st_med$mu, mixmdl_red_1st_med$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_red_1st_long$lambda, mixmdl_red_1st_long$mu, mixmdl_red_1st_long$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_red_2nd_short$lambda, mixmdl_red_2nd_short$mu, mixmdl_red_2nd_short$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_red_2nd_med$lambda, mixmdl_red_2nd_med$mu, mixmdl_red_2nd_med$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_red_2nd_long$lambda, mixmdl_red_2nd_long$mu, mixmdl_red_2nd_long$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_red_3rd_short$lambda, mixmdl_red_3rd_short$mu, mixmdl_red_3rd_short$sigma))
  } 
  if (testing.data$fieldpos >= 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_red_3rd_med$lambda, mixmdl_red_3rd_med$mu, mixmdl_red_3rd_med$sigma))
  } 
  if (testing.data$fieldpos >= 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_red_3rd_long$lambda, mixmdl_red_3rd_long$mu, mixmdl_red_3rd_long$sigma))
  } 
  if (testing.data$fieldpos >= 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 4) {
    return(rnormmix(1, mixmdl_red_4th_short$lambda, mixmdl_red_4th_short$mu, mixmdl_red_4th_short$sigma))
  } 
  else {
    return(0)
  }
}
####Mixture Model Passing####

set.seed(1)
library(mixtools)
passopen <- pass[pass$zone == 'open',]

plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}


## Open First Down
# Subset the data
pass_open_1st_short <- passopen[passopen$down == 1 & passopen$disttogo == 'short',]
pass_open_1st_med<- passopen[passopen$down == 1 & passopen$disttogo == 'med',]
pass_open_1st_long<- passopen[passopen$down == 1 & passopen$disttogo == 'long',]
pass_open_1st_xlong<- passopen[passopen$down == 1 & passopen$disttogo == 'extra long',]

#Normal Mixture Modes with two distributions
pass_open_1st_shortgain <- pass_open_1st_short$yardsgained
mixmdl_pass_open_1st_short <- normalmixEM(pass_open_1st_shortgain, k = 2, epsilon = .03)

pass_open_1st_medgain <- pass_open_1st_med$yardsgained
mixmdl_pass_open_1st_med <- normalmixEM(pass_open_1st_medgain, k = 2, epsilon = .8)

pass_open_1st_longgain <- pass_open_1st_long$yardsgained
mixmdl_pass_open_1st_long <- normalmixEM(pass_open_1st_longgain, k = 2, epsilon = .001)

pass_open_1st_xlonggain <- pass_open_1st_xlong$yardsgained
mixmdl_pass_open_1st_xlong <- normalmixEM(pass_open_1st_xlonggain, k = 2, epsilon = .006)



#Example mixture model plot for Pass, Open Field, 1st down, Short 
data.frame(x = mixmdl_pass_open_1st_short$x) %>%
  ggplot() +
  geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", 
                 fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl_open_1st_short$mu[1], mixmdl_open_1st_short$sigma[1], lam = mixmdl_open_1st_short$lambda[1]),
                colour = "red", lwd = 1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl_open_1st_short$mu[2], mixmdl_open_1st_short$sigma[2], lam = mixmdl_open_1st_short$lambda[2]),
                colour = "blue", lwd = 1.5) +
  ylab("Density")

## Open Second Down
pass_open_2nd_short <- passopen[passopen$down == 2 & passopen$disttogo == 'short',]
pass_open_2nd_med <- passopen[passopen$down == 2 & passopen$disttogo == 'med',]
pass_open_2nd_long<- passopen[passopen$down == 2 & passopen$disttogo == 'long',]
pass_open_2nd_xlong<- passopen[passopen$down == 2 & passopen$disttogo == 'extra long',]

pass_open_2nd_shortgain <- pass_open_2nd_short$yardsgained
mixmdl_pass_open_2nd_short <- normalmixEM(pass_open_2nd_shortgain, k = 2)

pass_open_2nd_medgain <- pass_open_2nd_med$yardsgained
mixmdl_pass_open_2nd_med <- normalmixEM(pass_open_2nd_medgain, k = 2)

pass_open_2nd_longgain <- pass_open_2nd_long$yardsgained
mixmdl_pass_open_2nd_long <- normalmixEM(pass_open_2nd_longgain, k = 2)

pass_open_2nd_xlonggain <- pass_open_2nd_xlong$yardsgained
mixmdl_pass_open_2nd_xlong <- normalmixEM(pass_open_2nd_xlonggain, k = 2, epsilon = .1)


## Open Third Down
pass_open_3rd_short<- passopen[passopen$down == 3 & passopen$disttogo == 'short',]
pass_open_3rd_med<- passopen[passopen$down == 3 & passopen$disttogo == 'med',]
pass_open_3rd_long<- passopen[passopen$down == 3 & passopen$disttogo == 'long',]
pass_open_3rd_xlong<- passopen[passopen$down == 3 & passopen$disttogo == 'extra long',]

pass_open_3rd_shortgain <- pass_open_3rd_short$yardsgained
mixmdl_pass_open_3rd_short <- normalmixEM(pass_open_3rd_shortgain, k = 2)

pass_open_3rd_medgain <- run_open_3rd_med$yardsgained
mixmdl_pass_open_3rd_med <- normalmixEM(pass_open_3rd_medgain, k = 2)

pass_open_3rd_longgain <- run_open_3rd_long$yardsgained
mixmdl_pass_open_3rd_long <- normalmixEM(pass_open_3rd_longgain, k = 2)

pass_open_3rd_xlonggain <- run_open_3rd_xlong$yardsgained
mixmdl_pass_open_3rd_xlong <- normalmixEM(pass_open_3rd_xlonggain, k = 2)


## Open Fourth Down
pass_open_4th_short<- passopen[passopen$down == 4 & passopen$disttogo == 'short',]
pass_open_4th_med<- passopen[passopen$down == 4 & passopen$disttogo == 'med',]
pass_open_4th_long<- passopen[passopen$down == 4 & passopen$disttogo == 'long',]
pass_open_4th_xlong<- passopen[passopen$down == 4 & passopen$disttogo == 'extra long',]

pass_open_4th_shortgain <- pass_open_4th_short$yardsgained
mixmdl_pass_open_4th_short <- normalmixEM(pass_open_4th_shortgain, k = 2, epsilon = .3)

pass_open_4th_medgain <- pass_open_4th_med$yardsgained
mixmdl_pass_open_4th_med <- normalmixEM(pass_open_4th_medgain, k = 2, epsilon = .1)

pass_open_4th_longgain <- pass_open_4th_long$yardsgained
mixmdl_pass_open_4th_long <- normalmixEM(pass_open_4th_longgain, k = 2)

pass_open_4th_xlonggain <- pass_open_4th_xlong$yardsgained
mixmdl_pass_open_4th_xlong <- normalmixEM(pass_open_4th_xlonggain, k = 2, epsilon = .19)



##Simulators for Passes in Open Field
pass_open_1st_short_sim <- rnormmix(1, mixmdl_pass_open_1st_short$lambda, mixmdl_pass_open_1st_short$mu, mixmdl_pass_open_1st_short$sigma)
pass_open_1st_med_sim <- rnormmix(1, mixmdl_pass_open_1st_med$lambda, mixmdl_pass_open_1st_med$mu, mixmdl_pass_open_1st_med$sigma)
pass_open_1st_long_sim <- rnormmix(1, mixmdl_pass_open_1st_long$lambda, mixmdl_pass_open_1st_long$mu, mixmdl_pass_open_1st_long$sigma)
pass_open_1st_xlong_sim <- rnormmix(1, mixmdl_pass_open_1st_xlong$lambda, mixmdl_pass_open_1st_xlong$mu, mixmdl_pass_open_1st_xlong$sigma)

pass_open_2nd_short_sim <- rnormmix(1, mixmdl_pass_open_2nd_short$lambda, mixmdl_pass_open_2nd_short$mu, mixmdl_pass_open_2nd_short$sigma)
pass_open_2nd_med_sim <- rnormmix(1, mixmdl_pass_open_2nd_med$lambda, mixmdl_pass_open_2nd_med$mu, mixmdl_pass_open_2nd_med$sigma)
pass_open_2nd_long_sim <- rnormmix(1, mixmdl_pass_open_2nd_long$lambda, mixmdl_pass_open_2nd_long$mu, mixmdl_pass_open_2nd_long$sigma)
pass_open_2nd_xlong_sim <- rnormmix(1, mixmdl_pass_open_2nd_xlong$lambda, mixmdl_pass_open_2nd_xlong$mu, mixmdl_pass_open_2nd_xlong$sigma)

pass_open_3rd_short_sim <- rnormmix(1, mixmdl_pass_open_3rd_short$lambda, mixmdl_pass_open_3rd_short$mu, mixmdl_pass_open_3rd_short$sigma)
pass_open_3rd_med_sim <- rnormmix(1, mixmdl_pass_open_3rd_med$lambda, mixmdl_pass_open_3rd_med$mu, mixmdl_pass_open_3rd_med$sigma)
pass_open_3rd_long_sim <- rnormmix(1, mixmdl_pass_open_3rd_long$lambda, mixmdl_pass_open_3rd_long$mu, mixmdl_pass_open_3rd_long$sigma)
pass_open_3rd_xlong_sim <- rnormmix(1, mixmdl_pass_open_3rd_xlong$lambda, mixmdl_pass_open_3rd_xlong$mu, mixmdl_pass_open_3rd_xlong$sigma)

pass_open_4th_short_sim <- rnormmix(1, mixmdl_pass_open_4th_short$lambda, mixmdl_pass_open_4th_short$mu, mixmdl_pass_open_4th_short$sigma)
pass_open_4th_med_sim <- rnormmix(1, mixmdl_pass_open_4th_med$lambda, mixmdl_pass_open_4th_med$mu, mixmdl_pass_open_4th_med$sigma)
pass_open_4th_long_sim <- rnormmix(1, mixmdl_pass_open_4th_long$lambda, mixmdl_pass_open_4th_long$mu, mixmdl_pass_open_4th_long$sigma)
pass_open_4th_xlong_sim <- rnormmix(1, mixmdl_pass_open_4th_xlong$lambda, mixmdl_pass_open_4th_xlong$mu, mixmdl_pass_open_4th_xlong$sigma)


##Mixture Model Red Zone

passred <- pass[pass$zone == 'red',]


## Red Zone First Down
# Subset the data
pass_red_1st_short <- passred[passred$down == 1 & passred$disttogo == 'short',]
pass_red_1st_med<- passred[passred$down == 1 & passred$disttogo == 'med',]
pass_red_1st_long<- passred[passred$down == 1 & passred$disttogo == 'long',]
pass_red_1st_xlong<- passred[passred$down == 1 & passred$disttogo == 'extra long',]

#Normal Mixture Modes with two distributions
pass_red_1st_shortgain <- pass_red_1st_short$yardsgained
mixmdl_pass_red_1st_short <- normalmixEM(pass_red_1st_shortgain, k = 2)

pass_red_1st_medgain <- pass_red_1st_med$yardsgained
mixmdl_pass_red_1st_med <- normalmixEM(pass_red_1st_medgain, k = 2)

pass_red_1st_longgain <- pass_red_1st_long$yardsgained
mixmdl_pass_red_1st_long <- normalmixEM(pass_red_1st_longgain, k = 2)

pass_red_1st_xlonggain <- pass_red_1st_xlong$yardsgained
mixmdl_pass_red_1st_xlong <- normalmixEM(pass_red_1st_xlonggain, k = 2)


#Example mixture model plot for Pass, Red Zone, 1st down, Short 
data.frame(x = mixmdl_pass_red_1st_short$x) %>%
  ggplot() +
  geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", 
                 fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl_red_1st_short$mu[1], mixmdl_red_1st_short$sigma[1], lam = mixmdl_red_1st_short$lambda[1]),
                colour = "red", lwd = 1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl_red_1st_short$mu[2], mixmdl_red_1st_short$sigma[2], lam = mixmdl_red_1st_short$lambda[2]),
                colour = "blue", lwd = 1.5) +
  ylab("Density")


## Red Zone Second Down
pass_red_2nd_short <- passred[passred$down == 2 & passred$disttogo == 'short',]
pass_red_2nd_med <- passred[passred$down == 2 & passred$disttogo == 'med',]
pass_red_2nd_long<- passred[passred$down == 2 & passred$disttogo == 'long',]
pass_red_2nd_xlong<- passred[passred$down == 2 & passred$disttogo == 'extra long',]

pass_red_2nd_shortgain <- pass_red_2nd_short$yardsgained
mixmdl_pass_red_2nd_short <- normalmixEM(pass_red_2nd_shortgain, k = 2, epsilon = .8)

pass_red_2nd_medgain <- pass_red_2nd_med$yardsgained
mixmdl_pass_red_2nd_med <- normalmixEM(pass_red_2nd_medgain, k = 2)

pass_red_2nd_longgain <- pass_red_2nd_long$yardsgained
mixmdl_pass_red_2nd_long <- normalmixEM(pass_red_2nd_longgain, k = 2)

pass_red_2nd_xlonggain <- pass_red_2nd_xlong$yardsgained
mixmdl_pass_red_2nd_xlong <- normalmixEM(pass_red_2nd_xlonggain, k = 2, epsilon = .2)

## Red Zone Third Down
pass_red_3rd_short<- passred[passred$down == 3 & passred$disttogo == 'short',]
pass_red_3rd_med<- passred[passred$down == 3 & passred$disttogo == 'med',]
pass_red_3rd_long<- passred[passred$down == 3 & passred$disttogo == 'long',]
pass_red_3rd_xlong<- passred[passred$down == 3 & passred$disttogo == 'extra long',]

pass_red_3rd_shortgain <- pass_red_3rd_short$yardsgained
mixmdl_pass_red_3rd_short <- normalmixEM(pass_red_3rd_shortgain, k = 2)

pass_red_3rd_medgain <- pass_red_3rd_med$yardsgained
mixmdl_pass_red_3rd_med <- normalmixEM(pass_red_3rd_medgain, k = 2)

pass_red_3rd_longgain <- pass_red_3rd_long$yardsgained
mixmdl_pass_red_3rd_long <- normalmixEM(pass_red_3rd_longgain, k = 2)

pass_red_3rd_xlonggain <- pass_red_3rd_xlong$yardsgained
mixmdl_pass_red_3rd_xlong <- normalmixEM(pass_red_3rd_xlonggain, k = 2, epsilon = .2)


## Red Zone Fourth Down
pass_red_4th_short<- passred[passred$down == 4 & passred$disttogo == 'short',]
pass_red_4th_med<- passred[passred$down == 4 & passred$disttogo == 'med',]
pass_red_4th_long<- passred[passred$down == 4 & passred$disttogo == 'long',]
pass_red_4th_xlong<- passred[passred$down == 4 & passred$disttogo == 'extra long',]

pass_red_4th_shortgain <- pass_red_4th_short$yardsgained
mixmdl_pass_red_4th_short <- normalmixEM(pass_red_4th_shortgain, k = 2, epsilon = .7)

pass_red_4th_medgain <- pass_red_4th_med$yardsgained
mixmdl_pass_red_4th_med <- normalmixEM(pass_red_4th_medgain, k = 2)

pass_red_4th_longgain <- pass_red_4th_long$yardsgained
mixmdl_pass_red_4th_long <- normalmixEM(pass_red_4th_longgain, k = 2)

pass_red_4th_xlonggain <- pass_red_4th_xlong$yardsgained
mixmdl_pass_red_4th_xlong <- normalmixEM(pass_red_4th_xlonggain, k = 2)


##Simulators for Runs in Red Zone
pass_red_1st_short_sim <- rnormmix(1, mixmdl_pass_red_1st_short$lambda, mixmdl_pass_red_1st_short$mu, mixmdl_pass_red_1st_short$sigma)
pass_red_1st_med_sim <- rnormmix(1, mixmdl_pass_red_1st_med$lambda, mixmdl_pass_red_1st_med$mu, mixmdl_pass_red_1st_med$sigma)
pass_red_1st_long_sim <- rnormmix(1, mixmdl_pass_red_1st_long$lambda, mixmdl_pass_red_1st_long$mu, mixmdl_pass_red_1st_long$sigma)
pass_red_1st_xlong_sim <- rnormmix(1, mixmdl_pass_red_1st_xlong$lambda, mixmdl_pass_red_1st_xlong$mu, mixmdl_pass_red_1st_xlong$sigma)


pass_red_2nd_short_sim <- rnormmix(1, mixmdl_pass_red_2nd_short$lambda, mixmdl_pass_red_2nd_short$mu, mixmdl_pass_red_2nd_short$sigma)
pass_red_2nd_med_sim <- rnormmix(1, mixmdl_pass_red_2nd_med$lambda, mixmdl_pass_red_2nd_med$mu, mixmdl_pass_red_2nd_med$sigma)
pass_red_2nd_long_sim <- rnormmix(1, mixmdl_pass_red_2nd_long$lambda, mixmdl_pass_red_2nd_long$mu, mixmdl_pass_red_2nd_long$sigma)
pass_red_2nd_xlong_sim <- rnormmix(1, mixmdl_pass_red_2nd_xlong$lambda, mixmdl_pass_red_2nd_xlong$mu, mixmdl_pass_red_2nd_xlong$sigma)

pass_red_3rd_short_sim <- rnormmix(1, mixmdl_pass_red_3rd_short$lambda, mixmdl_pass_red_3rd_short$mu, mixmdl_pass_red_3rd_short$sigma)
pass_red_3rd_med_sim <- rnormmix(1, mixmdl_pass_red_3rd_med$lambda, mixmdl_pass_red_3rd_med$mu, mixmdl_pass_red_3rd_med$sigma)
pass_red_3rd_long_sim <- rnormmix(1, mixmdl_pass_red_3rd_long$lambda, mixmdl_pass_red_3rd_long$mu, mixmdl_pass_red_3rd_long$sigma)
pass_red_3rd_xlong_sim <- rnormmix(1, mixmdl_pass_red_3rd_xlong$lambda, mixmdl_pass_red_3rd_xlong$mu, mixmdl_pass_red_3rd_xlong$sigma)

pass_red_4th_short_sim <- rnormmix(1, mixmdl_pass_red_4th_short$lambda, mixmdl_pass_red_4th_short$mu, mixmdl_pass_red_4th_short$sigma)
pass_red_4th_med_sim <- rnormmix(1, mixmdl_pass_red_4th_med$lambda, mixmdl_pass_red_4th_med$mu, mixmdl_pass_red_4th_med$sigma)
pass_red_4th_long_sim <- rnormmix(1, mixmdl_pass_red_4th_long$lambda, mixmdl_pass_red_4th_long$mu, mixmdl_pass_red_4th_long$sigma)
pass_red_4th_xlong_sim <- rnormmix(1, mixmdl_pass_red_4th_xlong$lambda, mixmdl_pass_red_4th_xlong$mu, mixmdl_pass_red_4th_xlong$sigma)
#### PASSING SIMULATION ####
pass_sim_yg <- function(fieldpos,down,pff_DISTANCE){
  
  if (testing.data$fieldpos < 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_pass_open_1st_short$lambda, mixmdl_pass_open_1st_short$mu, mixmdl_pass_open_1st_short$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_pass_open_1st_med$lambda, mixmdl_pass_open_1st_med$mu, mixmdl_pass_open_1st_med$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_pass_open_1st_long$lambda, mixmdl_pass_open_1st_long$mu, mixmdl_pass_open_1st_long$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 20 < testing.data$pff_DISTANCE & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_pass_open_1st_xlong$lambda, mixmdl_pass_open_1st_xlong$mu, mixmdl_pass_open_1st_xlong$sigma) )
  } 
  if (testing.data$fieldpos < 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_pass_open_2nd_short$lambda, mixmdl_pass_open_2nd_short$mu, mixmdl_pass_open_2nd_short$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_pass_open_2nd_med$lambda, mixmdl_pass_open_2nd_med$mu, mixmdl_pass_open_2nd_med$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_pass_open_2nd_long$lambda, mixmdl_pass_open_2nd_long$mu, mixmdl_pass_open_2nd_long$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 20 < testing.data$pff_DISTANCE & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_pass_open_2nd_long$lambda, mixmdl_pass_open_2nd_long$mu, mixmdl_pass_open_2nd_long$sigma) )
  } 
  if (testing.data$fieldpos < 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_pass_open_3rd_short$lambda, mixmdl_pass_open_3rd_short$mu, mixmdl_pass_open_3rd_short$sigma))
  } 
  if (testing.data$fieldpos < 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_pass_open_3rd_med$lambda, mixmdl_pass_open_3rd_med$mu, mixmdl_pass_open_3rd_med$sigma))
  } 
  if (testing.data$fieldpos < 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_pass_open_3rd_long$lambda, mixmdl_pass_open_3rd_long$mu, mixmdl_pass_open_3rd_long$sigma))
  } 
  if (testing.data$fieldpos < 80 & 20 < testing.data$pff_DISTANCE & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_pass_open_3rd_xlong$lambda, mixmdl_pass_open_3rd_xlong$mu, mixmdl_pass_open_3rd_xlong$sigma))
  } 
  if (testing.data$fieldpos < 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 4) {
    return(rnormmix(1, mixmdl_pass_open_4th_short$lambda, mixmdl_pass_open_4th_short$mu, mixmdl_pass_open_4th_short$sigma))
  } 
  if (testing.data$fieldpos < 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 4) {
    return(rnormmix(1, mixmdl_pass_open_4th_med$lambda, mixmdl_pass_open_4th_med$mu, mixmdl_pass_open_4th_med$sigma) )
  } 
  if (testing.data$fieldpos < 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 4) {
    return( rnormmix(1, mixmdl_pass_open_4th_long$lambda, mixmdl_pass_open_4th_long$mu, mixmdl_pass_open_4th_long$sigma))
  } 
  if (testing.data$fieldpos >= 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_pass_red_1st_short$lambda, mixmdl_pass_red_1st_short$mu, mixmdl_pass_red_1st_short$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_pass_red_1st_med$lambda, mixmdl_pass_red_1st_med$mu, mixmdl_pass_red_1st_med$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 1) {
    return(rnormmix(1, mixmdl_pass_red_1st_long$lambda, mixmdl_pass_red_1st_long$mu, mixmdl_pass_red_1st_long$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_pass_red_2nd_short$lambda, mixmdl_pass_red_2nd_short$mu, mixmdl_pass_red_2nd_short$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_pass_red_2nd_med$lambda, mixmdl_pass_red_2nd_med$mu, mixmdl_pass_red_2nd_med$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 2) {
    return(rnormmix(1, mixmdl_pass_red_2nd_long$lambda, mixmdl_pass_red_2nd_long$mu, mixmdl_pass_red_2nd_long$sigma) )
  } 
  if (testing.data$fieldpos >= 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_pass_red_3rd_med$lambda, mixmdl_pass_red_3rd_med$mu, mixmdl_pass_red_3rd_med$sigma))
  } 
  if (testing.data$fieldpos >= 80 & 5 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 10 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_pass_red_3rd_med$lambda, mixmdl_pass_red_3rd_med$mu, mixmdl_pass_red_3rd_med$sigma))
  } 
  if (testing.data$fieldpos >= 80 & 10 < testing.data$pff_DISTANCE & testing.data$pff_DISTANCE <= 20 & testing.data$down == 3) {
    return(rnormmix(1, mixmdl_pass_red_3rd_long$lambda, mixmdl_pass_red_3rd_long$mu, mixmdl_pass_red_3rd_long$sigma))
  } 
  if (testing.data$fieldpos >= 80 & testing.data$pff_DISTANCE <= 5 & testing.data$down == 4) {
    return(rnormmix(1, mixmdl_pass_red_4th_short$lambda, mixmdl_pass_red_4th_short$mu, mixmdl_pass_red_4th_short$sigma))
  } 
  else {
    return(rnormmix(1, mixmdl_pass_open_1st_short$lambda, mixmdl_pass_open_1st_short$mu, mixmdl_pass_open_1st_short$sigma) )
  }
}
##### FIELD GOAL SUCCESS #####

library(stringr)

unique(sraw1$pff_KICKRESULT)

##Cleaning data so that the model building is easier
tofind <- c("MADE", "MISSED","BLOCKED")

fg_made <- sraw1[grep(paste(tofind, collapse = "|"), sraw1$pff_KICKRESULT),]


unique(fg_made$pff_KICKRESULT)

fg_made <- fg_made[fg_made$pff_KICKRESULT != "FAKE - BLOCKED",]
fg_made$pff_KICKRESULT<-gsub(" - IL","",as.character(fg_made$pff_KICKRESULT)) 
fg_made$pff_KICKRESULT<-gsub(" - M","",as.character(fg_made$pff_KICKRESULT)) 
fg_made$pff_KICKRESULT<-gsub(" - S","",as.character(fg_made$pff_KICKRESULT)) 
fg_made$pff_KICKRESULT<-gsub(" - IR","",as.character(fg_made$pff_KICKRESULT)) 
fg_made$pff_KICKRESULT<-gsub(" - WR","",as.character(fg_made$pff_KICKRESULT)) 
fg_made$pff_KICKRESULT<-gsub(" - HL","",as.character(fg_made$pff_KICKRESULT)) 
fg_made$pff_KICKRESULT<-gsub(" - WL","",as.character(fg_made$pff_KICKRESULT)) 
fg_made$pff_KICKRESULT<-gsub(" - B","",as.character(fg_made$pff_KICKRESULT)) 
fg_made$pff_KICKRESULT<-gsub(" - HR","",as.character(fg_made$pff_KICKRESULT)) 


attach(fg_made)


#Vector for yardage
fgyards <- seq(min(fg_made$fieldpos), max(fg_made$fieldpos))

#Vector for field goal success and total tries
success <- rep(NA, length(fgyards))
totaltries <- rep(NA, length(fgyards))


#Loop through columns to fill vectors
for ( i in seq(along = fgyards)) {
  success[i]<- sum(fg_made$fieldpos==fgyards[i] & fg_made$pff_KICKRESULT == 'MADE')
  totaltries[i] <- sum(fg_made$fieldpos == fgyards[i])
}

#Field Goal Success Rate and table
successrate<- success/totaltries
cbind(fgyards,success, totaltries, successrate)


plot(fgyards, successrate, ylim=c(0,2), cex=sqrt(totaltries)/2, xlab='Yards',ylab="Success Rate")


##Logistic Regression 
fg_reg <- glm(cbind(success,totaltries-success) ~ fgyards,
              family=binomial(link='logit'))
summary(fg_reg)


##Field Goal Success Probability 
plot(fgyards, successrate, ylim=c(0,2), cex=sqrt(totaltries)/2, xlab='Yards',ylab="Success Rate")
lines(fgyards, fg_reg$fitted.values)
cbind(fgyards, probs= fg_reg$fitted.values)



#### FIELD GOAL SIMULATOR ####
fieldgoal_sim <- function(testing.data) { 
  prob_fg<- predict(fg_reg, testing.data, type="response")
  success_fg_prob <- prob_fg[testing.data$fieldpos]
  fg <- sample(c('Miss','FG'), size = 1, prob = c((1- round(success_fg_prob)), round(success_fg_prob)), replace = TRUE)
  return(fg)
}


#### PUNT SIMULATOR ####

punt_df<- plays[plays$playtype == 'punt',]

ggplot(punt_df, aes(x=pff_GAINLOSS, fill=fieldposbin)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity")


fit <- fitdistr(punt_df$pff_GAINLOSS, "normal")


class(fit)

para <- fit$estimate


rnorm(1, mean = para[1], sd = para[2])



punt_yg_sim <- function() {
  return(rnorm(1, mean = para[1], sd = para[2]))
}




##### FUNCTIONS MADE SO FAR #####

## OVERALL PLAY DECISION ##

# playtype_sim 


## RUN YARDS GAINED SIMULATOR ## 

# run_sim_yg

## FIELD GOAL SUCCESS SIMULATOR ##

# fieldgoal_sim 




#### FIRST DOWN ####
plays_firstdown_open <- plays[plays$down == 1 & plays$pff_DISTANCE == 10,]

#hard code this as probability for 1st and ten
pass_prop <- count(plays_firstdown_open[plays_firstdown_open$playtype == 'pass',]) /(count(plays_firstdown_open[plays_firstdown_open$playtype == 'run',]) + count(plays_firstdown_open[plays_firstdown_open$playtype == 'pass',]))
run_prop <- count(plays_firstdown_open[plays_firstdown_open$playtype == 'run',]) /(count(plays_firstdown_open[plays_firstdown_open$playtype == 'run',]) + count(plays_firstdown_open[plays_firstdown_open$playtype == 'pass',]))

#### NEXT SCORE SIMULATION ####

down_test <-  1
fieldpos_test <- 50
pff_DISTANCE_test <- 10
team <- 1
run_on_first_percentage <- 0

testing.data <- data.frame(down_test, fieldpos_test, pff_DISTANCE_test, team, run_on_first_percentage )

testing.data <- testing.data %>% 
  rename(down = down_test,
         fieldpos = fieldpos_test,
         pff_DISTANCE = pff_DISTANCE_test)


testing.data$play <- NA

yards_gained <- 0



next_points <- function(testing.data){
  
  down_test <-  1
  fieldpos_test <- 50
  pff_DISTANCE_test <- 10
  team <- 1
  run_on_first_percentage <- 0
  
  testing.data <- data.frame(down_test, fieldpos_test, pff_DISTANCE_test, team, run_on_first_percentage )
  
  testing.data <- testing.data %>% 
    rename(down = down_test,
           fieldpos = fieldpos_test,
           pff_DISTANCE = pff_DISTANCE_test)
  
  
  testing.data$play <- NA
  
  yards_gained <- 0
  
  
  
  while( 0 < testing.data$fieldpos && testing.data$fieldpos < 100){ # WHILE EITHER TEAM IS NOT IN EITHER ENDZONE
    
    if (testing.data$down == 1 & testing.data$team == 1){ # CALL DIFFERENT FUNCTION BASED ON PERCENTAGE
      testing.data$play <- sample(c('run','pass'), size = 1, prob = c(run_prop, pass_prop), replace = TRUE)
      if (testing.data$play== 'run'){
        yards_gained <- round(run_sim_yg(testing.data))
      }
      if (testing.data$play== 'pass'){
        yards_gained <- round(pass_sim_yg(testing.data))
      }
      
    }
    else{ # FIND WHAT KIND OF PLAY TO RUN
      ######################
      testing.data$play <- playtype_sim(testing.data) 
      ######################
    }
    
    if (testing.data$play == "punt"){                                       # PUNT
      # SIMULATE PUNT
      ######################
      yards_gained <- round(punt_yg_sim())
      ######################
      
      testing.data$fieldpos = testing.data$fieldpos + yards_gained
      testing.data$down <- 1
      testing.data$pff_DISTANCE <- 10
      testing.data$fieldpos <- 100-testing.data$fieldpos
      testing.data$team <- testing.data$team*-1
      next
    }
    else if (testing.data$play == "fg"){                                    # FG
      # SIMULATE FG
      ######################
      make_or_miss <- fieldgoal_sim(testing.data) 
      ######################
      
      if (make_or_miss == "FG"){ # MAKE
        if (testing.data$team == -1)
          return(-3)
        else
          return(-1*-3)
      }
      else { # MISS AND TURNOVER
        testing.data$fieldpos <- 100-testing.data$fieldpos
        testing.data$down <- 1
        testing.data$pff_DISTANCE <- 10
        testing.data$team <- -1*testing.data$team
      }
    }
    else if (testing.data$play == "run"){                                   # RUN
      # SIMULATE RUN
      ######################
      yards_gained <- round(run_sim_yg(testing.data))
      ######################
    }
    else if (testing.data$play == "pass"){                                   # PASS
      # SIMULATE PASS
      ######################
      yards_gained <- round(pass_sim_yg(testing.data)) # INSERT CORRECT FUNCTION
      ######################
    }
    
    # RECLALCULATE DOWN, YTG, FIELD_POS
    testing.data$fieldpos <- testing.data$fieldpos+yards_gained
    testing.data$pff_DISTANCE <- testing.data$pff_DISTANCE - yards_gained
    if (testing.data$pff_DISTANCE <= 0){
      testing.data$down <- 1
      testing.data$pff_DISTANCE <- 10
    }
    else{
      testing.data$down <- testing.data$down+1
    }
    
    # CHECK FOR TURNOVER ON DOWNS
    if (testing.data$down == 5 && testing.data$fieldpos < 100){
      testing.data$down <- 1
      testing.data$pff_DISTANCE <- 10
      testing.data$fieldpos = 100-testing.data$fieldpos
      testing.data$team = -1*testing.data$team
      next
    }
  }
  
  # CHECK WHO SCORED AND WHAT TYPE OF SCORE
  if (testing.data$fieldpos >= 100){                                          # TOUCHDOWN
    if (testing.data$team ==  1)
      return(-1*-7)
    else
      return(-7)
  }
  else if (testing.data$fieldpos <= 0){                                       # SAFETY
    if (testing.data$team ==  1)
      return(-2)
    else
      return(-1*-2)
  }
}

next_points_pass <- function(testing.data){
  
  down_test <-  1
  fieldpos_test <- 50
  pff_DISTANCE_test <- 10
  team <- 1
  run_on_first_percentage <- 0
  
  testing.data <- data.frame(down_test, fieldpos_test, pff_DISTANCE_test, team, run_on_first_percentage )
  
  testing.data <- testing.data %>% 
    rename(down = down_test,
           fieldpos = fieldpos_test,
           pff_DISTANCE = pff_DISTANCE_test)
  
  
  testing.data$play <- NA
  
  yards_gained <- 0
  
  
  
  while( 0 < testing.data$fieldpos && testing.data$fieldpos < 100){ # WHILE EITHER TEAM IS NOT IN EITHER ENDZONE
    
    if (testing.data$down == 1 & testing.data$team == 1){ # CALL DIFFERENT FUNCTION BASED ON PERCENTAGE
      testing.data$play <- sample(c('run','pass'), size = 1, prob = c(run_prop, pass_prop +.02), replace = TRUE)
      if (testing.data$play== 'run'){
        yards_gained <- round(run_sim_yg(testing.data))
      }
      if (testing.data$play== 'pass'){
        yards_gained <- round(pass_sim_yg(testing.data))
      }
      
    }
    else{ # FIND WHAT KIND OF PLAY TO RUN
      ######################
      testing.data$play <- playtype_sim(testing.data) 
      ######################
    }
    
    if (testing.data$play == "punt"){                                       # PUNT
      # SIMULATE PUNT
      ######################
      yards_gained <- round(punt_yg_sim())
      ######################
      
      testing.data$fieldpos = testing.data$fieldpos + yards_gained
      testing.data$down <- 1
      testing.data$pff_DISTANCE <- 10
      testing.data$fieldpos <- 100-testing.data$fieldpos
      testing.data$team <- testing.data$team*-1
      next
    }
    else if (testing.data$play == "fg"){                                    # FG
      # SIMULATE FG
      ######################
      make_or_miss <- fieldgoal_sim(testing.data) 
      ######################
      
      if (make_or_miss == "FG"){ # MAKE
        if (testing.data$team == -1)
          return(-3)
        else
          return(-1*-3)
      }
      else { # MISS AND TURNOVER
        testing.data$fieldpos <- 100-testing.data$fieldpos
        testing.data$down <- 1
        testing.data$pff_DISTANCE <- 10
        testing.data$team <- -1*testing.data$team
      }
    }
    else if (testing.data$play == "run"){                                   # RUN
      # SIMULATE RUN
      ######################
      yards_gained <- round(run_sim_yg(testing.data))
      ######################
    }
    else if (testing.data$play == "pass"){                                   # PASS
      # SIMULATE PASS
      ######################
      yards_gained <- round(pass_sim_yg(testing.data)) # INSERT CORRECT FUNCTION
      ######################
    }
    
    # RECLALCULATE DOWN, YTG, FIELD_POS
    testing.data$fieldpos <- testing.data$fieldpos+yards_gained
    testing.data$pff_DISTANCE <- testing.data$pff_DISTANCE - yards_gained
    if (testing.data$pff_DISTANCE <= 0){
      testing.data$down <- 1
      testing.data$pff_DISTANCE <- 10
    }
    else{
      testing.data$down <- testing.data$down+1
    }
    
    # CHECK FOR TURNOVER ON DOWNS
    if (testing.data$down == 5 && testing.data$fieldpos < 100){
      testing.data$down <- 1
      testing.data$pff_DISTANCE <- 10
      testing.data$fieldpos = 100-testing.data$fieldpos
      testing.data$team = -1*testing.data$team
      next
    }
  }
  
  # CHECK WHO SCORED AND WHAT TYPE OF SCORE
  if (testing.data$fieldpos >= 100){                                          # TOUCHDOWN
    if (testing.data$team ==  1)
      return(-1*-7)
    else
      return(-7)
  }
  else if (testing.data$fieldpos <= 0){                                       # SAFETY
    if (testing.data$team ==  1)
      return(-2)
    else
      return(-1*-2)
  }
}




#control_1 <- (replicate(200,next_points(testing.data)))
#control_2 <- (replicate(200,next_points(testing.data)))
#control_3 <- (replicate(200,next_points(testing.data)))
#control_4 <- (replicate(200,next_points(testing.data)))
#control_5 <- (replicate(200,next_points(testing.data)))
#control_11 <- (replicate(200,next_points(testing.data)))
#control_22 <- (replicate(200,next_points(testing.data)))
#control_33 <- (replicate(200,next_points(testing.data)))
#control_44 <- (replicate(200,next_points(testing.data)))
#control_55 <- (replicate(200,next_points(testing.data)))


#pass_more_1 <- (replicate(200,next_points_pass(testing.data)))
#pass_more_2 <- (replicate(200,next_points_pass(testing.data)))
#pass_more_3 <- (replicate(200,next_points_pass(testing.data)))
#pass_more_4 <- (replicate(200,next_points_pass(testing.data)))
#pass_more_5 <- (replicate(200,next_points_pass(testing.data)))
#pass_more_11 <- (replicate(200,next_points_pass(testing.data)))
#pass_more_22 <- (replicate(200,next_points_pass(testing.data)))
#pass_more_33 <- (replicate(200,next_points_pass(testing.data)))
#pass_more_44 <- (replicate(200,next_points_pass(testing.data)))
#pass_more_55 <- (replicate(200,next_points_pass(testing.data)))





control_open <- c(control_1, control_2,control_3, control_4, control_5,control_11, control_22,control_33, control_44, control_55)
pass_heavy <- c(pass_more_1, pass_more_2,pass_more_3, pass_more_4, pass_more_5,pass_more_11, pass_more_22,pass_more_33, pass_more_44, pass_more_55)

results <- data.frame(control_open,pass_heavy)

mean(results$control_open)
mean(results$pass_heavy)

boxplot(results$control_open, results$pass_heavy)






