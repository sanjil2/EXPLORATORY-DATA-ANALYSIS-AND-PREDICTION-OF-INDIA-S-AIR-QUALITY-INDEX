#Preprocessing

#setting up the directory
#setwd("D:\\SEM 6\\CSE3020 DV PROJECT\\data.csv")
getwd()

airq = read.csv("data.csv")
airq
head(airq)
ncol(airq)
nrow(airq)
class(airq)

#Number of missing values
#install.packages("tidyverse")
library(tidyverse)
missing = airq %>% summarise_all(~ sum(is.na(.)))
missing

#summary of the Arguments
summ = lapply(airq, summary)
summ

unique(airq$agency)

#Statewise count
statewise = airq %>% count(state)
statewise
ggplot(statewise, aes(x=n, y =state)) + geom_bar(stat = "identity")

#Area type count
areawise = airq %>% count(type)
areawise
ggplot(areawise, aes(x=n, y=type)) + geom_bar(stat = "identity")

#Function to find mean State-wise
mean_statewise = function(a,d) {
  airq1 = data.frame(a,d)
  airq2 = na.omit(airq1)
  return(aggregate(airq2$a,list(airq2$d), FUN=mean))  
}

#so2 State-wise
ggplot(mean_statewise(airq$so2, airq$state), aes(x=x, y =Group.1)) + geom_bar(stat = "identity") + labs(x="so2",y="States")

#no2 State-wise
ggplot(mean_statewise(airq$no2, airq$state), aes(x=x, y =Group.1)) + geom_bar(stat = "identity") + labs(x="no2",y="States")

#rspm State-wise
ggplot(mean_statewise(airq$rspm, airq$state), aes(x=x, y =Group.1)) + geom_bar(stat = "identity") + labs(x="rspm",y="States")

#spm State-wise
ggplot(mean_statewise(airq$spm, airq$state), aes(x=x, y =Group.1)) + geom_bar(stat = "identity") + labs(x="spm",y="States")

#pm2_5 State-wise
ggplot(mean_statewise(airq$pm2_5, airq$state), aes(x=x, y =Group.1)) + geom_bar(stat = "identity") + labs(x="pm2_5",y="States")

