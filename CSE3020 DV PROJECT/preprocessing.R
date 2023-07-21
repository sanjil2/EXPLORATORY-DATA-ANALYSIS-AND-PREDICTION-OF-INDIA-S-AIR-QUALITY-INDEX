#Preprocessing

#setting up the directory
#setwd("D:\\SEM 6\\CSE3020 DV PROJECT\\data.csv")
#getwd()

airq = read.csv("D:\\SEM 6\\CSE3020 DV PROJECT\\data.csv")
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

#Missing value percentage
percen = function(x){
  return((x*100)/nrow(airq))
}
missing_per = lapply(missing, percen)
missing_per

#Plot the missing values attribute based
df1 = data.frame(s <- c('stn_code','stn_code','sampling_date','sampling_date','state',
                             'state','location','location','agency','agency','type','type',
                             'so2','so2','no2','no2','rspm','rspm','spm','spm',
                             'location_monitoring_station','location_monitoring_station',
                             'pm2_5','pm2_5','date','date'),
                c <- c('Present','Missing','Present','Missing','Present','Missing',
                            'Present','Missing','Present','Missing','Present','Missing',
                            'Present','Missing','Present','Missing','Present','Missing',
                            'Present','Missing','Present','Missing','Present','Missing',
                            'Present','Missing'),
                v <- c(66.93525,33.06475,99.9993115192,0.0006884808,100,0,
                           99.99931152,0.00068848,65.69507,34.30493,98.762341,1.237659,
                           92.048965,7.951035,96.27463,3.72537,90.769308,9.230692,45.5212,54.4788,
                           93.690991,6.309009,2.1375,97.8625,99.998393545,0.001606455))

df1$s <- factor(df1$s)
df1$c <- factor(df1$c)

ggplot(data=df1, aes(x=" ", y=v, group=c, colour=c, fill=c)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ s) +theme_void()

#Dealing with missing values
aq = airq[,-c(1,2,11)] #Removing stn_code, location_monitoring_station, sampling_date data as they are not required
aq = head(aq, - 3) #Removing last 3 rows as they do not contain any relevant information 
aq
tail(aq,10)

#Categorical Data
#Replacing NA values in agency to "unknown"
aq <- aq %>%
  replace_na(list(agency = 'unknown'))
aq

#Replacing NA values in Date with last observed date
library(zoo)
aq$date = na.locf(na.locf(aq$date),fromLast=TRUE)

#Replacing NA values in type with last observed type
aq$type = na.locf(na.locf(aq$type),fromLast=TRUE)

#Numerical Data
#Replacing so2 with mean of the respective state
so2_mean_statewise = mean_statewise(aq$so2, aq$state)
colnames(so2_mean_statewise) <- c('state','so2')
so2_mean_statewise
aq = left_join(aq, so2_mean_statewise, by = "state")
aq$so2 = coalesce(aq$so2.x, aq$so2.y)
aq = select(aq, -so2.x, -so2.y)

#Replacing no2 with mean of the respective state
no2_mean_statewise = mean_statewise(aq$no2, aq$state)
colnames(no2_mean_statewise) <- c('state','no2')
no2_mean_statewise
aq = left_join(aq, no2_mean_statewise, by = "state")
aq$no2 = coalesce(aq$no2.x, aq$no2.y)
aq = select(aq,-no2.x,-no2.y)

#Replacing rspm with mean of the respective state
rspm_mean_statewise = mean_statewise(aq$rspm, aq$state)
colnames(rspm_mean_statewise) <- c('state','rspm')
rspm_mean_statewise
aq = left_join(aq, rspm_mean_statewise, by = "state")
aq$rspm = coalesce(aq$rspm.x, aq$rspm.y)
aq = select(aq,-rspm.x,-rspm.y)

#Replacing NA values in spm with 0
aq$spm[is.na(aq$spm)] = 0

#Replacing NA values in pm2_5 with 0
aq$pm2_5[is.na(aq$pm2_5)] = 0

#Checking the count of NA values
missing = aq %>% summarise_all(~ sum(is.na(.)))
missing

#All the missing values have been dealt with.
aq1 = aq

#Calculating Day of the year for inputting into models
class(aq1$date)
d = as.Date(aq1$date, format = "%Y-%m-%d")
aq1$Day_of_yr = format(d,format="%j")

#Adding month of the year as an attribute
aq1$Month = format(d,format="%m")

#Caluclating SOi
SOi_calc = function(so2){
  si=0
  if (so2<=40){
    si= so2*(50/40)
  }
  else if(so2>40 && so2<=80){
      si= 50+(so2-40)*(50/40)
  }
  else if(so2>80 && so2<=380){
      si= 100+(so2-80)*(100/300)
  }
  else if(so2>380 && so2<=800){
      si= 200+(so2-380)*(100/420)
  }
  else if(so2>800 && so2<=1600){
      si= 300+(so2-800)*(100/800)
  }
  else if(so2>1600){
      si= 400+(so2-1600)*(100/800)
  }
  return(si)
}
aq1$SOi = sapply(aq1$so2, SOi_calc)


#Calculating NOi
NOi_calc = function(no2){
  ni=0
  if(no2<=40){
    ni= no2*(50/40)
  }
  else if(no2>40 && no2<=80){
      ni= 50+(no2-40)*(50/40)
  }
  else if(no2>80 && no2<=180){
      ni= 100+(no2-80)*(100/100)
  }
  else if(no2>180 && no2<=280){
      ni= 200+(no2-180)*(100/100)
  }
  else if(no2>280 && no2<=400){
      ni= 300+(no2-280)*(100/120)
  }
  else{
      ni= 400+(no2-400)*(100/120)
  }
  return(ni)
}

aq1$NOi = sapply(aq1$no2, NOi_calc)

#Calculating RSPMi
RSPMi_calc = function(rspm){
  rpi=0
  if(rpi<=30){
    rpi=rpi*50/30
  }
  else if(rpi>30 && rpi<=60){
      rpi=50+(rpi-30)*50/30
  }
  else if(rpi>60 && rpi<=90){
      rpi=100+(rpi-60)*100/30
  }
  else if(rpi>90 && rpi<=120){
      rpi=200+(rpi-90)*100/30
  }
  else if(rpi>120 && rpi<=250){
      rpi=300+(rpi-120)*(100/130)
  }
  else{
      rpi=400+(rpi-250)*(100/130)
  }
  return(rpi)
}

aq1$RSPMi = sapply(aq1$rspm, RSPMi_calc)

#Calculating SPMi
SPMi_calc = function(spm){
  spi=0
  if(spm<=50){
    spi=spm*50/50
  }
  else if(spm>50 && spm<=100){
      spi=50+(spm-50)*(50/50)
  }
  else if(spm>100 && spm<=250){
      spi= 100+(spm-100)*(100/150)
  }
  else if(spm>250 && spm<=350){
      spi=200+(spm-250)*(100/100)
  }
  else if(spm>350 && spm<=430){
      spi=300+(spm-350)*(100/80)
  }
  else{
      spi=400+(spm-430)*(100/430)
  }
  return(spi)
}

aq1$SPMi = sapply(aq1$spm, SPMi_calc)

#Calculating Air Quality Index (AQI)
AQI_calc = function(si,ni,rspmi,spmi){
  return(max(si,ni,rspmi,spmi))
}

aq1$AQI = mapply(AQI_calc, aq1$SOi,aq1$NOi, aq1$RSPMi, aq1$SPMi)

#Calculating AQI range
AQI_range_calc = function(x){
  if(x<=50){
    return("Good")
  }
  else if(x>50 && x<=100){
    return("Moderate")
  }
  else if(x>100 && x<=200){
    return("Poor")
  }
  else if(x>200 && x<=300){
    return("Unhealthy")
  }
  else if(x>300 && x<=400){
    return("Very Unhealthy")
  }
  else if(x>400){
    return("Hazardous")
  }
}

aq1$AQI_Range = sapply(aq1$AQI, AQI_range_calc)

#Displaying the frequency of AQI Range values
r = aq1 %>% count(AQI_Range)
r
ggplot(r, aes(x=c("Good","Poor","Moderate","Unhealthy","Very Unhealthy","Hazardous"), y=n)) + geom_bar(stat = "identity") + labs(x="AQI Range",y="Frequency")

aq1

#Checking Each state's AQI Range based on mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

statewise_AQI = aq1 %>% group_by(state)  %>% summarize(Range = getmode(AQI_Range))
statewise_AQI
print(as_tibble(statewise_AQI), n=34)

#Finding states which have bad pollution levels
bad_AQI = which(statewise_AQI$Range == "Hazardous" | statewise_AQI$Range == "Poor")
print(statewise_AQI[bad_AQI,1])


#Exporting as a .csv file
write.csv(aq1,"PreprocessedinR.csv", row.names = FALSE)