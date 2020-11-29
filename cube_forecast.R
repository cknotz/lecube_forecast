########################################################################
# Download, graph and forecast occupancy rates of Le Cube bouldering gym
########################################################################

# Carlo Knotz

library(ggplot2)
library(tidyverse)
library(googlesheets4)
library(forecast)

# Get metadata
sheet <- readLines("sheet.txt")
mail <- readLines("mail.txt")
load("bluethemes.RData") 

# Authenticate
options(gargle_oauth_cache = ".secrets")
gs4_auth(email = mail,
         cache = ".secrets")

# Download data
cubedata <- read_sheet(ss=sheet)

# Remove first observation - originally test
cubedata <- cubedata[2:nrow(cubedata),]

# Change datetime format
cubedata$time <- substr(cubedata$time, 1, 16)
  cubedata$time <- as.POSIXct(cubedata$time, tz="Europe/Paris")
  cubedata$day <- weekdays(cubedata$time,
                           abbreviate = T)
  cubedata$hour <- format(cubedata$time,
                          format = "%H:%M:%S")
  
# Setting values to zero during closed hours
cubedata$occ[cubedata$hour %in% c("23:00:00","23:30:00","00:00:00","00:30:00","01:00:00",
                                  "01:30:00","02:00:00","02:30:00","03:00:00","03:30:00",
                                  "04:00:00","04:30:00","05:00:00","05:30:00","06:00:00",
                                  "06:30:00","07:00:00","07:30:00","08:00:00","08:30:00",
                                  "09:00:00","09:30:00") ] <- 0

cubedata$occ[cubedata$hour %in% c("20:30:00","21:00:00","21:30:00","22:00:00","22:30:00")
             & cubedata$day %in% c("Sat","Sun")] <- 0
    
# Linearly interpolating missing values
cubedata$occ_inter <- zoo::na.approx(cubedata$occ)

# Graph
ggplot(data=cubedata,aes(x=time)) +
  geom_line(aes(y=occ_inter)) +
  #geom_line(aes(y=occ)) +
  geom_hline(yintercept = 100,linetype="dashed",color="gray") +
  #stat_smooth(aes(y=occ),linetype="dashed",color="gray21", alpha=.2,size=.5) +
  #scale_x_datetime(limits = as.POSIXct(c("2020-10-10 16:00:00","2020-10-17 16:00:00"), tz="Europe/Paris")) +
  ylab("Occupancy (%)") +
  xlab("") +
  theme_bw()
  ggsave("firstdata.png", dpi = 600)

# Averages per day
##################

# Comparison figures
compa <- aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),mean) # Specific day/time averages
norm <- compa$x[compa$Group.1 == tail(cubedata$day,1) & compa$Group.2 == tail(cubedata$hour,1)]

compa <- aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),max) # Specific day/time averages
max <- compa$x[compa$Group.1 == tail(cubedata$day,1) & compa$Group.2 == tail(cubedata$hour,1)]

##########
# Forecast
##########
  
# Setting test and train data
#############################
  
traindata <- subset(cubedata, time<= as.POSIXct("2020-11-03 23:30:00", tz="Europe/Paris")) # pre-Nov lockdown

  firsthr <- 48*(as.Date("2020-10-10 16:00:00")-as.Date("2020-01-01 00:00:00"))
  train <- ts(traindata$occ_inter,
                 start = c(2020,firsthr),
                 frequency = 48*365) 

testdata <- subset(cubedata, time>= as.POSIXct("2020-11-23 00:00:00", tz="Europe/Paris")) # post-Nov lockdown
  
  firsthr <- 48*(as.Date("2020-11-23 00:00:00")-as.Date("2020-01-01 00:00:00"))
  test <- ts(testdata$occ_inter,
             start = c(2020,firsthr),
             frequency = 48*365)
  
# Descriptives
#############
  
mean(traindata$occ_inter)
  sd(traindata$occ_inter)
mean(testdata$occ_inter)
  sd(testdta$occ_inter) # looks reasonably similar


# Result dataset
s <- 2 # start
  e <- 24 # end
  step <- 1 #step
  
eval <- data.frame(p = seq(from=s,to=e,by=step),
                   RMSE = seq(from=s,to=e,by=step),
                   MAE = seq(from=s,to=e,by=step))
  
# NNETAR

for(k in eval$p) {
fit <- nnetar(traindata$occ_inter,
              lambda = "auto",
              p=`k`) # loop over values for this
  
  res <- nnetar(testdata$occ_inter, model = fit) # fit model to test data
  eval[which(eval$p==`k`),"RMSE"] <-  accuracy(res)[1,"RMSE"] # retrieve RMSE
  eval[which(eval$p==`k`),"MAE"] <- accuracy(res)[1,"MAE"] # retrieve MAE
}

eval %>% 
  pivot_longer(cols = c("RMSE","MAE"),
               names_to = "ind",
               values_to = "val") %>% 
  ggplot(aes(x=p,y=val,fill=ind)) +
      geom_col(position = "dodge")
 
 