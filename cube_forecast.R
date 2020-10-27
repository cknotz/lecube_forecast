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
  cubedata$time <- as.POSIXct(cubedata$time)
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
  geom_line(aes(y=occ_inter), color="red") +
  geom_line(aes(y=occ)) +
  geom_hline(yintercept = 100,linetype="dashed",color="gray") +
  #stat_smooth(aes(y=occ),linetype="dashed",color="gray21", alpha=.2,size=.5) +
  ylab("Occupancy (%)") +
  xlab("") +
  theme_bw()


# Averages per day
##################

# Comparison figures
compa <- aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),mean) # Specific day/time averages
norm <- compa$x[compa$Group.1 == tail(cubedata$day,1) & compa$Group.2 == tail(cubedata$hour,1)]

compa <- aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),max) # Specific day/time averages
max <- compa$x[compa$Group.1 == tail(cubedata$day,1) & compa$Group.2 == tail(cubedata$hour,1)]

# Forecast
##########
fit <- nnetar(cubedata$occ_inter)
  fcast <- forecast(fit,h=72)
  preddata <- data.frame(vals = c(fcast$x,fcast$mean),
                       count = 1:(length(fcast$x)+length(fcast$mean)))
  preddata$ind <- as.factor(ifelse(preddata$count<=length(fcast$x),"Observed","Forecast"))
  preddata$time <- seq(as.POSIXct("2020-10-10 16:00:00"),
            length.out = length(preddata$count),
            by = 1800)

lo <- length(cubedata$occ)-32
  hi <- length(preddata$vals)
  
  
ggplot(preddata[lo:hi,],aes(x = time,y=vals, group = ind,color = ind)) +
  geom_line() +
  geom_vline(xintercept = tail(cubedata$time,1)) +
  scale_color_manual(values = c("#fcba04","#68e8ff")) +
    theme_bw()
  
