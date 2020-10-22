########################################################################
# Download, graph and forecast occupancy rates of Le Cube bouldering gym
########################################################################

# Carlo Knotz

library(ggplot2)
library(tidyverse)
library(googlesheets4)

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

aggregate(cubedata$occ_inter,list(cubedata$hour),mean) # average per hour

aggregate(cubedata$occ_inter,list(cubedata$day),mean) # average per day

aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),mean) # average per day & hour

res <- aggregate(cubedata$occ_inter,list(cubedata$day,cubedata$hour),mean) # Specific day/time average
  res$x[res$Group.1 == "Fri" & res$Group.2 == "14:00:00"]


