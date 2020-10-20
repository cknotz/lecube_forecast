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

# Change datetime format
cubedata$time <- substr(cubedata$time, 1, 16)
  cubedata$time <- as.POSIXct(cubedata$time)
  cubedata$day <- weekdays(cubedata$time,
                           abbreviate = T)
  
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


