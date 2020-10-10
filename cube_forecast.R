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


# Graph
ggplot(data=cubedata,aes(x=time,y=occ)) +
  geom_line() +
  ylab("Occupancy (%)") +
  xlab("") +
  theme_bw()


