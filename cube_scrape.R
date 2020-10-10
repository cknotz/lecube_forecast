######################################################################
# Code to scrape occupancy rate from lecube.ch & store in Google Sheet
######################################################################

# Carlo Knotz

library(tidyverse)
library(webdriver)
library(rvest)

# Invoking phantomjs() - website is dynamically created
#install_phantomjs()
pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)

# Scraping occupancy rate
#########################
url <- "http://www.lecube.ch/"
xpath <- "//*[@id='frequentation_jauge_current_raw']"

pjs_session$go(url)
rendered_source <- pjs_session$getSource()
cube <- read_html(rendered_source)

occ <- cube %>% 
  html_node(xpath=xpath) %>% 
  html_text() %>% 
  gsub("%","",.)

time <- as.character.Date(Sys.time())

cube_occ <- data.frame(time = time,
                          occ = as.numeric(occ))

# Export to Googlesheet
#######################
require(googlesheets4)

sheet <- readLines("sheet.txt")
mail <- readLines("mail.txt")

gs4_auth(
  cache = ".secrets",
 email = mail
)

sheet_append(ss=sheet,
             data=cube_occ,
             sheet = "data")

# Quitting R
quit(save = "no")