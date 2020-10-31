# lecube_forecast
Scraping occupancy stats from bouldering gym website &amp; forecasting future occupancy. See also https://carlo-knotz.medium.com/crowd-avoidance-with-data-science-cc2567f9eecb for more info.

`cube_scrape.R` extracts the current occupancy of the Le Cube climbing gym in Lausanne (lecube.ch) and saves it along with a timestamp in a dedicated GoogleSheet.

`lecube_forecast.R` downloads the scraped data from GoogleSheet and graphs them. This file is my sandbox for trying out new things or creating custom graphs.

`lecube_forecast_dashboard.R` generates a neat dashboard with `Shiny` to present the results and some background information. The dashboard is also deployed on: https://cknotz.shinyapps.io/lecube_forecast/

*To do*: Once more data are available, I will properly test different forecasting algorithms based on a train-test split of the data. The best-performing algorithm will then be used on the dashboard.
