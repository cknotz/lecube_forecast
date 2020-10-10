# lecube_forecast
Scraping occupancy stats from bouldering gym website &amp; forecasting future occupancy

`cube_scrape.R` extracts the current occupancy of the Le Cube climbing gym in Lausanne (lecube.ch) and saves it along with a timestamp in a dedicated GoogleSheet.

`lecube_forecast.R` downloads the scraped data from GoogleSheet and graphs them.

*To do*: Once more data are available, forecast using neural net; add summary stats; wrap in Shiny dashboard
