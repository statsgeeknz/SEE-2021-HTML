# Generation of JSON format example input ---------------------------------
#' Test scraped HTML input when converted to JSON format can be passed 
#' to the prediction function successfully


  library(jsonlite)  
  source("scrape_fourFiveDay.R")
  source("tools.r")
  source("predict_fault_5_day_forecast.R")


  # scrape an example HTML file
  exampleForecast <-  weatherScraperFourFiveDay("http://feed.meteogroup.co.uk/SSE/north_2021_07_05_06_08_33.html") 
  
  # create a list object with the two days separated. 
  exampleForecastList <- list("4_day_forecast" = exampleForecast %>% filter(day == 4),
  # Using names inferred from JSON file sent from Bellrock
                              "5_day_forecast" = exampleForecast %>% filter(day == 5))
  
  # write to JSON format
  exampleForecastJSON <- toJSON(exampleForecastList)
  
  # write out for use
  cat(exampleForecastJSON, file = "dmp_scraper_output.json")
  
  # make predictions for the day one on the basis of this
  testPredictions <- predictionDay5("dmp_scraper_output.json")
  
  # view output
  testPredictions
  


