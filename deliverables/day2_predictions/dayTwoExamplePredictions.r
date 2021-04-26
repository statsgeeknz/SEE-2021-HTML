# Generation of JSON format example input ---------------------------------
#' Test scraped HTML input when converted to JSON format can be passed 
#' to the prediction function successfully


  library(jsonlite)  
  source("scrape_oneTwoDay.r")
  source("tools.r")
  source("predict_fault_2_day_forecast.R")


  # scrape an example HTML file
  exampleForecast <-  weatherScraperOneTwoDay("north_2015_10_03_05_11_41.html") 
  
  # create a list object with the two days separated. 
  # Using names inferred from JSON file sent from Bellrock
  exampleForecastList <- list("1_day_forecast" = exampleForecast %>% filter(day == 1),
                              "2_day_forecast" = exampleForecast %>% filter(day == 2))
  
  # write to JSON format
  exampleForecastJSON <- toJSON(exampleForecastList)
  
  # write out for use
  cat(exampleForecastJSON, file = "dmp_scraper_output.json")
  
  # make predictions for the day one on the basis of this
  testPredictions <- predictionDay2("dmp_scraper_output.json")
  
  # view output
  testPredictions
  

