# Generation of JSON format example input ---------------------------------
#' Test scraped HTML input when converted to JSON format can be passed 
#' to the prediction function successfully


  library(jsonlite)  
  source("scrape_oneTwoThreeDay.r")
  source("tools.r")
  source("predict_fault_3_day_forecast.R")


  # scrape an example HTML file
  exampleForecast <-  weatherScraperOneTwoThreeDay("http://feed.meteogroup.co.uk/SSE/north_2021_07_05_06_08_33.html") 
  
  # create a list object with the two days separated. 
  # Using names inferred from JSON file sent from Bellrock
  exampleForecastList <- list("3_day_forecast" = exampleForecast %>% filter(day == 3),
                              "4_day_forecast" = exampleForecast %>% filter(day == 4),
                              "5_day_forecast" = exampleForecast %>% filter(day == 5))
  
  # write to JSON format
  exampleForecastJSON <- toJSON(exampleForecastList)
  
  # write out for use
  cat(exampleForecastJSON, file = "dmp_scraper_output.json")
  
  # make predictions for the day one on the basis of this
  testPredictions <- predictionDay3("dmp_scraper_output.json")
  
  # view output
  testPredictions
  


