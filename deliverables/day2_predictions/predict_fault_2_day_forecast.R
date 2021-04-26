#!/usr/bin/Rscript

#==================================================================================
#' Predictions for the 2 day forecasts
#' This function takes datasets from the webscraper (meteo-group forcast HTML) and
#' does a little formatting, before passing into model object fitted to the 2 day
#' forecast data. 
#' 
#' NB - hard-coding the model name and prediction day. Gets passed to JSON then Lumen
#' 
#' ==
#' 
#' Args:
#' 
#' inputData - a dataset from the 1-2 day forecast webscraper [i.e. dataframe with 
#' "dateOfForecast", "region", "temp_max", "temp_min", "wind_mean", "wind_direction" 
#' "wind_gust_max", "wind_gust_min", "rain_max", "rain_min", "snow_depth", "icing", "snow_height",
#' "lightningCat", "risk"
#' 
#'  ==
#'   
#'  Value:
#'  outputFrame - a dataframe containing:
#'  Region - the 9 regions by name (of format in HTML file)
#'  Prediction - best guess at number of predictions (rounded to whole)
#'  Worst_Case - Lower 95% PI - the lower level of predicted faults
#'  Best_Case - Upper 95% PI - the upper level of predicted faults
#============================================================================

predictionDay2 <- function(inputFile){

  library(tidyverse)
  library(lubridate)
  library(gamboostLSS) # these mask "select" from dplyr (because of MASS library)
  library(gamlss.dist)
  
  # hard-coding in the model object and day as Bellrock want separate analytics
  # Input data has days 1 & 2 days represented
  # Bit onerous loading these models - can be large
  
  
  modelObj <- readRDS("gamLSS_4111_8000.rds")
  inputDay <- 2
  inputData <- fromJSON(inputFile)
  inputData <- inputData[['2_day_forecast']]
  
  #' modify the data. Assume here that only have data for the day in question e.g.
  #' only day 2 forecast data corresponding to the day 2 model object
  #' Turn dates into a date object, calculate the day these forecasts apply to,
  #' and create factors with appropriate levels (i.e. all possible values)
  
  inputData <- inputData %>% mutate(dateOfForecast = parse_date_time(dateOfForecast, "d!%Om!*%Y!")) %>% 
    filter(day == inputDay) %>%
    mutate(region = factor(region),
           day = as.numeric(day),
           MonthFactor = as.factor(month(dateOfForecast)), 
           DayMonth = yday(dateOfForecast),
           faultDate = dateOfForecast + days(day-1),
           wind_direction = as.numeric(as.character(wind_direction)),
           wind_gust_min = as.numeric(as.character(wind_gust_min)),
           wind_gust_max = as.numeric(as.character(wind_gust_max)),
           wind_mean = as.numeric(as.character(wind_mean)),
           temp_max = as.numeric(as.character(temp_max)),
           temp_min = as.numeric(as.character(temp_min)),
           snow_depth = as.numeric(as.character(snow_depth)),
           lightningCat = factor(substr(lightningCat, 1, 1)),
           icing = factor(icing), # this is binary 0 no icing, 1 icing
           risk = factor(risk) # this is the meteogroup risk variable: green, amber, red
    )
  
  
  # make sure the factors in the input data match possible levels from the modelling phase
  inputData <- inputData %>% 
    mutate(region = factor(region,
                           levels = c("Aberdeenshire", "Argyll & West Highland", 
                                      "Central Highlands", "Moray Firth", 
                                      "NW Highland & Skye", "Orkney & NE Caithness", 
                                      "Perth & Angus", "Shetland", "Western Isles")),
           MonthFactor=factor(MonthFactor, 
                              levels = c("1",  "2" , "3",  "4",  
                                         "5" , "6" , "7",  "8",  
                                         "9",  "10", "11", "12" )),
           lightningCat = factor(lightningCat,
                                 levels = c("2", "3", "4") # note no level 1 lightning observed for day 2 training data
           ),
           icing = factor(icing, levels = c("0", "1")),
           risk = factor(risk, levels = c("Amber", "Green", "Red")),
    )
  
  # some checks on data - roughly bounding these based on the modelling data
  
  if(any(is.na(inputData$wind_direction) | inputData$wind_direction<0 | inputData$wind_direction>360 )==TRUE) {
    print("One or more wind directions invalid (missing or outside 0-360 degrees)")
  }
  
  if(any(is.na(inputData$wind_mean) | inputData$wind_mean < 5 | inputData$wind_mean > 50 )==TRUE) {
    print("One or more mean wind values are not supported (missing or more extreme than training data)")
    inputData$wind_mean <- sapply(inputData$wind_mean, rangeSnap, rangeToSnap = c(5, 50))
  }
  
  if(any(is.na(inputData$wind_gust_min) | inputData$wind_gust_min<5 | inputData$wind_gust_min>75 )==TRUE){
    print("One or more min gust values are not supported (missing or more extreme than training data)")
    inputData$wind_gust_min <- sapply(inputData$wind_gust_min, rangeSnap, rangeToSnap = c(5, 75))
  }
  
  if(any(is.na(inputData$wind_gust_max) | inputData$wind_gust_max<10 | inputData$wind_gust_max>90 )==TRUE){
    print("One or more min gust values are not supported (missing or more extreme than training data)")
    inputData$wind_gust_max <- sapply(inputData$wind_gust_max, rangeSnap, rangeToSnap = c(10, 90))
  }
  
  if(any(is.na(inputData$temp_max) | inputData$temp_max<(-1) | inputData$temp_max>30 )==TRUE){
    print("One or more temperature max values are not supported (missing or more extreme than training data)")
    inputData$temp_max <- sapply(inputData$temp_max, rangeSnap, rangeToSnap = c(-1, 30))
  }
  
  if(any(is.na(inputData$temp_min) | inputData$temp_min<(-8) | inputData$temp_min>17 )==TRUE){ 
    print("One or more temperature min values are not supported (missing or more extreme than training data)")
    inputData$temp_min <- sapply(inputData$temp_min, rangeSnap, rangeToSnap = c(-8, 17))
  }
  
  if(any(is.na(inputData$snow_depth) | inputData$snow_depth<0 | inputData$snow_depth>14 )==TRUE){
    print("One or more snow depth values are not supported (missing or more extreme than training data)")
    inputData$snow_depth <- sapply(inputData$snow_depth, rangeSnap, rangeToSnap = c(0, 14))
  }
  
  if(any(is.na(inputData$icing) | !(inputData$icing %in% c("0", "1"))) ==TRUE){
    print("One or more icing values are not supported (missing or not 0/1)")
  }
  
  if(any(is.na(inputData$risk) | !(inputData$risk %in% c("Amber", "Green", "Red"))) ==TRUE){
    print("One or more risk values are not supported (missing or not known category)")
  } 
  
  
  
  # Compute predictions and corresponding prediction interval
  mu  <-    predict(modelObj, parameter = "mu", newdata=inputData, type=c("response")) 
  sigma <-  predict(modelObj, parameter = "sigma", newdata=inputData, type=c("response"))
  fooR <- t(mapply(rZIP, mu, sigma, n=10000))
  LPI <- apply(fooR, 1, quantile, probs = 0.025)
  UPI <- apply(fooR, 1, quantile, probs = 0.975)
  
  outputFrame <- data.frame("Region"=inputData$region,
                            "Prediction"=round(mu, digits=3),  
                            "Prediction_rounded"=round(mu),
                            "Best_Case"=LPI, 
                            "Worst_Case"=UPI)
  
  # bound by historical max
  bounds <- data.frame(Region = factor(c("Aberdeenshire", "Argyll & West Highland", 
                                         "Central Highlands", "Moray Firth", 
                                         "NW Highland & Skye", "Orkney & NE Caithness", 
                                         "Perth & Angus", "Shetland", "Western Isles")),
                       maxFaults = c(34, 28, 17, 11, 23, 18, 45, 13, 11)
  )
  
  outputFrame <- outputFrame %>% right_join(bounds, by = "Region") %>% mutate(
    Prediction = ifelse(Prediction > maxFaults, maxFaults, Prediction),  
    Prediction_rounded = ifelse(Prediction_rounded > maxFaults, maxFaults, Prediction_rounded),
    Best_Case = ifelse(Best_Case > maxFaults, maxFaults, Best_Case),
    Worst_Case = ifelse(Worst_Case > maxFaults, maxFaults, Worst_Case)) %>%
    dplyr::select(-maxFaults)
  

  return(outputFrame)

} # end of function



# Additions to for Lumen --------------------------------------------------

# result<-tryCatch({
# 
#   #load libraries required for Bellrock functions
#   require(jsonlite)
#   require(rredis)
# 
#   #Source functions for saving results or exceptions to redis
#   source("writeToRedis.R")
#   source("writeExceptionToRedis.R")
# 
#   #take parameters from commanf line, first must be uuid.
#   args <- commandArgs(trailingOnly = TRUE)
#   uuid <-args[1]
#   fileLoc <-args[2]
# 
#   #execute the core function
#   output <- predictionFunction(fileLoc)
#   
#   #if repeating csv, save under a parameter name, otherwise this is not needed and each
#   #parameter can be saved on its own
#   res.list<-list(oneDayPredictions=output)
# 
#   #save to redis
#   writeToRedis(uuid,res.list)
# 
# }, error=function(err){
#   writeExceptionToRedis(uuid,err)
# })


