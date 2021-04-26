#!/usr/bin/Rscript

#==================================================================================
#' Predictions for the 3 day forecasts
#' This function takes datasets from the webscraper (meteo-group forcast HTML) and
#' does a little formatting, before passing into model object fitted to the 3 day
#' forecast data. 
#' 
#' NB - hard-codig the model name and prediction day. Gets passed to JSON then Lumen
#' 
#' ==
#' 
#' Args:
#' 
#' inputData - a dataset from the 3-5 day forecast webscraper [i.e. dataframe with 
#' HTML forecast date, temp (min/max), wind (mean/directin/gust), rain (min/max),
#'  snow/line-icing (depth/icing/height), lightning (risk group)]
#' 
#'  ==
#'   
#'  Value:
#'  outputFrame - a dataframe containing:
#'  DistrictCode - G1, G2, G3 corresponding to regions below
#'  Region - the 3 regions by name (of format in HTML file)
#'  Prediction - best guess at number of predictions (rounded to whole)
#'  Worst_Case - Lower 95% PI - the lower level of predicted faults
#'  Best_Case - Upper 95% PI - the upper level of predicted faults
#============================================================================

predictionDay3 <- function(inputFile){


  library(gamboostLSS) # these mask "select" from dplyr (because of MASS library)
  library(gamlss.dist)
  library(tidyverse)
  library(lubridate)
  
  # hard-coding in the model object and day as Bellrock want separate analytics
  # Input data has days 3 - 5 represented
  # Bit onerous loading these models - can be large
  
  modelObj <- readRDS("gamLSS_day3_130120.rds")
  inputDay <- 3
  
  #inputData <- read.csv(inputFile, header = T)
  inputData <- fromJSON(inputFile)
  inputData <- inputData[['3_day_forecast']]
  
  #' modify the data. Assume here that only have data for the day in question e.g.
  #' only day 3 forecast data corresponding to the day 3 model object
  #' Turn dates into a date object, calculate the day these forecasts apply to,
  #' and create factors with appropriate levels (i.e. all possible values)
  
  inputData <- inputData %>% mutate(dateOfForecast = parse_date_time(dateOfForecast, "d!%Om!*%Y!")) %>% 
    filter(day == inputDay) %>%
    mutate(region = factor(region),
           day = as.numeric(day),
           MonthFactor = as.factor(month(dateOfForecast)), 
           DayMonth = yday(dateOfForecast),
           faultDate = dateOfForecast + days(day-1),
           wind_direction = as.character(wind_direction),
           wind_gust = as.numeric(as.character(wind_gust)),
           wind_mean = as.numeric(as.character(wind_mean)),
           temp_max = as.numeric(as.character(temp_max)),
           temp_min = as.numeric(as.character(temp_min)),
           snow_depth = as.numeric(as.character(snow_depth)),
           lightning = factor(substr(lightning, 1, 1)),
           icing = factor(icing), # this is binary 0 no icing, 1 icing
    )


# make sure the factors in the input data match possible levels from the modelling phase
  inputData <- inputData %>% mutate(region = factor(region,
                                     levels = c("Perth, Angus, Aberdeen & Moray Firth",
                                                "Shetland, Orkney, NE Caithness",
                                                "Western Isles, NW & Cent Highlands, Skye, Argyll")),
                                    wind_direction = factor(wind_direction, levels = c( "E", "N", "NE", "NW", "S", "SE", "SW", "W" )),
                     MonthFactor = factor(MonthFactor, 
                                          levels = c("1",  "2" , "3",  "4",  
                                                     "5" , "6" , "7",  "8",  
                                                     "9",  "10", "11", "12" )),
                     icing = factor(icing, levels = c("0", "1")),
                     lightning = factor(x = lightning,
                                         levels = c("2", "3", "4")
                                         ))

  # some checks on data - roughly bounding these based on the modelling data
  
 
  
  if(any(is.na(inputData$wind_mean) | inputData$wind_mean < 10 | inputData$wind_mean > 60 )==TRUE) {
    print("One or more mean wind values are not supported (missing or more extreme than training data)")
    inputData$wind_mean <- sapply(inputData$wind_mean, rangeSnap, rangeToSnap = c(10, 60))
  }
  

  
  if(any(is.na(inputData$wind_gust) | inputData$wind_gust<15 | inputData$wind_gust>90 )==TRUE){
    print("One or more gust values are not supported (missing or more extreme than training data)")
    inputData$wind_gust <- sapply(inputData$wind_gust, rangeSnap, rangeToSnap = c(15, 90))
  }
  
  if(any(is.na(inputData$temp_max) | inputData$temp_max<(0) | inputData$temp_max>29 )==TRUE){
    print("One or more temperature max values are not supported (missing or more extreme than training data)")
    inputData$temp_max <- sapply(inputData$temp_max, rangeSnap, rangeToSnap = c(0, 29))
  }
  
  if(any(is.na(inputData$temp_min) | inputData$temp_min<(0) | inputData$temp_min>15 )==TRUE){ 
    print("One or more temperature min values are not supported (missing or more extreme than training data)")
    inputData$temp_min <- sapply(inputData$temp_min, rangeSnap, rangeToSnap = c(0, 15))
  }
  
  if(any(is.na(inputData$snow_depth) | inputData$snow_depth<0 | inputData$snow_depth>10 )==TRUE){
    print("One or more snow depth values are not supported (missing or more extreme than training data)")
    inputData$snow_depth <- sapply(inputData$snow_depth, rangeSnap, rangeToSnap = c(0, 10))
  }
  
  if(any(is.na(inputData$icing) | !(inputData$icing %in% c("0", "1"))) ==TRUE){
    print("One or more icing values are not supported (missing or not 0/1)")
  }
  
 
  
  
  # Compute predictions and corresponding prediction interval
  mu  <-    predict(modelObj, parameter = "mu", newdata=inputData, type=c("response")) 
  sigma <-  predict(modelObj, parameter = "sigma", newdata=inputData, type=c("response"))
  fooR <- t(mapply(rZIP, mu, sigma, n=10000))
  LPI <- apply(fooR, 1, quantile, probs = 0.025)
  UPI <- apply(fooR, 1, quantile, probs = 0.975)
  
  outputFrame <- data.frame(DistrictCode = inputData$regionCode,
                            Region = inputData$region,
                            Prediction = round(mu, digits=3),  
                            Prediction_rounded = round(mu),
                            Best_Case = LPI, 
                            Worst_Case =UPI)

  
  # bound by historical max
  bounds <- data.frame(Region = factor(c("Perth, Angus, Aberdeen & Moray Firth",
                                         "Shetland, Orkney, NE Caithness",
                                         "Western Isles, NW & Cent Highlands, Skye, Argyll")),
                       maxFaults = c(91, 46, 90)
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
#   res.list<-list(threeDayPredictions=output)
# 
#   #save to redis
#   writeToRedis(uuid,res.list)
# 
# }, error=function(err){
#   writeExceptionToRedis(uuid,err)
# })
# 
