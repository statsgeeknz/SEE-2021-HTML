# Weather scraping function -----------------------------------------------
#' weatherScraper - Bellrock variant
#' Author: CRD
#' Created: 10/07/18
#' 
#' Purpose: Function for scraping 3-5 day weather forecasts from meteogroup hmtl
#' selectorGadget used to ID CSS definitions for desired bits
#' (Some not divisible solely this way - some hacking required)
#' 
#' Args: 
#'  - htmlFile: path to html to be scraped (NB assumes this is the current North version)
#'  
#' Value: 
#'  - dataframe with temp (min/max), wind (mean/directin/gust), rain (min/max),
#'  snow/line-icing (depth/icing/height), lightning (risk group)
  

weatherScraperFourFiveDay <- function(htmlFile){
    
  require(rvest)
  require(lubridate)
  require(tidyverse)

  
  # can't find path to these, so hard-coded
  
  groups <- c('Perth, Angus, Aberdeen & Moray Firth', 'Western Isles, NW & Cent Highlands, Skye, Argyll', 
              'Shetland, Orkney, NE Caithness')
  
  groupCode <- paste("G", 1:3, sep='')
  
  groupCode <- rep(groupCode, 2)
  
  days <- rep(c(4,5), c(3, 3))
  
  outputGroups <- rep(groups, 3)
  
  htmlTarget <- read_html(htmlFile)


# Region groups -----------------------------------------------------------
  # dates is in the header - save parsing til prediction
  
  datePath <- html_nodes(htmlTarget, ".header-north+ h1")[1] 
  
  # determine if scheduled forecast, or update
  scheduledForecast <- grepl("Scheduled", datePath)
  
  datePath <- unlist(strsplit(as.character(datePath), "-"))
  dateVals <- datePath[3]
  dateVals <- unlist(strsplit(as.character(dateVals), "[ ,<]"))[3:5]
  dateVals <- paste(dateVals, collapse = '-')
  


# Temperature -------------------------------------------------------------
  # temperature - min and max. Odd structure, select individually
  
  tempPath <- html_nodes(htmlTarget, ".header-logo tr:nth-child(6) td+ td")
  
  tempPath <- unlist(strsplit(as.character(tempPath), " "))
  tempPath <- tempPath[grep("value", tempPath)]
  tempVals <- tempPath %>% parse_number() 
  tempVals <- matrix(tempVals, ncol = 2, byrow = T)
  tempVals <- as.data.frame(tempVals); names(tempVals) <- c('temp_max', 'temp_min')


# Wind --------------------------------------------------------------------
  #= Wind values - mean, direction and gust. Units?
  
  windPath <- html_nodes(htmlTarget, ".header-logo tr:nth-child(8) .combo td")
  
  windValues <- windPath[grepl("value", windPath)]
  
  # locate rows of different measures
  meanLoc <- grepl("wind_mean", windValues)
  dirLoc <- grepl("wind_dir", windValues)
  gustLoc <- grepl("wind_gust", windValues)
  
  windValues <- regmatches(windValues, gregexpr('(?<=value=").*(?=" disable)', windValues, perl = TRUE )) %>% 
    unlist() 
  
  # three measures in the table - locate/separate out
  wind_mean <- windValues[meanLoc] %>% parse_number()
  wind_direction <- windValues[dirLoc]
  wind_gust <- windValues[gustLoc] %>% parse_number()
  
  windValsArray <- data.frame(wind_mean, wind_direction, wind_gust, stringsAsFactors = F)
  
  
  
# Rain --------------------------------------------------------------------
  #= rain in mm - get min and max
  
  rainPath <- html_nodes(htmlTarget, ".header-logo tr:nth-child(10) td+ td")
  
  rainVals <- regmatches(rainPath, gregexpr('(?<=value=").*(?=" disable)', rainPath, perl = TRUE )) %>% 
    unlist() %>% parse_number() 
  
  rainVals <- matrix(rainVals, ncol = 2, byrow = T)
  
  rainVals <- as.data.frame(rainVals); names(rainVals) <- c('rain_min', 'rain_max')
  
  
  
# Snow/line icing ---------------------------------------------------------
  #= level of icing groups + snow height (cm) and height (m above sea-level I guess)
  
  snowPath <- html_nodes(htmlTarget, "tr:nth-child(12) .combo td")
  
  snowPath <- snowPath[grepl("value", snowPath)]
  
  iceLoc <- grepl("line_icing", snowPath)
  snowFallLoc <- grepl("snowfall", snowPath)
  snowHeightLoc <- grepl("snow_height", snowPath)
  
  snowVals <- regmatches(snowPath, gregexpr('(?<=value=").*(?=" disable)', snowPath, perl = TRUE )) %>% unlist()
  
  snow_depth <- as.numeric(snowVals[snowFallLoc])
  icing <- snowVals[iceLoc] 
  icing <- ifelse(icing == "Nil", 0, 1)
  snow_height <- snowVals[snowHeightLoc]
  snow_height <- as.numeric(ifelse(snow_height == "N/A", NA, snow_height)) 
  
  snowValsArray <- data.frame(snow_depth, icing, snow_height, stringsAsFactors = F)
  
  
# Lightning ---------------------------------------------------------------
  #= lightning categories
  
  lightningPath <- html_nodes(htmlTarget, ".header-logo tr:nth-child(14) td+ td")
  
  lightningPath <- lightningPath[grepl("value", lightningPath)]
  
  lightningVals <- regmatches(lightningPath, gregexpr('(?<=value=").*(?=" disable)', lightningPath, perl = TRUE )) %>% unlist()
  
  
# gather all together -----------------------------------------------------

  outputFrame <- data.frame(scheduledForecast = scheduledForecast, dateOfForecast = dateVals, day = days, region = groups, regionCode = groupCode, 
                            tempVals, windValsArray, rainVals, snowValsArray, lightning = lightningVals, stringsAsFactors = F)
  
  return(outputFrame)
    
} # end of weatherScraper
