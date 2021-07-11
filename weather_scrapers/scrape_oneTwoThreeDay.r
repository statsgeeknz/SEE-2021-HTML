# Weather scraping function -----------------------------------------------
#' weatherScraper - Bellrock variant
#' Author: CRD
#' Created: 03/11/2019
#' Modified: April 2021 - addition of day 3
#' 
#' Purpose: Function for scraping 1, 2 & 3 day weather forecasts from meteogroup hmtl
#' selectorGadget used to ID CSS definitions for desired bits
#' 
#' Args: 
#'  - htmlFile: path to html to be scraped (NB assumes this is the current North version)
#'  
#' Value: 
#'  dataframe with 
#'   - dateOfForecast: (char) date of HTML forecast file
#'   - day: (numeric) indicating 1 or 2 day forcast
#'   - region: (char) the name of the forecast regions in the HTML
#'   - regionCode: (char) code for each region - probably redundant, just in keeping with 3-5 day code
#'   - temp_min & temp_max: (numeric) min and max temperature (C)
#'   - wind_mean, wind_gust_max, wind_gust_min: (numeric) mean wind speed, min and max gust speeds
#'   - wind_direction: (numeric) weighted (by speed) average wind-direction, in degrees
#'   - rain_min & rain_max: (numeric) minimum and maximum rainfall of the forecasted day
#'   - snow_depth & snow_height: (numeric) mean depth of snow, height above sea level (first of the day selected - usually NULL)
#'   - icing: (numeric) binary indicating icing (1) or not (0)
#'   - lightningCat: (char) lightning category 1, 1b etc - chooses "highest" of the day alphabetically
#'   - risk: (char) risk category from HTML - green, amber or red
#'   
#'   Depends: requires averageWindDirection function from associated tools.r

  
  
weatherScraperOneTwoThreeDay <- function(htmlFile){
    
  require(rvest)
  require(lubridate)
  require(tidyverse)
  
  
  htmlTarget <- read_html(htmlFile)

# Scrape region names -----------------------------------------------------

  # get the region codes
  groupPath <- html_nodes(htmlTarget, "tr:nth-child(1) td+ td")
  groups <- html_text(groupPath[1:9])
  
  # 9 regions collected
  groupCode <- paste("G", 1:9, sep='')
  
  # collecting for two days (day 1, 2 & 3)
  groupCode <- rep(groupCode, 3)
  
  days <- rep(c(1,2,3), c(9,9,9))
  
  outputGroups <- rep(groups, 3)
  

# Scrape risk class -------------------------------------------------------
 
  RAGpath <- html_nodes(htmlTarget, "p~ p+ table tr:nth-child(3) td+ td")
  
  
  oneTwoThreeDayRAG <- lapply(RAGpath, function(q){
      risks <- strsplit(as.character(q), "value=")[[1]][2]
      risks <- strsplit(risks, '\"')[[1]][2]
      risks
    }) %>% unlist() %>% as.character() %>% na.omit()
  
   


# Region groups -----------------------------------------------------------
  # date is in the header - save parsing til prediction
  
  datePath <- html_nodes(htmlTarget, ".header-north+ h1")[1] 
  
  # determine if scheduled forecast, or update
  scheduledForecast <- grepl("Scheduled", datePath)
  
  datePath <- unlist(strsplit(as.character(datePath), "-"))
  dateVals <- datePath[5]
  dateVals <- unlist(strsplit(as.character(dateVals), "[ ,<]"))[3:5]
  dateVals <- paste(dateVals, collapse = '-')



# Temperature -------------------------------------------------------------
  # temperature - min and max. 
  
  maxMinTempPath <- html_nodes(htmlTarget, "p+ table tr:nth-child(4) td+ td")
  
  tempVals <- maxMinTempPath[grepl("value", maxMinTempPath)]
  
  tempVals <- regmatches(tempVals, gregexpr('(?<=value=").*(?=")', tempVals, perl = TRUE )) %>% 
    unlist() %>% parse_number() %>% 
    matrix(., ncol = 2, byrow = T) %>%
    as.data.frame() %>% 
    rename(temp_max = V1, temp_min = V2)


# Wind --------------------------------------------------------------------
  #= Wind values - mean, direction and gust. Units?
  
  windPath <- html_nodes(htmlTarget, "p~ p+ table .wind td")
  
  windValues <- windPath[grepl("value", windPath)]
  
  # locate rows of different measures
  meanLoc <- grepl("wind_mean", windValues)
  dirLoc <- grepl("wind_dir", windValues)
  gustLoc <- grepl("wind_gust", windValues)
  
  windValues <- regmatches(windValues, gregexpr('(?<=value=").*(?=" style)', windValues, perl = TRUE )) %>% 
    unlist() 
  
  # three measures in the table - locate/separate out
  meanSpeed <- windValues[meanLoc] %>% parse_number()
  direction <- windValues[dirLoc]
  gust <- windValues[gustLoc] %>% parse_number()
    
  windDays <- rep(c("Day1", "Day2", "Day3"), rep(9*4, 3))
  windRegions <- rep(1:9, 3*4)
  
  summaryGroup <- paste(windDays, windRegions, sep = "_")
  
  # averaging the wind vectors for the different times of the day
  windValsArray <- data.frame(summaryGroup = summaryGroup, meanSpeed = meanSpeed, 
                         direction = direction, gust = gust) %>%
              mutate(numericDirection = case_when(
                direction == "N" ~ 360,
                direction == "NE" ~ 45, 
                direction == "E" ~ 90, 
                direction == "SE" ~ 135, 
                direction == "S" ~ 180, 
                direction == "SW" ~ 225, 
                direction == "W" ~ 270, 
                direction == "NW" ~ 315, 
              )) %>% group_by(summaryGroup) %>%
              summarise(overallMeanSpeed = mean(meanSpeed, na.rm = T),
                        meanDirection = averageWindDirection(meanSpeed, numericDirection),
                        maxGust = max(gust, na.rm = T), minGust = min(gust, na.rm = T)) %>%
              select(-summaryGroup)  
  
    names(windValsArray) <- c('wind_mean', 'wind_direction', 'wind_gust_max', 'wind_gust_min')
  
  
  
# Rain --------------------------------------------------------------------
  #= rain in mm - get min and max
  
  rainPath <- html_nodes(htmlTarget, "p~ p+ table tr:nth-child(12) td , p~ p+ table tr:nth-child(11) td")
    
    rainVals <- rainPath[grepl("value", rainPath)]
    
    rainVals <- regmatches(rainVals, gregexpr('(?<=value=").*(?=")', rainVals, perl = TRUE )) %>% 
      unlist() %>% parse_number() %>% 
      matrix(., ncol = 2, byrow = T) %>%
      as.data.frame() %>% rename(rain_min = V1, rain_max = V2)
    
    rainDays <- rep(c("Day1", "Day2", "Day3"), rep(9*2, 3))
    rainRegions <- rep(1:9, 3*2)
    
    summaryGroup <- paste(rainDays, rainRegions, sep = "_")
    
    rainFall <- rainVals %>% bind_cols(summaryGroup = summaryGroup) %>% 
      group_by(summaryGroup) %>%
      summarise(rain_min = min(rain_min, na.rm = T), rain_max = max(rain_max, na.rm = T)) %>%
      select(-summaryGroup)  
  
  
# Snow/line icing ---------------------------------------------------------
  #= level of icing groups + snow height (cm) and height (m above sea-level I guess)
    
  snowPath <- html_nodes(htmlTarget, "tr:nth-child(15) .combo td , tr:nth-child(14) .combo td")

  snowVals <- snowPath[grepl("value", snowPath)]
  
  iceLoc <- grepl("line_icing", snowVals)
  snowFallLoc <- grepl("snowfall", snowVals)
  snowHeightLoc <- grepl("snow_height", snowVals)
  
  snowVals <- regmatches(snowVals, gregexpr('(?<=value=").*(?=")', snowVals, perl = TRUE )) %>%
    unlist()
  
  snow_depth <- as.numeric(snowVals[snowFallLoc])
  icing <- snowVals[iceLoc] 
  icing <- ifelse(icing == "Nil", 0, 1)
  snow_height <- snowVals[snowHeightLoc]
  snow_height <- as.numeric(ifelse(snow_height == "N/A", NA, snow_height)) 
  
  snowDays <- rep(c("Day1", "Day2", "Day3"), rep(9*2, 3))
  snowRegions <- rep(1:9, 2*3)
  
  summaryGroup <- paste(snowDays, snowRegions, sep = "_")
  

  # average the snow depths, heights usually empty (choose first one per day), icing made binary
  snowValsArray <- data.frame(snow_depth, icing, snow_height, summaryGroup) %>%
    group_by(summaryGroup) %>% summarise(snow_depth = mean(snow_depth, na.rm = T),
                                         icing = ifelse(any(icing == 1), 1, 0),
                                         snow_height = snow_height[1]) %>% select(-summaryGroup)
    
  

# Lightning ---------------------------------------------------------------
  #= lightning categories

  lightningPath <- html_nodes(htmlTarget, "tr:nth-child(20) td , tr:nth-child(19) td , tr:nth-child(18) td , tr:nth-child(17) td")
  
  lightningVals <- lightningPath[grep("value", lightningPath)]
  
  lightningVals <- regmatches(lightningVals, gregexpr('(?<=value=").*(?=")', lightningVals, perl = TRUE )) %>%
    unlist()
  
  lightDays <- rep(c("Day1", "Day2", "Day3"), rep(9*4, 3))
  lightRegions <- rep(1:9, 4*3)
  
  summaryGroup <- paste(lightDays, lightRegions, sep = "_")
  
  # arrange such that most severe group is chosen (i.e. towards category 1)
  lightningCat <- data.frame(lightningCat = lightningVals, 
                              summaryGroup = summaryGroup) %>%
             group_by(summaryGroup) %>% arrange(lightningCat) %>%
             summarise(lightningCat = lightningCat[1]) %>% select(-summaryGroup)
  

# gather all together -----------------------------------------------------

  outputFrame <- data.frame(scheduledForecast = scheduledForecast, dateOfForecast = dateVals, day = days, region = rep(groups, 3), regionCode = groupCode, 
                            tempVals, windValsArray, rainFall, snowValsArray, lightningCat = lightningCat, risk = oneTwoThreeDayRAG, stringsAsFactors = F)
  
  outputFrame
    
} # end of weatherScraper
  
  

  
  

