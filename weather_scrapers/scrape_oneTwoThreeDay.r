# Weather scraping function -----------------------------------------------
#' weatherScraper - Bellrock variant
#' Author: CRD
#' Created: 03/11/2019
#' 
#' Purpose: Function for scraping 1 & 2  day weather forecasts from meteogroup hmtl
#' selectorGadget used to ID CSS definitions for desired bits
#' (Some not divisible solely this way - some hacking required)
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

  
  
weatherScraperOneTwoDay <- function(htmlFile){
    
  require(rvest)
  require(lubridate)
  require(tidyverse)
  
  
  htmlTarget <- read_html(htmlFile)
  
  numDays <- 3
  numRegions <- 9

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
  
  # RAGpath <- html_nodes(htmlTarget, "tr:nth-child(3) td:nth-child(10) , 
  #                  tr:nth-child(3) td:nth-child(9) , 
  #                  tr:nth-child(3) td:nth-child(8) , 
  #                  tr:nth-child(3) td:nth-child(7) , 
  #                  tr:nth-child(3) td:nth-child(6) , 
  #                  tr:nth-child(3) td:nth-child(5) , 
  #                  p+ table tr:nth-child(3) td:nth-child(4) , 
  #                  p+ table tr:nth-child(3) td:nth-child(3) , 
  #                  p+ table tr:nth-child(3) td:nth-child(2)")
  
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
  dateVals <- datePath[3]
  dateVals <- unlist(strsplit(as.character(dateVals), "[ ,<]"))[3:5]
  dateVals <- paste(dateVals, collapse = '-')



# Temperature -------------------------------------------------------------
  # temperature - min and max. 
  
  maxMinTempPath <- html_nodes(htmlTarget, "p~ p+ table tr:nth-child(12) td , p~ p+ table tr:nth-child(11) td")
  
  browser()
  
  maxMinTempPath <- maxMinTempPath[grepl("value", maxMinTempPath)]
  
  tempVals <- regmatches(maxMinTempPath, gregexpr("(?<=value).*(?=disable)", maxMinTempPath, perl = TRUE )) %>% 
    unlist() %>% parse_number() %>% 
    matrix(., ncol = 2, byrow = T) %>%
    as.data.frame() %>% rename(temp_max = V1, temp_min = V2)


# Wind --------------------------------------------------------------------
  #= Wind values - mean, direction and gust. Units?
  
  windPath <- html_nodes(htmlTarget, "tr:nth-child(9) .combo td , 
                     p+ table tr:nth-child(8) .combo td , 
                     p+ table tr:nth-child(7) .combo td , 
                     tr:nth-child(6) .combo td")
  
  windValues <- lapply(windPath, function(q){
    temp <- strsplit(as.character(q), "value=")[[1]][2]
    temp <- strsplit(temp, '\"')[[1]][2]
    temp
  }) %>% unlist()
  
  meanSpeed <- windValues[seq(1, 216, by = 3)]
  direction <- windValues[seq(2, 216, by = 3)]
  gust <- windValues[seq(3, 216, by = 3)]
  summaryGroup <- c(rep(1:9, 4), rep(10:18, 4))
  
  # averaging the wind vectors for the different times of the day
  windValsArray <- data.frame(summaryGroup = summaryGroup, meanSpeed = as.numeric(meanSpeed), 
                         direction = direction, gust = as.numeric(gust)) %>%
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
    
    rainPath <- rainPath[grepl("value", maxMinTempPath)]
    
    rainVals <- regmatches(rainPath, gregexpr("(?<=value).*(?=disable)", rainPath, perl = TRUE )) %>% 
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

  snowPath <- unlist(strsplit(as.character(snowPath), " "))
  snowVals <- snowPath[grep("value", snowPath)]
  snowVals <- gsub('value=\"', "", snowVals) %>% 
    gsub('"></td>', "", .) %>% gsub('">\n</td>', "", .) 
  
  snowValsArray <- array(dim = c(36, 3))
  
  snowValsArray[,1] <- snowVals[seq(1, 108, by = 3)]
  snowValsArray[,2] <- snowVals[seq(2, 108, by = 3)]
  snowValsArray[,3] <- snowVals[seq(3, 108, by = 3)]
 
  # average the snow depths, heights usually empty (choose first one per day), icing made binary
  snowValsArray <- as.data.frame(snowValsArray, stringsAsFactors = F) %>%
    mutate(summaryGroup = c(rep(1:9, 2), rep(10:18, 2))) %>%
    rename(snow_depth = V1, icing = V2, snow_height = V3) %>%
    mutate(snow_depth = as.numeric(snow_depth), icing = ifelse(icing == "Nil", 0, 1),
           snow_height = as.numeric(ifelse(snow_height == "N/A", NA, snow_height))) %>%
    group_by(summaryGroup) %>% summarise(snow_depth = mean(snow_depth, na.rm = T),
                                         icing = ifelse(any(icing == 1), 1, 0),
                                         snow_height = snow_height[1]) %>% select(-summaryGroup)
    
  

# Lightning ---------------------------------------------------------------
  #= lightning categories
  
  lightningPath <- html_nodes(htmlTarget, "tr:nth-child(18) td+ td , tr:nth-child(17) td+ td")
  
  lightningPath <- unlist(strsplit(as.character(lightningPath), " "))
  lightningVals <- lightningPath[grep("value", lightningPath)]
  
  lightningVals <- gsub('value=\"', "", lightningVals) %>%
    gsub('\">', "", .) %>% gsub('\n</td>', "", .) 
  
  # arrange such that most severe group is chosen (i.e. towards category 1)
  lightningCat <- data.frame(lightningCat = lightningVals, 
                              summaryGroup = c(rep(1:9, 2), rep(10:18, 2)), stringsAsFactors = F) %>%
             group_by(summaryGroup) %>% arrange(lightningCat) %>%
             summarise(lightningCat = lightningCat[1]) %>% select(-summaryGroup)
  

# gather all together -----------------------------------------------------

  outputFrame <- data.frame(scheduledForecast = scheduledForecast, dateOfForecast = dateVals, day = days, region = rep(groups, numDays), regionCode = groupCode, 
                            tempVals, windValsArray, rainFall, snowValsArray, lightningCat = lightningCat, risk = oneTwoThreeDayRAG, stringsAsFactors = F)
  
  outputFrame
    
} # end of weatherScraper
  
  

  
  

