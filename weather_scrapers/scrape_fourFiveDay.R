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
  
  groupCode <- rep(groupCode, 3)
  
  days <- rep(c(3,4,5), c(3, 3, 3))
  
  outputGroups <- rep(groups, 3)
  
  htmlTarget <- read_html(htmlFile)


# Region groups -----------------------------------------------------------
  # dates is in the header - save parsing til prediction
  
  datePath <- html_nodes(htmlTarget, "h1")
  
  
  # determine if scheduled forecast, or update
  scheduledForecast <- grepl("Scheduled", datePath)
  
  datePath <- unlist(strsplit(as.character(datePath), "-"))
  dateVals <- datePath[5]
  dateVals <- unlist(strsplit(as.character(dateVals), "[ ,<]"))[3:5]
  dateVals <- paste(dateVals, collapse = '-')


# Temperature -------------------------------------------------------------
  # temperature - min and max. Odd structure, select individually
  
  tempPath <- html_nodes(htmlTarget, ".day3_5 tr:nth-child(6) td:nth-child(2),
                         .day3_5 tr:nth-child(6) td:nth-child(3),
                         .day3_5 tr:nth-child(6) td:nth-child(4), 
                         .day3_5 tr:nth-child(6) td:nth-child(5),
                         .day3_5 tr:nth-child(6) td:nth-child(6),
                         .day3_5 tr:nth-child(6) td:nth-child(7),
                         .day3_5 tr:nth-child(6) td:nth-child(8),
                         .day3_5 tr:nth-child(6) td:nth-child(9),
                         .day3_5 tr:nth-child(6) td:nth-child(10)")
  
  tempPath <- unlist(strsplit(as.character(tempPath), " "))
  tempPath <- tempPath[grep("value", tempPath)]
  tempVals <- as.numeric(gsub("\\D", "", tempPath)) 
  tempVals <- matrix(tempVals, ncol = 2, byrow = T)
  tempVals <- as.data.frame(tempVals); names(tempVals) <- c('temp_max', 'temp_min')


# Wind --------------------------------------------------------------------
  #= Wind values - mean, direction and gust. Units?
  
  windPath <- html_nodes(htmlTarget, ".day3_5 tr:nth-child(8) td")
  windPath <- unlist(strsplit(as.character(windPath), " "))
  windVals <- windPath[grep("value", windPath)]
  windVals <- gsub('value=\"', "", windVals) %>%
    gsub('\"', "", .) %>% gsub('></td>', "", .) %>% gsub('\r\n', "", .)
  
 
  wind_mean <- as.numeric(windVals[seq(1, 54, by = 6)])
  wind_direction <- windVals[seq(2, 54, by = 6)]
  wind_gust <- as.numeric(windVals[seq(3, 54, by = 6)])
  
  windValsArray <- data.frame(wind_mean, wind_direction, wind_gust, stringsAsFactors = F)
  
  
  
# Rain --------------------------------------------------------------------
  #= rain in mm - get min and max
  
  rainPath <- html_nodes(htmlTarget, ".day3_5 tr:nth-child(10) td")
  
  rainPath <- unlist(strsplit(as.character(rainPath), " "))
  rainVals <- rainPath[grep("value", rainPath)]
  rainVals <- as.numeric(gsub("[^0-9.]", "", rainVals)) 
  # rainVals <- as.numeric(gsub("\\D", "", rainVals)) # error found by Bellrock
  rainVals <- matrix(rainVals, ncol = 2, byrow = T)
  rainVals <- as.data.frame(rainVals); names(rainVals) <- c('rain_min', 'rain_max')
  
  
  
# Snow/line icing ---------------------------------------------------------
  #= level of icing groups + snow height (cm) and height (m above sea-level I guess)
  
  snowPath <- html_nodes(htmlTarget, "tr:nth-child(12) .combo td")

  snowPath <- unlist(strsplit(as.character(snowPath), " "))
  snowVals <- snowPath[grep("value", snowPath)]
  snowVals <- gsub('value=\"', "", snowVals) %>% 
    gsub('"></td>', "", .) %>% gsub('">\n</td>', "", .) 
  

  snow_depth <- snowVals[seq(1, 27, by = 3)]
  icing <- snowVals[seq(2, 27, by = 3)]
  snow_height <- snowVals[seq(3, 27, by = 3)]
  
  snowValsArray <- data.frame(snow_depth, icing, snow_height, stringsAsFactors = F)
  
  # average the snow depths, heights usually empty (choose first one per day), icing made binary
  snowValsArray <- snowValsArray %>% mutate(
      snow_depth = as.numeric(snow_depth), 
      icing = ifelse(icing == "Nil", 0, 1),
      snow_height = as.numeric(ifelse(snow_height == "N/A", NA, snow_height))
    )
  

# Lightning ---------------------------------------------------------------
  #= lightning categories
  
  lightningPath <- html_nodes(htmlTarget, ".day3_5 tr:nth-child(14) td")
  
  lightningPath <- unlist(strsplit(as.character(lightningPath), " "))
  lightningVals <- lightningPath[grep("value", lightningPath)]
  
  lightningVals <- gsub('value=\"', "", lightningVals) %>%
    gsub('\">', "", .) %>% gsub('\n</td>', "", .)
  

# Link to fault date ------------------------------------------------------
  #= deal with dates outside function
  # faultDate <- datePath + days(days-1) 

# gather all together -----------------------------------------------------

  outputFrame <- data.frame(scheduledForecast = scheduledForecast, dateOfForecast = dateVals, day = days, region = groups, regionCode = groupCode, 
                            tempVals, windValsArray, rainVals, snowValsArray, lightning = lightningVals, stringsAsFactors = F)
  
  return(outputFrame)
    
} # end of weatherScraper
