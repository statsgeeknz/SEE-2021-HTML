# Averaging wind direction ------------------------------------------------
#' Function to rationalise multiple wind-drection measures within a day
#' Effectively breaks to Easting & Northing components and averages
#' weigthed by wind speed
#'  
#'  Args:
#'  windSpeed - vector of wind speed in whatever units, as long as consistent
#'  windDirection - vector of wind direction in degrees 0/360 being North
#'  
#'  Value:
#'  mean_windDirection - an averaged wind-direction in degrees (0/360 being North)



averageWindDirection <- function(windSpeed, windDirection){

    # extract east-west & north-south components - weighted averages
    easting <- mean(windSpeed * sin(windDirection * pi/180))
    northing <- mean(windSpeed * cos(windDirection * pi/180))
    
    # combine to give vector - and convert to degrees
    mean_windDirection = atan2(easting, northing) * 180/pi
    mean_windDirection = (360 + mean_windDirection) %% 360
    
    # return
    mean_windDirection
    
    }



# Constraining predictions to be within historic ranges -------------------
#' Function to take a value, compare to a range and snap to nearest boundary
#' 
#' Args: 
#' numberToSnap - value for alteration
#' rangeToSnap - sequence of numbers to find closest to. Expect a range 
#' although would work more generally
#' 
#' Value:
#' snap - the value in rangeToSnap that is nearest numberToSnap

rangeSnap <- function(numberToSnap, rangeToSnap){
    
    if(any(is.na(c(numberToSnap, rangeToSnap)))){
        return(NA)
    }
        
    diff <- numberToSnap-rangeToSnap
    if(sign(diff[1])==sign(diff[2])){
        
        diff <- abs(diff)
        snap <- rangeToSnap[which.min(diff)]
        return(snap)
        
        } else {
            
        return(numberToSnap)
            
        }    
    
    }

