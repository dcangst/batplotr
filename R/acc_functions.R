#' Time of Night
#' 
#' gives back a \code{POSIXct} object with date 1900-01-01 (evening) or 
#' 1900-01-02 but with same timestamp for plotting, corrects for daylight saving
#' time
#'
#' @param date a POSIXct object, or a vector thereof
#' @family accessory functions
#' @export
timeOfNight <- function(date){
    minDate <- strptime(min(date),format="%F")
    timeOfNight <- as.POSIXct("1900-01-01")+
        difftime(date,minDate)
    return(timeOfNight)
}

#' Convert to UTC
#' 
#' converts time to UTC (to avoid problems with daylight saving time)
#'
#' @param date a POSIXct object, or a vector thereof
#' @family accessory functions
#' @export
convertToUTC <- function(date){
    date_utc <- force_tz(date,tzone="UTC")-3600*dst(date)
}

#' Print Head of a list of data.frames
#' 
#' Displays the first \code{n} rows of a data.frame
#'
#' @param list a list of \code{data.frame}s
#' @param n number of rows to print
#' @family accessory functions
#' @export 
listHead <- function(list,n="5"){
    for(i in 1:length(list)){
        print(head(list[[i]],n))
    }
}

#' Get BatLogger GPS coordinates
#' 
#' 
#'
#' @param GPSCoord GPS coordinates
#' @param GPSValid is the value of GPSCoord reliable?
#' @family accessory functions
#' @export 
getBatLoggerGPS <- function(GPSCoord,GPSValid){
    if(sum(GPSValid=="yes")==0 | is.na(sum(GPSValid=="yes"))){
        out <- NA
    } else {
        out <- mean(GPSCoord[GPSValid=="yes"],na.rm=TRUE)
    }
    return(out)
}