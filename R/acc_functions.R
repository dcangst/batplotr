#' Time of Night
#' 
#' gives back a \code{POSIXct} object with date 1900-01-01 (evening) or 
#' 1900-01-02 but with same timestamp for plotting 
#'
#' @param date a POSIXct object
#' @family accessory functions
#' @export
timeOfNight <- function(df){
    minDate <- strptime(min(df$night),format="%F")
    df$timeOfNight <- as.POSIXct("1900-01-01")+
        difftime(df$bins,minDate)
    return(df)
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

#' Calculate sunset & sunrise for the night
#' 
#' function description
#'
#' @param nights dates of the nights
#' @family accessory functions
#' @export
nights_SRSS <- function(nights){
    nights_all <- as.POSIXct(
        format(nights,format="%F"))
    yr_start <- format(min(nights_all),format="%Y")
    yr_end   <- format(max(nights_all),format="%Y")
    n_days <- as.numeric(
        difftime(paste0(yr_end,"-12-31"),paste0(yr_start,"-01-01"),
        units="day"))

    SR_SS <- sunrise.set(station_lat, station_long, 
        paste0(yr_start,"-01-01"), timezone = "CET", num.days = n_days+10)
    SR_night <- data.frame(night=as.Date(SR_SS$sunset),
        sunrise.time=c(SR_SS$sunrise[2:length(SR_SS$sunrise)],NA))
    SR_night <- SR_night[complete.cases(SR_night),]
    SR_night$sunrise.ton <- as.POSIXct(paste0("1900-01-02 ",
        str_sub(as.character(SR_night$sunrise.time),-8,)))

    SS_night <- data.frame(night=as.Date(SR_SS$sunset),
        sunset.time=SR_SS$sunset)
    SS_night <- SS_night[complete.cases(SS_night),]
    SS_night$sunset.ton <- as.POSIXct(paste0("1900-01-01 ",
        str_sub(as.character(SS_night$sunset.time),-8,)))

    return(list(SR_night,SS_night))
}