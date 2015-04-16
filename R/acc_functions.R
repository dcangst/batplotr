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