#' Time of Night
#'
#' gives back a \code{POSIXct} object with date 1900-01-01 (evening) or
#' 1900-01-02 (morning) but with same timestamp for plotting,
#' corrects for daylight saving time.
#'
#' @param date a POSIXct object, or a vector thereof
#' @family accessory functions
#' @export
timeOfNight <- function(date) {
  evening <- lubridate::hour(date) >= 13
  evening[is.na(evening)] <- FALSE
  morning <- !lubridate::hour(date) >= 13
  morning[is.na(morning)] <- FALSE

  date[evening] <- update(date[evening],
    year = 1900, month = 1, mday = 1
  )
  date[morning] <- update(date[morning],
    year = 1900, month = 1, mday = 2
  )

  return(date)
}

#' Print Head of a list of data.frames
#'
#' Displays the first \code{n} rows of a data.frame
#'
#' @param list a list of \code{data.frame}s
#' @param n number of rows to print
#' @family accessory functions
#' @export
listHead <- function(list, n = 6) {
  for (i in seq_along(list)) {
    print(head(list[[i]], n))
  }
}
