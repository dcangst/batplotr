#' Time of Night
#' 
#' gives back a \code{POSIXct} object with date 1900-01-01 (evening) or 
#' 1900-01-02 (morning) but with same timestamp for plotting, 
#' corrects for daylight saving time.
#'
#' @param date a POSIXct object, or a vector thereof
#' @family accessory functions
#' @export
timeOfNight <- function(date){
  evening <- lubridate::hour(date) >= 13
  evening[is.na(evening)] <- FALSE
  morning <- !lubridate::hour(date) >= 13
  morning[is.na(morning)] <- FALSE

  date[evening] <- update(date[evening],
    year = 1900, month = 1, mday = 1)
  date[morning] <- update(date[morning],
    year = 1900, month = 1, mday = 2)

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
listHead <- function(list, n = 6){
  for (i in 1:length(list)){
    print(head(list[[i]], n))
  }
}

#' Check for Package updates on GitHub
#' 
#' checks for Package updates
#'
#' @param package the name of the package hosted at \link{repo}
#' @param repo Repository address in the format \link{username/repo }
#' @param ref Desired git reference. Defaults to "master".
#' @family accessory functions
#' @export 
github_update <- function(package, repo, ref = "master"){
  url <- paste0("https://raw.githubusercontent.com/",
    repo, "/", ref, "/DESCRIPTION")
  content <- tryCatch(RCurl::getURL(url),
    error = function(Cond){
      return(FALSE)
    },
    warning = function(Cond){
      return(FALSE)
    })
  if (content == FALSE){
    return("cannot update. no connection?")
  }

  version_git <- gsub(".*\\Version: (.*)\nA.*", "\\1", content)
  version_loc <- as.character(packageVersion(package))
  if (version_git != version_loc){
    message("A new version of batplotr is available!\n",
      "(", version_git, ", you have ", version_loc, ")", sep = "")
    devtools::install_github(repo, dependencies = TRUE)
    return(paste("batplotr updated to version", version_git))
  } else {
    return(paste(package, "is up to date."))
  }
}
