% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{periodPlot}
\alias{periodPlot}
\title{Period Plot}
\usage{
periodPlot(
  plotData,
  start_date = floor_date(min(plotData$survey_date), "year"),
  end_date = ceiling_date(max(plotData$survey_date), "year"),
  sel_species = "every",
  x_limits = NULL,
  y_limits = NULL,
  x_break_distance = "1 month",
  y_break_distance = "1 hour",
  x_break_label = "\%b",
  text_size = 16
)
}
\arguments{
\item{plotData}{data generated by \code{\link{sumBatscopeData}}}

\item{start_date}{in POSIXct format or \code{"YYYY-MM-DD"}, defaults to the
beginning of the year in which the first data were aquired.}

\item{end_date}{in POSIXct format or \code{"YYYY-MM-DD"}, defaults to the end
of the year in which the last data were aquired.}

\item{sel_species}{species_name or vector of species to plot, use "every" to
plot all species individually (differentiated by color)}

\item{x_limits}{x-axis limits, NULL (default) or a POSIXct vector of length 2}

\item{y_limits}{y-axis limits, NULL (default) or a character or POSIXct
vector of length 2 with format
\code{c("1900-01-01 HH:MM","1900-01-02 HH:MM")}}

\item{x_break_distance}{A string giving the distance between x breaks like
"2 weeks", or "10 years".}

\item{y_break_distance}{A string giving the distance between ybreaks like
"2 weeks", or "10 years".}

\item{x_break_label}{Time format for labels, see \code{\link{strptime}}.}

\item{text_size}{base text size}

\item{force_dst_corr}{if TRUE daylight saving time is forcefully corrected.}
}
\value{
a \code{ggplot} object
}
\description{
Plot activity (number of calls) of each night of a given period.
}
\seealso{
Other plot functions: 
\code{\link{nightPlot}()}
}
\concept{plot functions}
