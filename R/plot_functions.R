#' Night Plot
#'
#' Plot activity (number of calls) over one night.
#'
#' @param plotData data generated by \code{\link{sumBatscopeData}}
#' @param day day to plot in POSIXct format or "YYYY-MM-DD", defaults to first
#'   date in \code{plotData}
#' @param sel_species species name or vector of species to plot, use "every" to
#'   plot all species individually (differentiated by color)
#' @param x_limits x-axis limits, NULL (default) or a POSIXct vector of length 2
#' @param y_limits y-axis limits, NULL (default) or a numeric vector of length 2
#' @param plot_T logical, should Temperature be plotted?
#' @param plot_T_color r color string, see \code{\link{colors}}
#' @param n_ybreaks roughly how many tickmarks are plotted on the y-Axis
#' @param text_size base text size
#' @return a \code{ggplot} object
#' @family plot functions
#' @export
nightPlot <- function(plotData,
                      day = min(plotData$survey_date),
                      sel_species = "every",
                      x_limits = NULL,
                      y_limits = NULL,
                      plot_T = FALSE,
                      plot_T_color = "black",
                      n_ybreaks = 5,
                      text_size = 16) {
  time_zone <- tz(unique(plotData$survey_date)[1])
  if (sel_species[1] == "every") {
    plotData_sub <- dplyr::filter(plotData, survey_date == day & species != "all")
  } else {
    plotData_sub <- dplyr::filter(plotData, survey_date == day &
      species %in% sel_species)
  }
  if (is.null(x_limits)) {
    x_limits <- c(
      min(plotData_sub$sunset) - 0.5 * 3600,
      max(plotData_sub$sunrise) + 0.5 * 3600
    )
  }

  if (is.null(y_limits)) {
    y_limits <- c(0, max(plotData_sub$n_events))
  }

  plottitle <- paste(
    "Aktivität (# Sequenzen)",
    str_c(format(day, format = "%d.%m.%Y"), collapse = " - ")
  )

  bin_width <- plotData$bin_length[1] * 60
  plotData_sub$t <- "Temperatur [°C]"

  nightPlot <- ggplot(
    plotData_sub,
    aes(bins, n_events, fill = species)
  ) +
    facet_wrap(~project, ncol = 2) +
    geom_bar(stat = "identity", position = "dodge", width = bin_width) +
    geom_vline(
      aes(xintercept = as.numeric(sunset)),
      colour = "orange"
    ) +
    geom_vline(
      aes(xintercept = as.numeric(sunrise)),
      colour = "orange"
    ) +
    scale_x_datetime(
      limits = x_limits, breaks = date_breaks("2 hour"),
      minor_breaks = date_breaks("1 hour"),
      labels = date_format("%H:%M", tz = time_zone)
    ) +
    scale_fill_brewer(name = "Spezies", palette = "Set1") +
    scale_y_continuous(
      limits = y_limits,
      breaks = trans_breaks("identity", function(x) x, n = n_ybreaks)
    ) +
    labs(x = "Uhrzeit", y = "Aktivität (# Sequenzen)", title = plottitle) +
    theme(text = element_text(size = text_size))

  if (plot_T) {
    nightPlot <- nightPlot +
      geom_line(aes(bins, meanT_BL, group = project, colour = t)) +
      scale_color_manual(name = "", values = plot_T_color) +
      labs(
        x = "Uhrzeit",
        y = "Aktivität (# Sequencen) | Temperatur °C",
        title = plottitle
      )
  }

  return(nightPlot)
}

#' Period Plot
#'
#' Plot activity (number of calls) of each night of a given period.
#'
#' @param plotData data generated by \code{\link{sumBatscopeData}}
#' @param start_date in POSIXct format or \code{"YYYY-MM-DD"}, defaults to the
#'   beginning of the year in which the first data were aquired.
#' @param end_date in POSIXct format or \code{"YYYY-MM-DD"}, defaults to the end
#'   of the year in which the last data were aquired.
#' @param sel_species species_name or vector of species to plot, use "every" to
#'   plot all species individually (differentiated by color)
#' @param x_limits x-axis limits, NULL (default) or a POSIXct vector of length 2
#' @param y_limits y-axis limits, NULL (default) or a character or POSIXct
#'   vector of length 2 with format
#'   \code{c("1900-01-01 HH:MM","1900-01-02 HH:MM")}
#' @param x_break_distance A string giving the distance between x breaks like
#'   "2 weeks", or "10 years".
#' @param x_break_label Time format for labels, see \code{\link{strptime}}.
#' @param y_break_distance A string giving the distance between ybreaks like
#'   "2 weeks", or "10 years".
#' @param text_size base text size
#' @param force_dst_corr if TRUE daylight saving time is forcefully corrected.
#' @return a \code{ggplot} object
#' @family plot functions
#' @export
periodPlot <- function(plotData,
                       start_date = floor_date(min(plotData$survey_date), "year"),
                       end_date = ceiling_date(max(plotData$survey_date), "year"),
                       sel_species = "every",
                       x_limits = NULL,
                       y_limits = NULL,
                       x_break_distance = "1 month",
                       y_break_distance = "1 hour",
                       x_break_label = "%b",
                       text_size = 16) {
  time_zone <- tz(unique(plotData$survey_date)[1])
  print(time_zone)

  if (is.POSIXct(start_date) == FALSE) {
    start_date <- as.POSIXct(start_date, format = "%Y-%m-%d", tz = time_zone)
  }
  if (is.POSIXct(end_date) == FALSE) {
    end_date <- as.POSIXct(end_date, format = "%Y-%m-%d", tz = time_zone)
  }

  if (!is.null(y_limits) & is.POSIXct(y_limits)[1] == FALSE) {
    y_limits <- as.POSIXct(y_limits, tz = time_zone)
  }

  cat("Plotting number of sequences over period:\n")
  print(interval(start_date, end_date, tz = time_zone))
  if (sel_species[1] == "every") {
    plotData_sub <- subset(
      plotData,
      survey_date %within% interval(start_date, end_date, tz = time_zone) &
        species != "all"
    )
    plottitle <- paste(
      "Tagesaktivität",
      format(start_date, format = "%d.%m.%Y"),
      "bis", format(end_date, format = "%d.%m.%Y"), "| Summe aller Spezies"
    )
  } else {
    plotData_sub <- subset(
      plotData,
      survey_date %within% interval(start_date, end_date, tz = time_zone) &
        species %in% sel_species
    )
    if (length(sel_species) == 1) {
      plottitle <- paste(
        "Tagesaktivität",
        format(start_date, format = "%d.%m.%Y"),
        "bis", format(end_date, format = "%d.%m.%Y"), "|", sel_species
      )
    } else {
      plottitle <- paste(
        "Tagesaktivität",
        format(start_date, format = "%d.%m.%Y"),
        "bis", format(end_date, format = "%d.%m.%Y")
      )
    }
  }

  if (is.null(x_limits)) {
    x_limits <- c(start_date, end_date)
  }

  period_nights <- seq(start_date, end_date, by = "days")
  gps_coords <- plyr::ddply(plotData_sub, .(project), summarize,
    lat = lat[1], long = long[1]
  )

  sun_data <- plyr::ddply(gps_coords, .(project), cbind, period_nights)
  names(sun_data)[4] <- "survey_date"

  gps_matrix <- matrix(c(sun_data$long, sun_data$lat), ncol = 2)
  sun_data$sunset <- sunriset(
    gps_matrix, sun_data$survey_date,
    direction = "sunset", POSIXct.out = TRUE
  )[, 2]
  sun_data$sunrise <- sunriset(
    gps_matrix, sun_data$survey_date + 24 * 60 * 60,
    direction = "sunrise", POSIXct.out = TRUE
  )[, 2]

  plotData_final <- merge(plotData_sub, sun_data, all = TRUE)
  plotData_final$time <- timeOfNight(plotData_final$bins)
  plotData_final$sunrise_time <- timeOfNight(plotData_final$sunrise)
  plotData_final$sunset_time <- timeOfNight(plotData_final$sunset)

  periodPlot <- ggplot(
    plotData_final,
    aes(survey_date, time)
  ) +
    geom_line(aes(survey_date, sunrise_time),
      size = 0.3, color = "grey25"
    ) +
    geom_line(aes(survey_date, sunset_time),
      size = 0.3, color = "grey25"
    ) +
    facet_wrap(~project, ncol = 2) +
    scale_x_datetime(
      limits = x_limits,
      breaks = date_breaks(x_break_distance),
      labels = date_format(x_break_label, tz = time_zone)
    ) +
    scale_y_datetime(
      limits = y_limits,
      breaks = date_breaks(y_break_distance),
      minor_breaks = date_breaks("1 hour"),
      labels = date_format("%H:%M", tz = time_zone)
    ) +
    labs(x = "Datum", y = str_c("Uhrzeit (", time_zone, ")"), title = plottitle, size = "# Events", color = "Spezies") +
    theme(text = element_text(size = text_size))

  if (sel_species[1] != "every" & length(sel_species) == 1) {
    periodPlot <- periodPlot +
      geom_point(aes(size = n_events), alpha = 0.5, color = "blue")
  } else {
    periodPlot <- periodPlot +
      geom_point(aes(size = n_events, color = species),
        alpha = 0.5
      )
  }
  return(periodPlot)
}
