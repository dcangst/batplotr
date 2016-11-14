#' batPlotR
#' 
#' launches an interactive web app
#'
#' @param option.list a list of global shiny options
#' @return launches a WebApp
#' @family interactive functions
#' @export
shiny_batPlots <- function(
  option.list = list(
    shiny.launch.browser = TRUE,
    shiny.maxRequestSize = 100 * 1024 ^ 2,
    encoding = "UTF-8")
  ){
  shinyApp(
    onStart = function(options = option.list){
      options(options)
    },
    ui = shinyUI(fluidPage(
      # Application title
      titlePanel("Interactive batplotr"),
      # Sidebar with controls to:
      # - upload file
      # - set parameters for summary
      # - set parameters for plots
      sidebarLayout(
        sidebarPanel(
          tags$head(
                tags$style(type = "text/css",
                  "label.radio { display: inline-block; }",
                  ".radio input[type=\"radio\"] { float: none; }"),
                tags$style(type = "text/css",
                  "select { max-width: 200px; }"),
                tags$style(type = "text/css",
                  "textarea { max-width: 185px; }"),
                tags$style(type = "text/css",
                  ".jslider { max-width: 200px; }"),
                tags$style(type = "text/css",
                  ".well { max-width: 310px; }"),
                tags$style(type = "text/css",
                  ".span4 { max-width: 310px; }"),
                tags$style(type = "text/css",
                  ".control-label { font-weight: normal; }"),
                tags$style(type = "text/css",
                  "label { font-weight: normal; }")
          ),
          fileInput("file1", "BatScope xlsx auswählen",
            multiple = TRUE,
            accept = c(".xlsx", ".xls")
          ),
          selectizeInput(
            "project", "Standorte",
            choices = "?", multiple = TRUE
          ),
          selectizeInput(
            "species", "Species",
            choices = "?", multiple = TRUE
          ),
          dateRangeInput("dates",
            label = "Datumsbereich",
            language = "de", separator = "bis"
          ),
          checkboxInput("customScaleX",
            label = tags$b("Eigene Stundenachse"), value = FALSE),
          p("Stundenachse Start"),
          sliderInput("hourAxis1",
            label = NULL, min = 0.0,
            max = 24.0, value = 16.0, step = 0.5
          ),
          p("Stundenachse Ende"),
          sliderInput("hourAxis2",
            label = NULL, min = 0.0,
            max = 24.0, value = 10.0, step = 0.5
          ),
          checkboxInput("customScaleY",
            label = tags$b("Y-Achse (NightPlot)"),
            value = FALSE),
          sliderInput("yAxis", label = NULL, min = -10,
            max = 100, value = c(0, 20), step = 0.5
          ),
          tags$b("X-Achsenunterteilung (PeriodPlot)"),
          numericInput("x_breaks_num",
            label = NULL, value = 1, min = 1, max = NA, step = 1),
          selectInput(
            "x_breaks_unit", label = NULL,
            choices = c(
              "Tag" = "day",
              "Woche" = "week",
              "Monat" = "month",
              "Jahr" = "year"),
            selected = "week",
            multiple = FALSE
          ),
          selectInput("x_breaks_label", "Beschriftung",
            choices = c(
              "Datum" = "%e.%m.%Y",
              "nur Monat" = "%b",
              "Monat/Jahr" = "%b %Y",
              "Wochennummer" = "%U"),
            selected = "%e.%m.%Y",
            multiple = FALSE),
          div(id = "linkToSummary", tags$a("Mehr Optionen")),
          HTML("<script>$('#linkToSummary').click(function() {
             tabs = $('.tabbable .nav.nav-tabs li')
             tabs.each(function() {
              $(this).removeClass('active')
             })
             $(tabs[4]).addClass('active')

             tabsContents = $('.tabbable .tab-content .tab-pane')
             tabsContents.each(function() {
              $(this).removeClass('active')
             })
             $(tabsContents[4]).addClass('active')

            $('#summary').trigger('change').trigger('shown');

           })</script>")

        , width = 3), #sidebarpanel

        # Show a tabset that includes a plot, summary, and table view
        # of data downloadNightPlot
        mainPanel(
          tabsetPanel(type = "tabs", id = "tabs",
            tabPanel("NightPlot",
              plotOutput("nightPlot", height = "650px"),
              div(style = "display:inline-block",
                textInput("save_name_night", label = NULL,
                  value = paste0(
                    format(Sys.Date(), "%Y%m%d"), "_nightplot.pdf")
                  )
                ),
              div(style = "display:inline-block",
                downloadButton("downloadNightPlot", "Plot speichern (PDF)")
                )
            ),
            tabPanel("PeriodPlot",
              plotOutput("periodPlot", height = "650px"),
              div(style = "display:inline-block",
                textInput("save_name_period", label = NULL,
                  value = paste0(
                    format(Sys.Date(), "%Y%m%d"), "_periodplot.pdf")
                  )
                ),
              div(style = "display:inline-block",
                downloadButton("downloadPeriodPlot", "Plot speichern (PDF)")
                )
            ),
            tabPanel("Zusammenfassung",
              div(dataTableOutput("sum_table"), style = "font-size:90%"),
              div(style = "display:inline-block",
                textInput("save_name_sum", label = NULL,
                  value = paste0(
                    format(Sys.Date(), "%Y%m%d"), "_Zusammenfassung.xlsx")
                  )
                ),
              downloadButton("downloadSum", "Zusammenfassung Download")
              ),
            tabPanel("Daten",
              div(dataTableOutput("data_table"), style = "font-size:90%"),
              div(style = "display:inline-block",
                textInput("save_name_data", label = NULL,
                  value = paste0(
                    format(Sys.Date(), "%Y%m%d"), "_data.xlsx")
                  )
                ),
              downloadButton("downloadData", "Daten Download")),
            tabPanel("Optionen", id = "optsPanel",
              fluidRow(
                column(12,
                  fluidRow(
                    column(4,
                      tags$h4("Import/Daten Optionen"),
                      sliderInput("qual",
                        "Minimale Spezies Qualität",
                        value = 0.8,
                        min = 0,
                        max = 1),
                      selectizeInput("speciesColName",
                        label = "Kolonnenname Spezies",
                        choices = "AutoClass1",
                        selected = "AutoClass1"),
                      selectizeInput("speciesQualName",
                        label = "Kolonnenname Spezies Qualität",
                        choices = "AutoClass1Qual",
                        selected = "AutoClass1Qual"),
                      tags$br(),
                      sliderInput("bins", "Länge der bins",
                        value = 5,
                        min = 1,
                        max = 120)
                    ),
                    column(4,
                      tags$h4("GPS-Daten Optionen"),
                      checkboxInput("customCoord",
                        label = "eigene Koordinaten (in  Dezimalgrad)",
                        value = FALSE),
                      numericInput("lat", label = "Breite",
                        value =  47.4, step = 0.1),
                      numericInput("long", label = "Länge",
                        value =  8.52, step = 0.1)
                    ),
                    column(4,
                      tags$h4("Generelle Plotoptionen"),
                      sliderInput("text_size", "Textgrösse",
                        value = 16,
                        min = 1,
                        max = 32),
                      tags$h4("NightPlot Optionen"),
                      checkboxInput("plotTemp",
                        label = "Temperaturverlauf  anzeigen",
                        value = FALSE),
                      selectizeInput("plot_T_color",
                        label = "Farbe Temperaturkurve",
                        choices = colors(distinct = TRUE),
                        selected = "red"),
                      sliderInput("n_ybreaks",
                        "Anzahl Ticks auf der y-Achse",
                        value = 5,
                        min = 1,
                        max = 120),
                      tags$h4("Speicher-Optionen"),
                      numericInput("save_width", "Breite [cm]",
                        min = 1, max = NA,
                        value = 19, step = NA),
                      numericInput("save_heigth", "Höhe [cm]",
                        min = 1, max = NA,
                        value = 10, step = NA),
                      numericInput("save_text_size", "Textgrösse für PDF",
                        min = 1, max = NA,
                        value = 8, step = NA)
                    )
                  ),
                tags$br(),
                HTML("<span class=\"help-block\"><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/4.0/88x31.png\" /></a><br /><span xmlns:dct=\"http://purl.org/dc/terms/\" href=\"http://purl.org/dc/dcmitype/InteractiveResource\" property=\"dct:title\" rel=\"dct:type\">batplotr - v.",as.character(packageVersion("batplotr")),"</span> by <a xmlns:cc=\"http://creativecommons.org/ns#\" href=\"http://dcangst.github.io/batplotr/\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Daniel Angst</a> is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">Creative Commons Attribution-ShareAlike 4.0 International License</a>.</span>")
                )
              )
            )
          )
        ) #mainpanel
      )#sidebarLayout
    )),

    server = function(input, output, session){
      observe({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswählen")
        )
        # update Projectnames
        projectNames <- unique(dataInput()$ProjectName)
        names(projectNames) <- projectNames
        project_options <- as.list(projectNames)

        updateSelectizeInput(session, "project",
          choices = project_options,
          selected = projectNames
        )

        #update Speciesname
        speciesNames <- levels(data_r()$species)
        names(speciesNames) <- speciesNames
        species_options <- as.list(speciesNames)

        updateSelectizeInput(session, "species",
          choices = species_options,
          selected = species_options[species_options != "all"]
        )

        #update Columnnames
        colNames <- colnames(dataInput())
        colNames <- colNames[grepl("Class", colNames)]
        names(colNames) <- colNames
        col_options <- as.list(colNames)

        updateSelectizeInput(session, "speciesColName",
          choices = col_options,
          selected = "AutoClass1"
        )
        updateSelectizeInput(session, "speciesQualName",
          choices = col_options,
          selected = "AutoClass1Qual"
        )

        #update DateRange
        if (input$tabs == "NightPlot"){
          updateDateRangeInput(session, "dates",
            start = format(min(dataInput()$SurveyDate), "%Y-%m-%d"),
            end = format(min(dataInput()$SurveyDate), "%Y-%m-%d")
          )
        }
        if (input$tabs == "PeriodPlot"){
          updateDateRangeInput(session, "dates",
            start = format(min(dataInput()$SurveyDate), "%Y-%m-%d"),
            end = format(max(dataInput()$SurveyDate), "%Y-%m-%d")
          )
        }
        #update yAxis Input
        updateSliderInput(session, "yAxis",
          max = round_any(
            (max(data_r()$n_events, na.rm = TRUE)) * 1.4, 50,
            f = ceiling
            )
        )
      }) #observe

      dataInput <- reactive({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswählen")
        )
        inFile <- input$file1
        pathname <- gsub("//", "/", inFile$datapath)
        path <- file.copy(pathname, paste0(pathname, ".xlsx"))
        print(path)
        withProgress(
          message = paste0(inFile$name, " wird eingelesen... (Geduld!)"),
          value = 0.1, {
            data <- readBatscopeXLSX_multiple(
              path = paste0(pathname, ".xlsx"),
              species_col_name = input$speciesColName,
              quality_col_name = input$speciesQualName,
              quality_threshold  = input$qual,
              shiny_progress = TRUE)
            incProgress(1, detail = paste("Fertig!"))
          })
        return(data)
      }) #dataInput

      data_r <- reactive({
        gps_coords <- ddply(dataInput(), .(ProjectName), summarize,
          lat = mean(GPSLatitude[GPSValid == "yes"], na.rm = TRUE),
          long = mean(GPSLongitude[GPSValid == "yes"], na.rm = TRUE)
        )
        print( (any(is.na(gps_coords)) == FALSE | input$customCoord == TRUE))
        validate(
          need( (any(is.na(gps_coords)) == FALSE | input$customCoord == TRUE),
            "GPS Koordinaten nicht für alle Stationen vorhanden. Bitte manuell eingeben.")
        )
        inFile <- input$file1

        if (input$customCoord){
          c_lat <- input$lat
          c_long <- input$long
        } else {
          c_lat <- NULL
          c_long <- NULL
        }

        withProgress(
          message = paste(inFile$name, "wird analysiert... (Geduld!)"),
          value = 0, {
            data_r <- sumBatscopeData(
              dataInput(),
              bin_length = input$bins,
              lat = c_lat,
              long = c_long,
              progress = "none",
              shiny_progress = TRUE
            )
            incProgress(1, detail = paste("Fertig!"))
        })
        return(data_r)
      }) #data_r

      data_sum <- reactive({
        validate(
          need(is.null(data_r) != TRUE, "Bitte BatScope xlsx auswählen")
        )
        data_sum <- ddply(data_r(), .(ProjectName, SurveyDate), summarize,
          n_events_day = sum(n_events))
      })

      output$data_table <- renderDataTable({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswählen")
        )
        data_r()[, c(1, 2, 8, 3, 5, 6, 7, 11, 12)]
        },
        options = list(lengthMenu = c(10, 25, 50, 100), pageLength = 10)
      ) #output$data_table

      output$sum_table <- renderDataTable({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswählen")
        )
        data_sum()
        },
        options = list(lengthMenu = c(10, 25, 50, 100), pageLength = 10)
      ) #output$sum_table

      shiny_nightPlot <- reactive({
        validate(
          need(as.character(input$dates[1]) %in%
            format(data_r()$SurveyDate, "%Y-%m-%d"),
            "Keine Datenpunkte für ausgewähltes Datum")
        )
        if (input$customScaleX){
          hhmm1 <- str_c(
            c(floor(input$hourAxis1),
              str_pad(
                as.character(
                  (input$hourAxis1 - floor(input$hourAxis1)) * 60),
                2, "right", "0")),
            collapse = ":")
          hhmm2 <- str_c(
            c(floor(input$hourAxis2),
              str_pad(
                as.character(
                  (input$hourAxis2 - floor(input$hourAxis2)) * 60),
                2, "right", "0")),
            collapse = ":")
          xlim <- as.POSIXct(
            c(paste(as.character(input$dates[1]), hhmm1),
            paste(as.character(input$dates[2] + 1), hhmm2)))
        } else {
          xlim <- NULL
        }

        if (input$customScaleY){
          ylim <- input$yAxis
        } else {
          ylim <- NULL
        }

        plotData <- subset(data_r(), ProjectName %in% input$project)
        nightPlot(plotData,
          day = as.character(input$dates),
          sel_species = input$species,
          x_limits = xlim,
          y_limits = ylim,
          plot_T = input$plotTemp,
          plot_T_color = input$plot_T_color,
          n_ybreaks = input$n_ybreaks,
          text_size = input$text_size)
      }) #shiny_nightPlot

      output$nightPlot <- renderPlot({
        shiny_nightPlot()
      })

      shiny_periodPlot <- reactive({
        if (input$customScaleX){
          hhmm1 <- str_c(
            c(floor(input$hourAxis1),
              str_pad(
                as.character(
                  (input$hourAxis1 - floor(input$hourAxis1)) * 60),
                2, "right", "0")), collapse = ":")
          hhmm2 <- str_c(
            c(floor(input$hourAxis2),
              str_pad(
                as.character(
                  (input$hourAxis2 - floor(input$hourAxis2)) * 60),
                2, "right", "0")), collapse = ":")
          ylim <- as.POSIXct(
            c(paste("1900-01-01", hhmm1),
              paste("1900-01-02", hhmm2)))
        } else {
          ylim <- NULL
        }
        x_breaks <- paste(input$x_breaks_num, input$x_breaks_unit)
        print(x_breaks)
        x_breaks_label <- input$x_breaks_label
        plotData <- subset(data_r(), ProjectName %in% input$project)
        periodPlot(plotData,
          start_date = as.character(input$dates[1]),
          end_date = as.character(input$dates[2]),
          sel_species = input$species,
          y_limits = ylim,
          x_break_distance = x_breaks,
          y_break_distance = "2 hour",
          x_break_label = x_breaks_label,
          text_size = input$text_size)
      }) #shiny_periodPlot

      output$periodPlot <- renderPlot({
        shiny_periodPlot()
      })

      output$downloadSum <- downloadHandler(
        filename = function() {
          input$save_name_sum
        },
        content = function(file) {
          openxlsx::write.xlsx(data_sum(), file)
        }
      )

      output$downloadData <- downloadHandler(
        filename = function() {
          input$save_name_data
        },
        content = function(file) {
          openxlsx::write.xlsx(data_r(), file)
        }
      )

      output$downloadNightPlot <-  downloadHandler(
        filename = input$save_name_night,
        content = function(file) {
          device <- function(..., width, height){
            grDevices::pdf(..., width = width, height = height, pointsize = 1)
          }
          output_plot <- shiny_nightPlot() +
            theme(text = element_text(size = input$save_text_size))
          ggsave(file, plot = output_plot, device = device,
            width = input$save_width / 2.54, height = input$save_heigth / 2.54,
            scale = 1, dpi = 72)
        }
      )

      output$downloadPeriodPlot <-  downloadHandler(
        filename = input$save_name_period,
        content = function(file) {
          device <- function(..., width, height){
            grDevices::pdf(..., width = width, height = height, pointsize = 1)
          }
          output_plot <- shiny_periodPlot() +
            theme(text = element_text(size = input$save_text_size))
          ggsave(file, plot = output_plot, device = device,
            width = input$save_width / 2.54, height = input$save_heigth / 2.54,
            scale = 1, dpi = 72)
        }
      )
    }
  )
}