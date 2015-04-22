#' batPlotR
#' 
#' launches an interactive web app
#'
#' @param option.list a list of global shiny options
#' @return launches a WebApp
#' @family interactive functions
#' @export
shiny_batPlots <- function(
  option.list=list(shiny.launch.browser=TRUE,shiny.maxRequestSize=100*1024^2)
  ) 
  {
  shinyApp(
    onStart=function(options=option.list){options(options)},
    ui = shinyUI(fluidPage(
      # Application title
      titlePanel("batplotR_interactive"),
      
      # Sidebar with controls to:
      # - upload file
      # - set parameters for summary
      # - set parameters for plots
      sidebarLayout(
        sidebarPanel(
          tags$head(
                 tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
                 tags$style(type="text/css", "select { max-width: 200px; }"),
                 tags$style(type="text/css", "textarea { max-width: 185px; }"),
                 tags$style(type="text/css", ".jslider { max-width: 200px; }"),
                 tags$style(type='text/css', ".well { max-width: 310px; }"),
                 tags$style(type='text/css', ".span4 { max-width: 310px; }")
          ),
          
          fileInput('file1', 'Choose BatScope xlsx File',
            accept=c('.xlsx', 
            '.xls')
          ),
          
          selectizeInput("speciesColName", label = "Kolonnenname Spezies",
            choices = "AutoClass1",
            selected = "AutoClass1"),

          selectizeInput("speciesQualName", label = "Kolonnenname Spezies Qualitaet",
            choices = "AutoClass1Qual",
            selected = "AutoClass1Qual"),
          
          sliderInput("qual", 
                      "Minimale Spezies Qualitaet", 
                       value = 0.8,
                       min = 0, 
                       max = 1),

          sliderInput("bins", 
                      "Laenge der bins", 
                       value = 5,
                       min = 1,
                       max = 120),

          selectizeInput(
            'project', 'Standorte', choices = "?", multiple = TRUE
          ),
          selectizeInput(
            'species', 'Species', choices = "?", multiple = TRUE
          ),
          dateRangeInput("dates", label = "Date range"),
          tags$b("Eigene Stundenachse"),
          checkboxInput("customScaleX", label = "aktiv", value = FALSE),
          p("Stundenachse Start"),
          sliderInput("hourAxis1",label=NULL, min = 0.0, 
            max = 24.0, value = 16.0,step=0.5),
          p("Stundenachse Ende"),
          sliderInput("hourAxis2", label = NULL, min = 0.0, 
            max = 24.0, value = 10.0,step=0.5),

          tags$b("Eigene Y-Achse (nur NightPlot)"),
          checkboxInput("customScaleY", label = "aktiv" , value = FALSE),
          sliderInput("yAxis", label = NULL, min = -10, 
            max = 100, value = c(0,20),step=0.5),
          
          tags$b("Koordinaten (in Dezimalgrad)"),
          checkboxInput("customCoord", label = "aktiv" , value = FALSE),
          p("Breite"),
          numericInput("lat", label = NULL, value =  47.4),
          p("Länge"),
          numericInput("long", label = NULL, value =  8.52)
        ,width=3),#sidebarpanel
        
        # Show a tabset that includes a plot, summary, and table view
        # of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",id="tabs", 
            tabPanel("nightPlot", plotOutput("nightPlot",height = "700px")), 
            tabPanel("periodPlot", plotOutput("periodPlot",height = "700px")), 
            tabPanel("Summary", dataTableOutput("sum_table")),
            tabPanel("Data", dataTableOutput("data_table"))
          )
        ) #mainpanel
      )#sidebarLayout
    )),

    server = function(input, output, session)
    {
      observe({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswaehlen")
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
          speciesNames <- levels(dataSummary()$species)
          names(speciesNames) <- speciesNames
          species_options <- as.list(speciesNames)

          updateSelectizeInput(session, "species",
            choices = species_options,
            selected = species_options[species_options!="all"]
          )
          
          #update Columnnames
          colNames <- colnames(dataInput())
          colNames <- colNames[grepl("Class",colNames)]
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
          if(input$tabs=="nightPlot"){
            updateDateRangeInput(session, "dates", 
              start = format(min(dataInput()$SurveyDate),"%Y-%m-%d"), 
              end = format(min(dataInput()$SurveyDate),"%Y-%m-%d")
            )
          }
          if(input$tabs=="periodPlot"){
            updateDateRangeInput(session, "dates", 
              start = format(min(dataInput()$SurveyDate),"%Y-%m-%d"), 
              end = format(max(dataInput()$SurveyDate),"%Y-%m-%d")
            )
          }

          #update yAxis Input
          updateSliderInput(session, "yAxis", 
            max = round_any((max(dataSummary()$n_events,na.rm=TRUE))*1.4,50,f=ceiling))

      }) #observe

      dataInput <- reactive({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswaehlen")
        )
        inFile <- input$file1
        pathname <- gsub("//", "/",inFile$datapath)
        path <- file.copy(pathname,paste0(pathname,".xlsx"))

        withProgress(message = paste(inFile$name,"wird eingelesen... (Geduld!)"), value = 0.1, {
          data <- readBatscopeXLSX(
            path=paste0(pathname,".xlsx"),
            species_col_name = input$speciesColName,
            quality_col_name = input$speciesQualName,
            quality_threshold  = input$qual,
            shiny_progress=TRUE)
          incProgress(1, detail = paste("Fertig!"))
        })
        return(data)
      }) #dataInput
    
      dataSummary <- reactive({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswaehlen")
        )
        inFile <- input$file1

        if(input$customCoord){
          c_lat <- input$lat
          c_long <- input$long
        } else {
          c_lat <- NULL
          c_long <- NULL
        }

        withProgress(message = paste(inFile$name,"wird analysiert... (Geduld!)"), value = 0, {
          daten_sum <- sumBatscopeData(
            dataInput(),
            bin_length=input$bins,
            lat=c_lat,
            long=c_long,
            progress="none",
            shiny_progress=TRUE
          )
          incProgress(1, detail = paste("Fertig!"))
        })
        return(daten_sum)
      }) #dataSummary
    
      output$data_table <- renderDataTable({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswaehlen")
        )
        dataSummary()
      }) #output$data_table

      output$sum_table <- renderDataTable({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswaehlen")
        )
        ddply(dataSummary(),.(ProjectName,SurveyDate),summarize,n_events_day=sum(n_events))
      }) #output$sum_table
    
      output$nightPlot <- renderPlot({

        validate(
          need(as.character(input$dates[1]) %in% format(dataSummary()$SurveyDate,"%Y-%m-%d"),"Keine Datenpunkte fuer ausgewaehltes Datum")
        )
        if(input$customScaleX){
          hhmm1 <- str_c(c(floor(input$hourAxis1),
            str_pad(as.character((input$hourAxis1-floor(input$hourAxis1))*60),2,"right","0")),collapse=":")
          hhmm2 <- str_c(c(floor(input$hourAxis2),
            str_pad(as.character((input$hourAxis2-floor(input$hourAxis2))*60),2,"right","0")),collapse=":")
          xlim <- as.POSIXct(c(paste(as.character(input$dates[1]),hhmm1),
                      paste(as.character(input$dates[2]+1),hhmm2)))
        } else {
          xlim <- NULL
        }

        if(input$customScaleY){
          ylim <- input$yAxis
        } else {
          ylim <- NULL
        }

        plotData <- subset(dataSummary(),ProjectName %in% input$project)
        nightPlot(plotData,
          day=as.character(input$dates),
          sel_species=input$species,
          x_limits = xlim,
          y_limits = ylim)
    
      }) #output$nightPlot

      output$periodPlot <- renderPlot({

        if(input$customScaleX){
          hhmm1 <- str_c(c(floor(input$hourAxis1),
            str_pad(as.character((input$hourAxis1-floor(input$hourAxis1))*60),2,"right","0")),collapse=":")
          hhmm2 <- str_c(c(floor(input$hourAxis2),
            str_pad(as.character((input$hourAxis2-floor(input$hourAxis2))*60),2,"right","0")),collapse=":")
          ylim <- as.POSIXct(c(paste("1900-01-01",hhmm1),
                      paste("1900-01-02",hhmm2)))
        } else {
          ylim <- NULL
        }

        plotData <- subset(dataSummary(),ProjectName %in% input$project)
        periodPlot(plotData,
          start_date=as.character(input$dates[1]),
          end_date=as.character(input$dates[2]),
          sel_species=input$species,
          y_limits=ylim)
      }) #output$periodPlot
    }
  )
}

#' textInput
#' 
#' shiny helper function
#'
#' @return text input
#' @family interactive functions
textInput<-function (inputId, label, value = "",...) 
{
    tagList(tags$label(label, `for` = inputId), tags$input(id = inputId, 
                                                           type = "text", value = value,...))
}