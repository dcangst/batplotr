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
  ) {
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
          fileInput('file1', 'Choose BatScope xlsx File',
            accept=c('.xlsx', 
            '.xls')
          ),
          tags$hr(),

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
          tags$hr(),
          sliderInput("bins", 
                      "Laenge der bins", 
                       value = 5,
                       min = 1,
                       max = 120),
          tags$hr(),
          selectizeInput(
            'project', 'Standorte', choices = "?", multiple = TRUE
          ),
          selectizeInput(
            'species', 'Species', choices = "?", multiple = TRUE
          ),
          dateRangeInput("dates", label = "Date range")
        ),
        
        # Show a tabset that includes a plot, summary, and table view
        # of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",id="tabs", 
            tabPanel("nightPlot", plotOutput("nightPlot",height = "700px")), 
            tabPanel("periodPlot", plotOutput("periodPlot",height = "700px")), 
            tabPanel("Summary", dataTableOutput("sum_table")),
            tabPanel("Data", dataTableOutput("data_table"))
          )
        )
      )
    )), 
    server = function(input, output, session) {
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
      })

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
      })
    
      dataSummary <- reactive({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswaehlen")
        )
        inFile <- input$file1
        withProgress(message = paste(inFile$name,"wird analysiert... (Geduld!)"), value = 0, {
          daten_sum <- sumBatscopeData(
            dataInput(),
            bin_length=input$bins,
            progress="none",
            shiny_progress=TRUE
          )
          incProgress(1, detail = paste("Fertig!"))
        })

        return(daten_sum)
      })
    
      output$data_table <- renderDataTable({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswaehlen")
        )
        dataSummary()
      })

      output$sum_table <- renderDataTable({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswaehlen")
        )
        ddply(dataSummary(),.(ProjectName,SurveyDate),summarize,n_events_day=sum(n_events))
      })
    
      output$nightPlot <- renderPlot({

        validate(
          need(as.character(input$dates[1]) %in% format(dataSummary()$SurveyDate,"%Y-%m-%d"),"Keine Datenpunkte fuer ausgewaehltes Datum")
        )
    
        plotData <- subset(dataSummary(),ProjectName %in% input$project)
        nightPlot(plotData,
          day=as.character(input$dates),
          sel_species=input$species)
    
      })

      output$periodPlot <- renderPlot({
        plotData <- subset(dataSummary(),ProjectName %in% input$project)
        periodPlot(plotData,
          start_date=as.character(input$dates[1]),
          end_date=as.character(input$dates[2]),
          sel_species=input$species)
      })  
    }
  )
}

