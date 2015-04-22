#' batPlotR
#' 
#' launches an interactive web app
#'
#' @param shiny_options a list of options passed to shinyApp
#' @return launches a WebApp
#' @family interactive functions
#' @export
batPlotR <- function(
  shiny_options=list(shiny.maxRequestSize=100*1024^2,
                     launch.browser=TRUE)
  ) {
  shinyApp(
    options=shiny_options,
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
          textInput("speciesName", label = "Kolonnenname Spezies", value = "AutoClass1"),
          textInput("speciesQualName", label = "Kolonnenname Spezies Qualität", value = "AutoClass1Qual"),
          sliderInput("qual", 
                      "Minimale Spezies Qualität", 
                       value = 0.8,
                       min = 0, 
                       max = 1),
          tags$hr(),
          sliderInput("bins", 
                      "Länge der bins", 
                       value = 5,
                       min = 1,
                       max = 120),
          tags$hr(),
          checkboxGroupInput("project", "Standorte:",
                       c("?" = "?"
                         )),
          dateRangeInput("dates", label = "Date range")
        ),
        
        # Show a tabset that includes a plot, summary, and table view
        # of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs", 
            tabPanel("nightPlot", plotOutput("nightPlot")), 
            tabPanel("periodPlot", plotOutput("periodPlot")), 
            tabPanel("Data", dataTableOutput("table"))
          )
        )
      )
    )), 
    server = function(input, output, session) {
      observe({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswählen")
        )
        
          projectNames <- unique(dataInput()$ProjectName)
          names(projectNames) <- projectNames
          cb_options <- as.list(projectNames)
        
          updateCheckboxGroupInput(session, "project",
            choices = cb_options,
            selected = projectNames
          )
    
          updateDateRangeInput(session, "dates", 
            start = format(min(dataInput()$SurveyDate),"%Y-%m-%d"), 
            end = format(min(dataInput()$SurveyDate),"%Y-%m-%d")
          )
        
      })

      dataInput <- reactive({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswählen")
        )
        inFile <- input$file1
        pathname <- gsub("//", "/",inFile$datapath)
        path <- file.copy(pathname,paste0(pathname,".xlsx"))
    
        data <- readBatscopeXLSX(
          path=paste0(pathname,".xlsx"),
          species_col_name = input$speciesName,
          quality_col_name = input$speciesQualName,
          quality_threshold  = input$qual)
    
        return(data)
      })
    
      dataSummary <- reactive({
        daten_sum <- sumBatscopeData(
          dataInput(),
          bin_length=input$bins,
          progress="none"
        )
        return(daten_sum)
      })
    
      output$table <- renderDataTable({
        validate(
          need(is.null(input$file1) != TRUE, "Bitte BatScope xlsx auswählen")
        )
        dataSummary()
      })
    
      output$nightPlot <- renderPlot({
        print(input$dates[1])
        print(as.character(input$dates[1]) %in% format(dataSummary()$SurveyDate,"%Y-%m-%d"))
        validate(
          need(as.character(input$dates[1]) %in% format(dataSummary()$SurveyDate,"%Y-%m-%d"),"Keine Datenpunkte für ausgewähltes Datum")
        )
    
          plotData <- subset(dataSummary(),ProjectName %in% input$project)
          nightPlot(plotData,day=as.character(input$dates))
    
      }) 
    }
  )
}