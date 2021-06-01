
#packages needed
library(shiny)
library(DT)
library(ggplot2)
library(shinydashboard)
library(dplyr)

ns <-NS("info2")

#library(tidyverse)
if (interactive()) {
  
  #import Lab value data
Random_LabValuesInfo_2021 <- read.delim("~/monomyth/Random_LabValuesInfo_2021.tsv")

#import patient data
Random_PatientLevelInfo_2021 <- read.delim("~/monomyth/Random_PatientLevelInfo_2021.tsv" )

#after looking through lab data, it seemed each patient id only had one result for these columns found below right here; easier for someone to view
Random_PatientLevelInfo_2021 <-left_join(Random_PatientLevelInfo_2021,unique(Random_LabValuesInfo_2021[c("USUBJID", "BMRKR1","BMRKR2")]),by = "USUBJID")

#getting all column names
clin <- colnames(Random_PatientLevelInfo_2021[1,])
  
#getting all unique test types
TestTypes <- unique(Random_LabValuesInfo_2021$LBTEST)

#getting all unique visit
Visits <- unique(Random_LabValuesInfo_2021$AVISIT) 

#Modular UI
  ModuleUI <- function(id,thing){
    
    #assigning the namespace here
    ns <- NS(id)
    
    #tagList is neeeded because it's a module
    tagList(
      
      #call the function that uses all the columns for a drop down selection
      uiOutput(ns("ColumnControl")),
      
      #select all checkboxes
      actionButton(ns("SelectAll"), "Select All"),
      
      #Deselect all checkboxes
      actionButton(ns("SelectNone"), "Deselect All"),
      
      #call the function Listy in the server
      uiOutput(ns("Listy")),

      #seperator
      hr()
    )}
  
  #server to contol the left panel
  swerver <-  function(id) {
    #assigning the namespace
    ns <- NS(id)
    
    #Modul magic happens here
    moduleServer(id, function(input, output, session) {
      #Really important line
      ns <- session$ns
      
      #print out summary of biomarker averages
      output$value <-
        renderPrint({
          input$ListofItemsCheckbox
        })
      
      
      #Once a data file is imported, this adds a selectable list of each column avoid spaces or atomic vectors if possible, not tested
      output$ColumnControl <- renderUI({
        selectInput(
          inputId = ns("Columns"),
          label = "Choose a column:",
          choices = clin
        )
      })
      
      #Function for group check box works well if the count of unique values isn't too many
      #TODO find a scroll viewer or something
      listChoosey <- reactive({
        #Reactive function that creates a list of unique values depending on which column you choose
        columnName <- input$Columns
        
        #Matches column name to array of all column names to pull index out, might need that function later
        res <-
          unique(Random_PatientLevelInfo_2021[, match(columnName, clin)])
        
        #the position of the group check box makes it look weird if there are too many results for this. setting a max of 500
        if (length(res) > 500) {
          res = "Too many"
        }
        
        #Group check box cant recieve atomic values
        as.character(res)
      })
      
      
      #Utilized ListChoosey function and creates a group checkbox
      output$Listy <- renderUI({
        
        #store result so it doesn't call multiple times
        d = listChoosey()
        checkboxGroupInput(
          ns("ListofItemsCheckbox"),
          label = "Unique values in column:",
          choiceNames   = d,
          choiceValues = d
        )
      })
      
      #Observe action button and autocheck all checkboxes
      observeEvent(ns(input$SelectAll), {
        d = listChoosey()
        updateCheckboxGroupInput(
          session,
          "ListofItemsCheckbox",
          choiceNames   = d,
          choiceValues = d,
          selected = d
        )
      })
      
      #Observe action button and un check all checkboxes
      observeEvent(ns(input$SelectNone), {
        d = listChoosey()
        updateCheckboxGroupInput(
          session,
          "ListofItemsCheckbox",
          choiceNames   = d,
          choiceValues = d,
          selected = NULL
        )
        
      })
      
      #Function for filtering data using the checkboxes on the side panel
      FilteredDataplease <- reactive({
        
        #confirms list isn't empty
        #TODO doesn't work
        if (is.null(input$ListofItemsCheckbox)) {
          return("Nothing")
        }
        
        #grabs columen name from input
        columnName <- input$Columns
        
        #storing data set post filtering, using column name and group checkbox
        NUTDATA <-
          filter(
            Random_PatientLevelInfo_2021,
            Random_PatientLevelInfo_2021[, match(columnName, clin)] %in%  input$ListofItemsCheckbox
          )
        
        #storing lab data that shares lab subject with data set above
        NUDATA <-
          Random_LabValuesInfo_2021[Random_LabValuesInfo_2021$USUBJID %in%  NUTDATA$USUBJID,]
        
        #return data
        return(NUDATA)
      })
     
      
      
    # output text summary
      #TODO specific statistics the client is looking for can go here
      output$summary <- renderPrint({
        #columns to average
        cols <- c("BMRKR1")
        
        #call filtering function
        dataset <- FilteredDataplease()
        
        #if nothing is choosen in the group checkbox
        if (length(dataset) < 2) {
          return("Nothing")
        }
        
        #summarize the columns choosen above, and filtered data
        
                    summary(dataset[cols])
      })
      
      
      
    })
    
    
    
  }
  
  
  
  
# Define UI for dataset viewer app ----
  ui <- fluidPage(# App title ----
                  
                 #title 
                  titlePanel("Random Clinical Data"),
                  
                  
                  sidebarLayout(sidebarPanel(
                    #call 
                    ModuleUI("info2", "run")), 
                      mainPanel(tabsetPanel(
                        tabPanel("Table",
                                 
                                 
                                 #Text output that averages the biomarker values from the values selected in the group checkbox
                                 titlePanel("Average of Biomarker1"),
                                 fluidRow(column(10, verbatimTextOutput(ns("summary")))),
                            
                                 #Seperator
                                  hr(),
                                 
                                 #Patient Data Table with a few columns from lab values
                             dataTableOutput("table1")),
                        
                        #second tab
                        tabPanel("Summary",
                                 #Lab resuts formatted using selected rows on other tab
                             tableOutput('SummaryOfLabResults'))
                  ))))

#main server
server <- function(input, output) {
 
  
  #Nice interactive table with search capabilities. can filter pretty easily
  output$table1 <- DT::renderDataTable(FilteredDataplease2())
  
  FilteredDataplease2 <- reactive({
    
    #confirms list isn't empty
    #TODO filter first table with this
    if (is.null(input$ListofItemsCheckbox)) {
      return("Nothing")
    }
    
    #grabs columen name from input
    columnName <- input$Columns
    
    #storing data set post filtering, using column name and group checkbox
    NUTDATA <-
      filter(
        Random_PatientLevelInfo_2021,
        Random_PatientLevelInfo_2021[, match(columnName, clin)] %in%  input$ListofItemsCheckbox
      )
    
    return(NUTDATA)
  })
  
  #modular Shiny
  swerver("info2")

  #function make visits the column headers, so viewing data will be more pleasent
  #TODO make dynamic in case data doesn't match this example
  model.data2<- reactive({
    
    #pulls selected rows from first table
    RowsSelected<- input$table1_rows_selected
    
    #tests if the length is 0 so it doesn't pull everything
    if(length(RowsSelected)==0){return("Select a patient on table tab")}
    
    # Import Data into function
    FilteredData2 <- model.data()
    
    
    #Unique values for speceific columns, intentially removing AVAL and AVISIT
      FarLeftDF <- unique(FilteredData2[c("LBCAT","USUBJID","LBTEST", "AVALU")])
      
      
      #Loop thorugh visits and merge per visit, personal experiences Visit will change often
      for(visit in Visits){
        
        #filter per visit i.e. Screening, baseline
        valuedf <- filter(FilteredData2, FilteredData2$AVISIT == visit)
        
        #Select specific columns from datafram
        valuedf <- valuedf[c("USUBJID","LBTEST" ,"AVAL")]
        
        #change column names to stop suffixes
        colnames(valuedf) <-c("USUBJID","LBTEST",visit)
        
        #merge newly name df to unique values above. works not sure if another join might be better
        FarLeftDF <- left_join(FarLeftDF,valuedf, by = c("LBTEST","USUBJID"))
        
      }
    
    #return value
    return(FarLeftDF)
    
    
    
  }

  )
  
 #function to filter Lab Data by rows selected
    model.data <- reactive({
      #pulls selected rows from first table
      RowsSelected<- input$table1_rows_selected
      
      #Selects Patient ids from df
      PatientIDs <- Random_PatientLevelInfo_2021[ RowsSelected,2]
     
     #pulls lab data that shares patient ids with rows selected
      LabData_SelectedID<-filter(Random_LabValuesInfo_2021, USUBJID %in% PatientIDs)
    
      #returns variable
      return(LabData_SelectedID)
    })
    
    #RenderTable on the summary tab, This will use two functions
    output$SummaryOfLabResults <- renderTable({ model.data2() })
 
}


shinyApp(ui, server)
  
  
}
  

  
  
  
  
  

  
  
