# 5 august 2022
# app.R

# ---------- libraries ---------- #

library(shiny)
library(stringr)
library(shinythemes)

# ---------- OTHER ---------- #

sampleData <- read.csv("data/testingSet_withVars_DATA_ONE.csv")

# ---------- UI ---------- #

ui <- fluidPage(theme = shinytheme("united"),
  titlePanel("PhosphoPep"),
  sidebarLayout(
    sidebarPanel(
      h4("How to Format Custom Data:"),
      p("this is placeholder text")
    ),
    mainPanel(
      tabsetPanel(
        id = "main",
        type = "tabs",
        tabPanel(
          "Data", 
          br(),
          p("Data Options:"),
          radioButtons(
            inputId = "selectDataButton",
            label = "",
            choices = c("Use Sample Data", "Upload Custom Data"),
            selected = "Use Sample Data"
          ),
          br(),
          tabsetPanel(
            id = "dataOptions",
            type = "hidden",
            tabPanelBody("Use Sample Data",
                         "Sample Data is Selected"),
            tabPanelBody("Upload Custom Data", 
                         p("Upload Custom Data is Selected"),
                         fileInput(
                           inputId = "fileUpload",
                           label = "Upload CSV or TSV File",
                           multiple = FALSE,
                           accept = c(".csv", ".tsv")
                         ),

                        )
            
          ),
          plotOutput("exploreHist"),
          actionButton(inputId = "toModelsButton",
                       label = "Continue")
        ),
        tabPanel(
          "Models",
          p("run predictions"),
          p("download predictions"),
          p("results and plots")
        ),
        tabPanel(
          "About", 
          p("This is placehodler text"),
          tableOutput(outputId = "datatable"),
        )
      )
    )
  )
)

# ---------- SERVER ---------- #

server <- function(input, output)
{
  # TEMP
  output$datatable <- renderTable({
    head(selectedData())
  })
  
  # handles switching input options based on sample/custom data set
  observeEvent(input$selectDataButton, {
    updateTabsetPanel(inputId = "dataOptions", selected = input$selectDataButton)
  })
  
  # handles uploading custom data set
  customData <- reactive({
    req(input$fileUpload)
    # validate extension
    fileExtension <- tools::file_ext(input$fileUpload$name)
    # read file
    switch(fileExtension,
      # csv
      csv = read.csv(input$fileUpload$datapath),
      # tsv
      tsv = read.csv(input$fileUpload$datapath, sep = "\t"),
      validate("Error: File is not of type .csv or .tsv")
    )
  })
  
  # encode a custom data set
  customDataWithVars <- reactive({
    req(input$fileUpload)
    data <- customData()
    colnames(data) <- c("Peptide.Sequence2", "RetentionTime")
    data$peptideLength <- nchar(data$Peptide.Sequence2)
    data$unmodA <- str_count(data$Peptide.Sequence2, "A")
    data$unmodC <- str_count(data$Peptide.Sequence2, "C")
    data$unmodD <- str_count(data$Peptide.Sequence2, "D")
    data$unmodE <- str_count(data$Peptide.Sequence2, "E")
    data$unmodF <- str_count(data$Peptide.Sequence2, "F")
    data$unmodG <- str_count(data$Peptide.Sequence2, "G")
    data$unmodH <- str_count(data$Peptide.Sequence2, "H")
    data$unmodI <- str_count(data$Peptide.Sequence2, "I")
    data$unmodK <- str_count(data$Peptide.Sequence2, "K")
    data$unmodL <- str_count(data$Peptide.Sequence2, "L")
    data$unmodM <- str_count(data$Peptide.Sequence2, "M")
    data$unmodN <- str_count(data$Peptide.Sequence2, "N")
    data$unmodP <- str_count(data$Peptide.Sequence2, "P")
    data$unmodQ <- str_count(data$Peptide.Sequence2, "Q")
    data$unmodR <- str_count(data$Peptide.Sequence2, "R")
    data$unmodS <- str_count(data$Peptide.Sequence2, "S")
    data$unmodT <- str_count(data$Peptide.Sequence2, "T")
    data$unmodV <- str_count(data$Peptide.Sequence2, "V")
    data$unmodW <- str_count(data$Peptide.Sequence2, "W")
    data$unmodY <- str_count(data$Peptide.Sequence2, "Y")
    data$modS <- str_count(data$Peptide.Sequence2, "s")
    data$modT <- str_count(data$Peptide.Sequence2, "t")
    data$modY <- str_count(data$Peptide.Sequence2, "y")
    data$modM <- str_count(data$Peptide.Sequence2, "m")
    data
  })
  
  # selects the intended data set
  selectedData <- reactive({
    switch(input$selectDataButton,
           "Use Sample Data" = sampleData,
           "Upload Custom Data" = customDataWithVars())
  })
  
  histogramMainTitle <- reactive({
    switch(input$selectDataButton,
           "Use Sample Data" = "Sample Data Retention Time Frequency Histogram",
           "Upload Custom Data" = "Custom Data Retention Time Frequency Histogram")
  })
 
  # exploratory histogram
  output$exploreHist <- renderPlot({
    hist(selectedData()$RetentionTime,
         main = histogramMainTitle(),
         xlab = "Retention Time (minutes)",
         col = "#215563")
  })
  
  # to models button
  observeEvent(input$toModelsButton,{
    updateTabsetPanel(inputId = "main", selected = "Models")
  })
  
}

# ---------- SHINY ---------- #

shinyApp(ui = ui, server = server)