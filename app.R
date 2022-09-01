# 25 august 2022
# app.R

# ---------- libraries ---------- #

library(shiny)
library(stringr)
library(shinythemes)
library(shinyBS)
library(shinycssloaders)
library(glmnet)
library(xgboost)
library(ranger)

# ---------- OTHER ---------- #

# load sample data
sampleData <- read.csv("data/testingSet_withVars_DATA_ONE.csv")
# load models
load("data/models/slr.RData")
load("data/models/stepwise.RData")
load("data/models/ridge.RData")
load("data/models/lasso.RData")
load("data/models/elasticNet.RData")
#load("data/models/rf.RData")
#svr
load("data/models/xgb.RData")
# load sample results
load("data/sample/slr_sample_result.RData")
load("data/sample/stepwise_sample_result.RData")
load("data/sample/ridge_sample_result.RData")
load("data/sample/lasso_sample_result.RData")
load("data/sample/elasticNet_sample_result.RData")
#load("data/sample/svm_sample_result.RData")
load("data/sample/rf_sample_result.RData")
load("data/sample/xgb_sample_result.RData")
colnames(xgb_sample_results) <- c("rmse", "mse", "window", "cor")
sampleResults <- rbind(slr_sample_result, stepwise_sample_result)
sampleResults <- rbind(sampleResults, ridge_sample_result)
sampleResults <- rbind(sampleResults, lasso_sample_result)
sampleResults <- rbind(sampleResults, elasticNet_sample_result)
sampleResults <- rbind(sampleResults, rf_sample_result)
sampleResults <- rbind(sampleResults, xgb_sample_results)
colnames(sampleResults) <- c("Root Mean Square Error (minutes)", "Mean Absolute Error (minutes)", "95% Error Window Size (minutes)", "Correlation (minutes)")
row.names(sampleResults) <- c("Simple Linear Regression", "Stepwise Regresion", "Ridge Regression", "Lasso Regression", "Elastic Net Regression", "Random Forest", "XG Boost")
sampleRF <- read.csv("data/sample/sample_rf.csv")

# ---------- UI ---------- #

ui <- fluidPage(theme = shinytheme("united"),titlePanel("PhosphoPep"),sidebarLayout(sidebarPanel(h4("How to Format Custom Data:"),
                                                                                                 p("Data should be formatted in a single column with a header, \"x\". Accepted file types are .csv (comma separated values) or .tsv (tab separated values)."),
                                                                                                 p("Each peptide should be represented as a character string, with one character per amino acid. Unmodified peptides should use a capitilalized letter and modified peptides should use a lower-case letter."),
                                                                                                 p("Accepted modifications are phosphorylated serine, threonine, or tyrosine, indicated as \"s\", \"t\", or \"y\", or oxidated methionine, represented with \"m\". Other modifications are not supported."),
                                                                                                 h4("Example:"),
                                                                                                 p("x,"),
                                                                                                 p("AsMTyS,",),
                                                                                                 p("AAStSyPGD,"),
                                                                                                 p("HYQmmsDRS,")
                                                                                                 ),
    
                                                                                    
    mainPanel(
      tabsetPanel(
        id = "main",
        type = "tabs",
        tabPanel(
          "Data", 
          br(),
          
          h4("Data Options:"),
          
          radioButtons(
            inputId = "selectDataButton",
            label = "",
            choices = c("Use Sample Data", "Upload Custom Data", "Upload Custom Data with Alignment Data"),
            selected = "Use Sample Data"
          ),
          
          br(),
          
          tabsetPanel(
            id = "dataOptions",
            type = "hidden",
            tabPanelBody("Use Sample Data", 
                         h4("Sample Data is Selected")),
            tabPanelBody("Upload Custom Data", 
                         h4("Upload Custom Data is Selected"),
                         fileInput(inputId = "fileUpload",label = "Upload CSV or TSV File",multiple = FALSE,accept = c(".csv", ".tsv"))),
            tabPanelBody("Upload Custom Data with Alignment Data", 
                         h4("Upload Custom Data with Alignment Data is Selected"),
                         fileInput(inputId = "alignFileUpload",label="Upload CSV or TSV File of Alignment Data"),multiple = FALSE, accept = c(".csv",".tsv"),
                         fileInput(inputId = "fileUpload2",label = "Upload CSV or TSV File of Data to Predict"),multiple = FALSE,accept = c(".csv",".tsv"))
          ),
          
          br(),
          tableOutput(outputId = "datatable"),
          
          tabsetPanel(
            id = "goButton",
            type = "hidden",
            tabPanelBody("hide"),
            tabPanelBody("show", actionButton(inputId = "toModelsButton", label = "Continue"))
          )
          
          
        ),   # END OF DATA TAB
        tabPanel(
          "Models",
          # put any of the models where results aren't cached here
          br(),
          

          
          tabsetPanel(
            id = "hideIfNoData",
            type = "hidden",
            tabPanel(id = "showingIfData", title = "showingIfData",
                     tabsetPanel(
                       id ="modelSelect",
                       type = "pills",
                       tabPanel(id = "slr_panel", title = "Linear Regression", br(), h4("Linear Regression"),p("Select Results based on Linear Regression model:")),
                       tabPanel(id = "stepwise_panel", title = "Best Subset Regression", br(), h4("Best Subset Regression"),p("Select Results based on Best Subset Regression model:")),
                       tabPanel(id = "ridge_panel", title = "Ridge Regression", br(), h4("Ridge Regression"),p("Select Results based on Ridge Regression model:")),
                       tabPanel(id = "lasso_panel", title = "Lasso Regression", br(), h4("Lasso Regression"),p("Select Results based on Lasso Regression model:")),
                       tabPanel(id = "elastic_panel", title = "Elastic Net Regression", br(), h4("Elastic Net Regression"),p("Select Results based on Elastic Net Regression Model")),
                       tabPanel(id = "svr_panel", title = "Support Vector Regression", br(), h4("Support Vector Regression"),p("Select Results based on Support Vector Regression Model")),
                       tabPanel(id = "rf_panel", title = "Random Forest", br(), h4("Random Forest"),textOutput(outputId = "rf_flag"),bsTooltip(id = "rf_flag", title="Random Forest sample results are precalculated to avoid excessive computations", placement = "left"),),
                       tabPanel(id = "xgb_panel", title = "XG Boost", br(), h4("Extreme Gradient Boosting"),p("Select Results based on Extreme Gradient Boosted model"))
                     ),
                     withSpinner(tableOutput(outputId = "selected_model_results"),  type = 6),
                     bsTooltip(id = "selected_model_results", title="Predictions are rounded to two decimal places in this table. For unrounded results, download predictions", placement = "left"),
                     downloadButton(outputId = "download_predictions", "Download Predictions as .csv"),
                     downloadButton(outputId = "download_predictions_tsv", "Download Predictions as .tsv")
                     ),
            tabPanel(id = "notShowingNoData", title = "notShowingNoData", h5("Custom Data has been selected, but a custom data set has not been uploaded yet."))
          ),
          
        ), # END OF MODELS TAB
        tabPanel(
          "About", 
          br(),
          p("The models presented here were created using a data set with over 100,000 peptides. Approximately 72,000 peptides were used as a training set to build the models. The remaining peptides serve as a testing set and are presented as the sample data in this application."),
          p("The accuracy of the models is shown with test results of the sample data set, shown below."),
          h3("Sample Dataset Results"), 
          plotOutput(outputId = "rmsePlot"),
          plotOutput(outputId = "maeplot"),
          plotOutput(outputId = "windowplot"),
          plotOutput(outputId = "corplot"),
          bsTooltip(id = "sampleResultsTable", title="Sample Data Results are pre-calculated and cached to reduce computation time."), 
          tableOutput(outputId = "sampleResultsTable"),
        )
      )
    )
  )
)

# ---------- SERVER ---------- #

server <- function(input, output)
{

  ############################################ DATA TAB #######################################

  
  # show head of data on data upload page
  output$datatable <- renderTable({
    if(input$selectDataButton == "Upload Custom Data")
    {
      req(input$fileUpload) 
    }
    
    if(input$selectDataButton == "Upload Custom Data with Alignment Data")
    {
      req(input$fileUpload2)
    }
    head(selectedData()[,c("Peptide.Sequence2")])
  },
  colnames = FALSE,
  hover = TRUE
  )
  
  # handles switching input options based on sample/custom data set
  observeEvent(input$selectDataButton, {
    updateTabsetPanel(inputId = "dataOptions", selected = input$selectDataButton)
    switch(input$selectDataButton,
           "Use Sample Data" = {updateTabsetPanel(inputId = "goButton", selected = "show")},
           "Upload Custom Data" = {
               updateTabsetPanel(inputId = "goButton", selected = "hide")
               req(input$fileUpload)
               updateTabsetPanel(inputId = "goButton", selected = "show")
           },
           "Upload Custom Data with Alignment Data" = {
               updateTabsetPanel(inputId = "goButton", selected = "hide")
               req(input$fileUpload2)
               req(input$alignFileUpload)
               updateTabsetPanel(inputId = "goButton", selected = "show")
           })
  })
  
  # handles uploading custom data set
  customData <- reactive({
    # read from either fileUpload or fileUpload 2
    if(input$selectDataButton == "Upload Custom Data")
    {
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
    }
    else
    {
      req(input$fileUpload2)
      # validate extension
      fileExtension <- tools::file_ext(input$fileUpload2$name)
      # read file
      switch(fileExtension,
             # csv
             csv = read.csv(input$fileUpload2$datapath),
             # tsv
             tsv = read.csv(input$fileUpload2$datapath, sep = "\t"),
             validate("Error: File is not of type .csv or .tsv")
      )
    }

  })
  
  # encode a custom data set
  customDataWithVars <- reactive({
    if(input$selectDataButton == "Upload Custom Data")
    {
      req(input$fileUpload)
    }
    else
    {
      req(input$fileUpload2)
    }
    data <- customData()
    updateTabsetPanel(inputId = "goButton", selected = "show")
    colnames(data) <- c("Peptide.Sequence2")
    data$RetentionTime <- 0 # this is done for glmnet predictions - this value is not actually used anywhere
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
           "Upload Custom Data" = customDataWithVars(),
           "Upload Custom Data with Alignment Data" = customDataWithVars())
  })
  
  alignmentData <- reactive({
    req(input$alignFileUpload)
    # validate extension
    fileExtension <- tools::file_ext(input$alignFileUpload$name)
    # read file
    switch(fileExtension,
           # csv
           csv = read.csv(input$alignFileUpload$datapath),
           # tsv
           tsv = read.csv(input$alignFileUpload$datapath, sep = "\t"),
           validate("Error: File is not of type .csv or .tsv")
    )
  })
  
  alignmentDataWithVars <- reactive({
    data <- alignmentData()
    updateTabsetPanel(inputId = "goButton", selected = "show")
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
  
  # to models button
  observeEvent(input$toModelsButton,{
    updateTabsetPanel(inputId = "main", selected = "Models")
  })
  
  # this is a smarter way to do this
  observeEvent(input$main, {
    switch(input$selectDataButton,
           "Use Sample Data" = {
             updateTabsetPanel(inputId = "modelResults", selected = "sample")
             updateTabsetPanel(inputId = "hideIfNoData", selected = "showingIfData")
           },
           "Upload Custom Data" = {
             updateTabsetPanel(inputId = "modelResults", selected = "running")
             if(is.null(input$fileUpload))
             {
               updateTabsetPanel(inputId = "hideIfNoData", selected = "notShowingNoData")
             }
             else
             {
               updateTabsetPanel(inputId = "hideIfNoData", selected = "showingIfData")
             }
           },
           "Upload Custom Data with Alignment Data" = {
             if(is.null(input$fileUpload2) || is.null(input$alignFileUpload))
             {
               updateTabsetPanel(inputId = "hideIfNoData", selected = "notShowingNoData")
             }
             else
             {
               updateTabsetPanel(inputId = "hideIfNoData", selected = "showingIfData")
             }
           })
  })
  
  ############################################### MODELS TAB #################################
  
  # results table
  output$selected_model_results <- renderTable(
    head(selected_results()), hover = TRUE, bordered = TRUE
  )
  
  
  selected_results_no_sequences <- reactive({
    switch(input$modelSelect,
           "Linear Regression" = slr_results(),
           "Best Subset Regression" = stepwise_results(),
           "Ridge Regression" = ridge_results(),
           "Lasso Regression" = lasso_results(),
           "Elastic Net Regression" = elastic_results(),
         #  "svr_panel" = "",
           "Random Forest" = rf_results(),
           "XG Boost" = xgb_results()
    )
  })
  
  selected_results <- reactive({
    data.frame(Sequence = selectedData()$Peptide.Sequence2, Prediction = selected_results_no_sequences())
  })
                          
  # slr
  slr_results <- reactive({
    if(input$selectDataButton != "Upload Custom Data with Alignment Data")
    {
      predict(slr_one, selectedData())
    }
    else
    {
      # predict the alignment data with slr
      predictions_of_alignment_data <- predict(slr_one, alignmentDataWithVars())
      #print(head(predictions_of_alignment_data)) #### DEBUG
      #print(length(predictions_of_alignment_data)) #### DEBUG
      
      # make a data frame for the lm we are about to create
      align_df <- data.frame(predictions_of_alignment_data, alignmentDataWithVars()$RetentionTime)
      colnames(align_df) <- c("predicts_of_align", "acutal_align") # easier names to work with
      
      #print(head(align_df)) #### DEBUG
      #print(nrow(align_df)) #### DEBUG
      
      # make a SLR model for alignment
      new_LM <- lm(acutal_align ~ predicts_of_align, data = align_df)
      
      # now, we do the predictions of the data we want to predict
      predictions_of_selected_data <- predict(slr_one, selectedData())
      
      #print(head(predictions_of_selected_data)) #### DEBUG
      #print(length(predictions_of_selected_data)) #### DEBUG
      
      new_df <- as.data.frame(predictions_of_selected_data)
      colnames(new_df) <- c("predicts_of_align")
      
      # now, we run the predictions_of_selected_data through the new_LM to align it
      final_predictions <- predict(new_LM, new_df)
    }
  })
  
  # stepwise
  stepwise_results <- reactive({
    if(input$selectDataButton != "Upload Custom Data with Alignment Data")
    {
      predict(stepwiseModel, selectedData())
    }
    else
    {
      predictions_of_alignment_data <- predict(stepwiseModel, alignmentDataWithVars())
      
      align_df <- data.frame(predictions_of_alignment_data, alignmentDataWithVars()$RetentionTime)
      colnames(align_df) <- c("predicts_of_align", "acutal_align") # easier names to work with
      
      new_LM <- lm(acutal_align ~ predicts_of_align, data = align_df)
      
      predictions_of_selected_data <- predict(stepwiseModel, selectedData())
      
      new_df <- as.data.frame(predictions_of_selected_data)
      colnames(new_df) <- c("predicts_of_align")
      
      final_predictions <- predict(new_LM, new_df)
    }
  })
  
  # matrix format for data
  glm_matrix <- reactive({
    model.matrix(RetentionTime ~ unmodA+unmodC+unmodD+unmodE+unmodF+
                                 unmodG+unmodH+unmodI+unmodK+unmodL+
                                 unmodM+unmodN+unmodP+unmodQ+unmodR+
                                 unmodS+unmodT+unmodV+unmodW+unmodY+
                                 modS+modY+modT+modM+peptideLength, 
                 selectedData())[, -1]
  })
  
  # matrix format for alignment data
  glm_matrix_align <- reactive({
    model.matrix(RetentionTime ~ unmodA+unmodC+unmodD+unmodE+unmodF+
                   unmodG+unmodH+unmodI+unmodK+unmodL+
                   unmodM+unmodN+unmodP+unmodQ+unmodR+
                   unmodS+unmodT+unmodV+unmodW+unmodY+
                   modS+modY+modT+modM+peptideLength, 
                 alignmentDataWithVars())[, -1]
  })
  
  # ridge
  ridge_results <- reactive({
    if(input$selectDataButton != "Upload Custom Data with Alignment Data")
    {
      predict(ridgeModel, newx = glm_matrix())[,1]
    }
    else
    {
      predictions_of_alignment_data <- predict(ridgeModel, newx = glm_matrix_align())[,1]
      
      align_df <- data.frame(predictions_of_alignment_data, alignmentDataWithVars()$RetentionTime)
      colnames(align_df) <- c("predicts_of_align", "acutal_align") # easier names to work with
      
      new_LM <- lm(acutal_align ~ predicts_of_align, data = align_df)
      
      predictions_of_selected_data <- predict(ridgeModel, newx = glm_matrix())[,1]
      
      new_df <- as.data.frame(predictions_of_selected_data)
      colnames(new_df) <- c("predicts_of_align")
      
      final_predictions <- predict(new_LM, new_df)
    }
  })

  # lasso
  lasso_results <- reactive({
    if(input$selectDataButton != "Upload Custom Data with Alignment Data")
    {
      predict(lassoModel, newx = glm_matrix())[,1]
    }
    else
    {
      predictions_of_alignment_data <- predict(lassoModel, newx = glm_matrix_align())[,1]
      
      align_df <- data.frame(predictions_of_alignment_data, alignmentDataWithVars()$RetentionTime)
      colnames(align_df) <- c("predicts_of_align", "acutal_align") # easier names to work with
      
      new_LM <- lm(acutal_align ~ predicts_of_align, data = align_df)
      
      predictions_of_selected_data <- predict(lassoModel, newx = glm_matrix())[,1]
      
      new_df <- as.data.frame(predictions_of_selected_data)
      colnames(new_df) <- c("predicts_of_align")
      
      final_predictions <- predict(new_LM, new_df)
    }
  })
  
  # elastic
  elastic_results <- reactive({
    if(input$selectDataButton != "Upload Custom Data with Alignment Data")
    {
      predict(elasticModel, newx = glm_matrix())[,1]
    }
    else
    {
      predictions_of_alignment_data <- predict(elasticModel, newx = glm_matrix_align())[,1]
      
      align_df <- data.frame(predictions_of_alignment_data, alignmentDataWithVars()$RetentionTime)
      colnames(align_df) <- c("predicts_of_align", "acutal_align") # easier names to work with
      
      new_LM <- lm(acutal_align ~ predicts_of_align, data = align_df)
      
      predictions_of_selected_data <- predict(elasticModel, newx = glm_matrix())[,1]
      
      new_df <- as.data.frame(predictions_of_selected_data)
      colnames(new_df) <- c("predicts_of_align")
      
      final_predictions <- predict(new_LM, new_df)
    }
  })
  
  # xgb matrix
  xgb_matrix <- reactive({
    xgb.DMatrix(data.matrix(subset(selectedData(), select = c("peptideLength", "unmodA", "unmodC", "unmodD", "unmodE", "unmodF", "unmodG", "unmodH", "unmodI", "unmodK", "unmodL", "unmodM", "unmodN", "unmodP", "unmodQ", "unmodR", "unmodS", "unmodT", "unmodV", "unmodW", "unmodY", "modS", "modT", "modY", "modM"))), label = selectedData()$RetentionTime)
  })
  
  xgb_matrix_align <- reactive({
    xgb.DMatrix(data.matrix(subset(alignmentDataWithVars(), select = c("peptideLength", "unmodA", "unmodC", "unmodD", "unmodE", "unmodF", "unmodG", "unmodH", "unmodI", "unmodK", "unmodL", "unmodM", "unmodN", "unmodP", "unmodQ", "unmodR", "unmodS", "unmodT", "unmodV", "unmodW", "unmodY", "modS", "modT", "modY", "modM"))), label = alignmentDataWithVars()$RetentionTime)
  })
  
  # xgb results
  xgb_results <- reactive({
    if(input$selectDataButton != "Upload Custom Data with Alignment Data")
    {
      predict(xgb_model, newdata = xgb_matrix())
    }
    else
    {
      predictions_of_alignment_data <- predict(xgb_model, newdata = xgb_matrix_align())
      
      align_df <- data.frame(predictions_of_alignment_data, alignmentDataWithVars()$RetentionTime)
      colnames(align_df) <- c("predicts_of_align", "acutal_align") # easier names to work with
      
      new_LM <- lm(acutal_align ~ predicts_of_align, data = align_df)
      
      predictions_of_selected_data <- predict(xgb_model, newdata = xgb_matrix())
      
      new_df <- as.data.frame(predictions_of_selected_data)
      colnames(new_df) <- c("predicts_of_align")
      
      final_predictions <- predict(new_LM, new_df)
    }
  })
  
  rf_results <- reactive({
    switch(input$selectDataButton,
           "Use Sample Data" = sampleRF$RetentionTime,
           "Upload Custom Data" = rf_results_custom(),
           "Upload Custom Data with Alignment Data" = rf_results_align()
    )
  })
  
  rf_results_custom <- reactive({
    predict(rfModel, selectedData())
  })
  
  rf_results_align <- reactive({
    predictions_of_alignment_data <- predict(rfModel, alignmentDataWithVars())
    
    align_df <- data.frame(predictions_of_alignment_data, alignmentDataWithVars()$RetentionTime)
    colnames(align_df) <- c("predicts_of_align", "acutal_align") # easier names to work with
    
    new_LM <- lm(acutal_align ~ predicts_of_align, data = align_df)
    
    predictions_of_selected_data <- predict(rfModel, selectedData())
    
    new_df <- as.data.frame(predictions_of_selected_data)
    colnames(new_df) <- c("predicts_of_align")
    
    final_predictions <- predict(new_LM, new_df)
  })
  
  # download button
  output$download_predictions <- downloadHandler(
    filename = function(){
      
      paste0(input$modelSelect, ".csv")
    },
    content = function(file){
      write.csv(selected_results(), file, row.names = FALSE, quote = FALSE)
    }
  )
  
  # download button (tsv)
  output$download_predictions_tsv <- downloadHandler(
    filename = function(){
      
      paste0(input$modelSelect, ".tsv")
    },
    content = function(file){
      write.table(selected_results(), file, row.names = FALSE, quote = FALSE, sep = '\t')
    }
  )
  
  output$rf_flag <- renderText({"Select Results based on Random Forest model:"})


  ################################################ ABOUT TAB #################################

  # sample results display
  output$sampleResultsTable <- renderTable(
    sampleResults, rownames = TRUE, hover = TRUE
  )
  
  output$rmsePlot <- renderPlot({
    rbPal <- colorRampPalette(c("green", "blue"))
    sampleResults$Col <- rbPal(20)[as.numeric(cut(sampleResults[,1],breaks = 20))]
    barplot(height = sampleResults[,1],names.arg = c("SLR", "Stepwise", "Ridge", "Lasso", "Elastic Net", "RF", "XGB"),  horiz = FALSE, main = "Root Mean Square Error on Sample Data", ylab = "RMSE (Minutes)", xlab = "Model", col = sampleResults$Col)
  })
  
  output$maeplot <- renderPlot({
    rbPal <- colorRampPalette(c("green", "blue"))
    sampleResults$Col2 <- rbPal(20)[as.numeric(cut(sampleResults[,2],breaks = 20))]
    barplot(height = sampleResults[,2],names.arg = c("SLR", "Stepwise", "Ridge", "Lasso", "Elastic Net", "RF", "XGB"),  horiz = FALSE, main = "Mean Absolute Error on Sample Data", ylab = "MAE (Minutes)", xlab = "Model", col = sampleResults$Col2)
  })
  
  output$windowplot <- renderPlot({
    rbPal <- colorRampPalette(c("green", "blue"))
    sampleResults$Col3 <- rbPal(20)[as.numeric(cut(sampleResults[,3],breaks = 20))]
    barplot(height = sampleResults[,3],names.arg = c("SLR", "Stepwise", "Ridge", "Lasso", "Elastic Net", "RF", "XGB"),  horiz = FALSE, main = "95% Error Window Size on Sample Data", ylab = "Window Size (Minutes)", xlab = "Model", col = sampleResults$Col3)
  })
  
  output$corplot <- renderPlot({
    rbPal <- colorRampPalette(c("blue", "green"))
    sampleResults$Col4 <- rbPal(20)[as.numeric(cut(sampleResults[,4],breaks = 20))]
    barplot(height = sampleResults[,4],names.arg = c("SLR", "Stepwise", "Ridge", "Lasso", "Elastic Net", "RF", "XGB"),  horiz = FALSE, main = "Correlation on Sample Data", xlab = "Model", col = sampleResults$Col4)
  })

}
# ---------- SHINY ---------- #

shinyApp(ui = ui, server = server)