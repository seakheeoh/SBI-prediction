library(caret)
library(readr)
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(rms)
library(SHAPforxgboost)
library(DALEX)
library(xgboost)
library(caTools)
library(parallel)

my_model <- readRDS("C:/download/xgboost_model4.rds")

save(my_model, file = 'XGBoost.rda')

explainer <- explain(my_model,
                     data = train.x,
                     y = train.y,
                     type = "classification",
                     label = "xgboost")

save(explainer, file = 'explainer.rda')

load("XGBoost.rda")    # Load saved model
load("explainer.rda")    # Load saved model

ui <- dashboardPage(skin="black",
                    dashboardHeader(title=tags$em("Step I XGBoost-based prediction for diagnosis of biliary atresia", style="text-align:center;color:#0063B7;font-size:100%"),titleWidth = 800),
                    
                    dashboardSidebar(width = 200,
                                     sidebarMenu(
                                       br(),
                                       menuItem(tags$em("Upload Patient Data",style="font-size:100%"),icon=icon("upload"),tabName="data"),
                                       menuItem(tags$em("Download Predictions",style="font-size:100%"),icon=icon("download"),tabName="download")
                                       
                                       
                                     )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName="data",
                                tags$h4("To predict using this model, upload patient data in csv format by using the button below.", style="font-size:100%"),
                                tags$h4("Only a single dataset of a patient is applicable.", style="font-size:100%"),
                                tags$h4("Then, go to the", tags$span("Download Predictions",style="color:red"),
                                        tags$span("section in the sidebar."), style="font-size:100%"),
                                
                                br(),
                                br(),
                                br(),
                                column(width = 12,
                                       fileInput('file1', em('Upload patient data in csv format ',style="text-align:center;color:blue;font-size:100%"),multiple = FALSE,
                                                 accept=c('.csv')),
                                       
                                       uiOutput("sample_input_data_heading"),
                                       tableOutput("sample_input_data"),
                                       
                                       
                                       br(),
                                       br(),
                                       br(),
                                       br()
                                ),
                                br()
                                
                        ),
                        tabItem(tabName="download",
                                fluidRow(
                                  
                                  column(width = 12,
                                         tags$h4("After you upload a test dataset, you can download the predictions in csv format by clicking the button below. The first plot (break-down profile) presents the decomposition of the model prediction into contributions that can be attributed to different explanatory variables. The second plot (Shapley values plot with box plots) showed variable-specific contribution of patient's data compared to that of reference dataset.", 
                                                 style="font-size:100%"),
                                         br(),
                                         tags$h4("Defendable cut-off value of Step I prediction model = 2.0% (at cos of FN = 100, sensitivity = 97.7% and specificity =74.4% ) (blue line). We recommend Step II prediction due to the low performance of Step I prediction.", 
                                                 style="font-size:100%"),
                                         br(),
                                         br(),
                                         br()
                                  )),
                                fluidRow(
                                  
                                  column(width = 12,
                                         downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:100%")),
                                         uiOutput("sample_prediction_heading"), 
                                         tableOutput("sample_predictions")
                                  )),
                                fluidRow(
                                  
                                  column(width = 6,
                                         plotOutput('plot_predictions')
                                  ),
                                  column(width = 6,
                                         plotOutput('plot_predictions2')
                                  )
                                  
                                )
                                
                                
                        ),
                        tabItem(tabName="download2",
                                fluidRow(
                                  
                                  column(width = 12,
                                         tags$h4("You can download our reference dataset in csv format by
                                    clicking the button below. This dataset came from the Seoul Asan Medical Center (n=1605) and Seoul National University Hostpital (n=912), Korea.", 
                                                 style="font-size:100%"),
                                         tags$h4("[Code and unit] BA; biliary atresia (1=diagnosed), Age; age at enrolment, Wt; weight at enrolment, WBC; white blood cell (x10^3/uL), PLT; platelet (x10^3/uL), 
                                         Hb; haemoglobin (g/dL), AST; aspartate aminotransferase (IU/L), 
                                                 ALT; alanine aminotransferase (ALT, IU/L), ALP; alkaline phosphatase (IU/L)), ALB; albumin (g/dL), TB; total bilirubin (mg/dL), 
                                                 DB; direct bilirubin (mg/dL), Cr; creatinine (mg/dL), CRP; C-reactive protein (mg/dL). PT INR, GGT, US and HBS were not included to Step I prediction", 
                                                 style="font-size:100%"),
                                         br()
                                  )),
                                fluidRow(
                                  
                                  column(width = 12,
                                         downloadButton("downloadData2", em('Download Reference Dataset',style="text-align:center;color:blue;font-size:100%")),
                                         br(),
                                         br(),
                                         tableOutput("reference")
                                  )
                                  
                                ))
                        
                        
                        
                        
                      )))




server <- shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize = 800*1024^2)
  output$sample_input_data_heading = renderUI({   
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Patient data')
    }
  })
  
  output$sample_input_data = renderTable({    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
      head(input_data)
    }
  })
  
  
  predictions<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        test.X <- as.matrix(input_data[1:1,2:15])
        Probability = predict(my_model, newdata = test.X)
        input_data_with_prediction = cbind(input_data, Probability)
        input_data_with_prediction       
        
      })
    }
  })
  
  
  output$sample_prediction_heading = renderUI({
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Prediction for biliary atresia')
    }
  })
  
  output$sample_predictions = renderTable({
    pred = predictions()
    head(pred)
    
  })
  
  output$plot_predictions = renderPlot({ 
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        test.X <- as.matrix(input_data[1:1,2:15])
        my_model2 <- predict_parts(explainer = explainer, new_observation = test.X, type = "break_down")
        plot(my_model2, max_features = 16)+
        annotate("segment", x = 0, xend = 18, y = 0.02, yend = 0.02, colour = "blue", size=0.5)
        
        
      })
    }
  })
  
  output$plot_predictions2 = renderPlot({ 
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        test.X <- as.matrix(input_data[1:1,2:15])
        my_model2 <- predict_parts(explainer = explainer, new_observation = test.X, type = "shap")
        plot(my_model2, max_features = 16)
      })
    }
  })
  
  
  
  
  
  output$reference = renderTable({   # the last 6 rows to show
    data1 <- df3[,1:15]
    head(data1)
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("reference", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df3, file, row.names = FALSE)
    })
  
  
  
})

shinyApp(ui = ui, server = server)


