library(shiny)
library(shinydashboard)
library(shinythemes)
library(caret)
library(shiny)
library(LiblineaR)
library(readr)
library(ggplot2)
library(readxl)
library(plyr)
library(writexl)
library(kernlab)
library(shinythemes)
library(DT)
library(shinyFiles)

# Load saved model
load("theta_r.rda")
load("theta_s.rda")
load("lgalpha.rda")
load("n.rda")
load("l.rda")
load("logKo.rda")



ui <- fluidPage(tags$head(
  includeCSS("www/CSS.css")),
titlePanel(windowTitle = "SOHYP-SVM",title = tags$head(tags$link(rel = "icon", type = "image/gif",sizes = "200x200",href = "3.jpg"))), 
dashboardHeader(title=tags$span("SOHYP-SVM", style="font-family: 'Lato-Black';text-align: center;margin-bottom: 1000px;font-size:160%;color:#1E64C8",
                                br(),
                                br(),
                                tags$img(src='logo.png', height = '85', width ='499'))
                ),
                              sidebarLayout(
                                sidebarPanel(
                                tags$h4("Welcome to SOHYP-SVM, a web-based tool to predict soil hydraulic properties using a Support Vector Machine approach",
                                style="font-family: 'Lato-Regular';text-align: justify;font-size:120%;color:#1E64C8"),
                                br(),
                                tags$h4("version v1.0, released on 1 November 2021",
                                        style="font-family: 'Lato-Regular';text-align: justify;font-size:80%;color:#1E64C8")
                                
                                ),
              
              mainPanel(tags$head(tags$style(HTML('.skin-black .sidebar-menu{background-color: rgb(255,255,255))}
                                                 .skin-black .main-sidebar {background-color: rgb(255,255,255;}'))),
                             
                            tabsetPanel(type = "tabs",
                                tabPanel("Get started",
                                        br(), 
                                        br(),
                                        tags$h4("Soil hydraulic properties of your soils are predicted in four steps (press each time the corresponding button):",
                                                style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8" ),  
                                        
                                        tags$h4("1. Download input data template spreadsheet
	                                   You can save the spreadsheet anywhere on your computer. You can then fill in or copy/paste your own basic soil properties. Alternatively, you can organise your dataset file in a similar fashion as the template.",
                          style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8" ),
                                         downloadButton("downloadData", "Download input data template spreadsheet",
                                                        style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                          br(),
                          br(),
                          
                          tags$h4("2.	Import input data spreadsheet
	Now you import the spreadsheet you made in step 1. Browse your computer to find the spreadsheet and upload it.",
                                  style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8" ),
                          fileInput('file1',label = NULL ,multiple = FALSE,buttonLabel = tags$span("Browse",
                                    style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                                   accept=c('.xlsx'),placeholder = "No file selected",
                                   ),
                          tags$h4("3.	Predict soil hydraulic properties (output).
	                      Now you get the output, i.e., the hydraulic properties in a table.",
                style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8" ),
                fluidRow(column(width = 2, dataTableOutput(outputId = "sample_predictions1", width = '900px'))),
                         br(),
                         br(),
                         tags$h4("4. Download output data spreadsheet
	                          This will download the generated output table to your computer.",
                                  style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8" ),
                fluidRow(column(width = 7,
                                downloadButton("downloadData1", "Download output data spreadsheet",
                       style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8")))),
                
                tabPanel("Learn more",
                         br(),
                         tags$h4("SOHYP-SVM predicts the water retention and hydraulic conductivity curves from basic soil information using a database 
                         from samples taken between 2015 and 2019 in 252 horizons under arable land in Belgium and covering a wide range of soil textures 
                         and degrees of soil structure degradation.", style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"), 
                         br(),
                         tags$h4("The predictor variables are bulk density BD in Mg m",tags$sup("-3"),", sand content Sa in (mass) percentage, clay content Cl in (mass) percentage, 
                         soil organic carbon content OC in (mass) percentage, and depth D as a proxy, which is given a value of 1 when from the 
                         ploughed topsoil, a value of 2 when from the subsoil or a value of 3 when from a compacted subsoil (typically a plough 
                         sole between the topsoil and the deeper subsoil). The response variables are the van Genuchten-Mualem parameters: residual 
                         water content theta_r in m",tags$sup("3"),"m",tags$sup("-3"),", water content at saturation theta_s in m",tags$sup("3"),"m",tags$sup("-3"), ", air entry parameter alpha in cm", tags$sup("-1"), 
                         ", pore size distribution parameter n dimensionless, connectivity and tortuosity parameter lambda dimensionless, and 
                         hydraulic conductivity K0 at 0 matric potential in cm d",tags$sup("-1"),". Additionally, the water contents at -33 kPa and -1500 kPa
                         matric potential (theta_33 and theta_1500, respectively) are predicted as well.", style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                         br(),
                         tags$h4("Predictor variables were determined with standard procedures: bulk density with the core (250 cm",tags$sup("3"),") method (Grossman & Reinsch, 2002), sand and clay content with 
                         the pipette-sieve method (Gee & Bauder, 1986) and soil organic carbon with the wet oxidation method (Walkley & Black, 1934). 
                         The response variables were derived by curve-fitting the van Genuchten-Mualem equation (Mualem, 1976; Van Genuchten, 1980) 
                         to data pairs obtained from the modified evaporation method (HYPROP setup, METER group; Schindler et al., 2010) 
                         that allows to simultaneously determine water retention and hydraulic conductivity curves.", style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                         br(),
                         tags$h4("The dataset was compiled in the context of a project on assessing the effects of soil compaction and crust formation on soil hydraulic properties and water movement 
                         funded by the Flemish Government of Belgium and executed by Ghent University and Wageningen University & Research.", style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                         br(),
                         tags$h4("References:",style="font-family: 'Lato-Black';text-align: justify;font-size:100%;color:#1E64C8" ),
                         
                         tags$h4("Gee, G. W. & Bauder, J. W. (1986). Particle-size analysis. In Klute: Methods of soil analysis: Part 1 Physical and mineralogical methods. SSSA Book Ser. 5. SSSA, Madison, WI. ",style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                        
                         tags$h4("Grossman, R. & Reinsch, T. G. (2002). Bulk density and linear extensibility. In Dane & Topp: Methods of soil analysis: Part 4 Physical methods. SSSA Book Ser. 5. SSSA, Madison, WI.",style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                         
                        tags$h4("Mualem, Y. (1976). A new model for predicting the hydraulic conductivity of unsaturated porous media. Water Resources Research, 12, 513-522.",style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                        
                        tags$h4("Schindler, U., Durner, W., Unold, G. Von, Mueller, L. & Wieland, R. (2010). The evaporation method: Extending the measurement range of soil hydraulic properties using the air-entry pressure of the ceramic cup. Plant Nutrition and Soil Science, 173, 563-572.",style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                        
                        tags$h4("Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil Science Society of America Journal, 44, 892-898.",style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
                        
                        tags$h4("Walkley, A. & Black, I. A. (1934). An examination of the Degtjareff method for determining soil organic matter, and a proposed modification of the chromic acid titration method. Soil Science, 37, 29-38.",style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8")
 ),tabPanel("Contact",
            tags$h4("If you have any problems with the application, find any errors, or have any questions or recommendations, please contact:",style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
            tags$h4("Do T. Chung (",tags$a(href = "mailto:dt.chung1011@gmail.com","dt.chung1011@gmail.com", 
                                           style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),")",
                    style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
            tags$h4("Wim Cornelis (",tags$a(href = "mailto:wim.cornelis@ugent.be","wim.cornelis@ugent.be", 
                                           style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),")",
                    style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8"),
            tags$h4("Soil Physics Research Group, Department of Environment, Ghent University.",style="font-family: 'Lato-Regular';text-align: justify;font-size:100%;color:#1E64C8")
            ))
            )))
                



server <- function(input, output, session) {
 
  predictions1 <-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readxl::read_excel(input$file1$datapath, col_names = TRUE)
        
        colnames(input_data) = c("D", "OC", "SD", "CL", "BD")
        
        theta_r1 = round(predict.train(Van.svm, input_data), digits = 3)
        for (i in 1:length(theta_r1)){
          if (theta_r1[i] < 0.0){
            theta_r1[i] = 0.001
          }
        }
        theta_s1 = round(predict.train(Van.svm1, input_data), digits =3)
        lgalpha1 = predict.train(Van.svm2, input_data)
        alpha1 = round(10^(lgalpha1), digits = 3)
        n1 = round(predict.train(Van.svm3, input_data), digits = 3)
        l2 = predict.train(Van.svm4, input_data)
        l1 = sprintf("%6.3f",l2)
        logKo = predict.train(Van.svm5, input_data)
        K01 = sprintf("%6.3f",10^(logKo))
        theta_331 = sprintf("%6.3f",theta_r1+(theta_s1-theta_r1)/(1+(alpha1*336.6)^n1)^(1-1/n1))
        theta_15001 = sprintf("%6.3f",theta_r1+(theta_s1-theta_r1)/(1+(alpha1*15300)^n1)^(1-1/n1))
        predictions1 = cbind(seq(from =1, to =length(theta_r1), by = 1), theta_r1, theta_s1 , alpha1, n1, l1, K01, theta_331, theta_15001)
        
        
        
      })
    }
  })
  predictions1b <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readxl::read_excel(input$file1$datapath, col_names = TRUE)
        
        colnames(input_data) = c("D", "OC", "SD", "CL", "BD")
        
        theta_r = round(predict.train(Van.svm, input_data), digits = 3)
        for (i in 1:length(theta_r)){
          if (theta_r[i] < 0.0){
            theta_r[i] = 0.001
          }
        }
        theta_s = round(predict.train(Van.svm1, input_data), digits =3)
        lgalpha = predict.train(Van.svm2, input_data)
        alpha = round(10^(lgalpha), digits = 3)
        n = round(predict.train(Van.svm3, input_data), digits = 3)
        l = round(predict.train(Van.svm4, input_data), digits = 3)
        logKo = predict.train(Van.svm5, input_data)
        K0 = round(10^(logKo), digits = 3)
        theta_33 = round(theta_r+(theta_s-theta_r)/(1+(alpha*336.6)^n)^(1-1/n), digits = 3)
        theta_1500 = round(theta_r+(theta_s-theta_r)/(1+(alpha*15300)^n)^(1-1/n), digits = 3)
        predictions1b = cbind(theta_r, theta_s , alpha, n, l, K0, theta_33, theta_1500)
        
        
        
      })
    }
  })

  output$sample_predictions1 = renderDataTable({datatable(predictions1(),
                                                          options = list(autoWidth = TRUE,
                                                                         columnDefs = list(list(targets=c(0), visible=TRUE, width='30'),
                                                                                           list(targets=c(1), visible=TRUE, width='120'),
                                                                                           list(targets=c(2), visible=TRUE, width='120'),
                                                                                           list(targets=c(3), visible=TRUE, width='120'),
                                                                                           list(targets=c(4), visible=TRUE, width='100'),
                                                                                           list(targets=c(5), visible=TRUE, width='170'),
                                                                                           list(targets=c(6), visible=TRUE, width='200'),
                                                                                           list(targets=c(7), visible=TRUE, width='120'),
                                                                                           list(targets=c(8), visible=TRUE, width='90')),
                                                                         lengthMenu = list(c(10,20,-1), c('10','20','All')),
                                                                         columns = list(list(title = "No."),
                                                                                        list(title = "theta_r (m3 m-3)"),
                                                                                        list(title = 'theta_s (m3 m-3)'),
                                                                                        list(title = 'alpha (cm-1)'),
                                                                                        list(title = 'n (-)'),
                                                                                        list(title = 'lambda (-)'),
                                                                                        list(title = 'K0 (cm d-1)'),
                                                                                        list(title = 'theta_33 (m3 m-3)'),
                                                                                        list(title = 'theta_1500 (m3 m-3)'))))})
input_template <- read_excel('input_template.xlsx')
output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_template", ".xlsx", sep = "")
    }, 
    content = function(file) {
      write_xlsx(input_template, file)
                 })
output$downloadData1 <- downloadHandler(
  filename = function() {
    paste("Output data spreadsheet", ".xlsx", sep = "")
  }, 
  content = function(file) {
    write_xlsx(as.data.frame(predictions1b()), file)})
}

shinyApp(ui = ui, server = server)



















