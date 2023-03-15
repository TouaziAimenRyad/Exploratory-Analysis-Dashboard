library(shiny)
library(shinydashboard)
shinyUI(
  dashboardPage(skin ="yellow", # change the header background color
                dashboardHeader(title = "Exploratory Analysis", titleWidth = 400), 
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Initialise Data", tabName = "Init_data_tab", icon=icon("check")),
                    menuItem("Data Preparation", tabName = "data_prep", icon=icon("check")),
                    menuItem("Real Time Data", tabName = "real_time", icon=icon("check"))
                  )
                  
                  
                ),
                dashboardBody(
                  tabItems(
                    tabItem(
                      tabName = "Init_data_tab",
                      fluidRow(
                        box(
                              id="upload_box",
                              radioButtons("sep","Select Seperator",c("semicolon" = ";", "comma" = ",", "space" = " ", "tabulation" = "\t")),
                              checkboxInput("header_present","Is the header Present in the Data Set ",FALSE),
                              fileInput("file",  accept = c("text/plain", ".csv",".tsv",".xls"),"Upload DATA SET")
                           ),

                      )  ,
                        fluidRow( 
                         tabBox(id="init_ana",
                                tabPanel("Data View",div(DT::dataTableOutput("init_data_view_tb"))),
                                tabPanel("Quick Summary",verbatimTextOutput("summary"))
                                )
                       )
                    ),
                    
                    tabItem(
                      tabName = "data_prep",
                      tabsetPanel(
                        type="tab",
                        tabPanel("Missing Values",
                                 #visulaisation des valeur manquant dynamique
                                 uiOutput("display_missing"),
                                  tabBox(
                                    tabPanel("Quantitative",
                                      uiOutput("quant_var_list"),
                                      fluidRow(actionButton("quant_rv_mean","Click here to handle using the MEAN methode")),
                                      fluidRow(actionButton("quant_rv_median","Click here to handle using the MEDIAN methode")),
                                      fluidRow(actionButton("quant_rv_intrp","Click here to handle using the SPLINE INTERPOLATION methode")),
                                      fluidRow(actionButton("quant_rv_col","Click here to Delete the column if the percentage exceeds 30% ")),
                                      uiOutput("quant_rv_plt")
                                    ),
                                    tabPanel("Qualitative",
                                      uiOutput("qual_var_list"),
                                      fluidRow(actionButton("qual_rv_mode","Click here to handle using the MODE methode")),
                                      fluidRow(actionButton("qual_rv_col","Click here to Delete the column if the percentage exceeds 30% ")),
                                      uiOutput("qual_rv_plt")
                                    )
                                    
                                  )
                                 ),
                        tabPanel("Outliers",
                                 uiOutput("out_var_list"),
                                 fluidRow(actionButton("out_rv","Click here to remove Outliers")),
                                 uiOutput("out_plot")
                                 ),
                        tabPanel("Normalisation",
                                 uiOutput("norm_var_list"),
                                 fluidRow(actionButton("norm_z_scr","Click here to normalize using z_socre  ")),
                                 fluidRow(actionButton("norm_min_max","Click here to normalize using Min-Max")),
                                 uiOutput("norm_plot")
                                 
                                 ),
                        tabPanel("Class Rebalance",plotOutput("myhist"))
                      ),
                      
                    ),
                    tabItem(tabName = "real_time",
                      box(id="real_t_d",div(DT::dataTableOutput("real_time_data_tb")))
                    )
                  )
                  
                  
                )
  ) 
)


  

#   tabsetPanel(
  #     type = "tab",
  #     tabPanel(
  #       "UPLOAD DATASET",
  #       sidebarLayout(
  #         sidebarPanel(
  #          
  #           radioButtons("hasHeader","Does the File contain a header",c("YES"=TRUE,"NO"=FALSE)),
  #           fileInput("file",  accept = c("text/plain", ".csv",".tsv"),"Choose data file"),
  #           actionButton(inputId = "loadBtn", label = "Load"),
  #           uiOutput("removeNullBtn"),
  #           br(),
  #           uiOutput("removeDupSelect"),
  #           uiOutput("removeDupBtn")
  #         ),
  #         mainPanel(
  #           tableOutput("data"),
  #           textOutput("test")
  #         
  #           
  #         )
  #       ),
  #     ),
  #     tabPanel(
  #       "UNIVARIATE ANALYSIS"
  #     ),
  #     tabPanel(
  #       "BIVARIATE ANALYSIS"
  #     ),
  #     
  #   )

