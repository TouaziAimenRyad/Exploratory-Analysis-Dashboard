library(shiny)
library(shinydashboard)
shinyUI(
  dashboardPage(skin ="yellow", # change the header background color
                dashboardHeader(title = "Exploratory Analysis", titleWidth = 400), 
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Initialise Data", tabName = "Init_data_tab", icon=icon("check")),
                    menuItem("Data Preparation", tabName = "data_prep", icon=icon("check")),
                    menuItem("Data Exploration", tabName = "data_exp", icon=icon("check")),
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
                                 uiOutput("display_missing"),#a plot that shows which cols contain missing values
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
                                 uiOutput("display_out"),#a plot that shows which cols contain outliers
                                 uiOutput("out_var_list"),
                                 fluidRow(actionButton("out_rv","Click here to remove Outliers")),
                                 uiOutput("out_plot")
                                 ),
                        tabPanel("Normalisation",
                                 uiOutput("display_norm"),#a plot that shows which cols before and after normalisation
                                 uiOutput("norm_var_list"),
                                 fluidRow(actionButton("norm_z_scr","Click here to normalize using z_socre  ")),
                                 fluidRow(actionButton("norm_min_max","Click here to normalize using Min-Max")),
                                 uiOutput("norm_plot")
                                 
                                 ),
                        tabPanel("Class Rebalance",
                                 uiOutput("display_dist_class"),#a plot that shows class distribution
                                 uiOutput("balance_var_list"),
                                 fluidRow(actionButton("balance_over_samp","Click here to rebalance using over sampling  ")),
                                 fluidRow(actionButton("balance_under_samp","Click here to rebalance using under sampling")),
                                 uiOutput("balance_plot")
                                 )
                      ),
                      
                    ),
                    tabItem(tabName = "data_exp",
                            tabsetPanel(
                              type="tab",
                              tabPanel("Quantitative Variables",
                                       tableOutput("quant_detail")),
                              tabPanel("Qualitative Variables",
                                       tableOutput("qual_detail")),
                              tabPanel("Detailed Summary",
                                       htmlOutput("sumarize"),
                                       #verbatimTextOutput("summerize2")
                                       ),
                              tabPanel("Data Correlation",
                                       sidebarLayout(
                                         
                                         sidebarPanel(

                                           uiOutput("corr_col1"),
                                           uiOutput("corr_col2"),
                                           actionButton("corr_str","GO!")
                                         ),
                                         
                                         mainPanel(
                                           plotOutput("correlation_plot")
                                         )
                                       ),
                                       #fluidRow(box("heatmap"))
                                       ),
                              tabPanel("Rename Columns")
                            )
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

