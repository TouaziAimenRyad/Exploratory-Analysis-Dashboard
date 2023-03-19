library(shiny)
library(shinydashboard)
library(visdat)
shinyUI(
  dashboardPage(skin ="yellow", # change the header background color
                dashboardHeader(title = "Exploratory Analysis", titleWidth = 400), 
                dashboardSidebar(
                  sidebarMenu(id="tabs",
                    menuItem("Initialise Data", tabName = "Init_data_tab", icon=icon("check")),
                    menuItem("Data Preparation", tabName = "data_prep", icon=icon("check")),
                    menuItem("Data Exploration", tabName = "data_exp", icon=icon("check")),
                    menuItem("Statistical Tests", tabName = "stat_test", icon=icon("check")),
                    menuItem("Univariate Analysis", tabName = "univar", icon=icon("check"),
                             menuSubItem("Quantitative",tabName = "univar_quant", icon=icon("check")),
                             menuSubItem("Qualitative",tabName = "univar_qual", icon=icon("check"))
                             ),
                    menuItem("Biivariate Analysis", tabName = "Biivar", icon=icon("check"),
                             menuSubItem("Quantitative/Quantitative",tabName = "bivar_quant_quant", icon=icon("check")),
                             menuSubItem("Quantitative/Qualitative",tabName = "bivar_quant_qual", icon=icon("check")),
                             menuSubItem("Qualitative/Qualitative",tabName = "bivar_qual_qual", icon=icon("check"))
                             ),
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
                                 plotOutput("display_missing"),#a plot that shows which cols contain missing values
                                 tabBox(
                                   tabPanel("Quantitative",
                                            uiOutput("quant_var_list"),
                                            fluidRow(actionButton("quant_rv_mean","Click here to handle using the MEAN methode")),
                                            fluidRow(actionButton("quant_rv_median","Click here to handle using the MEDIAN methode")),
                                            fluidRow(actionButton("quant_rv_intrp","Click here to handle using the SPLINE INTERPOLATION methode")),
                                            fluidRow(actionButton("quant_rv_col","Click here to Delete the column if the percentage exceeds 30% ")),
                                            plotOutput("quant_rv_plt")
                                   ),
                                   tabPanel("Qualitative",
                                            uiOutput("qual_var_list"),
                                            fluidRow(actionButton("qual_rv_mode","Click here to handle using the MODE methode")),
                                            fluidRow(actionButton("qual_rv_col","Click here to Delete the column if the percentage exceeds 30% ")),
                                            plotOutput("qual_rv_plt")
                                   )
                                   
                                 )
                        ),
                        tabPanel("Outliers",
                                 plotOutput("display_out"),#a plot that shows which cols contain outliers
                                 uiOutput("out_var_list"),
                                 fluidRow(actionButton("out_rv","Click here to remove Outliers")),
                                 plotOutput("out_plot")
                        ),
                        tabPanel("Normalisation",
                                 uiOutput("display_norm"),#a plot that shows which cols before and after normalisation
                                 uiOutput("norm_var_list"),
                                 fluidRow(actionButton("norm_z_scr","Click here to normalize using z_socre  ")),
                                 fluidRow(actionButton("norm_min_max","Click here to normalize using Min-Max")),
                                 plotOutput("norm_plot")

                                 
                        ),
                        tabPanel("Class Rebalance",
                                 uiOutput("display_dist_class"),#a plot that shows class distribution
                                 uiOutput("balance_var_list"),
                                 fluidRow(actionButton("balance_over_samp","Click here to rebalance using over sampling  ")),
                                 fluidRow(actionButton("balance_under_samp","Click here to rebalance using under sampling")),
                                 uiOutput("balance_plot")
                        ),
                        tabPanel("Dummification",
                                 uiOutput("dummi_var_list"),
                                 fluidRow(actionButton("dummi_str","Click here to apply dummification on chosen Variable"))
                                 
                                 
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
                              # tabPanel("Detailed Summary",
                              #          htmlOutput("sumarize"),
                              #          #verbatimTextOutput("summerize2")
                              # ),
                              tabPanel("Data Correlation",
                                       sidebarLayout(
                                         
                                         sidebarPanel(
                                           plotOutput("corr_matrix"),
                                           uiOutput("corr_col1"),
                                           uiOutput("corr_col2"),
                                           actionButton("corr_str","GO!")
                                         ),
                                         
                                         mainPanel(
                                           plotOutput("correlation_plot")
                                         )
                                       ),
                                       uiOutput("corr_matrix_ui")
                                       #fluidRow(box("heatmap"))
                              ),
                              tabPanel("Rename Columns",
                                       uiOutput("rename_var"),
                                       uiOutput("rename_new_var"),
                                       actionButton("rename","Rename")
                                       )
                            )
                    ),
                    tabItem(tabName = 'stat_test',
                            tabsetPanel(
                              type='tab',
                              tabPanel("Statistics",
                                       tabBox(
                                         tabPanel("A-D TEST",
                                                  uiOutput("var_test_ad"),
                                                  actionButton("run_ad","RUN TEST"),
                                                  verbatimTextOutput("result_ad"),
                                                  plotOutput("plot_test_ad")
                                                  ),
                                         tabPanel("SHAPIRO TEST",
                                                  uiOutput("var_test_shapiro"),
                                                  actionButton("run_shapiro","RUN TEST"),
                                                  verbatimTextOutput("result_shapiro"),
                                                  plotOutput("plot_test_shapiro")
                                                  ),
                                         tabPanel("K-S TEST",
                                                  uiOutput("var_test_ks_1"),
                                                  uiOutput("var_test_ks_2"),
                                                  actionButton("run_ks","RUN TEST"),
                                                  verbatimTextOutput("result_ks"),
                                                  plotOutput("plot_test_ks")
                                                  ),
                                         tabPanel("MV SHAPIRO TEST",
                                                  uiOutput("var_test_mv"),
                                                  actionButton("run_mv","RUN TEST"),
                                                  verbatimTextOutput("result_mv"),
                                                  plotOutput("plot_test_mv")
                                                  )
                                       )
                                  ),
                              tabPanel("Covariance & Correlation",
                                       tabBox(
                                         tabPanel("COVARIANCE TEST",
                                                  uiOutput("var_test_cov_1"),
                                                  uiOutput("var_test_cov_2"),
                                                  actionButton("run_cov","RUN TEST"),
                                                  verbatimTextOutput("result_cov"),
                                                  #plotOutput("plot_test_cov")
                                                  ),
                                         tabPanel("KARL PEARSON TEST",
                                                  uiOutput("var_test_karl_1"),
                                                  uiOutput("var_test_karl_2"),
                                                  actionButton("run_karl","RUN TEST"),
                                                  verbatimTextOutput("result_karl"),
                                                  plotOutput("plot_test_karl")
                                                  ),
                                         tabPanel("SPEARMAN TEST",
                                                  uiOutput("var_test_spear_1"),
                                                  uiOutput("var_test_spear_2"),
                                                  actionButton("run_spear","RUN TEST"),
                                                  verbatimTextOutput("result_spear"),
                                                  plotOutput("plot_test_spear")),
                                         tabPanel("KENDALL TEST",
                                                  uiOutput("var_test_kend_1"),
                                                  uiOutput("var_test_kend_2"),
                                                  actionButton("run_kend","RUN TEST"),
                                                  verbatimTextOutput("result_kend"),
                                                  plotOutput("plot_test_kend")
                                                  )
                                       )
                                    )
                            )
                            
                          ),

                    tabItem(tabName = "univar_quant",
                             uiOutput("univar_quant_var"),
                             uiOutput("univar_quant_graph")
                           ),
                    tabItem(tabName = "univar_qual",
                             uiOutput("univar_qual_var"),
                             uiOutput("univar_qual_graph")
                           ),
                    
                    tabItem(tabName = "bivar_quant_quant",
                            fluidRow(
                              uiOutput("bivar_quant_quant_var1"),
                              uiOutput("bivar_quant_quant_var2"),
                              
                            ),
                            
                            uiOutput("bivar_quant_quant_graph")
                    ),
                    tabItem(tabName = "bivar_quant_qual",
                            
                            uiOutput("bivar_quant_qual_var"),
                            uiOutput("bivar_quant_qual_graph")
                    ),
                    tabItem(tabName = "bivar_qual_qual",
                            fluidRow(
                              uiOutput("bivar_qual_qual_var1"),
                              uiOutput("bivar_qual_qual_var2")
                                  
                            ),
                            
                            uiOutput("bivar_qual_qual_graph")
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
