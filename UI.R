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
                    menuItem("Bivariate Analysis", tabName = "Biivar", icon=icon("check"),
                             menuSubItem("Quantitative/Quantitative",tabName = "bivar_quant_quant", icon=icon("check")),
                             menuSubItem("Quantitative/Qualitative",tabName = "bivar_quant_qual", icon=icon("check")),
                             menuSubItem("Qualitative/Qualitative",tabName = "bivar_qual_qual", icon=icon("check"))
                             ),
                    menuItem("Real Time Data", tabName = "real_time", icon=icon("check")),
                    menuItem("Machine Learning", tabName = "ml", icon=icon("check"))
                  )
                  
                  
                ),
                dashboardBody(
                  tags$style(HTML("#bivar1 {padding: 10px ;} #bivar2 {padding: 10px ;} #bivar3 {padding: 10px ;} #missing_content {padding: 10px ;} button{margin: 5px 0;} #cov_corr_content{padding:10px} #statistics_content{padding:10px} #rename_content{padding:10px} #data_corr_content{padding:10px} #out_content{padding:10px;} #norm_content{padding:10px;} #dumm_content{padding:30px}  #norm_content_plot{padding-top:30px} ")),
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
                                 fluidRow(id="missing_content",
                                   column(6,
                                     tabBox(width=12,
                                       tabPanel("Quantitative",
                                                uiOutput("quant_var_list"),
                                                fluidRow(
                                                  column(8,offset = 2,
                                                         fluidRow(column(12,actionButton("quant_rv_mean","Click here to handle using the MEAN methode",width="100%"))),
                                                         fluidRow(column(12,actionButton("quant_rv_median","Click here to handle using the MEDIAN methode",width="100%"))),
                                                         fluidRow(column(12,actionButton("quant_rv_intrp","Click here to handle using the SPLINE INTERPOLATION methode",width="100%"))),
                                                         fluidRow(column(12,actionButton("quant_rv_col","Click here to Delete the column if the percentage exceeds 30% ",width="100%"))),
                                                  )
                                                  
                                                ),
                                                
                                                plotOutput("quant_rv_plt")
                                       ),
                                       tabPanel("Qualitative",
                                                uiOutput("qual_var_list"),
                                                fluidRow(
                                                  column(8,offset = 2,
                                                         fluidRow(column(12,actionButton("qual_rv_mode","Click here to handle using the MODE methode",width="100%"))),
                                                         fluidRow(column(12,actionButton("qual_rv_col","Click here to Delete the column if the percentage exceeds 30% ",width = "100%"))),
                                                  )),
                                                
                                                plotOutput("qual_rv_plt")
                                       )
                                       
                                     )
                                   ),
                                   column(6,
                                     plotOutput("display_missing"),
                                     
                                   )
                                 )
                                 #visulaisation des valeur manquant dynamique
                                 #a plot that shows which cols contain missing values
                                
                        ),
                        tabPanel("Outliers",
                              fluidRow(id="out_content",
                                 column(4,
                                        fluidRow(id="out_content2",
                                          column(12,
                                            uiOutput("out_var_list")
                                          ),
                                          column(12,
                                            fluidRow(column(8,offset=2,actionButton("out_rv","Click here to remove Outliers",width="100%"))),
                                          )
                                        )
                                   
                                   
                                 ),
                                 column(8,
                                   plotOutput("display_out"),
                                   plotOutput("out_plot")
                                 )
                              )
                                 
                                
                        ),
                        tabPanel("Normalisation",
                                 fluidRow(id="norm_content",
                                   column(8,offset = 2,
                                          fluidRow(
                                            column(8,offset = 2,
                                                uiOutput("norm_var_list"),
                                              )
                                          ),
                                          fluidRow(
                                            column(8,offset = 2,
                                                   fluidRow(column(8,offset = 2,actionButton("norm_z_scr","Click here to normalize using z_socre",width="100%"))),
                                                   fluidRow(column(8,offset = 2,actionButton("norm_min_max","Click here to normalize using Min-Max",width="100%"))),  
                                            )
                                          )
                                          
                                   ),
                                   column(12,
                                          
                                          fluidRow(id="norm_content_plot", plotOutput("norm_plot"))
                              
                                   )
                                 )
                                 
                                 
                                 
                        ),
                        tabPanel("Class Rebalance",
                                 uiOutput("display_dist_class"),#a plot that shows class distribution
                                 uiOutput("balance_var_list"),
                                 fluidRow(actionButton("balance_over_samp","Click here to rebalance using over sampling  ")),
                                 fluidRow(actionButton("balance_under_samp","Click here to rebalance using under sampling")),
                                 uiOutput("balance_plot")
                        ),
                        tabPanel("Dummification",
                                 fluidRow(id="dumm_content",
                                   column(8,offset = 2,
                                          uiOutput("dummi_var_list"),
                                          fluidRow(column(6,offset=3,actionButton("dummi_str","Click here to apply dummification on chosen Variable",width = "100%")))
                                        )
                                 )
                                 
                                 
                                 
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
                              
                              tabPanel("Data Correlation",
                                       fluidRow(id="data_corr_content",
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
                                       )
                                       
                                       #fluidRow(box("heatmap"))
                              ),
                              tabPanel("Rename Columns",
                                       fluidRow(id="rename_content",
                                                uiOutput("rename_var"),
                                                uiOutput("rename_new_var"),
                                                actionButton("rename","Rename")
                                        )
                                       
                                       )
                            )
                    ),
                    tabItem(tabName = 'stat_test',
                            tabsetPanel(
                              type='tab',
                              tabPanel("Statistics",
                                       fluidRow(id="statistics_content",
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
                                      )
                                       
                                  ),
                              tabPanel("Covariance & Correlation",
                                       fluidRow(id="cov_corr_content",
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
                                     fluidRow(id="bivar1",
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
                            fluidRow(id="bivar3",
                                       uiOutput("bivar_qual_qual_var1"),
                                       uiOutput("bivar_qual_qual_var2"),
                                       
                                ),
                                     
                                     uiOutput("bivar_qual_qual_graph")
                                     
                            
                    ),
                    tabItem(tabName = "real_time",
                            box(id="real_t_d",div(DT::dataTableOutput("real_time_data_tb")))
                    ),
                    tabItem(tabName = "ml",
                            tabsetPanel(type='tab',
                                        tabPanel("Penalized Logistic Regression",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     radioButtons("lr_sampling_method", "Sampling Method:",
                                                                  choices = c("Under Sampling", "Over Sampling"),
                                                                  selected = "Under Sampling"),
                                                     sliderInput("lr_training_portion", "Training Portion:",
                                                                 min = 0.5, max = 0.9, value = 0.8, step = 0.05),
                                                     actionButton("lr_run_button", "Run Classifier"),
                                                   ),
                                                   mainPanel(
                                                     tabsetPanel(
                                                       tabPanel("Model",verbatimTextOutput("lr_model")),
                                                       tabPanel("ROC Curve", plotOutput("lr_roc_plot")),
                                                       #tabPanel("Cluster Plot", plotOutput("knn_cluster_plot")),
                                                       tabPanel("Confusion Matrix",tableOutput("lr_conmtrx")),
                                                       tabPanel("Performance Metrics",tableOutput("lr_tab_metrics"))
                                                     )
                                                   ))
                                                 ),
                                        tabPanel("KNN",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     radioButtons("knn_sampling_method", "Sampling Method:",
                                                                  choices = c("Under Sampling", "Over Sampling"),
                                                                  selected = "Under Sampling"),
                                                     sliderInput("knn_training_portion", "Training Portion:",
                                                                 min = 0.5, max = 0.9, value = 0.8, step = 0.05),
                                                     actionButton("knn_run_button", "Run Classifier"),
                                                   ),
                                                   mainPanel(
                                                     tabsetPanel(
                                                       tabPanel("Model",verbatimTextOutput("knn_model")),
                                                       tabPanel("ROC Curve", plotOutput("knn_roc_plot")),
                                                       #tabPanel("Cluster Plot", plotOutput("knn_cluster_plot")),
                                                       tabPanel("Confusion Matrix",tableOutput("knn_conmtrx")),
                                                       tabPanel("Performance Metrics",tableOutput("knn_tab_metrics"))
                                                     )
                                                 ))
                                                 ),
                                        tabPanel("Random Forests",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     radioButtons("rf_sampling_method", "Sampling Method:",
                                                                  choices = c("Under Sampling", "Over Sampling"),
                                                                  selected = "Under Sampling"),
                                                     sliderInput("rf_training_portion", "Training Portion:",
                                                                 min = 0.5, max = 0.9, value = 0.8, step = 0.05),
                                                     actionButton("rf_run_button", "Run Classifier"),
                                                   ),
                                                   mainPanel(
                                                     tabsetPanel(
                                                       tabPanel("Model",verbatimTextOutput("rf_model")),
                                                       tabPanel("ROC Curve", plotOutput("rf_roc_plot")),
                                                       #tabPanel("Cluster Plot", plotOutput("rf_cluster_plot")),
                                                       tabPanel("Confusion Matrix",tableOutput("rf_conmtrx")),
                                                       tabPanel("Performance Metrics",tableOutput("rf_tab_metrics"))
                                                     )
                                                   ))
                                                 )
                                        )
                    )
                  )
                  
                  
                ),
                

  ) 
)


