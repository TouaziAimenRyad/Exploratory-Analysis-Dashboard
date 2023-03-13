library(shiny)
library(shinydashboard)
shinyUI(
  dashboardPage(skin ="yellow", # change the header background color
                dashboardHeader(title = "Exploratory Analysis", titleWidth = 400), 
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Initialise Data", tabName = "Init_data_tab", icon=icon("check"))
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

