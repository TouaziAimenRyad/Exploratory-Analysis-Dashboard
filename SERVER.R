library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
read_dataSet<-function(input,initial_data,data)
{
  infile<-input$file
  if(is.null(infile)) return (NULL)
  ext <- tools::file_ext(infile$datapath)
  if(ext=="xls"){
    d=read_xls(infile$datapath,header=input$header_present, sep =input$sep)
    data(d)
    initial_data(d)
    
  }else{
    d=read.csv(infile$datapath, header=input$header_present, sep =input$sep)
    data(d)
    initial_data(d)

  } 
}


init_data_view<-function(input,output,data)
{
  output$init_data_view_tb<-  DT::renderDataTable({
    
    df<-DT::datatable(
      data,
      options = list(scrollY = 650,scrollX = 500,scroller = TRUE),
    )
  })
  
  output$summary <- renderPrint(
  {
    if(is.null(data)){
      return("Data hasn't been uploaded yet")
    }
    else
    {
      return(summary(data))
    }
  })
        
}


shinyServer(
  function(input,output)
  {
    initial_data<-reactiveVal(NULL)
    data <- reactiveVal(NULL)
    df_outliers<-reactiveVal(NULL)
    Variable_outliers<-reactiveVal(NULL)
    
    #uploading files
    
    observeEvent(input$file,{
      
       read_dataSet(input,initial_data,data)
       init_data_view(input,output,data())
      # exploration_server(input,output,data())
      # univaree_server(input,output, data())
      # Bivaree_server(input,output, data())
      # Qnt_Qlt_server(input,output, data())
      # Qlt_Qlt_server(input,output, data())
      # Modele_server(input, output, data())
      # Missing_server(input, output, data())
    })
    
    # changing seprator 
    observeEvent(input$sep,{
      
      read_dataSet(input,initial_data,data)
      init_data_view(input,output,data())
      # exploration_server(input,output,data())
      # univaree_server(input,output, data())
      # Bivaree_server(input,output, data())
      # Qnt_Qlt_server(input,output, data())
      # Qlt_Qlt_server(input,output, data())
      # Modele_server(input, output, data())
      # 
      
    })
    
    
    # changing header if present or not
    observeEvent(input$header_present,{
      
      read_dataSet(input,initial_data,data)
      init_data_view(input,output,data())
      # exploration_server(input,output,data())
      # univaree_server(input,output, data())
      # Bivaree_server(input,output, data())
      # Qnt_Qlt_server(input,output, data())
      # Qlt_Qlt_server(input,output, data())
      # Modele_server(input, output, data())
    })
    
    
    
    #Reset Data Set
    observeEvent(input$reset,{
      data(initial_data())
      init_data_view(input,output,data())
      # exploration_server(input,output,data())
      # univaree_server(input,output, data())
      # Bivaree_server(input,output, data())
      # Qnt_Qlt_server(input,output, data())
      # Qlt_Qlt_server(input,output, data())
      # Modele_server(input, output, data())
      # Missing_server(input, output, data())
    })
    

  }
)