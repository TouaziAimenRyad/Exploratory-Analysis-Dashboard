library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(zoo)

# add a function similar to init data view to keep a display at hand of your data

# what i need to do in missing values !!!!!!!!!!! 
  # in the select variable only put quantitave 
  # interpolation and delete columns
  # conditional plots to be displayed after each action 
  # real time data view 

init_data_view<-function(input,output,data) # adds the Ui that we want to be displayed after uploading the data set 
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
  
  #select variable for handeling missing values for quantitative data 
  output$quant_var_list = renderUI({
    selectInput('quant_var_list', 'Select the variable to apply the amputation on',names(data))
  })
  
  
  #visulazing missing data to be put in the display funnction to be created later  as ui output 
        
}



read_dataSet<-function(input,output,initial_data,data)
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
  init_data_view(input,output,data())
}



shinyServer(
  function(input,output)
  {
    initial_data<-reactiveVal(NULL)
    data <- reactiveVal(NULL)
    df_outliers<-reactiveVal(NULL)
    Variable_outliers<-reactiveVal(NULL)
    
    #uploading files
    # any action of chosing the seprator or header will reupload the file and reset our data set 
    
    observeEvent(input$file,{
      
       read_dataSet(input,output,initial_data,data)
       
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
      
      read_dataSet(input,output,initial_data,data)
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
      
      read_dataSet(input,output,initial_data,data)
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
      # exploration_server(input,output,data())
      # univaree_server(input,output, data())
      # Bivaree_server(input,output, data())
      # Qnt_Qlt_server(input,output, data())
      # Qlt_Qlt_server(input,output, data())
      # Modele_server(input, output, data())
      # Missing_server(input, output, data())
    })
    
    ########################################################################################"
    # handeling missing values for quantitative data  we have a problem in interpolation and col and plot rendering
    
    # handeling  using the mean method 
    observeEvent(input$quant_rv_mean,{
      #add condition of the exisitence of quantitaive data or do it in the select display
      df=data()
      df2=df
        
      if(sum(is.na(data()[,input$quant_var_list]))!=0){
        
        df[is.na(df[,input$quant_var_list]), input$quant_var_list]<-mean(data()[,input$quant_var_list], na.rm = TRUE)
        # output$quant_rv_mean_plt<- renderPlot({
        #   plot(df[,input$quant_var_list], type='l', pch=16, col='red', xlab=input$quant_var_list, ylab='')
        #   lines(df2[,input$quant_var_list],col="green")
        #   legend(x = "topright",        
        #          legend = c(paste("with removing the missing values",input$quant_var_list,""), "With keeping missing values"),  
        #          lty = c(1, 2),         
        #          col = c(2, 3),           
        #          lwd = 2) 
        #   
        # } )
        
      }
      
      data(df)
      # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
      # init_data_view(input,output,data())
      # exploration_server(input,output,data())
      # univaree_server(input,output, data())
      # Bivaree_server(input,output, data())
      # Qnt_Qlt_server(input,output, data())
      # Qlt_Qlt_server(input,output, data())
      # Modele_server(input, output, data())
      #print("les valeurs manquantes sont imputées")
      
    })
    
    
    #handeling using the median
    observeEvent(input$quant_rv_median,{
      df=data()
      df2=df
      if(sum(is.na(data()[,input$quant_var_list]))!=0){
        
        df[is.na(df[,input$quant_var_list]), input$quant_var_list]<-median(data()[,input$quant_var_list], na.rm = TRUE)
        # output$quant_rv_mean_plt<- renderPlot({
        #   plot(df[,input$quant_var_list], type='l', pch=16, col='red', xlab=input$quant_var_list, ylab='')
        #   lines(df2[,input$quant_var_list],col="green")
        #   legend(x = "topright",        
        #          legend = c(paste("with removing the missing values",input$quant_var_list,""), "With keeping missing values"),  
        #          lty = c(1, 2),         
        #          col = c(2, 3),           
        #          lwd = 2) 
        #
        # } )
        
      }
    
      data(df)
      # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
       init_data_view(input,output,data())
      # exploration_server(input,output,data())
      # univaree_server(input,output, data())
      # Bivaree_server(input,output, data())
      # Qnt_Qlt_server(input,output, data())
      # Qlt_Qlt_server(input,output, data())
      # Modele_server(input, output, data())
      #print("les valeurs manquantes sont imputées")
        
    })
    
    
    #handeling using the knn interpolation
    observeEvent(input$quant_rv_intrp,{
      df=data()
      df2=df
      if(sum(is.na(data()[,input$quant_var_list]))!=0){
        
        df[,input$quant_var_list] <- na.spline(df[,input$quant_var_list]) # change the method
        
        #print(df[,strCol])
        
        # output$resultat_Ip <- renderPlot({
        #   plot(df[,strCol], type='l', pch=16, col='red', xlab=' x', ylab='')
        #   lines(df2[,strCol],col="green")
        #   legend(x = "topright",          # Position
        #          legend = c(paste("avec imputation par l'interpolation e de la variable ",strCol,""), "avec valeurs manquantes"),  # Legend texts
        #          lty = c(1, 2),           # Line types
        #          col = c(2, 3),           # Line colors
        #          lwd = 2) 
        # } )
        # 
        
      }
      
      data(df)
      # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
      init_data_view(input,output,data())
      # exploration_server(input,output,data())
      # univaree_server(input,output, data())
      # Bivaree_server(input,output, data())
      # Qnt_Qlt_server(input,output, data())
      # Qlt_Qlt_server(input,output, data())
      # Modele_server(input, output, data())
      #print("les valeurs manquantes sont imputées")
      
      
      
    })
    
   
    #handeling missing data by deleting the col
    observeEvent(input$quant_rv_col,{
      df <- data() # delete the col when >30%
      data(df)
      
      init_data_view(input,output,data())
      # exploration_server(input,output,data())
      # univaree_server(input,output, data())
      # Bivaree_server(input,output, data())
      # Qnt_Qlt_server(input,output, data())
      # Qlt_Qlt_server(input,output, data())
      # Modele_server(input, output, data())
      #print("les valeurs manquantes sont imputées")
    })
###############################################################################        

  }
)