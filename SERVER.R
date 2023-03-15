library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(zoo)
library(DT)

# add a function similar to init data view to keep a display at hand of your data

real_time_data<-function(input,output,data)
{
  output$real_time_data_tb<-  DT::renderDataTable({
    
    df<-DT::datatable(
      data,
      options = list(scrollY = 650,scrollX = 500,scroller = TRUE),
    )
  })
  
  
}



# what i need to do in missing values !!!!!!!!!!! 
  # uioutput plots and missing valplots to be displayed after each action 
   
# what do i need to do outliers


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
  
  # setting up the real time view of data 
  real_time_data(input,output ,data)
  
  #select variable for handeling missing values for quantitative data 
  output$quant_var_list = renderUI({
    selectInput('quant_var_list', 'Select the variable to apply the amputation on',names(data)[sapply(data, is.numeric)]
)
  })
  #select variable for handeling missing values for qualitative data 
  output$qual_var_list = renderUI({
    selectInput('qual_var_list', 'Select the variable to apply the amputation on',names(data[grepl('factor|logical|character',sapply(data,class))])
    )
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




handeling_missing_values_quant<-function(input,output,data) # add a condition so it happens only when we have select col
{
  # handeling missing values for quantitative data  we have a problem in interpolation and col and plot rendering
  
    # handeling  using the mean method 
    observeEvent(input$quant_rv_mean,{
      #add condition of the exisitence of quantitaive data or do it in the select display
      df=data()
      df2=df
      if(!is.null(input$quant_var_list))
      {
        if((input$quant_var_list!=""))
        {
          if((sum(is.na(data()[,input$quant_var_list]))!=0)){
            
            df[is.na(df[,input$quant_var_list]), input$quant_var_list]<-mean(data()[,input$quant_var_list], na.rm = TRUE)
            
            
          }
          
          data(df)
          # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
          real_time_data(input,output,data())
          # exploration_server(input,output,data())
          # univaree_server(input,output, data())
          # Bivaree_server(input,output, data())
          # Qnt_Qlt_server(input,output, data())
          # Qlt_Qlt_server(input,output, data())
          # Modele_server(input, output, data())
          #print("les valeurs manquantes sont imputées")
        }
        
      }
      
      
    })
    
    
    #handeling using the median
    observeEvent(input$quant_rv_median,{
      df=data()
      df2=df
      if((!is.null(input$quant_var_list)))
      {
        if((input$quant_var_list!=""))
        {
          if((sum(is.na(data()[,input$quant_var_list]))!=0)){
            
            df[is.na(df[,input$quant_var_list]), input$quant_var_list]<-median(data()[,input$quant_var_list], na.rm = TRUE)
            
            
          }
          
          data(df)
          # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
          real_time_data(input,output,data())
          # exploration_server(input,output,data())
          # univaree_server(input,output, data())
          # Bivaree_server(input,output, data())
          # Qnt_Qlt_server(input,output, data())
          # Qlt_Qlt_server(input,output, data())
          # Modele_server(input, output, data())
          #print("les valeurs manquantes sont imputées")
        }
        
      }
      
      
    })
    
    
    #handeling using the knn interpolation
    observeEvent(input$quant_rv_intrp,{
      df=data()
      df2=df
      if((!is.null(input$quant_var_list)))
      {
        if((input$quant_var_list!=""))
        {
          if((sum(is.na(data()[,input$quant_var_list]))!=0)){
            
            df[,input$quant_var_list]<-na.spline(df[,input$quant_var_list]) # change the method
            
            
            
          }
          
          data(df)
          # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
          real_time_data(input,output,data())
          # exploration_server(input,output,data())
          # univaree_server(input,output, data())
          # Bivaree_server(input,output, data())
          # Qnt_Qlt_server(input,output, data())
          # Qlt_Qlt_server(input,output, data())
          # Modele_server(input, output, data())
          #print("les valeurs manquantes sont imputées") 
        }
      }
     
      
    })
    
    
    #handeling missing data by deleting the col
    observeEvent(input$quant_rv_col,{
      df <- data() # delete the col when >30%
      # Calculate percentage of missing values in selected column
      if((!is.null(input$quant_var_list)))
      {
        if((input$quant_var_list!=""))
        {
          missing_percent <- sum(is.na(data()[, input$quant_var_list]))/length(data()[, input$quant_var_list])
          # Filter out selected column if missing values > 30% you can make it more generic by letting the user specify the percentage
          if (missing_percent <= 0.3) {
            df<- data()
          } else {
            col_index <- which(names(data()) == input$quant_var_list)
            df <-data()[,-col_index]
          }
          data(df)
          output$quant_var_list = renderUI({
            selectInput('quant_var_list', 'Select the variable to apply the amputation on',names(data())[sapply(data(), is.numeric)]
            )
          })
          
          
          real_time_data(input,output,data())
          # exploration_server(input,output,data())
          # univaree_server(input,output, data())
          # Bivaree_server(input,output, data())
          # Qnt_Qlt_server(input,output, data())
          # Qlt_Qlt_server(input,output, data())
          # Modele_server(input, output, data())
          #print("les valeurs manquantes sont imputées")
        }
      }
      
      
    })

  
}



handeling_missing_values_qual<-function(input,output,data) #both NA and empty strings are considered missing
{
  #handeling using the mode
  observeEvent(input$qual_rv_mode,{
    df=data()
    df2=df
    if((!is.null(input$qual_var_list)))
    {
      if((input$qual_var_list!=""))
      {
        if(sum(as.character(data()[,input$qual_var_list])=="")>0){
          non_missing_vals <- df[,input$qual_var_list][df[,input$qual_var_list] != ""& !is.na(df[,input$qual_var_list])]
          mode_val <- ifelse(length(non_missing_vals) > 0, names(which.max(table(non_missing_vals))), NA)
          
          
          df[,input$qual_var_list][is.na(df[,input$qual_var_list])]<-mode_val
          df[,input$qual_var_list][df[,input$qual_var_list] == ""] <- mode_val
          data(df)
          
        }
        
        # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
        real_time_data(input,output,data())
        # exploration_server(input,output,data())
        # univaree_server(input,output, data())
        # Bivaree_server(input,output, data())
        # Qnt_Qlt_server(input,output, data())
        # Qlt_Qlt_server(input,output, data())
        # Modele_server(input, output, data())
        #print("les valeurs manquantes sont imputées")
      }
    }
    
    
    
  })
  
  
  #handeling missing data by deleting the col
  observeEvent(input$qual_rv_col,{
    df <- data() # delete the col when >30%
    # Calculate percentage of missing values in selected column
    if((!is.null(input$qual_var_list)))
    {
      if((input$qual_var_list!=""))
      {
        missing_percentage <- sum(df[[input$qual_var_list]] == "" | is.na(df[[input$qual_var_list]])) / length(df[[input$qual_var_list]])
        print(missing_percentage)
        # Filter out selected column if missing values > 30% you can make it more generic by letting the user specify the percentage
        if (missing_percentage <= 0.3) {
          df<- data()
        } else {
          col_index <- which(names(data()) == input$qual_var_list)
          df <-data()[,-col_index]
        }
        data(df)
        output$qual_var_list = renderUI({
          selectInput('qual_var_list', 'Select the variable to apply the amputation on',names(data()[grepl('factor|logical|character',sapply(data(),class))])
          )
        })
        
        
        real_time_data(input,output,data())
        # exploration_server(input,output,data())
        # univaree_server(input,output, data())
        # Bivaree_server(input,output, data())
        # Qnt_Qlt_server(input,output, data())
        # Qlt_Qlt_server(input,output, data())
        # Modele_server(input, output, data())
        #print("les valeurs manquantes sont imputées")
      }
      
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
    # data visualisation for missing values is comman
    handeling_missing_values_quant(input,output,data)
    handeling_missing_values_qual(input ,output,data)
###############################################################################        

  }
)