display_handle_missing_quant_plot<-function(input,output,df,df2)
{
  output$quant_rv_plt<- renderPlot({
    plot(df[,input$quant_var_list], type='l', pch=16, col='red', xlab=input$quant_var_list, ylab='')
    lines(df2[,input$quant_var_list],col="green")
    legend(x = "topleft",          # Position
           legend = c(paste("avec imputation par la moyenne de la variable ",input$quant_var_list,""), "avec valeurs manquantes"),  # Legend texts
           lty = c(1, 2),           # Line types
           col = c(2, 3),           # Line colors
           lwd = 2) 
    
    
    
  } )
}

display_handle_missing_qual_plot<-function(input,output,df,df2)
{
  output$qual_rv_plt<- renderPlot({
    ggplot(df, aes(x = df2[,input$qual_var_list],fill = df[,input$qual_var_list])) + geom_bar(position = "dodge")
    
    
    
  } )
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
          
          display_handle_missing_quant_plot(input,output,df,df2)
          
          
        }
        
        data(df)
        # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
        real_time_data(input,output,data())
        data_exploration(input,output,data())
        run_test(input,output,data())
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
          
          display_handle_missing_quant_plot(input,output,df,df2)
          
        }
        
        data(df)
        # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
        real_time_data(input,output,data())
        data_exploration(input,output,data())
        run_test(input,output,data())
        
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
          
          display_handle_missing_quant_plot(input,output,df,df2)
          
          
          
        }
        
        data(df)
        # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
        real_time_data(input,output,data())
        data_exploration(input,output,data())
        run_test(input,output,data())
        
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
        add_select_ui(input,output ,data())
        
        
        real_time_data(input,output,data())
        data_exploration(input,output,data())
        run_test(input,output,data())
        
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
        if(sum(as.character(data()[,input$qual_var_list])==""|is.na(data()[,input$qual_var_list]))>0){
          non_missing_vals <- df[,input$qual_var_list][df[,input$qual_var_list] != ""& !is.na(df[,input$qual_var_list])]
          mode_val <- ifelse(length(non_missing_vals) > 0, names(which.max(table(non_missing_vals))), NA)
          
          
          df[,input$qual_var_list][is.na(df[,input$qual_var_list])]<-mode_val
          df[,input$qual_var_list][df[,input$qual_var_list] == ""] <- mode_val
          data(df)
          display_handle_missing_qual_plot(input,output,df,df2)
          
          
        }
        
        # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
        real_time_data(input,output,data())
        data_exploration(input,output,data())
        run_test(input,output,data())
        
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
        add_select_ui(input,output ,data())
        
        
        real_time_data(input,output,data())
        data_exploration(input,output,data())
        run_test(input,output,data())
        
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


handle_missing_values<-function(input,output,data)
{
  handeling_missing_values_quant(input,output,data)
  handeling_missing_values_qual(input ,output,data)
}
