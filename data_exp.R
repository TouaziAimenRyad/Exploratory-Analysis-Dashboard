# # 
#  detailed_summary<-function(input,output,data)
#  {
#    if(!is.null(data))
#    {
#     
#        
#      # output$summerize2<-renderPrint({
#      #   
#      #     descr(data, stats = "common", plain.ascii = FALSE)
#      # 
#      # })
#    }
#    
#  }
# 
 correlation_view<-function(input,output,data)
 {
   
   if((!is.null(data))&(length(names(data)[sapply(data, is.numeric)])>0))
   {
     quant_cols <- data.frame(data[, sapply(data, is.numeric)])
     colnames(quant_cols) <- colnames(data)[sapply(data, is.numeric)]
     
     if(!is.null(quant_cols))
     {
       corr_mat<-round(cor(quant_cols),2)
       melted_data<-melt( corr_mat)
       print(melted_data)
       output$corr_matrix<-renderPlot({
           ggplot(data = melted_data, aes(x=Var1, y=Var2, fill=value)) + 
           geom_tile()
         })
     }
     
      #Create correlation plot based on selected columns
      observeEvent(input$corr_str,{
        output$correlation_plot <- renderPlot({# maybe if use render ui solve problem
          plot(data[,input$corr_col1], data[,input$corr_col2], pch = 19, col = "blue",main=(paste("Correlation:", round(cor(data[,input$corr_col1], data[,input$corr_col2]), 2))),xlab="VAR1",ylab="VAR2")
          
          # Regression line
          abline(lm(data[,input$corr_col1] ~ data[,input$corr_col2]), col = "red", lwd = 3)
          
         
        })
      })  
      
     
   }

  
}


quant_var_detail<-function(input,output,data)
{
  quant_list<-names(data)[sapply(data, is.numeric)]
  output$quant_detail <- renderTable({
    
    if(length(quant_list)>0){
      df<-data
      l<-list()
      var.names <-quant_list
      caract.df <- data.frame()
      
      for(strCol in var.names){
        
        prg=(sum(is.na(df[,strCol])))/(nrow(df))
        caract.vect <- c("Quantitative", 
                         sum(is.na(df[,strCol])),prg)
        caract.df <- rbind.data.frame(caract.df, caract.vect)
      }
      rownames(caract.df) <- var.names
      colnames(caract.df) <- c("Variable Type","Nomber of missing values","Percentage %")
      caract.df
    }
    else{
      print("No Quantitative value was detected ")
    }
    
  }, rownames = TRUE, digits = 0)
}

qual_var_detail<-function(input,output,data)
{
  qual_list<-names(data[grepl('factor|logical|character',sapply(data,class))])
  output$qual_detail <- renderTable({
    
    if(length(qual_list)>0){
      df<-data
      l<-list()
      var.names <-qual_list
      caract.df <- data.frame()
      
      for(strCol in var.names){
        
        prg<-(sum(is.na(df[,strCol]) | df[,strCol]==""))/(nrow(df))
        caract.vect <- c("Quantitative",  sum(is.na(df[,strCol]) | df[,strCol]=="")  ,prg)
        caract.df <- rbind.data.frame(caract.df, caract.vect)
      }
      rownames(caract.df) <- var.names
      colnames(caract.df) <- c("Variable Type","Nomber of missing values","Percentage %")
      caract.df
    }
    else{
      print("No Qualitative variable was detected ")
    }
    
  }, rownames = TRUE, digits = 0)
}

data_exploration<-function(input,output,data)
{
  if(sum(is.na(data))==0)
  {
    quant_var_detail(input,output,data)
    qual_var_detail(input,output,data)
    # detailed_summary(input,output ,data)
    correlation_view(input,output ,data)
  }
  else
  {
    output$corr_message<-renderUI({ h3("You Need To Handle Missing Values First") })
    output$quant_v_message<-renderUI({h3("You Need To Handle Missing Values First")})
    output$qual_v_message<-renderUI({h3("You Need To Handle Missing Values First")})
    
  }
  
  # 
  
}



rename_var<-function(input,output,data)
{
  observeEvent(input$rename,{
    df<-data()
    if(sum(is.na(data()))==0)
    {
      if((!is.null(input$rename_var))&(!is.null(input$rename_new_var)))
      {
        if((input$rename_var!="")&(input$rename_new_var)!="")
        {
          colnames(df)[colnames(df) == input$rename_var] <- input$rename_new_var
          data(df)
          real_time_data(input,output,data())
          data_exploration(input,output,data())
          add_select_ui(input,output,data())
          run_test(input,output,data())
          univariate_ana(input,output,data())
          bivariate_ana(input,output,data())
          # exploration_server(input,output,data())
          # univaree_server(input,output, data())
          # Bivaree_server(input,output, data())
          # Qnt_Qlt_server(input,output, data())
          # Qlt_Qlt_server(input,output, data())
          # Modele_server(input, output, data())
          #print("les valeurs manquantes sont imputées")
        }
      }
    }
    else
    {
      shinyalert("Oops!", "You Need To Handle all the Missing Values First", type = "error")
      
    }
    
  })
}
