display_out_col<-function(input,output,df,df2)
{
  df3<-list(before=df2[,input$out_var_list],after=df[,input$out_var_list])
  output$out_plot<- renderPlot({
    boxplot(df3)
    stripchart(df3,col =c("yellow","green"), vertical = TRUE, add = TRUE, pch = 19)
    
    
  })
}
handle_outliers<-function(input,output,data)
{
  observeEvent(input$out_rv,{
    df<-data()
    df2<-df
    if(!is.null(input$out_var_list))
    {
      if(input$out_var_list!="")
      {
        col<-df[,input$out_var_list]
        q1 <- quantile(col, 0.25,na.rm = TRUE)#na.rm incase there's missing values 
        q3 <- quantile(col, 0.75,na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - input$out_thr * iqr / 100
        upper_bound <- q3 + input$out_thr * iqr / 100
        df<-df %>% filter(col >= lower_bound & col <= upper_bound)
        display_out_col(input,output ,df,df2)
        
        data(df)
        
        real_time_data(input,output,data())
        data_exploration(input,output,data())
        run_test(input,output,data())
        univariate_ana(input,output,data())
        
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
