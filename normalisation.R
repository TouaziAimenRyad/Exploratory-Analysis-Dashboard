normalisation<-function(input,output,data)
{
  observeEvent(input$norm_z_scr,{
    df=data()
    if(!is.null(input$norm_var_list))
    {
      if(input$norm_var_list!="")
      {
        norm_col <- scale(df[, input$norm_var_list])
        
        # Add the normalized column to the original data frame
        df[, input$norm_var_list] <- norm_col
        
        # Plot the distribution of the normalized column
        # output$plot <- renderPlot({
        #   ggplot(data = mydata, aes(x = mydata_z)) +
        #     geom_histogram(binwidth = 0.5, fill = "steelblue") +
        #     xlab("Z-score") +
        #     ylab("Frequency") +
        #     ggtitle("Distribution of Normalized Column")
        # })
      }
    }
    data(df)
    real_time_data(input,output,data())
    data_exploration(input,output,data())
    
    # exploration_server(input,output,data())
    # univaree_server(input,output, data())
    # Bivaree_server(input,output, data())
    # Qnt_Qlt_server(input,output, data())
    # Qlt_Qlt_server(input,output, data())
    # Modele_server(input, output, data())
    #print("les valeurs manquantes sont imputées")
  })
  
  
  observeEvent(input$norm_min_max,{
    df=data()
    if(!is.null(input$norm_var_list))
    {
      if(input$norm_var_list!="")
      {
        # Calculate the minimum and maximum values of the selected column
        col_min <- min(df[, input$norm_var_list])
        col_max <- max(df[, input$norm_var_list])
        
        # Calculate the min-max normalization of the selected column
        norm_col <- (df[, input$norm_var_list] - col_min) / (col_max - col_min)
        
        # Add the normalized column to the original data frame
        df[,input$norm_var_list ] <- norm_col
        
        # Plot the distribution of the normalized column
        # output$plot <- renderPlot({
        #   ggplot(data = mydata, aes(x = mydata_norm)) +
        #     geom_histogram(binwidth = 0.05, fill = "steelblue") +
        #     xlab("Normalized Value") +
        #     ylab("Frequency") +
        #     ggtitle("Distribution of Normalized Column")
        # })
        
        
      }
    }
    data(df)
    real_time_data(input,output,data())
    data_exploration(input,output,data())
    
    # exploration_server(input,output,data())
    # univaree_server(input,output, data())
    # Bivaree_server(input,output, data())
    # Qnt_Qlt_server(input,output, data())
    # Qlt_Qlt_server(input,output, data())
    # Modele_server(input, output, data())
    #print("les valeurs manquantes sont imputées")
  })
}
