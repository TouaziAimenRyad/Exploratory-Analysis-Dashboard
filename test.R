run_test<-function(input,output,data)
{
  
  observeEvent(input$run_ad,{
    if(!is.null(data))
    {
      if(length(names(data[sapply(data, is.numeric)]))>0)
      {
        if((input$var_test_ad!="")&(!is.null(input$var_test_ad)))
        {
          selected_variable_column<-data[,input$var_test_ad]
          output$result_ad<-renderPrint({ad.test(selected_variable_column)})
          
        }
      }
    }
  })
  
  observeEvent(input$run_shapiro,{
    if(!is.null(data))
    {
      if(length(names(data[sapply(data, is.numeric)]))>0)
      {
        if((input$var_test_shapiro!="")&(!is.null(input$var_test_shapiro)))
        {
          selected_variable_column<-data[,input$var_test_shapiro]
          output$result_shapiro<-renderPrint({shapiro.test(selected_variable_column)})
          
        }
      }
    }
  })
  
  observeEvent(input$run_mv,{
    if(!is.null(data))
    {
      if(length(names(data[sapply(data, is.numeric)]))>0)
      {
        if((input$var_test_mv!="")&(!is.null(input$var_test_mv)))
        {
          selected_variable_column<-data[,input$var_test_mv]
          output$result_mv<-renderPrint({mvShapiro.Test(selected_variable_column)})
          
        }
      }
    }
  })
  
  observeEvent(input$run_ks,{
    if(!is.null(data))
    {
      if(length(names(data[sapply(data, is.numeric)]))>0)
      {
        if(((input$var_test_ks_1!="")&(!is.null(input$var_test_ks_1)))&((input$var_test_ks_2!="")&(!is.null(input$var_test_ks_2))))
        {
          selected_variable_column_1<-data[,input$var_test_ks_1]
          selected_variable_column_2<-data[,input$var_test_ks_2]
          output$result_ks<-renderPrint({ks.test(selected_variable_column_1,selected_variable_column_2)})
          
        }
      }
    }
  })
  
  
  #####################################
  observeEvent(input$run_cov,{
    if(!is.null(data))
    {
      if(length(names(data[sapply(data, is.numeric)]))>0)
      {
        if(((input$var_test_cov_1!="")&(!is.null(input$var_test_cov_1)))&((input$var_test_cov_2!="")&(!is.null(input$var_test_cov_2))))
        {
          selected_variable_column_1<-data[,input$var_test_cov_1]
          selected_variable_column_2<-data[,input$var_test_cov_2]
          output$result_cov<-renderPrint({cov(selected_variable_column_1,selected_variable_column_2)})
          
        }
      }
    }
  })
  
  observeEvent(input$run_karl,{
    if(!is.null(data))
    {
      if(length(names(data[sapply(data, is.numeric)]))>0)
      {
        if(((input$var_test_karl_1!="")&(!is.null(input$var_test_karl_1)))&((input$var_test_karl_2!="")&(!is.null(input$var_test_karl_2))))
        {
          selected_variable_column_1<-data[,input$var_test_karl_1]
          selected_variable_column_2<-data[,input$var_test_karl_2]
          output$result_karl<-renderPrint({cor(selected_variable_column_1,selected_variable_column_2,method = "pearson")})
          
        }
      }
    }
  })
  
  observeEvent(input$run_spear,{
    if(!is.null(data))
    {
      if(length(names(data[sapply(data, is.numeric)]))>0)
      {
        if(((input$var_test_spear_1!="")&(!is.null(input$var_test_spear_1)))&((input$var_test_spear_2!="")&(!is.null(input$var_test_spear_2))))
        {
          selected_variable_column_1<-data[,input$var_test_spear_1]
          selected_variable_column_2<-data[,input$var_test_spear_2]
          output$result_spear<-renderPrint({cor(selected_variable_column_1,selected_variable_column_2,method = "spearman")})
          
        }
      }
    }
  })
  
  observeEvent(input$run_kend,{
    if(!is.null(data))
    {
      if(length(names(data[sapply(data, is.numeric)]))>0)
      {
        if(((input$var_test_kend_1!="")&(!is.null(input$var_test_kend_1)))&((input$var_test_kend_2!="")&(!is.null(input$var_test_kend_2))))
        {
          selected_variable_column_1<-data[,input$var_test_kend_1]
          selected_variable_column_2<-data[,input$var_test_kend_2]
          output$result_kend<-renderPrint({cor(selected_variable_column_1,selected_variable_column_2,method = "kendall")})
          
        }
      }
    }
  })
  
  # observeEvent(input$submit_test,{
  #   print("eeeeeeeee")
  #   results <-switch(input$type_test,
  #            ad.test = ad.test(data[[input$var_test_1]], data[[input$var_test_2]]),
  #            shapiro.test = shapiro.test(data[[input$var_test_1]]),
  #            ks.test = ks.test(data[[input$var_test_1]], data[[input$var_test_2]]),
  #            mvShapiroTest = mvShapiroTest(data[, c(input$var_test_1, input$var_test_2)]),
  #            cov.test = cov.test(data[[input$var_test_1]], data[[input$var_test_2]]),
  #            cor.test = cor.test(data[[input$var_test_1]], data[[input$var_test_2]], method = input$test)
  #     )
  # 
  #   
  #   output$test_plot <- renderPlot({
  #     if (input$test == "mvShapiroTest") {
  #       df <- data %>% select(input$var_test_1, input$var_test_2)
  #       mvShapiroTest(df)
  #     } else {
  #       df <- data %>% select(input$var_test_1, input$var_test_2) %>% na.omit()
  #       if (nrow(data) < 2) {
  #         return(NULL)
  #       } else {
  #         switch(input$test,
  #                ad.test = ad.test(df[[1]], df[[2]]),
  #                shapiro.test = shapiro.test(df[[1]]),
  #                ks.test = ks.test(df[[1]], df[[2]]),
  #                cov.test = cov.test(df[[1]], df[[2]]),
  #                cor.test = cor.test(df[[1]], df[[2]], method = input$test))
  #       }
  #     }
  #   })
  #   
  #   # Display results
  #   output$result <- renderPrint({
  #     results
  #   })
  # })
  # 
}