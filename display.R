display_missing_in_cols<-function(input,output,data)
{
 
   output$display_missing<- renderPlot({
      #gg_miss_var(data) + labs(y = "Look at all the missing ones") find a place where to put this
       gg_miss_which(data)
     
   })
  

}

display_outliers_in_cols<-function(input,output,data)
{
  if((!is.null(data))&(length(names(data)[sapply(data, is.numeric)])>0))
  {
    quant_cols <- sapply(data, is.numeric)
    quant_data <- data[, quant_cols]
    output$display_out<- renderPlot({
      boxplot(quant_data)
      stripchart(quant_data,col = 1:length(quant_cols), vertical = TRUE, add = TRUE, pch = 19)
      
      
    })
  }
  
    
}

real_time_data<-function(input,output,data){
        
      output$real_time_data_tb<-  DT::renderDataTable({
      
      df<-DT::datatable(
        data,
        options = list(scrollY = 650,scrollX = 500,scroller = TRUE),
    )
  })
  display_missing_in_cols(input,output,data)
  display_outliers_in_cols(input,output,data)
  
}

add_select_ui<-function(input,output,data)
{
  #select variable for handeling missing values for quantitative data 
  output$quant_var_list = renderUI({
    selectInput('quant_var_list', 'Select the variable to apply the amputation on',names(data)[sapply(data, is.numeric)])
  })
  #select variable for handeling missing values for qualitative data 
  output$qual_var_list = renderUI({
    selectInput('qual_var_list', 'Select the variable to apply the amputation on',names(data[grepl('factor|logical|character',sapply(data,class))]))
  })
  
  #select variable for handeling outliers for quantitative data 
  output$out_var_list = renderUI({
    box(id="out_param",selectInput('out_var_list', 'Select the variable to remove the outliers from',names(data)[sapply(data, is.numeric)]),sliderInput("out_thr", "Outlier threshold:", min = 0, max = 100, value = 5))
    
  })
  
  
  #select variable for normalization for quantitative data 
  output$norm_var_list = renderUI({
    selectInput('norm_var_list', 'Select the variable to normalize',names(data)[sapply(data, is.numeric)])
  })
  
  #select variable to rebalnce for all data 
  output$balance_var_list = renderUI({
    selectInput('balance_var_list', 'Select the variable to normalize',names(data))
  })
  
  
  output$corr_col1 <- renderUI({
    col_options <- names(data)[sapply(data, is.numeric)]
    selectInput("corr_col1", "Select Column 1", col_options)
  })
  
  # Create column 2 select input based on loaded dataset
  output$corr_col2 <- renderUI({
    col_options <- names(data)[sapply(data, is.numeric)]
    selectInput("corr_col2", "Select Column 2", col_options)
  })
  
  #select var to rename
  output$rename_var<-renderUI({
    selectInput('rename_var', 'Select the variable to Rename',names(data))
    
  })
  output$rename_new_var<-renderUI({
    textInput('rename_new_var', 'Type in the new name')
    
  })
  
  #slect var ti dummification
  output$dummi_var_list = renderUI({
    selectInput('dummi_var_list', 'Select the variable to apply the Dummification on',names(data[grepl('factor|logical|character',sapply(data,class))]))
  })
  
  #select var for statstcical test
  output$var_test_ad<-renderUI({  selectInput(inputId = "var_test_ad", label = "Choose the Variable for AD test", names(data)[sapply(data, is.numeric)])})
  output$var_test_shapiro<-renderUI({  selectInput(inputId = "var_test_shapiro", label = "Choose the Variable for SHAPIRO test", names(data)[sapply(data, is.numeric)])})
  output$var_test_ks_1<-renderUI({  selectInput(inputId = "var_test_ks_1", label = "Choose the first Variable for KS test", names(data)[sapply(data, is.numeric)])})
  output$var_test_ks_2<-renderUI({  selectInput(inputId = "var_test_ks_2", label = "Choose the second Variable for KS test", names(data)[sapply(data, is.numeric)])})
  output$var_test_mv<-renderUI({  selectInput(inputId = "var_test_mv", label = "Choose the Variable for MV SHAPIRO test", names(data)[sapply(data, is.numeric)])})
  
  output$var_test_cov_1<-renderUI({  selectInput(inputId = "var_test_cov_1", label = "Choose the first Variable for COVARIANCE test", names(data)[sapply(data, is.numeric)])})
  output$var_test_cov_2<-renderUI({  selectInput(inputId = "var_test_cov_2", label = "Choose the second Variable for COVARIANCE test", names(data)[sapply(data, is.numeric)])})  
  
  output$var_test_karl_1<-renderUI({  selectInput(inputId = "var_test_karl_1", label = "Choose the first Variable for KARL PEARSON test", names(data)[sapply(data, is.numeric)])})
  output$var_test_karl_2<-renderUI({  selectInput(inputId = "var_test_karl_2", label = "Choose the second Variable for KARL PEARSON test", names(data)[sapply(data, is.numeric)])})
  
  output$var_test_spear_1<-renderUI({  selectInput(inputId = "var_test_spear_1", label = "Choose the first Variable for SPEARMAN test", names(data)[sapply(data, is.numeric)])})
  output$var_test_spear_2<-renderUI({  selectInput(inputId = "var_test_spear_2", label = "Choose the second Variable for SPEARMAN test", names(data)[sapply(data, is.numeric)])})
  
  output$var_test_kend_1<-renderUI({  selectInput(inputId = "var_test_kend_1", label = "Choose the first Variable for KENDALL test", names(data)[sapply(data, is.numeric)])})
  output$var_test_kend_2<-renderUI({  selectInput(inputId = "var_test_kend_2", label = "Choose the second Variable for KENDALL test", names(data)[sapply(data, is.numeric)])})
  
}


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
  
  add_select_ui(input,output ,data)
  
  #visulazing missing data to be put in the display funnction to be created later  as ui output 
  
}
