

real_time_data<-function(input,output,data)
{
  output$real_time_data_tb<-  DT::renderDataTable({
    
    df<-DT::datatable(
      data,
      options = list(scrollY = 650,scrollX = 500,scroller = TRUE),
    )
  })
 
  
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
