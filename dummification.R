dummification<-function(input,output,data)
{
  observeEvent(input$dummi_str,{
    df<-data()
    if(!is.null(input$dummi_var_list))
    {
      if((input$dummi_var_list!=""))
      {
        col_index <- which(names(df) == input$dummi_var_list)
        #creating a category if there were missing values incase he skipped handeling them
        df[,input$dummi_var_list][is.na(df[,input$dummi_var_list])] <- "Missing_Values"
        df<-dummy_cols(df,select_columns=input$dummi_var_list)
        df <-df[,-col_index]
        data(df)
        # if we want to modifie the data displayed in init data we call here innit_dta_view else we don't
        real_time_data(input,output,data())
        add_select_ui(input,output ,data())
        data_exploration(input,output,data())
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
  })
}