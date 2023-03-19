
class_rebalance<-function(input,output,data)
{
  
  observeEvent(input$balance_over_samp, {
    df <- data()
    
    if((!is.null(input$balance_var_list)))
    {
      if((input$balance_var_list!=""))
      {
        print("fff")
      }
    }
    
    
    
   
  })
  
  
  observeEvent(input$balance_under_samp,{
    df=data()
    if((!is.null(input$balance_var_list)))
    {
      if((input$balance_var_list!=""))
      {
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