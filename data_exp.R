
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
  quant_var_detail(input,output,data)
  qual_var_detail(input,output,data)
  
}