quant_quant<-function(input,output,data)
{
  output$bivar_quant_quant_graph<-renderUI({
    fluidRow(
      column(
        width = 12,
        box(
          title = "Characteristics", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          tableOutput(outputId = "car_quant_quant")
        ),
        box(
          title = "Linear Regression Scatter Plot", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(outputId = "lg_sp_quant_quant")
        )
        
      ),
      column(
        width = 12,
        box(
          title = "Scatter plot Histogram", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(outputId = "hsit_sp_quant_quant")
        ),
        box(
          title = "back-to-back Histogram", status = "info", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(outputId = "b2b_hist"))
        
      ),
    )
  })
  
  
  observeEvent(input$tabs,{
    if(input$tabs=="bivar_quant_quant")
    {
      if(!is.null(data))
      {
        
        if((length(names(data)[sapply(data, is.numeric)])>1))
        {
          output$car_quant_quant<- renderTable({
            
            var.names <-c(input$bivar_quant_quant_var1,input$bivar_quant_quant_var2)
            caract.df <- data.frame()
            
            for(strCol in var.names){
              caract.vect <- c(min(data[, strCol]), max(data[,strCol]), 
                               mean(var(data[,strCol])), sqrt(var(data[,strCol])))
              caract.df <- rbind.data.frame(caract.df, caract.vect)
            }
            
            rownames(caract.df) <- var.names
            colnames(caract.df) <- c("Minimum", "Maximum", "Mean", "Standard-Deviation")
            caract.df
          }, rownames = TRUE, digits = 0)
          
          output$hsit_sp_quant_quant<-renderPlot({
            
            scatter.with.hist(data[,input$bivar_quant_quant_var1],data[,input$bivar_quant_quant_var2])
          })
          
          output$lg_sp_quant_quant<-renderPlot({
            plot(x = data[, input$bivar_quant_quant_var1], y = data[, input$bivar_quant_quant_var2], col = "blue",
                 las = 2, cex.axis = 0.7,
                 main = paste(input$bivar_quant_quant_var2, "as a function of ", input$bivar_quant_quant_var1),
                 xlab = input$bivar_quant_quant_var1, ylab = input$bivar_quant_quant_var2, cex.lab = 1.2
            )
            
            abline(lm(data[, input$bivar_quant_quant_var2]~data[, input$bivar_quant_quant_var1]), col="red", lwd = 2)
          })
          
          output$b2b_hist<-renderPlot({
            histbackback(x = data[,input$bivar_quant_quant_var1], y = data[,input$bivar_quant_quant_var2],
                         xlab = c(input$bivar_quant_quant_var1, input$bivar_quant_quant_var2), main = paste(input$bivar_quant_quant_var1, "and", input$bivar_quant_quant_var2), 
                         las = 2)
          })
          
          
        }
      }
    }
  })
  
  
  
}



quant_qual<-function(input,output,data)
{
  output$bivar_quant_qual_graph<-renderUI({
    fluidRow(
      column(
        width = 12,
        box(
          title = "Bar Plot", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(outputId = "bar_quant_qual")
        ),
        box(
          title = "Parallel Box Plot", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(outputId = "prl_box_quant_qual")
        )
        
      ),
      
    )
  })
  
  observeEvent(input$tabs,{
    if(input$tabs=="bivar_quant_qual")
    {
      if(!is.null(data))
      {
        
        if((length(names(data)[sapply(data, is.numeric)])>0)&(length(names(data)[grepl('factor|logical|character',sapply(data,class))])>0))
        {
          output$bar_quant_qual<-renderPlot({
            var1<-sym(input$bivar_quant_qual_var1)
            var2<-sym(input$bivar_quant_qual_var2)
            ggplot(data, aes(x = !!var1, fill = !!var2)) + geom_bar()
          })
          
          output$prl_box_quant_qual<-renderPlot({
            d.stack <- melt(data, measure.vars = (names(data)[sapply(data, is.numeric)]))
            # Boxplot basique
            d.stack$value <- as.numeric(d.stack$value)
            boxplot(d.stack$value ~ d.stack$variable , col=rainbow(length(names(data)[sapply(data, is.numeric)])) ,
                    xlab = "Modalités", ylab = "Mesures")
          })
          
        }
      }
    }
  })
  
}



qual_qual<-function(input,output,data)
{
  output$bivar_qual_qual_graph<-renderUI({
    fluidRow(
      column(
        width = 12,
        box(
          title = "Statistical Indexes", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          tableOutput(outputId = "stat_qual_qual")
        ),
        box(
          title = "bar chart of column profiles", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput(outputId = "bar_qual_qual")
        )
        
      ),
      column(width = 12,
             box(
               width = 12,
               title = " Contingency Table ", status = "info", solidHeader = TRUE,
               collapsible = TRUE,
               tableOutput(outputId = "cont_tbl")
               
             )
      )
    )
  })
  
  observeEvent(input$tabs,{
    if(input$tabs=="bivar_qual_qual")
    {
      if(!is.null(data))
      {
        
        if((length(names(data)[grepl('factor|logical|character',sapply(data,class))])>1))
        {
          output$cont_tbl<-renderTable({
            var1<-input$bivar_qual_qual_var1
            var2<-input$bivar_qual_qual_var2
            df<-cbind(data[,var1],data[,var2])
            df<-as.data.frame(df)
            colnames(df)<-c("var1","var2")
            tab <-with(df,table(var1,var2))
            tab
          })
          
          output$stat_qual_qual<-renderTable({
            force.df <- as.data.frame(matrix(NA, nrow = 3, ncol = 1))
            rownames(force.df) = c("X2", "Phi2", "Cramer")
            var1<-input$bivar_qual_qual_var1
            var2<-input$bivar_qual_qual_var2
            df<-cbind(data[,var1],data[,var2])
            df<-as.data.frame(df)
            colnames(df)<-c("var1","var2")
            tab <-with(df,table(var1,var2))
            
            tab.indep = tab
            n = sum(tab)
            tab.rowSum = apply(tab, 2, sum)
            tab.colSum = apply(tab, 1, sum)
          
            for(i in c(1:length(tab.colSum))){
              for(j in c(1:length(tab.rowSum))){
                tab.indep[i,j] = tab.colSum[i]*tab.rowSum[j]/n
              }
            }
            
            # Calcul du X²
            force.df[1,1] = sum((tab-tab.indep)^2/tab.indep)
            # Calcul du Phi²
            force.df[2,1] = force.df[1,1]/n
            # Calcul du Cramer
            force.df[3,1] = sqrt(force.df[2,1]/(min(nrow(tab), ncol(tab))-1))
            
            force.df
          },rownames=TRUE, colnames=FALSE)
          
          output$bar_qual_qual<-renderPlot({
            var1<-sym(input$bivar_qual_qual_var1)
            var2<-sym(input$bivar_qual_qual_var2)
            ggplot(data, aes(x = !!var2, fill = !!var1)) + geom_bar(position = "dodge")
          })
          
        }
      }
    }
  })
}



bivariate_ana<-function(input,output,data)
{
  quant_quant(input,output,data)
  quant_qual(input,output,data)
  qual_qual(input,output,data)
}