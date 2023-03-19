univariate_ana<-function(input,output,data)
{
  output$univar_quant_graph<-renderUI({
    fluidRow(column(
      width = 12,
      box(
        title = "Size Distibution Diagram", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput(outputId = "size_diag")
      ),
      box(
        title = "Box Plot", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput(outputId = "box_plot")
      )
      
    ),
    column(
      width = 12,
      box(
        title = "Size Histogram", status = "info", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput(outputId = "size_hist")
      ),
      box(
        title = "Measures of Central Tendency and Dispersion", status = "info", solidHeader = TRUE,
        collapsible = TRUE,
        tableOutput(outputId = "centreDisp"))
      
    ),
    
    column(width = 12,
           box(
             width = 12,
             title = "Cumulative curve", status = "info", solidHeader = TRUE,
             collapsible = TRUE,
             plotOutput(outputId = "cum_curve")
             
           )
    ))
  })
  
  observeEvent(input$tabs,{
    if(input$tabs=="univar_quant")
    {
      if(!is.null(data))
      {
       
          if((length(names(data)[sapply(data, is.numeric)])>0))
          {
              
              output$size_diag<-renderPlot({
                plot(table(data.frame(data[,input$univar_quant_var])), col ="darkgreen", xlab =sym(input$univar_quant_var), ylab ="Sample Size", 
                     main ="Sample Size Distribution")
              })
              
              output$box_plot<-renderPlot({
                boxplot( data.frame(as.numeric(as.character(data[,input$univar_quant_var]))), col = grey(0.8), 
                         main = " ",
                         ylab = "", las = 1)
                rug(data[,input$univar_quant_var], side = 2)
              })
              
              
              output$size_hist<-renderPlot({
                hist(as.numeric(as.character(data[,input$univar_quant_var])) , freq = TRUE, cex.axis = 1.5, cex.main = 1.5,
                     main = "Histogram", col = "cyan",
                     xlab = sym(input$univar_quant_var), ylab = "Sample Size", las = 1,
                     right = FALSE, cex.lab = 1.5)
                
              })
              
      
              tabCentreDisp <- reactive({
                dt =data[,input$univar_quant_var]
                names.tmp <- c("Maximum", "Minimum", "Mean", "Median",
                               "1st quartile", "3rd quartile", "Variance", "Standard-Deviation")

                summary.tmp <- c(max(dt), min(dt), mean(dt), median(dt),
                                 quantile((dt))[2], quantile((dt))[4],
                                 var(dt), sqrt(var(dt)))
                summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
                colnames(summary.tmp) <- c("Measure", "Value")
                
                summary.tmp
              })
              output$centreDisp <- renderTable({tabCentreDisp()})
                     
             
              
              output$cum_curve<-renderPlot({
                
                break_points<-seq(min(data[,input$univar_quant_var]), max(data[,input$univar_quant_var]), by=1)
                data_transform = cut(data[,input$univar_quant_var], break_points,right=FALSE)
                effectif_table = table(data_transform)
                cumulative_effectif = c(0, cumsum(effectif_table))
                
                plot(break_points, cumulative_effectif,
                     xlab =sym(input$univar_quant_var),
                     ylab = "Cumulative Distribution 'Size' ", cex.axis = 1.5, cex.lab = 1.5,
                     main = "Cumulative Distribution Curve",
                     type = "o", col = "green", lwd = 2, cex.main = 1.5)
                lines(break_points, cumulative_effectif)

                
              })
            
            
          }
          
        
      }
      
    }
  })
  
  #######################################################################################
  
  output$univar_qual_graph<-renderUI({
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          title = "Stats Table", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          tableOutput("stat_tbl")
        )
        
        
      ),
      column(
        width = 12,
        
        box(
          width = 6,
          title = "Pie Chart", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("pie_chart")
        )
        
        
        
      ),
      column(
        width = 12,
        box(
          width = 12,
          title = "Bar Chart", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("bar_chart")
        )
      )
    )
    
  })
  
  observeEvent(input$tabs,{
    if(input$tabs=="univar_qual")
    {
      if((!is.null(data)))
      {
        
        if((length(names(data[grepl('factor|logical|character',sapply(data,class))]))>0))
        {
          output$pie_chart <- renderPlot({
            pie(table(data[,input$univar_qual_var]), labels = substr(names(table(data[,input$univar_qual_var])), 1, 7), 
                main = " Pie Chart ", col=rainbow(length(names(table(data[,input$univar_qual_var])))))
          })
          
          output$bar_chart <- renderPlot({
            barplot(table(data[,input$univar_qual_var]), main = "Bar Chart ", 
                    xlab=sym(input$univar_qual_var),
                    ylab="Sample Size", las = 2,
                    names.arg = substr(names(table(data[,input$univar_qual_var])), 1, 9),col=rainbow(length(names(table(data[,input$univar_qual_var])))))
          })
          
          output$stat_tbl <- renderPlot({
            barplot(table(data[,input$univar_qual_var]), main = "Bar Chart ", 
                    xlab=sym(input$univar_qual_var),
                    ylab="Sample Size", las = 2,
                    names.arg = substr(names(table(data[,input$univar_qual_var])), 1, 9),col=rainbow(length(names(table(data[,input$univar_qual_var])))))
          })
          
          tabStat<-reactive({
            table.tmp <- as.data.frame(table(data[,input$univar_qual_var]))
            table.tmp<-cbind(table.tmp,table.tmp[[2]]/nrow(data)*100)
          
             colnames(table.tmp) <- c(input$univar_qual_var, "Sample Size", "Frequency %")
                                   
            table.tmp
          })
          
          output$stat_tbl <- renderTable({ 
              tabStat()
            })
          
        }
      }
    }
  })
 }
