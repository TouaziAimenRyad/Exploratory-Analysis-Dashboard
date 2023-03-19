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
      )
      
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
    if(input$tabs=="univar")
    {
      print("jk,,kd,kd,kd,k,d,")
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
              
      
               # # Noms des caractéristiques
               # names.tmp <- c("Maximum", "Minimum", "Moyenne", "Médiane",
               #                "1e quartile", "3e quartile", "Variance", "Ecart-type")
               # # Calcul des caractéristiques
               # summary.tmp <- c(max(data()[,1]), min(data()[,1]), mean(data()[,1]), median(data()[,1]),
               #                  quantile((data()[,1]))[2], quantile((data()[,1]))[4],
               #                  var(data()[,1]), sqrt(var(data()[,1])))
               # # Ajout des nomes au vecteur de valeurs
               # summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
               # # Ajout des noms de colonnes
               # colnames(summary.tmp) <- c("Caractéristique", "Valeur")
               # 
               # summary.tmp
               # 
                     
             
              
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
  
  
 }
# 
# 

# 
# tabCentreDisp <- reactive({
#   # Noms des caractéristiques
#   dt =df[,input$radio]
#  
#   
#   summary.tmp
# })
# output$centreDisp <- renderTable({tabCentreDisp()})
# 
# hist( as.numeric(as.character(dt[,input$radio])), freq = FALSE, cex.axis = 1.5, cex.main = 1.5,
#       main = "Histogramme de la variable", col = "green",
#       xlab = dt[1,input$radio] , ylab = "Densité de fréquences", las = 1,
#       right = FALSE, cex.lab = 1.5)
# 
# 
# tmp.hist <- hist(as.numeric(as.character(dt[,input$radio])) , plot = FALSE,
#                  
#                  right = FALSE)
# # Courbe cumulative (effectifs)
# plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
#      xlab =sym(input$radio),
#      ylab = "Effectifs cumulés", cex.axis = 1.5, cex.lab = 1.5,
#      main = "Courbe cumulative ",
#      type = "o", col = "green", lwd = 2, cex.main = 1.5)