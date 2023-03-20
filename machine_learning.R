
random_forest<-function(input,output,data)
{
  data <- read.csv("./heart.csv")
  
  # Convert the output column to a factor
  data$output <- as.factor(data$output)
  data_subset<-data
  
  sample2<-reactive({
    portion<-input$rf_training_portion
    sample(c(TRUE, FALSE), nrow(data_subset), replace=TRUE, prob=c(portion,1-portion))
  })
  training_set <- reactive({
    data_subset[sample2(),]
  })
  
  test_set <- reactive({
    data_subset[!(sample2()),]
  })
  
  
  # Define run button action
  observeEvent(input$rf_run_button, {
    
    
    # Train model
    preProcess <- c("center","scale")
    if(input$rf_sampling_method == "Under Sampling")
    {
      trControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10,sampling ="down")
      print("under")
    }
    else
    {
      trControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10,sampling ="up")
      print("over")
      
    }
    
    model <- train(output ~ ., method='rf', data = training_set(), metric='Accuracy',preProcess = preProcess, trControl = trControl)
    
    # Make predictions on test set
    test_pred<-test_set()
    test_pred$pred <- predict(model, test_set())
    test_pred$factor_pred <- as.factor(test_pred$pred)
    test_pred$factor_truth <- as.factor(test_pred$output)
    # Compute performance metrics
    precision <- posPredValue(test_pred$factor_truth, test_pred$factor_pred)
    recall <- sensitivity(test_pred$factor_truth, test_pred$factor_pred)
    f1_score <- 2 * precision * recall / (precision + recall)
    roc_score<-roc(test_pred$output, predictor = factor(test_pred$pred, ordered = TRUE))
    auc <- roc_score$auc
    cm <- confusionMatrix(test_pred$pred, test_pred$output)
    accuracy <- cm$overall[1]
    confusion_matrix <- cm$table
    
    output$rf_tab_metrics<-renderTable({
      force.df <- as.data.frame(matrix(NA, nrow = 4, ncol = 1))
      rownames(force.df) = c("Precision", "Recall","F1_score", "AUC")
      force.df[1,1]<-precision
      force.df[2,1]<-recall
      force.df[3,1]<-f1_score
      force.df[4,1]<-auc
      force.df
    },rownames=TRUE, colnames=FALSE)
    
    output$rf_roc_plot<-renderPlot({
      plot(roc_score ,main ="ROC curve -- KNN")
    })
    
    output$rf_model<-renderPrint({
      model
    })
    output$rf_conmtrx<-renderTable({
      confusion_matrix
    })
    
    
    
  })
}


logestic_reg<-function(input,output,data)
{
  
  data <- read.csv("./heart.csv")
  
  # Convert the output column to a factor
  data$output <- as.factor(data$output)
  data_subset<-data
  
  sample2<-reactive({
    portion<-input$lr_training_portion
    sample(c(TRUE, FALSE), nrow(data_subset), replace=TRUE, prob=c(portion,1-portion))
  })
  training_set <- reactive({
    data_subset[sample2(),]
  })
  
  test_set <- reactive({
    data_subset[!(sample2()),]
  })
  
  
  # Define run button action
  observeEvent(input$lr_run_button, {
    
    
    # Train model
    preProcess <- c("center","scale")
    if(input$lr_sampling_method == "Under Sampling")
    {
      trControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10,sampling ="down")
      print("under")
    }
    else
    {
      trControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10,sampling ="up")
      print("over")
      
    }
    
    model <- train(output ~ ., method='plr', data = training_set(), metric='Accuracy',preProcess = preProcess, trControl = trControl)
    
    # Make predictions on test set
    test_pred<-test_set()
    test_pred$pred <- predict(model, test_set())
    test_pred$factor_pred <- as.factor(test_pred$pred)
    test_pred$factor_truth <- as.factor(test_pred$output)
    # Compute performance metrics
    precision <- posPredValue(test_pred$factor_truth, test_pred$factor_pred)
    recall <- sensitivity(test_pred$factor_truth, test_pred$factor_pred)
    f1_score <- 2 * precision * recall / (precision + recall)
    roc_score<-roc(test_pred$output, predictor = factor(test_pred$pred, ordered = TRUE))
    auc <- roc_score$auc
    cm <- confusionMatrix(test_pred$pred, test_pred$output)
    accuracy <- cm$overall[1]
    confusion_matrix <- cm$table
    
    output$lr_tab_metrics<-renderTable({
      force.df <- as.data.frame(matrix(NA, nrow = 4, ncol = 1))
      rownames(force.df) = c("Precision", "Recall","F1_score", "AUC")
      force.df[1,1]<-precision
      force.df[2,1]<-recall
      force.df[3,1]<-f1_score
      force.df[4,1]<-auc
      force.df
    },rownames=TRUE, colnames=FALSE)
    
    output$lr_roc_plot<-renderPlot({
      plot(roc_score ,main ="ROC curve -- KNN")
    })
    
    output$lr_model<-renderPrint({
      model
    })
    output$lr_conmtrx<-renderTable({
      confusion_matrix
    })
    
    
    
  })
}


knn_modle<-function(input,output,data)
{
  
  data <- read.csv("./heart.csv")
  
  # Convert the output column to a factor
  data$output <- as.factor(data$output)
  data_subset<-data
  
  sample2<-reactive({
    portion<-input$knn_training_portion
    sample(c(TRUE, FALSE), nrow(data_subset), replace=TRUE, prob=c(portion,1-portion))
    })
  training_set <- reactive({
    data_subset[sample2(),]
  })
  
  test_set <- reactive({
    data_subset[!(sample2()),]
  })

  
  # Define run button action
   observeEvent(input$knn_run_button, {
     
     
  # Train model
     preProcess <- c("center","scale")
     if(input$knn_sampling_method == "Under Sampling")
     {
       trControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10,sampling ="down")
       print("under")
     }
     else
     {
       trControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10,sampling ="up")
       print("over")
       
     }
     
     model <- train(output ~ ., method='knn', data = training_set(), metric='Accuracy',preProcess = preProcess, trControl = trControl)
     
  # Make predictions on test set
     test_pred<-test_set()
     test_pred$pred <- predict(model, test_set())
     test_pred$factor_pred <- as.factor(test_pred$pred)
     test_pred$factor_truth <- as.factor(test_pred$output)
  # Compute performance metrics
     precision <- posPredValue(test_pred$factor_truth, test_pred$factor_pred)
     recall <- sensitivity(test_pred$factor_truth, test_pred$factor_pred)
     f1_score <- 2 * precision * recall / (precision + recall)
     roc_score<-roc(test_pred$output, predictor = factor(test_pred$pred, ordered = TRUE))
     auc <- roc_score$auc
     cm <- confusionMatrix(test_pred$pred, test_pred$output)
     accuracy <- cm$overall[1]
     confusion_matrix <- cm$table

     #display results
     
     output$knn_tab_metrics<-renderTable({
       force.df <- as.data.frame(matrix(NA, nrow = 4, ncol = 1))
       rownames(force.df) = c("Precision", "Recall","F1_score", "AUC")
       force.df[1,1]<-precision
       force.df[2,1]<-recall
       force.df[3,1]<-f1_score
       force.df[4,1]<-auc
       force.df
     },rownames=TRUE, colnames=FALSE)
     
    output$knn_roc_plot<-renderPlot({
      plot(roc_score ,main ="ROC curve -- KNN")
    })
    
    output$knn_model<-renderPrint({
      model
    })
    output$knn_conmtrx<-renderTable({
      confusion_matrix
    })
    
    
     
  })
    
  
  
}

machine_learning<-function(input,output)
{
  random_forest(input,output,data)
  knn_modle(input,output,data)
  logestic_reg(input,output,data)
}