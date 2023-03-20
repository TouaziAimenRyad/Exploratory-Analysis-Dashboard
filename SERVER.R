library(shiny)
library(magrittr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(zoo)
library(DT)
library(ROSE)
library(lattice)
library(caret)
library(naniar)
library(fastDummies)
library(stats)
library(mvShapiroTest)
library(nortest)
library(mvnormtest)
library(Hmisc)
library(UsingR)
library(pROC)
library(cluster)
source('./display.R')
source('./data_exp.R')
source('./ana_univar.R')
source('./ana_bivar.R')
source('./test.R')
source('./read.R')
source('./missing_val.R')
source('./outliers.R')
source('./normalisation.R')
source('./class_balance.R')
source('./dummification.R')

source('./machine_learning.R')

# add a function similar to init data view to keep a display at hand of your data



# xls file not installed library 
# what i need to do in missing values !!!!!!!!!!! 
# uioutput plots and missing valplots to be displayed after each action 
# add an all column choice to handle all of the cols at once

# what do i need to do outliers
# make a conditon to wait until handling missing values 
# add an all cols choice 
# class blancing not done 
#class balancing not working 
# today you must finish data exp and stat tests and seperate functions into files  and tommorpw do analysis and put plots
#add dummification it's important for corr on quantitave
#make the app more robust especially in data exploration 
# when loading the app functions are getting called twice don't know why and data is null hence add a condition
# coorelation and data exploration  shouldnt' be done right  after the upload
#add a reset btn to all data prepartion
#change the method of habdeling outliers
#plot output not that goo especialy in normalisation
#action button don't reset it's problem
#condition you must handle missing values first before skipping to outliers and other stuff
#your using if not null data do plots and stuff you need to add an else
#fix labls on plots using this xlab = sym(input$univar_quant_var)
#small loading problem with scatterplot histogrma in bivariate
shinyServer(
  function(input,output)
  {
    initial_data<-reactiveVal(NULL)
    data <- reactiveVal(NULL)
    df_outliers<-reactiveVal(NULL)
    Variable_outliers<-reactiveVal(NULL)
    nb_outliers<-reactiveVal(0)
    nb_missing<-reactiveVal(0)
    #uploading files
    # any action of chosing the seprator or header will reupload the file and reset our data set 
    reader(input,output,initial_data,data)
    
    
    ########################################################################################"
    # data visualisation for missing values is comman
    handle_missing_values(input,output,data)
    ###############################################################################        
    handle_outliers(input,output,data)
    ###############################################################################
    normalisation(input,output,data)
    ##############################################################################"
    class_rebalance(input,output,data)
    dummification(input,output,data)
    rename_var(input,output,data)
    machine_learning(input,output)
  }
)