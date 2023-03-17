library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(zoo)
library(DT)
library(ROSE)
library(lattice)
library(caret)
library(naniar)

source('./display.R')
source('./data_exp.R')
source('./read.R')
source('./missing_val.R')
source('./outliers.R')
source('./normalisation.R')
source('./class_balance.R')


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
  }
)