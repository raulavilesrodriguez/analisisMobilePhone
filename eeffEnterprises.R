library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(shiny)
library(shinyjs)
library(rsconnect)
library(DT)

#_____Read Super-Company files EEFF________

#---Methods----
desempaquetar <- function(paqueto){
  unzip(paqueto)
}

# Function to process the different data bases
procesamiento <- function(documento){
  data <- read.table(documento, sep = '\t', dec = ',', quote = "",
                            encoding="latin1", fill=TRUE)
  data <- data |> mutate_all(list(~str_replace(., "CUENTA_", "")))
  colnames(data) <- data[1,]
  # remove the last column NA
  data <- data[1:ncol(data)-1]
  data <- data |> mutate(across(.fns = function(x){
                                        str_replace(x, ',', '.')
                                }))
  data
}

# Function to process the different catalogues
catalogueProc <- function(documento){
  c <- read.table(documento, sep = '\t', dec = ',', quote = "",
                    encoding="latin1", fill=TRUE)
}

# Edit name of Columns
changeColnames <- function(base, catl, since){
  for(i in 1: nrow(catl)){
    if(!is.na(base[1, i+since]) & base[1, i+since]==catl[i,1]){
      print(catl[i,2])
      base[1, i+since] <- catl[i,2]
    } else{
      print('It is no in this Base')
    }
  }
  colnames(base) <- base[1,]
  base <- base[-1,]
  rownames(base) <- NULL # Resetting index numbers of rows
  base
}

# Function to filter 
filterTelecom <- function(df){
  df <- df |> 
    subset(CIIU %in% c('J6110.01', 
                       'J6110.02', 
                       'J6110.03', #
                       'J6110.04',
                       'J6120.01',
                       'J6120.02', #
                       'J6130.01',
                       'J6190.01', #
                       'J6190.02',
                       'J6190.03', #
                       'J6190.04',
                       'J6190.05'
    ))
  rownames(df) <- NULL # Resetting index numbers of rows
  df
}





