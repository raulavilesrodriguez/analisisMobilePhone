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
library(here)
library(readxl)
library(stringr)

source(here::here('eeffEnterprises.R'))

#------Process the different BD using the methods of eeffEnterprises.R------

# Unzip de EEFF of Ecuador companies
eeff <- "estadosFinancieros_2022.zip"
desempaquetar(eeff)

# First Data Base
balances_1 <- "balances_2022_1.txt"
resultado_1 <- procesamiento(balances_1)
catalogo_1 <- "catalogo_2022_1.txt"
c_1 <- catalogueProc(catalogo_1)
resultado_1 <- changeColnames(resultado_1, c_1, 7)
resultado_1 <- filterTelecom(resultado_1)


# Second Data Base
balances_2 <- "balances_2022_2.txt"
resultado_2 <- procesamiento(balances_2)
catalogo_2 <- "catalogo_2022_2.txt"
c_2 <- catalogueProc(catalogo_2)
resultado_2 <- changeColnames(resultado_2, c_2, 7)
resultado_2 <- filterTelecom(resultado_2)

# Third Data Base
balances_3 <- "balances_2022_3.txt"
resultado_3 <- procesamiento(balances_3)
catalogo_3 <- "catalogo_2022_3.txt"
c_3 <- catalogueProc(catalogo_3)
resultado_3 <- changeColnames(resultado_3, c_3, 7)
resultado_3 <- filterTelecom(resultado_3)


resultado_1[,8:ncol(resultado_1)] <- resultado_1 |> select(c(8:ncol(resultado_1))) |>
  mutate_if(is.character, as.numeric)

eeff <- resultado_1 |> select(RUC, NOMBRE, CIIU,
                              `COSTO DE VENTAS Y PRODUCCIÓN`,
                              GASTOS, `INGRESOS DE ACTIVIDADES ORDINARIAS`) |>
  mutate(costTotal = `COSTO DE VENTAS Y PRODUCCIÓN` + GASTOS,
         ingresos = `INGRESOS DE ACTIVIDADES ORDINARIAS`
         )

#Export tibble Financial statements
writexl::write_xlsx(eeff, 'eeff.xlsx')








