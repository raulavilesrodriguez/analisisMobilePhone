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

#----Read Financial statements (eeff tibble)-------
eeff <- read_excel('eeff.xlsx')

eeff <- eeff |> select(RUC, NOMBRE, CIIU, ingresos, costTotal) |>
  filter(costTotal !=0) |> arrange(desc(costTotal))

#Plot bars
eeff |> select(NOMBRE, costTotal) |> filter(costTotal !=0) |> arrange(desc(costTotal)) |>
  slice_max(costTotal, n = 15) |> mutate(NOMBRE = (reorder(NOMBRE, - costTotal))) |>
  ggplot(aes(NOMBRE, costTotal)) + geom_col(position = "dodge", width=0.9, fill="#0E8388", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Costos y Gastos") + xlab("")

#plot density
eeff |> select(costTotal) |> arrange(desc(costTotal)) |>
  ggplot(aes(log10(costTotal))) + geom_density(fill="#94AF9F") +
  labs(x = "Costos y gastos telco (normalizado)")

# mean costos y gastos
mean(eeff$costTotal)
# sd costos y gastos
sd(eeff$costTotal)

# sma telecom service
sma <- read_xlsx("sma_2022.xlsx")
eeff <- eeff |> left_join(sma, by = 'NOMBRE') 

eeff <- eeff |> 
  mutate(
    profitability = round((ingresos/costTotal) -1, 3),
    costoUsuario = costTotal/(sma_con*12),
    incomeUsuario = ingresos / (sma_con*12),
    profUsuario = round((incomeUsuario/costoUsuario) -1, 3)
  )

inalambrica <- eeff |>
  filter(CIIU == str_extract(eeff$CIIU, regex("J6120\\.\\d+", ignore_case = TRUE)))
we <- str_subset(eeff$CIIU, regex("J6120.\\d+", ignore_case = TRUE))

# Result
mean(inalambrica$profitability)

