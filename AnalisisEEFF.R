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
library(highcharter)

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

df_total <- eeff |> pivot_longer(cols = 4:5, names_to = "variables", values_to = "valores")
df_total |> ggplot(aes(log10(valores), fill = variables)) +
  geom_density(adjust=1.5, alpha=.25) +
  labs(x = "Costos-gastos e ingresos Actividades Telecomunicaciones (normalizado)")


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


# Results ALL telecom enterprise
mT <- mean(eeff$profitability)
mT
# sd costos y gastos
sT <- sd(eeff$profitability)
medianT <- median(eeff$profitability)
medianT
pnorm(medianT, mT, sT)

#the probability that a randomly selected enterprise wireless is highier than median_prof
1 - pnorm(medianT, mT, sT)

# ---Result wireless enterprise---
inalambrica <- eeff |>
  filter(CIIU == str_extract(eeff$CIIU, regex("J6120\\.\\d+", ignore_case = TRUE)))
we <- str_subset(eeff$CIIU, regex("J6120.\\d+", ignore_case = TRUE))


m <- mean(inalambrica$profitability)
m
s <- sd(inalambrica$profitability)
median_prof <- median(inalambrica$profitability)
pnorm(median_prof, m, s)

#the probability that a randomly selected wireless enterprise is highier than median_prof
1 - pnorm(median_prof, m, s)

df_i <- inalambrica |> pivot_longer(cols = 4:5, names_to = "variables", values_to = "valores")
df_i |> ggplot(aes(log10(valores), fill = variables)) +
  geom_density(adjust=1.5, alpha=.25) +
  labs(x = "Costos-gastos e ingresos Actividades Inalámbricas (normalizado)")

# Interctive plot
hchart(density(log10(inalambrica$ingresos)), 
       type = "area", color = "#FF0060", name = "Ingresos") |>
  hc_add_series(density(log10(inalambrica$costTotal)), type = "area", color = "#00DFA2", name = "costos") |>
  hc_add_theme(hc_theme_alone())


hchart(density(inalambrica$profitability), 
       type = "area", color = "#F90716", name = "Profitability") |>
  hc_add_theme(hc_theme_db())
