## ***************************************************************************
##. Clase 4 Introducción a la estadística con R        
##  Ejercicio  Parte 1                                      
## 
##  UMAD - 2023  
##
## Importar y exportar datos (repaso)
## ***************************************************************************


rm(list = ls())

library(tidyverse)
library(haven)
library(readxl)

data_sheet1 <- readxl::read_excel("data/datauru_sheets.xlsx", sheet = 1) 
data_sheet1 <- readxl::read_excel("data/datauru_sheets.xlsx", sheet = 2) 
saveRDS(data_sheet1, file = "resultados/sheet1")
haven::write_dta(data_sheet1, "resultados/sheet1")