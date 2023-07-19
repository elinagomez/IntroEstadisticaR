
#------------------------------------------------------------------------------#
#               Introducción a la programación en R                            #                                    
#                         Clase 3                                              # 
#                      Ejercicios - Solución                                   #
#------------------------------------------------------------------------------#


## Trabajaremos con un dataframe con datos de económicos y políticos de Uruguay. 
# La base se llama "datauru" y esta en formato excel (.xlsx)

## 1. Importar dataframe "datauru" y asignarle el mismo nombre "datauru" 
library(tidyverse)
library(readxl)

datauru <- read_excel("data/datauru.xlsx")

## 2. Imprime "datauru" y fijate las variables del dataframe y su tipo
# (puedes ayudarte con el codebook que está en la carpeta "data")
print(datauru)



