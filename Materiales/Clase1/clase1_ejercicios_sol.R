#------------------------------------------------------------------------------#
#               Introducción a la programación en R                            #                                    
#                         Clase 1                                              # 
#                      Ejercicios - Solución                                   #
#------------------------------------------------------------------------------#


## 1. Crea un objeto con el año de tu nacimiento y suma tu edad
nacimiento <- 1996
nacimiento + 26

## 2. Borra el objeto
rm(nacimiento)

## 3. Crea un vector con el apellido de los últimos 5 presidentes de tu país
presidentes <- c("Batlle", "Vázquez", "Mujica", "Vázquez", "Lacalle Pou")

## 4. Crea un vector los números enteros que van del 0 al 35 y otro con los 
# números de 0 a 100  que son múltiplos de 5
numeros_35 <- 0:35
numeros_100 <- seq(0, 100, 5)

## 5. Averigua el tipo de los vectores y comprueba que sean los correctos
class(presidentes) # El vector de los presidentes debe ser de tipo "character"
class(numeros_35) # El vector de 0 a 35 debe ser numérico (integer o double)
class(numeros_100) # El vector de 0 a 100 debe ser numérico (integer o double)
rm(list=ls())







