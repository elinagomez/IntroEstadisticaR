#------------------------------------------------------------------------------#
#               Introducción a la programación en R                            #                                    
#                         Clase 2                                              # 
#                      Ejercicios - Solución                                   #
#------------------------------------------------------------------------------#


## 1. Crea un dataframe con tus cuatro canciones favoritas en orden (primer 
# fila canción favorita y así sucesivamente).
# Tres columnas: nombre, banda, año de publicación
mis_canciones_favoritas <- data.frame(nombre = c("Sugar Magnolia","Touch of grey", 
                                                 "Workingman blues´", "Antenas Rubias"),
                                      banda = c("Grateful Dead", "Grateful Dead",
                                                "Bob Dylan", "Buenos Muchachos"),
                                      year = c(1970, 1987, 2006, 2017))

## 2. Usando la indexación guarda en un objeto tu segunda canción favorita y 
# las bandas de tus  cuatro canciones favoritas
# Imprime ambos objetos
mi_segunda_cfavorita <- mis_canciones_favoritas[2, ]
print(mi_segunda_cfavorita)
mis_bandas <- mis_canciones_favoritas[ , 3]
print(mis_bandas)

## 3. Ahora vamos a trabajar con el siguiente dataframe con datos del Banco 
# Mundial para 2019:
paises_eco <- data.frame(
  pais = c("Argentina", "Brasil", "Chile", "Uruguay", "Gibraltar"),
  continente = c("América Latina", "América Latina", "América Latina",
                 "América Latina", "Europa"),
  desempleo = c(10.4, 12, 7.1, 8.8, NA),
  pbi_perc = c(9912, 8717, 14896, 16190, NA))

paises_eco

## 4. Cuál fue la tasa de desempleo en Chile en 2019? y el pbi per capita en
# Uruguay? (responde en código)
paises_eco[3, 3]
paises_eco[4, 4]

## 5. Cuál fue la media de desempleo para estos países en 2019? (si hay datos 
# faltantes excluir al país)
media_desempleo <- mean(paises_eco$desempleo, na.rm = TRUE)

## 6. Redondea ese resultado a un dígito después de la coma
media_desempleo <- round(media_desempleo, digits = 1)
print(media_desempleo)

## 7. Usando el dataframe "paises_eco" prueba que:

# A. Argentina y Chile están en el mismo continente y Uruguay y Gibraltar en distintos
paises_eco[1, 2] == paises_eco[2, 2]
paises_eco[4, 2] == paises_eco[5, 2]

# B. Que el desempleo en Brasil es mayor que en Argentina
paises_eco[2, 3] > paises_eco[1, 3]

# C. Que al menos uno de los países tiene una tasa de desempleo de 8.8
8.8 %in% paises_eco$desempleo

## 8. Por último, instala el paquete "dplyr", cargalo y usa la función "filter" para filtrar 
# "paises_eco" de modo que  solo queden los países de América Latina. 

# Utiliza help(filter) para entender como usar la función. Cuidado que el paquete "stats" también
# tiene una función "filter", por lo que cuando uses help(filter), selecciona la función del
# paquete "dplyr". También puedes buscar directamente en un navegador
install.packages("dplyr")
library(dplyr)
help(filter)
paises_eco <- filter(paises_eco, continente == "América Latina")
paises_eco















