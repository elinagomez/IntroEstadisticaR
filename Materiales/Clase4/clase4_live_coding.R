
## ***************************************************************************
## Clase 4: Transformación y descripción de datos         
##  Código de la presentación                                      
##  Introducción a la estadística con R
##                                          
## ***************************************************************************



##  1. Importar datos con etiquetas  =======================================

## 1.1. Cargar base de datos ----
data <- haven::read_stata("data/ej_encuesta.dta")
head(data, 5) # ver el tipo de variables

# 1.2. Pasar las variables a factores ----
data %>% 
  dplyr::mutate(P1 = haven::as_factor(P1),
                P14 = haven::as_factor(P14)) %>% 
  head()


# Podemos chequear y coercionar factores
data_gapminder <- gapminder
is.factor(data_gapminder$continent) # Chequeo si es factor

levels(data_gapminder$continent) # Chequeo los niveles

# Transformo a caracter
data_gapminder$continent <- as.character(data_gapminder$continent) 
class(data_gapminder$continent)

# De vuelta a factor
data_gapminder$continent <- as.factor(data_gapminder$continent)

class(data_gapminder$continent)

# Para crear un factor usamos la función factor()
paises_mercosur <- factor(c("Argentina", "Brasil", "Paraguay", "Uruguay"))
table(paises_mercosur)

# La función fct_relevel() nos permite reordenar los niveles del factor
paises_mercosur <- fct_relevel(paises_mercosur, "Uruguay")
table(paises_mercosur)


#### Data frames: tibbles


data_gapminder <- (gapminder)
class(data_gapminder) # Ya es un tibble
data_gapminder <- as.data.frame(data_gapminder)
class(data_gapminder) # Ahora solamente dataframe

print(data_gapminder)

data_gapminder <- as_tibble(data_gapminder) # Pasamos nuevamente a tibble
class(data_gapminder)



# R tiene un visor para datos. Pueden dar click 
#en el dataframe en el ambiente o:
view(data_gapminder)

dim(data_gapminder) # Número de filas y columnas

names(data_gapminder) # Nombre de variables

head(data_gapminder, 3) # Imprime primeras filas (3 en este caso)

# Resumen más completo:
glimpse(gapminder)


##  2. Filtrar observaciones   ==============================================
library(gapminder)

data_gap <- gapminder

## Una de las transformaciones más frecuentes cuando manipulamos datos 
# Tenemos datos de muchos años:
table(d_gap$year)

# Filtremos para con los datos a partir de 2007
gapminder_07 <- filter(d_gap, year == 2007)
table(gapminder_07$year)

# Todas los años menos 2007
gapminder_pre07 <- filter(d_gap, year != 2007)
table(gapminder_pre07$year)

# Solo los siguientes años: 1952, 1992 y 2007
anios_especificos <- c(1952, 1992, 2007)
gapminder_esp <- filter(d_gap, year %in% anios_especificos)
table(gapminder_esp$year)

# O podría hacer
gapminder_esp <- filter(d_gap, year %in% c(1952, 1992, 2007))
table(gapminder_esp$year)



##  3. Seleccionar variables (columnas)  ====================================

d_gap <- gapminder # cargo la base de vuelta
colnames(d_gap)

# Selccionar un conjunto de variables (país, año, población)
select(d_gap, country, year, pop)

# Selccionar todas las variables menos las especificadas
select(d_gap, -continent)

# Seleccionar un rango de variables según orden
select(d_gap, country:lifeExp)
select(d_gap, 1:3) # Orden numérico



##  4. Pipeline   ===========================================================

# Supongamos que queremos filtrar la base para quedarnos con las observaciones
# del año 2007 y del continente americano. Y desoués quedemos borrar la variable
# continente porque ya no nos sirve (todos los valores son "Americas") y ordenar
# de mayor a menor según expectativa de vida

# Con el pipeline podemos hacer todo esto especificando una sola vez el 
# dataframe al comienzo. Cada %>% se lee como un "y entonces". R interpreta
# las distintas funciones en orden
gapminder_07_america <- d_gap %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-continent) %>% 
  arrange(desc(lifeExp))

print(gapminder_07_america)



##  5. Crear y recodificar variables   ======================================

rm(list=ls())

## 5.1.  Crear variables con mutate() de dplyr ----

d_gap <- gapminder
head(d_gap, 3)

# Primero veamos cómo crear una constante
d_gap <- mutate(d_gap, var1 = "Valor fijo") # Variable de caracteres
d_gap <- mutate(d_gap, var2 = 7) # Variable numérica
head(d_gap, 3)

# También podemos crear ambas variables dentro de la misma línea 
d_gap <- gapminder
d_gap <- mutate(d_gap, 
                var1 = "Valor fijo",
                var2 = 7)
head(d_gap, 3)

# Podríamos haber hecho lo mismo con R Base
d_gap <- gapminder
d_gap$var1 <- "Valor fijo"
d_gap$var2 <- 7
head(d_gap, 3)

# Con mutate() también podemos hacer operaciones sobre variables ya existentes
# Calculemos el pbi total (pbi per capita * población)
d_gap <- mutate(gapminder, gdp = gdpPercap * pop)
head(d_gap, 3)

# Podemos calcular el logaritmo
d_gap <- mutate(d_gap, gdp_log = log(gdp))
head(d_gap, 3)


## 5.2 Transformar tipo de datos de variables 

# Exploro tipo de variables
glimpse(d_gap)

# Variable continente a caracteres y año a factor
d_gap <- d_gap %>% 
  mutate(continent = as.character(continent),
         year = as.factor(year))

# Exploro tipo de variables
glimpse(d_gap)

# Variable año a numérica nuevamente
d_gap <- d_gap %>% 
  mutate(year = as.numeric(year))

# Exploro tipo de variables
glimpse(d_gap)



## 6. Recodificaciones condicionales =========================================

rm(list=ls())


## 6.1. Recodificación con case_when() ----
d_gap <- gapminder

# Creemos una variable que indique si el país es Uruguay o no
d_gap <- mutate(d_gap, uruono = case_when(country == "Uruguay" ~ "Si",
                                          TRUE ~ "No"))

table(d_gap$uruono)

## Con case_when() podemos establecer varias condiciones fácilmente
d_gap <- mutate(d_gap, mercosur = case_when(country == "Uruguay" ~ 1,
                                            country == "Argentina" ~ 1,
                                            country == "Paraguay" ~ 1,
                                            country == "Brazil" ~ 1,
                                            TRUE ~ 0))
table(d_gap$mercosur)

# También podemos usar operadores para simplificar
d_gap <- mutate(d_gap, mercosur_2 = case_when(
  country %in% c("Argentina", "Paraguay", "Brazil", "Uruguay") ~ 1,
  TRUE ~ 0)
) 

d_gap <- mutate(d_gap, mercosur_3 = case_when(
  country == "Argentina" | country == "Paraguay" | 
    country == "Brazil" | country == "Uruguay" ~ 1,
  TRUE ~ 0)
)

identical(d_gap$mercosur, d_gap$mercosur_2)
identical(d_gap$mercosur_2, d_gap$mercosur_3)

# Recodificaciones numéricas
# Supongamos que queremos crear una nueva variable pob_rec, que clasifica a 
# los países en población grande (más de 20 millones), mediana (entre 5 y 20) 
# o pequeña (menos de 5)
d_gap <- d_gap %>% 
  mutate(pob_rec = case_when(
    pop >= 20000000 ~ "Grande",
    pop >= 5000000 & pop < 20000000 ~ "Mediana",
    pop < 5000000 ~ "Pequeña",
    TRUE ~ "Error")
  ) 

table(d_gap$pob_rec)

# También puedo crear variables en función a dos variables 
d_gap <- mutate(d_gap, var1 = case_when(gdpPercap > 20000 ~ 1,
                                        lifeExp > 75 ~ 1,
                                        TRUE ~ 0))
table(d_gap$var1)



##  7. Explorar datos  ======================================================

# R tiene un visor para datos. Pueden clickear en el dataframe en el ambiente o:
View(d_gap)
dim(d_gap) # Número de filas y columnas
names(d_gap) # Nombre de variables
head(d_gap, 3) # Imprime primeras filas (3 en este caso)
glimpse(d_gap) # Recomiendo utilizar esta función


# Para obtener una tabla de frecuencias de una variable usamos la función 
# table() de R Base
table(d_gap$continent) # Frecuencia simple

tabla_1 <- table(d_gap$continent) # Frecuencia simple

prop.table(tabla_1) # Proporciones

addmargins(tabla_1) # Totales

addmargins(prop.table(tabla_1)) # Proporciones y totales



## 8. Estadisticos descriptivos ============================================

# R cuenta también con funciones para obtener estadísticos descriptivos
mean(d_gap$lifeExp) # Media
median(d_gap$lifeExp) # Mediana
sd(d_gap$lifeExp) # Desvío estandar
range(d_gap$lifeExp) # Rango
max(d_gap$lifeExp) # Minimo
min(d_gap$lifeExp) # Maximo


# También podemos crear un histograma muy fácilmente
hist(d_gap$lifeExp,
     main = "Distribución de expectativa de vida (Gapminder)")

# Gráfico de dispersión (scatterplot)
plot(d_gap$lifeExp, d_gap$gdpPercap,
     main = "Relación entre expectativa de vida y PBI per cápita")


## 9. Resumir datos  ====================================================

# Frecuencia simple
d_gap %>% 
  count(continent)

# Frecuencia proporción
d_gap %>% 
  count(continent) %>% 
  mutate(per = n/sum(n))

# Frecuencia proporción
d_gap %>% 
  count(continent) %>% 
  mutate(per = n/sum(n)*100)

# Tabla cruzada en formato largo
d_gap %>% 
  count(continent, pob_rec)

# Tabla cruzada en formato ancho
d_gap %>% 
  count(continent, pob_rec) %>% 
  spread(pob_rec, n)

# Porcentaje total
d_gap %>% 
  count(continent, pob_rec) %>%
  mutate(n = n/sum(n)*100) %>% 
  spread(pob_rec, n)

# % de paises en cada tamaño de población por continente 
d_gap %>% 
  count(continent, pob_rec) %>%
  mutate(n = n/sum(n)*100, .by = continent) %>% 
  spread(pob_rec, n)

# % de continente por tamaño de población  
d_gap %>% 
  count(continent, pob_rec) %>%
  mutate(n = n/sum(n)*100, .by = pob_rec) %>% 
  spread(pob_rec, n)

## Resumen con la media de lifeExp
gapminder %>% 
  summarise(media = mean(lifeExp, na.rm=T))

# Por ahora no hay mucha diferencia con
mean(gapminder$lifeExp, na.rm = TRUE)

## Pero, con group_by() podemos crear grupos para nuestros resumenes
gapminder %>% 
  group_by(year) %>% 
  summarise(media = mean(lifeExp, na.rm = T))

## Como con la mayoría de las funciones de dplyr, nos devuelve un dataframe, 
# que podemos guardar en un objeto
gap_resumen <- gapminder %>% 
  group_by(year) %>% 
  summarise(media = mean(lifeExp, na.rm = T))

## Podemos agrupar por dos o más variables si queremos también
# Por ejemplo, calculemos la media de expectativa de vida por año comparando
# America y Europa para 1997, 2002 y 2007:
resumen_1 <- gapminder %>% 
  filter(continent %in% c("Americas", "Europe")) %>% 
  filter(year >= 1997) %>% 
  group_by(continent, year) %>% 
  summarise(media = mean(lifeExp, na.rm = TRUE))

# Podemos generar varios resumenes son summarise(), que son variables del 
# dataframe que devuelve
resumen_2 <- gapminder %>% 
  filter(continent %in% c("Americas", "Europe")) %>%
  filter(year == 2007) %>%
  group_by(continent) %>% 
  summarise(media = mean(gdpPercap),
            desvio = sd(gdpPercap),
            suma = sum(gdpPercap),
            max = max(gdpPercap),
            min = min(gdpPercap),
            paises = n()) 
resumen_2

# Es muy flexible por ejemplo podemos hacer operaciones dentro de las variables
# por ejemplo sumarle el número 5 a la variable de la media
gapminder %>% 
  filter(continent %in% c("Americas", "Europe")) %>%
  filter(year == 2007) %>%
  group_by(continent) %>% 
  summarise(media = mean(lifeExp),
            media_5 = mean(lifeExp) + 5) 


## Por último, para exportar nuestros resultados volvemos a usar write_xlsx()
# Por ejemplo exportemos resultados_2
library(writexl)
writexl::write_xlsx(resumen_2, "resultados/mi_primera_tabla.xlsx")






















