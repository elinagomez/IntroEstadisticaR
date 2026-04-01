## * 5.4. Crear una función ---- 

# Supongamos que tenemos un dataframe con tres variables: pais, vacas y personas 
data <- data.frame(pais = c("Uruguay", "Argentina", "Brasil", "Mexico"),
                   humanos = c(3.4, 43.8, 209.5, 128.6),
                   vacas = c(11800, 53500, 22600, 16500))
data

# Ahora quiero calcular la cantidad de vacas per capita. Podría hacer:
data$vacas_pc <- (data$vacas / 1000) / data$humanos 
data

# Ahora me gustaría tener una tabla un poco más prolija: números redondeados y "per"
data$vacas_pc <- round(data$vacas_pc, digits = 1)
data$vacas_pc <- paste(data$vacas_pc, "per", sep = " ")
data

# Ok, lo logramos. Pero necesitar calcular más tablas:
data_2 <- data.frame(pais = c("Uruguay", "Nueva Zelanda", "Australia", "Japón"),
                     humanos = c(3.4, 4.5, 43.8, 126.3),
                     vacas = c(11800, 9900, 53500, 3800))
data_2

# Tendría que copiar y pegar varias veces el mismo código, cambiando el nombre 
# de los objetos. En este tipo de casos es muy util crear nuestra propia 
# función, para resumir este conjunto  de operaciones:

calc_vacas <- function(x, y){ 
  vacas_pc <- (x / 1000) / y   # Calculo la proporción de x / 1000 sobre y
  vacas_pc_1 <- round(vacas_pc, digits = 2) # Redondeo
  vacas_pc_2 <- paste(vacas_pc_1, "per", sep = " ")
  return(vacas_pc_2)
}

data_2$vacas_pc <- calc_vacas(x = data_2$vacas, y = data_2$humanos)
data_2


## * 5.5. Errores ---- 
vector_ej <- rnorm(n = 10, mean = 10, sd = 5) # Creo valores aleatorios
mean(Vector_ej) # Aplico función para obtener la media

# No funciona porque el nombre del objeto está mal escrito
mean(vector_ej) # Aplico función para obtener la media

## * 5.6. Advertencias ----
vector_1 <- c("10", "35%", "35", "50") # Vector de caracteres que contiene números 
vector_1

vector_2 <- as.numeric(vector_1) # Transformo a vector númerico
vector_2 # Los valores que además del número tenían (%) no pueden pasarse a númericos

vector_1 <- gsub("%", "", vector_1) # Quito los % del vector original y evito la advertencia
vector_1

vector_2 <- as.numeric(vector_1) # Transformo a vector númerico
vector_2 # Los valores que además del número tenían (%) no pueden pasarse a númericos

