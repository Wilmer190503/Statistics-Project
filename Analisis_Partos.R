# Instalar y cargar paquetes necesarios
if (!require("httr")) install.packages("httr")
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("skimr")) install.packages("skimr")
if (!require("skimr")) install.packages("tidyr")
if (!require("sf")) install.packages("sf")
library(httr)
library(readr)
library(dplyr)
library(ggplot2)
library(skimr)
library(tidyr)
library(sf)

##########################################################################
################### LEER DATOS DIRECTAMENTE DE GITHUB ####################
##########################################################################

# URL del archivo CSV en GitHub (versión "raw")
url <- 'https://raw.githubusercontent.com/Cvaslml/Statistics-Project/main/Partos.csv'

# Descargar y leer el archivo CSV
aux <- read.csv(url, na.strings = c("N/A", "NA", "----"), header = FALSE)

##########################################################################
###################     AJUSTE DE LAS FILAS   ############################
##########################################################################

# La primera fila contiene las descripciones, así que la asignamos como nombres de columnas
colnames(aux) <- aux[1, ]

# Eliminar la primera fila
aux <- aux[-1, ]

# La segunda fila contiene los conteos totales, entonces la guardamos por separado
conteos_totales <- aux[1, ]

# Eliminar la segunda fila, ya que ahora tenemos los conteos por separado
aux <- aux[-1, ]

##########################################################################
###################     ANALYSIS DE LOS DATOS     ########################
##########################################################################

# Mirar el inicio y final de la tabla
head(aux)
tail(aux)
View(aux)

# Ver los nombres de las columnas
names(aux)

# Ver el tipo de clase de una columna específica (Ejemplo: Municipio)
class(aux$Municipio)

# Ver la longitud de la columna Municipio
length(aux$Municipio)

# Ver los departamentos únicos
unique(aux$Departamento)

# Comprobar si hay valores faltantes
is.na(aux)
sum(is.na(aux))

# Resumen estadístico general
summary(aux)

# Resumen estadístico de una columna específica (Ejemplo: Número de Atenciones en 2017)
summary(aux$Número.de.Atenciones.2017)

# Resumen con el paquete skimr
skim(aux)


# Detectar maximos atipicos
which.max(aux$Número.de.Atenciones.2017)

##########################################################################
###################     LIMPIEZA DE DATOS Y CONVERSIÓN DE TIPOS     ######
##########################################################################

# Limpiar los nombres de las columnas para eliminar espacios extra y caracteres especiales
colnames(aux) <- gsub(" ", ".", colnames(aux))  
colnames(aux) <- gsub(",", ".", colnames(aux))  

# Verificar los nombres de las columnas
print(names(aux))

# Convertir las columnas relevantes a tipo numérico
aux$Número.de.Atenciones.2017 <- as.numeric(as.character(aux$Número.de.Atenciones.2017))
aux$Número.de.Atenciones.2018 <- as.numeric(as.character(aux$Número.de.Atenciones.2018))
aux$Número.de.Atenciones.2019 <- as.numeric(as.character(aux$Número.de.Atenciones.2019))

aux$Número.de.Personas.Atendidas.2017 <- as.numeric(as.character(aux$Número.de.Personas.Atendidas.2017))
aux$Número.de.Personas.Atendidas.2018 <- as.numeric(as.character(aux$Número.de.Personas.Atendidas.2018))
aux$Número.de.Personas.Atendidas.2019 <- as.numeric(as.character(aux$Número.de.Personas.Atendidas.2019))

# Ver si las conversiones generaron NA no deseados
sum(is.na(aux$Número.de.Atenciones.2017))  # Verificar los NA
sum(is.na(aux$Número.de.Atenciones.2018))
sum(is.na(aux$Número.de.Atenciones.2019))

# Verificar la estructura de las columnas
str(aux)

##########################################################################
###################     AGRUPAR POR DEPARTAMENTO Y SUMAR     #############
##########################################################################

# Agrupar por Departamento y hacer la sumatoria de las columnas numéricas
aux_departamento <- aux %>%
  dplyr::group_by(Departamento) %>%
  dplyr::summarise(
    `Número.de.Atenciones.2017` = sum(`Número.de.Atenciones.2017`, na.rm = TRUE),
    `Número.de.Atenciones.2018` = sum(`Número.de.Atenciones.2018`, na.rm = TRUE),
    `Número.de.Atenciones.2019` = sum(`Número.de.Atenciones.2019`, na.rm = TRUE),
    `Número.de.Personas.Atendidas.2017` = sum(`Número.de.Personas.Atendidas.2017`, na.rm = TRUE),
    `Número.de.Personas.Atendidas.2018` = sum(`Número.de.Personas.Atendidas.2018`, na.rm = TRUE),
    `Número.de.Personas.Atendidas.2019` = sum(`Número.de.Personas.Atendidas.2019`, na.rm = TRUE)
  )

# Eliminar filas con valores vacíos en la columna 'Departamento'(No arrojan información alguna)
aux_departamento <- aux_departamento %>%
  filter(Departamento != "")

# Eliminar la segunda fila (Son los totales)
aux_departamento <- aux_departamento[-2, ]

# Mostrar el dataframe final agrupados por Departamento
head(aux_departamento)
View(aux_departamento)


##########################################################################
###################     SOME PLOTS     ###################################
##########################################################################

# Función para crear gráfico # de atenciones 
create_barplot <- function(data, year_column, title) {
  barplot_heights <- barplot(data[[year_column]], 
                             names.arg = data$Departamento,
                             horiz = FALSE,  # Gráfico vertical
                             las = 2,  # Rotar las etiquetas del eje X
                             main = title,  # Título del gráfico
                             ylab = "Número de Atenciones",  # Etiqueta del eje Y
                             col = "skyblue",  # Color de las barras
                             border = "blue",  # Color de los bordes
                             cex.names = 0.5)  # Ajuste del tamaño
  
  # Agregar los valores encima de las barras para todos los departamentos
  text(x = barplot_heights, 
       y = data[[year_column]], 
       labels = data[[year_column]], 
       pos = 3,  # Colocar el valor encima
       cex = 0.8,  # Ajuste del tamaño del texto
       col = "black")  # Color del texto
  
  # Identificar la posición de "Norte de Santander" y agregar su valor dentro de la barra
  norte_santander_index <- which(data$Departamento == "Norte de Santander")
  
  if (length(norte_santander_index) > 0) {
    text(x = barplot_heights[norte_santander_index], 
         y = data[[year_column]][norte_santander_index] / 2,  # Posición dentro de la barra
         labels = data[[year_column]][norte_santander_index], 
         cex = 0.8, 
         col = "black")  # Color del texto dentro de la barra
  }
}

# Gráfico para 2017
create_barplot(aux_departamento, "Número.de.Atenciones.2017", "Número de Atenciones por Departamento en 2017")

# Gráfico para 2018
create_barplot(aux_departamento, "Número.de.Atenciones.2018", "Número de Atenciones por Departamento en 2018")

# Gráfico para 2019
create_barplot(aux_departamento, "Número.de.Atenciones.2019", "Número de Atenciones por Departamento en 2019")


# Función para crear gráfico para el # de personas atendidas
create_barplot_personas <- function(data, year_column, title) {
  barplot_heights <- barplot(data[[year_column]], 
                             names.arg = data$Departamento,
                             horiz = FALSE,  # Gráfico vertical
                             las = 2,  # Rotar las etiquetas del eje X
                             main = title,  # Título del gráfico
                             ylab = "Número de Personas Atendidas",  # Etiqueta del eje Y
                             col = "lightgreen",  # Color de las barras
                             border = "darkgreen",  # Color de los bordes
                             cex.names = 0.5)  # Ajuste del tamaño
  
  # Agregar los valores encima de las barras para todos los departamentos
  text(x = barplot_heights, 
       y = data[[year_column]], 
       labels = data[[year_column]], 
       pos = 3,  # Colocar el valor encima
       cex = 0.8,  # Ajuste del tamaño del texto
       col = "black")  # Color del texto
  
  # Identificar la posición de "Norte de Santander" y agregar su valor dentro de la barra
  norte_santander_index <- which(data$Departamento == "Norte de Santander")
  
  if (length(norte_santander_index) > 0) {
    text(x = barplot_heights[norte_santander_index], 
         y = data[[year_column]][norte_santander_index] / 2,  # Posición dentro de la barra
         labels = data[[year_column]][norte_santander_index], 
         cex = 0.8, 
         col = "black")  # Color del texto dentro de la barra
  }
}

# Gráfico para el número de personas atendidas en 2017
create_barplot_personas(aux_departamento, "Número.de.Personas.Atendidas.2017", "Número de Personas Atendidas por Departamento en 2017")

# Gráfico para el número de personas atendidas en 2018
create_barplot_personas(aux_departamento, "Número.de.Personas.Atendidas.2018", "Número de Personas Atendidas por Departamento en 2018")

# Gráfico para el número de personas atendidas en 2019
barplot_heights_2019 <- barplot(aux_departamento$`Número.de.Personas.Atendidas.2019`, 
                                names.arg = aux_departamento$Departamento,
                                horiz = FALSE,  # Gráfico vertical
                                las = 2,  # Rotar las etiquetas del eje X
                                main = "Número de Personas Atendidas por Departamento en 2019",  # Título del gráfico
                                ylab = "Número de Personas Atendidas",  # Etiqueta del eje Y
                                col = "lightgreen",  # Color de las barras
                                border = "darkgreen",  # Color de los bordes
                                cex.names = 0.5)  # Ajuste del tamaño

# Agregar los valores encima de las barras para todos los departamentos en 2019
text(x = barplot_heights_2019, 
     y = aux_departamento$`Número.de.Personas.Atendidas.2019`, 
     labels = aux_departamento$`Número.de.Personas.Atendidas.2019`, 
     pos = 3,  # Colocar el valor encima de las barras
     cex = 0.8,  # Ajuste del tamaño del texto
     col = "black")  # Color del texto

# Identificar la posición de "Bogotá, D.C." y agregar su valor dentro de la barra para 2019
bogota_dc_index_2019 <- which(aux_departamento$Departamento == "Bogotá, D.C.")

if (length(bogota_dc_index_2019) > 0) {
  text(x = barplot_heights_2019[bogota_dc_index_2019], 
       y = aux_departamento$`Número.de.Personas.Atendidas.2019`[bogota_dc_index_2019] / 2,  # Posición dentro de la barra
       labels = aux_departamento$`Número.de.Personas.Atendidas.2019`[bogota_dc_index_2019],  # Colocar el valor de Bogotá D.C. dentro de la barra
       cex = 0.8, 
       col = "black")  # Color del texto dentro de la barra
}

# Función para crear un histograma de Atenciones
histograma_atenciones <- function(data, variable, año, color_barras, color_borde) {
  hist(data[[variable]], 
       main = paste("Distribución de Atenciones en", año),  # Título del gráfico
       xlab = "Número de Atenciones",  # Etiqueta del eje X
       ylab = "Frecuencia",  # Etiqueta del eje Y
       col = color_barras,  # Color de las barras
       border = color_borde,  # Color del borde de las barras
       breaks = 15,  # Número de intervalos (bins)
       col.main = "darkblue",  # Color del título (azul oscuro)
       col.lab = "black",  # Color de las etiquetas (negro)
       cex.main = 1.2,  # Tamaño del título
       cex.lab = 1,  # Tamaño de las etiquetas
       cex.axis = 0.9)  # Tamaño de los números de los ejes
}

# Función para crear un histograma de Personas Atendidas
histograma_personas_atendidas <- function(data, variable, año, color_barras, color_borde) {
  hist(data[[variable]], 
       main = paste("Distribución de Personas Atendidas en", año),  # Título del gráfico
       xlab = "Número de Personas Atendidas",  # Etiqueta del eje X
       ylab = "Frecuencia",  # Etiqueta del eje Y
       col = color_barras,  # Color de las barras
       border = color_borde,  # Color del borde de las barras
       breaks = 15,  # Número de intervalos (bins)
       col.main = "darkblue",  # Color del título (azul oscuro)
       col.lab = "black",  # Color de las etiquetas (negro)
       cex.main = 1.2,  # Tamaño del título
       cex.lab = 1,  # Tamaño de las etiquetas
       cex.axis = 0.9)  # Tamaño de los números de los ejes
}

# Crear histogramas para el número de atenciones en 2017, 2018 y 2019
histograma_atenciones(aux_departamento, "Número.de.Atenciones.2017", 2017, "lightblue", "black")
histograma_atenciones(aux_departamento, "Número.de.Atenciones.2018", 2018, "lightcoral", "darkred")
histograma_atenciones(aux_departamento, "Número.de.Atenciones.2019", 2019, "lightgreen", "darkgreen")

# Crear histogramas para el número de personas atendidas en 2017, 2018 y 2019
histograma_personas_atendidas(aux_departamento, "Número.de.Personas.Atendidas.2017", 2017, "lightyellow", "orange")
histograma_personas_atendidas(aux_departamento, "Número.de.Personas.Atendidas.2018", 2018, "lightseagreen", "darkgreen")
histograma_personas_atendidas(aux_departamento, "Número.de.Personas.Atendidas.2019", 2019, "lightskyblue", "darkblue")

# Función para crear gráfico de torta para el número de atenciones por año con porcentajes
grafico_torta_atenciones <- function(data) {
  # Sumar el número total de atenciones por cada año
  atenciones_por_anio <- c(
    sum(data$`Número.de.Atenciones.2017`, na.rm = TRUE),
    sum(data$`Número.de.Atenciones.2018`, na.rm = TRUE),
    sum(data$`Número.de.Atenciones.2019`, na.rm = TRUE)
  )
  
  # Calcular el porcentaje para cada año
  porcentaje_atenciones <- round(atenciones_por_anio / sum(atenciones_por_anio) * 100, 2)
  
  # Etiquetas con los porcentajes dentro del gráfico
  labels_atenciones <- paste(c("2017", "2018", "2019"), ": ", atenciones_por_anio, " (", porcentaje_atenciones, "%)", sep = "")
  
  # Crear el gráfico circular
  pie(atenciones_por_anio, 
      labels = labels_atenciones, 
      col = c("lightblue", "lightcoral", "lightgreen"), 
      main = "Distribución de Atenciones por Año", 
      radius = 1, 
      border = "white")
}

# Función para crear gráfico de torta para el número de personas atendidas por año con porcentajes
grafico_torta_personas_atendidas <- function(data) {
  # Sumar el número total de personas atendidas por cada año
  personas_atendidas_por_anio <- c(
    sum(data$`Número.de.Personas.Atendidas.2017`, na.rm = TRUE),
    sum(data$`Número.de.Personas.Atendidas.2018`, na.rm = TRUE),
    sum(data$`Número.de.Personas.Atendidas.2019`, na.rm = TRUE)
  )
  
  # Calcular el porcentaje para cada año
  porcentaje_personas_atendidas <- round(personas_atendidas_por_anio / sum(personas_atendidas_por_anio) * 100, 2)
  
  # Etiquetas con los porcentajes dentro del gráfico
  labels_personas_atendidas <- paste(c("2017", "2018", "2019"), ": ", personas_atendidas_por_anio, " (", porcentaje_personas_atendidas, "%)", sep = "")
  
  # Crear el gráfico circular
  pie(personas_atendidas_por_anio, 
      labels = labels_personas_atendidas, 
      col = c("lightyellow", "lightseagreen", "lightskyblue"), 
      main = "Distribución de Personas Atendidas por Año", 
      radius = 1, 
      border = "white")
}

# Llamar las funciones para crear los gráficos con porcentajes
grafico_torta_atenciones(aux_departamento)  # Gráfico de Atenciones
grafico_torta_personas_atendidas(aux_departamento)  # Gráfico de Personas Atendidas

# Función para crear el gráfico de dispersión de Atenciones vs Personas Atendidas por Año
grafico_dispersion <- function(data, anio) {
  # Seleccionar las columnas correspondientes para el número de atenciones y el número de personas atendidas
  atenciones <- data[[paste0("Número.de.Atenciones.", anio)]]
  personas_atendidas <- data[[paste0("Número.de.Personas.Atendidas.", anio)]]
  
  # Crear gráfico de dispersión
  plot(atenciones, 
       personas_atendidas, 
       main = paste("Dispersión de Atenciones vs Personas Atendidas en", anio), 
       xlab = "Número de Atenciones", 
       ylab = "Número de Personas Atendidas", 
       col = "darkblue", 
       pch = 16,  # Tipo de punto
       cex = 1.2,  # Tamaño de los puntos
       xlim = c(0, max(atenciones, personas_atendidas, na.rm = TRUE) * 1.1), # Ajuste de límites
       ylim = c(0, max(atenciones, personas_atendidas, na.rm = TRUE) * 1.1)) # Ajuste de límites
  
  # Agregar una línea de ajuste (regresión lineal)
  abline(lm(personas_atendidas ~ atenciones), col = "red", lwd = 2)  # Línea de regresión
}

# Crear los tres gráficos de dispersión para los años 2017, 2018 y 2019
grafico_dispersion(aux_departamento, 2017)  # Dispersión para el año 2017
grafico_dispersion(aux_departamento, 2018)  # Dispersión para el año 2018
grafico_dispersion(aux_departamento, 2019)  # Dispersión para el año 2019

# Crear un dataframe largo para facilitar el análisis
aux_departamento_long <- aux_departamento %>%
  gather(key = "variable", value = "valor", 
         `Número.de.Atenciones.2017`, `Número.de.Atenciones.2018`, `Número.de.Atenciones.2019`,
         `Número.de.Personas.Atendidas.2017`, `Número.de.Personas.Atendidas.2018`, `Número.de.Personas.Atendidas.2019`) %>%
  mutate(
    Año = case_when(
      variable %in% c("Número.de.Atenciones.2017", "Número.de.Personas.Atendidas.2017") ~ "2017",
      variable %in% c("Número.de.Atenciones.2018", "Número.de.Personas.Atendidas.2018") ~ "2018",
      variable %in% c("Número.de.Atenciones.2019", "Número.de.Personas.Atendidas.2019") ~ "2019"
    ),
    Tipo = ifelse(grepl("Atenciones", variable), "Atenciones", "Personas Atendidas")
  )

# Boxplot con salto de línea en las etiquetas del eje X
boxplot(valor ~ Tipo * Año, data = aux_departamento_long,
        main = "Distribución de Atenciones y Personas Atendidas por Año",
        ylab = "Número de Atenciones y Personas Atendidas",
        xlab = "",
        col = c("lightblue", "lightgreen"),  # Colores para Atenciones y Personas Atendidas
        border = "darkblue",  # Color de los bordes de las cajas
        outline = TRUE,  # Mostrar los outliers (valores atípicos)
        las = 2,  # Rotar las etiquetas del eje X verticalmente
        cex.axis = 0.8,  # Reducir el tamaño de las etiquetas en el eje X
        cex.lab = 1,  # Tamaño de las etiquetas de los ejes
        cex.main = 1.2,  # Tamaño del título
        names = c("Personas\nAtendidas\n2017", "Personas\nAtendidas\n2018", "Personas\nAtendidas\n2019", 
                  "Atenciones\n2017", "Atenciones\n2018", "Atenciones\n2019"))  

##########################################################################
######################## WORKING WITH MAPS ###############################
##########################################################################

## Cargar shapefile de departamentos
map <- st_read("MGN_ADM_DPTO_POLITICO.shp")
plot(map)

# Ver columnas del shapefile
names(map)
names(map)[names(map) == "dpto_cnmbr"] <- "Departamento"

# Convertir nombres de departamento a mayúsculas
aux_departamento$Departamento <- toupper(aux_departamento$Departamento)
aux_departamento$Departamento <- trimws(aux_departamento$Departamento)  # Eliminar espacios en 'aux_departamento'

# Unir los datos del dataframe con el shapefile por la columna 'Departamento'
map_data <- merge(map, aux_departamento, by = "Departamento")

# Mapa # de atenciones en 2017
ggplot(data = map_data) +
  geom_sf(aes(fill = `Número.de.Atenciones.2017`)) +  
  scale_fill_viridis_c(option = "C", na.value = "gray") +  # Colores del mapa
  ggtitle("Mapa de Calor: Número de Atenciones 2017 por Departamento") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

# Mapa # de atenciones en 2018
ggplot(data = map_data) +
  geom_sf(aes(fill = `Número.de.Atenciones.2018`)) +  
  scale_fill_viridis_c(option = "C", na.value = "gray") +  # Colores del mapa
  ggtitle("Mapa de Calor: Número de Atenciones 2018 por Departamento") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

# Mapa # de atenciones en 2019
ggplot(data = map_data) +
  geom_sf(aes(fill = `Número.de.Atenciones.2019`)) +  
  scale_fill_viridis_c(option = "C", na.value = "gray") +  # Colores del mapa
  ggtitle("Mapa de Calor: Número de Atenciones 2019 por Departamento") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

# Mapa de personas atendidas en 2017
ggplot(data = map_data) +
  geom_sf(aes(fill = `Número.de.Personas.Atendidas.2017`)) +  
  scale_fill_viridis_c(option = "C", na.value = "gray") +  # Colores del mapa
  ggtitle("Mapa de Calor: Número de Personas Atendidas 2017 por Departamento") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

# Mapa de personas atendidas en 2018
ggplot(data = map_data) +
  geom_sf(aes(fill = `Número.de.Personas.Atendidas.2018`)) +  
  scale_fill_viridis_c(option = "C", na.value = "gray") +  # Colores del mapa
  ggtitle("Mapa de Calor: Número de Personas Atendidas 2018 por Departamento") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

# Mapa de personas atendidas en 2019
ggplot(data = map_data) +
  geom_sf(aes(fill = `Número.de.Personas.Atendidas.2019`)) +  
  scale_fill_viridis_c(option = "C", na.value = "gray") +  # Colores del mapa
  ggtitle("Mapa de Calor: Número de Personas Atendidas 2019 por Departamento") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

