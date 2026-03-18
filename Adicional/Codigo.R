install.packages("ggplot2")
# LIBRERÍAS

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

# RUTA DE ARCHIVO
setwd("C:/Users/metin/OneDrive/Documentos/Codigo")



# CARGA DE DATOS

data <- read_excel( "datos_sucios.xlsx")

cat("=== DATOS ORIGINALES ===\n")
print(data)


#EXPLORACIÓN INICIAL

cat("\n=== ESTRUCTURA ===\n")
str(data)

cat("\n=== VALORES NULOS POR COLUMNA ===\n")
print(colSums(is.na(data)))

cat("\n=== VALORES ÚNICOS EN NOMBRE ===\n")
print(unique(data$nombre))


# LIMPIEZA DE DATOS

data_limpia <- data %>%
  
  # Limpiar texto
  mutate(
    nombre = str_trim(nombre),           
    nombre = str_to_title(nombre)        
  ) %>%
  
  # Convertir tipos
  mutate(
    edad = as.numeric(edad),
    ingreso = as.numeric(ingreso)
  ) %>%
  
  # Eliminar registros incompletos
  filter(
    !is.na(nombre),
    !is.na(edad),
    !is.na(ingreso)
  ) %>%
  
  
  # Eliminar duplicados
  distinct()

cat("\n=== DATOS LIMPIOS ===\n")
print(data_limpia)


# ANÁLISIS
resumen <- data_limpia %>%
  summarise(
    promedio_ingreso = mean(ingreso),
    max_ingreso = max(ingreso),
    min_ingreso = min(ingreso)
  )

cat("\n=== RESUMEN ===\n")
print(resumen)

#VISUALIZACIONES


# Gráfica de barras
grafica_barras <- ggplot(data_limpia, aes(x = nombre, y = ingreso)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Ingreso por persona",
    x = "Nombre",
    y = "Ingreso") +
  theme_minimal()

print(grafica_barras)

# Boxplot
grafica_boxplot <- ggplot(data_limpia, aes(y = ingreso)) +
  geom_boxplot() +
  labs(
    title = "Distribución de ingresos",
    y = "Ingreso") +
  theme_minimal()

print(grafica_boxplot)


# 8. EXPORTAR RESULTADO LIMPIO (OPCIONAL)

write.csv(data_limpia, "datos_limpios.csv", row.names = FALSE)

cat("\nArchivo limpio exportado como 'datos_limpios.csv'\n")