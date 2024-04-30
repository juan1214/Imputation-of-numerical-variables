install.packages("mice")
library(mice)
library(readxl)

BD <- read_excel("C:/Users/UNALMED/Desktop/One Health/Tesis doctora Olga/BD con Resultados_hospitalizados.xlsx")

# Renombrar las variables para eliminar los guiones y los puntos suspensivos
names(BD) <- gsub("-", "_", names(BD))
names(BD) <- gsub("\\.\\.\\.", "_", names(BD))
names(BD) <- gsub(" ", "_", names(BD))
names(BD) <- make.names(names(BD))

variables_IL <- c("IL_6_42", "IL_10_43", "IL_6_52", "IL_10_53" , "IL_6_62", "IL_10_63")

convertir_a_numerico <- function(data, variables) {
  for (col in variables) {
    data[[col]] <- as.numeric(data[[col]])
  }
  return(data)
}

# Llamar a la función con las variables específicas
BD <- convertir_a_numerico(BD, variables_IL)

# Ahora puedes imputar los datos
imputed_data <- mice(BD[variables_IL], m=5, maxit=50, method='pmm', seed=500)

# Crear un conjunto de datos completo utilizando la primera imputación
BD_complete <- complete(imputed_data, 1)


# Calcular los resúmenes
summary_orig <- summary(BD[, variables_IL[c(TRUE, FALSE)]])
summary_imputed <- summary(BD_complete[, variables_IL[c(TRUE, FALSE)]])

par(mfrow=c(3,2))  # Organizar los gráficos en una matriz de 3 filas y 2 columnas

for (var in variables_IL) {
    # Calcular las densidades
    dens_orig <- density(BD[[var]], na.rm=TRUE)
    dens_imputed <- density(BD_complete[[var]])

    # Dibujar el gráfico de densidad de los datos originales en rojo
    plot(dens_orig, main=var, xlab="", ylab="Density", ylim=c(0, max(dens_orig$y, dens_imputed$y)), xlim=c(min(dens_orig$x, dens_imputed$x), max(dens_orig$x, dens_imputed$x)), col="red")
    
    # Añadir el gráfico de densidad de los datos imputados en azul
    lines(dens_imputed, lty=2, col="blue")
    
    # Añadir una leyenda
    legend("topright", legend=c("Original", "Imputed"), lty=c(1,2), col=c("red", "blue"), cex=0.8)
}


# Comparar la distribución de los datos originales y los datos imputados
par(mfrow=c(2,1))

# Histograma de los datos originales
hist(BD$IL_6_42, main="Original data", xlab="IL_6_42")

# Histograma de los datos imputados
hist(BD_complete$IL_6_42, main="Imputed data", xlab="IL_6_42")




####Analisis de sensibilidad###

# Aplicar un modelo lineal a los datos originales
# Nota: Esto excluirá automáticamente las filas con valores faltantes
original_model <- lm(IL_6_42 ~ IL_10_43, data=BD)

# Imprimir los resultados del modelo original
print(summary(original_model))

# Aplicar un modelo lineal a los datos imputados
imputed_model <- lm(IL_6_42 ~ IL_10_43, data=BD_complete)

# Imprimir los resultados del modelo imputado
print(summary(imputed_model))


###Imputación de variables PBMC VIABILITY PERCENTAGE	Lymphocytes T (RELATIVE)	Lymphocytes T (ADSOLUTE)	T Helper cells (RELATIVE)	T Helper cells (ADSOLUTE)	T Cytotoxic Cells (RELATIVE) 	T Cytotoxic Cells (ADSOLUTE) 


# Ahora tus variables se llamarán "PBMC_VIABILITY_PERCENTAGE_44", "Lymphocytes_T_(RELATIVE)_45", etc.
variables <- c("PBMC_VIABILITY_PERCENTAGE_44", "Lymphocytes_T_.RELATIVE._45", "Lymphocytes_T_.ADSOLUTE._46", "T_Helper_cells_.RELATIVE._47", "T_Helper_cells_.ADSOLUTE._48", "T_Cytotoxic_Cells_.RELATIVE._49", "T_Cytotoxic_Cells_.ADSOLUTE._50")


convertir_a_numerico <- function(data, variables) {
  for (col in variables) {
    data[[col]] <- as.numeric(data[[col]])
  }
  return(data)
}

# Llamar a la función con las variables específicas
BD <- convertir_a_numerico(BD, variables)

summary(BD[, variables])

# Ahora puedes imputar los datos
imputed_data_V <- mice(BD[variables], m=5, maxit=50, method='pmm', seed=500)

BD_V <- complete(imputed_data_V, 1)

summary(BD_V[, variables])


par(mfrow=c(3,2))  # Organizar los gráficos en una matriz de 3 filas y 2 columnas

for (var in variables) {
    # Calcular las densidades
    dens_orig <- density(BD[[var]], na.rm=TRUE)
    dens_imputed <- density(BD_V[[var]])

    # Dibujar el gráfico de densidad de los datos originales en rojo
    plot(dens_orig, main=var, xlab="", ylab="Density", ylim=c(0, max(dens_orig$y, dens_imputed$y)), xlim=c(min(dens_orig$x, dens_imputed$x), max(dens_orig$x, dens_imputed$x)), col="red")
    
    # Añadir el gráfico de densidad de los datos imputados en azul
    lines(dens_imputed, lty=2, col="blue")
    
    # Añadir una leyenda
    legend("topright", legend=c("Original", "Imputed"), lty=c(1,2), col=c("red", "blue"), cex=0.8)
}
