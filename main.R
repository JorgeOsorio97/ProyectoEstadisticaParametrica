library(stringr)
library(gtools)

source("utils.R")

# Tomamos todos los datos del data set
complete_data <- read.csv("fifa19.csv")

# Tomamos las 4 variable a tarbajar
# * Cuantitativas:
#     Values -> Valor en euros del Jugador
#     Overall -> Calificación General del jugador
#     AttackRate -> Que tanto participa en jugadas de ataque
#     Position -> Posicion en la que juega 
data <- data.frame(
  Value = unlist(lapply(as.list(complete_data$Value), cleanPlayerValue)),
  Overall = complete_data$Overall,
  AttackRate = str_split_fixed(complete_data$Work.Rate, "/ ", 2)[,1],
  Position = unlist(lapply(as.list(complete_data$Position, getTwoMaxValues(complete_data$Position)), cleanPlayerPosition))
)

# Eliminamos valores 0 de Value
data <- data[data$Value>0,]

# Calculamos valores poblacionales
#   * Medias
mu_value <- mean(data$Value)
mu_overall <- mean(data$Overall)
#   * Desviaciones estandar
sigma_value <- pop.sd(data$Value)
sigma_overall <- pop.sd(data$Overall)
# * Coeficiente de correlacion
corr = cor(data$Value, data$Overall)

# Calsificacion de variables cuantitativas
gk_value = data$Value[data$Position=="GK"]
st_value = data$Value[data$Position=="ST"]
other_value = data$Value[data$Position=="Other"]

gk_overall = data$Overall[data$Position=="GK"]
st_overall = data$Overall[data$Position=="ST"]
other_overall = data$Overall[data$Position=="Other"]

#Tomamos muestea de tamaño 100
sample <- data[sample(nrow(data), 100),]

# Creamos tabla de resumen de ambas variables cuantitativas
# Inciso 2.b
cuantitatives <- data.frame(sample$Value, sample$Overall)
resume_cuantitatives <- summary(cuantitatives)
resume_cuantitatives <- rbind(resume_cuantitatives, c(paste("Sd. :" ,sd(cuantitatives$sample.Value)), paste("Sd. :" ,sd(cuantitatives$sample.Overall))))

hist(sample$Value)
hist(sample$Overall)

boxplot(sample$Value)
boxplot(sample$Overall)


# TEMA 4

# * Creamos la poblacion de 6 observaciones, calculamos media y varianza usando la variable Overall
omega_x3 <- data$Overall[sample(nrow(data),6)]
mean_omega_x3 <- mean(omega_x3)
var_omega_x3 <- pop.var(omega_x3)

# * Generamos todas las mustras posibles de tamaño 3 con usando combinatorias
omega_samples <- combinations(length(omega_x3), 3, omega_x3, set = FALSE, repeats.allowed = FALSE)

# * Calculamos media y varianza de cada muestra
omega_distributions = data.frame(
  "mean" = sapply(seq_len(nrow(omega_samples)), function(x) mean(omega_samples[x,])),
  "var" = sapply(seq_len(nrow(omega_samples)), function(x) var(omega_samples[x,]))
)

# * Generamos la tabla de distribucion para la media y calculamos el valor esperado
omega_mean_distribution=table(omega_distributions$mean)
e_mean_omega_x3 = sum(sapply(seq_len(nrow(omega_mean_distribution)),function(x) as.numeric(names(omega_mean_distribution[x]))*omega_mean_distribution[x]))/length(omega_samples[,1])

# * Generamos la tabla de distribucion para la varianza y calculamos el valor esperado
omega_var_distribution=table(omega_distributions$var)
e_var_omega_x3 = sum(sapply(seq_len(nrow(omega_var_distribution)),function(x) as.numeric(names(omega_var_distribution[x]))*omega_var_distribution[x]))/length(omega_samples[,1]-1)

       