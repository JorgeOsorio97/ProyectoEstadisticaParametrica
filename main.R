library(stringr)

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

unique(data$Position)

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
