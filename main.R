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
#Tomamos muestea de tamaño 100
data <- data[sample(nrow(data), 100),]

# Creamos tabla de resumen de ambas variables cuantitativas
# Inciso 2.b
cuantitatives <- data.frame(data$Value, data$Overall)
resume_cuantitatives <- summary(cuantitatives)
resume_cuantitatives <- rbind(resume_cuantitatives, c(paste("Sd." ,sd(cuantitatives$data.Value)), paste("Sd." ,sd(cuantitatives$data.Overall))))

hist(data$Value)
hist(data$Overall)

boxplot(data$Value)
boxplot(data$Overall)
