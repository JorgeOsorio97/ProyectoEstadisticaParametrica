library(stringr)
library(gtools)
library(ggplot2)

source("utils.R")

# Tomamos todos los datos del data set
complete_data <- read.csv("fifa19.csv", encoding="UTF-8")

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
# * Medias
mu_value <- mean(data$Value)
mu_overall <- mean(data$Overall)
# * Varianza
var_value <- pop.var(data$Value)
var_overall <- pop.var(data$Overall)
# * Desviaciones estandar
sigma_value <- pop.sd(data$Value)
sigma_overall <- pop.sd(data$Overall)
# * Coeficiente de correlacion
corr = pop.cor(data$Value, data$Overall)

# Calsificacion de variables cuantitativas
gk_value = data$Value[data$Position=="GK"]
st_value = data$Value[data$Position=="ST"]
other_value = data$Value[data$Position=="Other"]

gk_overall = data$Overall[data$Position=="GK"]
st_overall = data$Overall[data$Position=="ST"]
other_overall = data$Overall[data$Position=="Other"]

#Tomamos muestea de tamaño 100
n=100 #sample size
sample <- data[sample(nrow(data), n),]

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

# * 1. Creamos la poblacion de 6 observaciones usando la variable Overall
omega_x3 <- data$Overall[sample(nrow(data),6)]
# * 2. Calculamos media y varianza
mean_omega_x3 <- mean(omega_x3)
var_omega_x3 <- pop.var(omega_x3)

# * 3. Generamos todas las mustras posibles de tamaño 3 con usando combinatorias
omega_samples <- combinations(length(omega_x3), 3, omega_x3, set = FALSE, repeats.allowed = FALSE)

# * Calculamos media y varianza de cada muestra
omega_distributions = data.frame(
  "mean" = sapply(seq_len(nrow(omega_samples)), function(x) mean(omega_samples[x,])),
  "var" = sapply(seq_len(nrow(omega_samples)), function(x) var(omega_samples[x,]))
)

# * 4. Generamos la tabla de distribucion para la media y varianza
omega_mean_distribution=table(omega_distributions$mean)
omega_var_distribution=table(omega_distributions$var)


# * 5. Calculamos el valor esperado de la media y la varianza
e_mean_omega_x3 = sum(sapply(seq_len(nrow(omega_mean_distribution)),function(x) as.numeric(names(omega_mean_distribution[x]))*omega_mean_distribution[x]))/length(omega_samples[,1])
e_var_omega_x3 = sum(sapply(seq_len(nrow(omega_var_distribution)),function(x) as.numeric(names(omega_var_distribution[x]))*omega_var_distribution[x]))/length(omega_samples[,1]-1)


# Teorema central de límite (TCL)

# * 1. Revisammos con graficas si la variable parece ser normal
ggplot(sample, aes(Overall)) + geom_histogram(fill="#33c7ff") + geom_freqpoly(colour="#000000")
boxplot(sample$Overall)

# * Creamos 10,000 medias muestrales para verificar el TCL
samples_means = numeric()
for(i in 0:10000){
  samples_means[i] <- mean(sample(sample$Overall,25))
}
# * Creamos 10,000 numeros aleatorios de una distribucion normal con
#     media=media de values
#     varianza= varianza de values entre el tamaño de muestra
mean_distribution_samples = rnorm(10000, mean = mean(sample$Overall), sd = pop.sd(sample$Overall)/25)
tcl_results=data.frame(
  means=samples_means,
  distribution = mean_distribution_samples
)
# * Comparamos los datos con gráficas
ggplot(tcl_results, aes(means)) + geom_density(fill="#33c7ff", alpha=0.5) 
ggplot(tcl_results, aes(distribution)) + geom_density(fill="#33cf7f", alpha=0.5)

# * Comparamos la media y la varianza de las medias generadas con la de la poblacion
# ** Recordemos la media poblacional debe ser igual a la media de las medias
mean(sample$Overall)
mean(samples_means)
# ** Recordemos la varianza de las medias debe ser igual a la varianza poblacional dividido en la longitud del interalo
pop.var(sample$Overall)/25
pop.var(samples_means)

# TEMA 5

# * 1. Estimación puntual la proporción para las variables cualitativas
# ** Position
pr_position_GK = length(sample$Position[sample$Position == "GK"])/n
pr_position_ST = length(sample$Position[sample$Position == "ST"])/n
pr_position_Other = length(sample$Position[sample$Position == "Other"])/n
# ** AttackRate
pr_attack_High = length(sample$AttackRate[sample$AttackRate == "High"])/n
pr_attack_Medium = length(sample$AttackRate[sample$AttackRate == "Medium"])/n
pr_attack_Low = length(sample$AttackRate[sample$AttackRate == "Low"])/n

# * 2. Estimación puntual de variables cuantitativas
# ** Values
value_sample_mean = mean(sample$Value)
value_sample_var = sd(sample$Value)
# ** Overall
overall_sample_mean = mean(sample$Overall)
overall_sample_var = var(sample$Overall)
overall_sample_sd = sd(sample$Overall)
# ** Correlacion
sample_corr = cor(sample$Value, sample$Overall)


# * 3. Intervalos de confianza
# ** a) Para variables cualitativas X1 y X2 parametro p 90%
confianza_p =1-0.9
# *** AttackRate X1
itr_p_attack_High = c(pr_attack_High-dnorm(confianza_p/2)*sqrt((pr_attack_High*(1-pr_attack_High)/n)),
                            pr_attack_High+dnorm(confianza_p/2)*sqrt((pr_attack_High*(1-pr_attack_High)/n)))
itr_p_attack_Medium = c(pr_attack_Medium-dnorm(confianza_p/2)*sqrt((pr_attack_Medium*(1-pr_attack_Medium)/n)),
                              pr_attack_Medium+dnorm(confianza_p/2)*sqrt((pr_attack_Medium*(1-pr_attack_Medium)/n)))
itr_p_attack_Low = c(pr_attack_Low-dnorm(confianza_p/2)*sqrt((pr_attack_Low*(1-pr_attack_Low)/n)),
                           pr_attack_Low+dnorm(confianza_p/2)*sqrt((pr_attack_Low*(1-pr_attack_Low)/n)))

# *** Position X2
itr_p_position_GK = c(pr_position_GK-dnorm(confianza_p/2)*sqrt((pr_position_GK*(1-pr_position_GK)/n)),
                      pr_position_GK+dnorm(confianza_p/2)*sqrt((pr_position_GK*(1-pr_position_GK)/n)))
itr_p_position_ST = c(pr_position_ST-dnorm(confianza_p/2)*sqrt((pr_position_ST*(1-pr_position_ST)/n)),
                      pr_position_ST+dnorm(confianza_p/2)*sqrt((pr_position_ST*(1-pr_position_ST)/n)))
itr_p_position_Other = c(pr_position_Other-dnorm(confianza_p/2)*sqrt((pr_position_Other*(1-pr_position_Other)/n)),
                         pr_position_Other+dnorm(confianza_p/2)*sqrt((pr_position_Other*(1-pr_position_Other)/n)))

# ** b) Para variables cuantitativas X3 y X4
confianza_m=1-0.9
confianza_v=1-0.95
# *** Overall X3
itr_mean_overall = c(overall_sample_mean-dnorm(confianza_m/2)*(sigma_overall/sqrt(n)),
                     overall_sample_mean+dnorm(confianza_m/2)*(sigma_overall/sqrt(n)))
itr_var_overall = c(((n-1)*overall_sample_var)/qchisq(confianza_v/2,n-1, lower.tail = FALSE),
                    ((n-1)*overall_sample_var)/qchisq(1-(confianza_v/2),n-1, lower.tail = FALSE))

# *** Value X4
itr_mean_value = c(value_sample_mean-dnorm(confianza_m/2)*(sigma_value/sqrt(n)),
                   value_sample_mean+dnorm(confianza_m/2)*(sigma_value/sqrt(n)))
itr_var_value = c(((n-1)*value_sample_var)/qchisq(confianza_v/2,n-1, lower.tail = FALSE),
                    ((n-1)*value_sample_var)/qchisq(1-(confianza_v/2),n-1, lower.tail = FALSE))
