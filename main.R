library(stringr)
library(gtools)
library(ggplot2)

source("utils.R")

# Definimos seed para mantener resultados a lo largo del desarollo
set.seed(5)

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
#Calculamos los cuantiles de las variables
quantile(data$Value)
quantile(data$Overall)

# Clasificacion de variables cuantitativas
# * Position-Value
gk_value = data$Value[data$Position=="GK"]
st_value = data$Value[data$Position=="ST"]
other_value = data$Value[data$Position=="Other"]
# * Position-Overall
gk_overall = data$Overall[data$Position=="GK"]
st_overall = data$Overall[data$Position=="ST"]
other_overall = data$Overall[data$Position=="Other"]
# * AttackRate-Value
high_value = data$Value[data$AttackRate=="High"]
medium_value = data$Value[data$AttackRate=="Medium"]
low_value = data$Value[data$AttackRate=="Low"]
# * AttackRate-Overall
high_overall = data$Overall[data$AttackRate=="High"]
medium_overall = data$Overall[data$AttackRate=="Medium"]
low_overall = data$Overall[data$AttackRate=="Low"]

#Tomamos muestea de tamaño 100
n=100 #sample size
sample <- data[sample(nrow(data), n),]

# Creamos tabla de resumen de ambas variables cuantitativas
# Inciso 2.b
cuantitatives <- data.frame(sample$Value, sample$Overall)
resume_cuantitatives <- summary(cuantitatives)
resume_cuantitatives <- rbind(resume_cuantitatives, c(paste("Sd. :" ,sd(cuantitatives$sample.Value)), paste("Sd. :" ,sd(cuantitatives$sample.Overall))))

corr_muestra = pop.cor(sample$Value, sample$Overall)

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
#     media=media de overall
#     varianza= varianza de overall entre el tamaño de muestra
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
value_sample_var = var(sample$Value)
value_sample_sd = sd(sample$Values)
# ** Overall
overall_sample_mean = mean(sample$Overall)
overall_sample_var = var(sample$Overall)
overall_sample_sd = sd(sample$Overall)
# ** Correlacion
sample_corr = cor(sample$Value, sample$Overall)


# * 3. Intervalos de confianza una poblacion
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
#        consideramos varianza conocida
confianza_m=1-0.9
confianza_v=1-0.95
# *** Overall X3
itr_mean_overall = c(overall_sample_mean-dnorm(confianza_m/2)*(sigma_overall/sqrt(n)),
                     overall_sample_mean+dnorm(confianza_m/2)*(sigma_overall/sqrt(n)))
itr_var_overall = c(((n-1)*overall_sample_var)/pchisq(confianza_v/2,n-1, lower.tail = FALSE),
                    ((n-1)*overall_sample_var)/pchisq(1-(confianza_v/2),n-1, lower.tail = FALSE))

# *** Value X4
itr_mean_value = c(value_sample_mean-dnorm(confianza_m/2)*(sigma_value/sqrt(n)),
                   value_sample_mean+dnorm(confianza_m/2)*(sigma_value/sqrt(n)))
itr_var_value = c(((n-1)*value_sample_var)/pchisq(confianza_v/2,n-1, lower.tail = FALSE),
                    ((n-1)*value_sample_var)/pchisq(1-(confianza_v/2),n-1, lower.tail = FALSE))

# * Intervalos de confianza dos poblaciones
# ** Construimos muestra de tamaño 30
n2 = 30
# *** Position-Value
sample_gk_value <- gk_value[sample(length(gk_value),n2)]
sample_st_value <- st_value[sample(length(st_value),n2)]
sample_other_value <- other_value[sample(length(other_value),n2)]
# *** Position-Overall
sample_gk_overall <- gk_overall[sample(length(gk_overall),n2)]
sample_st_overall <- st_overall[sample(length(st_overall),n2)]
sample_other_overall <- other_overall[sample(length(other_overall),n2)]
# *** AttackRate-Value
sample_high_value <- high_value[sample(length(high_value),n2)]
sample_medium_value <- medium_value[sample(length(medium_value),n2)]
sample_low_value <- low_value[sample(length(low_value),n2)]
# *** AttackRate-Overall
sample_high_overall <- high_overall[sample(length(high_overall),n2)]
sample_medium_overall <- medium_overall[sample(length(medium_overall),n2)]
sample_low_overall <- low_overall[sample(length(low_overall),n2)]

# ** Intervalo de confianza de la diferencia de medias
confianza_dif_m = 1-0.85
# *** Overall-Position
dif_m_overall_gk_st <- c((mean(sample_gk_overall)-mean(sample_st_overall))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(gk_overall)/length(gk_overall)+pop.var(st_overall)/length(st_overall)))),
                         (mean(sample_gk_overall)-mean(sample_st_overall))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(gk_overall)/length(gk_overall)+pop.var(st_overall)/length(st_overall)))))

dif_m_overall_gk_other <- c((mean(sample_gk_overall)-mean(sample_other_overall))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(gk_overall)/length(gk_overall)+pop.var(other_overall)/length(other_overall)))),
                            (mean(sample_gk_overall)-mean(sample_other_overall))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(gk_overall)/length(gk_overall)+pop.var(other_overall)/length(other_overall)))))

dif_m_overall_st_other <- c((mean(sample_st_overall)-mean(sample_other_overall))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(st_overall)/length(st_overall)+pop.var(other_overall)/length(other_overall)))),
                            (mean(sample_st_overall)-mean(sample_other_overall))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(st_overall)/length(st_overall)+pop.var(other_overall)/length(other_overall)))))

# *** Overall-Attack
dif_m_overall_high_medium <- c((mean(sample_high_overall)-mean(sample_medium_overall))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(high_overall)/length(high_overall)+pop.var(medium_overall)/length(medium_overall)))),
                         (mean(sample_high_overall)-mean(sample_medium_overall))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(high_overall)/length(high_overall)+pop.var(medium_overall)/length(medium_overall)))))

dif_m_overall_high_low <- c((mean(sample_high_overall)-mean(sample_low_overall))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(high_overall)/length(high_overall)+pop.var(low_overall)/length(low_overall)))),
                            (mean(sample_high_overall)-mean(sample_low_overall))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(high_overall)/length(high_overall)+pop.var(low_overall)/length(low_overall)))))

dif_m_overall_medium_low <- c((mean(sample_medium_overall)-mean(sample_low_overall))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(medium_overall)/length(medium_overall)+pop.var(low_overall)/length(low_overall)))),
                            (mean(sample_medium_overall)-mean(sample_low_overall))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(medium_overall)/length(medium_overall)+pop.var(low_overall)/length(low_overall)))))

# *** Value-Position
dif_m_value_gk_st <- c((mean(sample_gk_value)-mean(sample_st_value))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(gk_value)/length(gk_value)+pop.var(st_value)/length(st_value)))),
                         (mean(sample_gk_value)-mean(sample_st_value))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(gk_value)/length(gk_value)+pop.var(st_value)/length(st_value)))))

dif_m_value_gk_other <- c((mean(sample_gk_value)-mean(sample_other_value))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(gk_value)/length(gk_value)+pop.var(other_value)/length(other_value)))),
                            (mean(sample_gk_value)-mean(sample_other_value))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(gk_value)/length(gk_value)+pop.var(other_value)/length(other_value)))))

dif_m_value_st_other <- c((mean(sample_st_value)-mean(sample_other_value))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(st_value)/length(st_value)+pop.var(other_value)/length(other_value)))),
                            (mean(sample_st_value)-mean(sample_other_value))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(st_value)/length(st_value)+pop.var(other_value)/length(other_value)))))

# *** Value-Attack
dif_m_value_high_medium <- c((mean(sample_high_value)-mean(sample_medium_value))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(high_value)/length(high_value)+pop.var(medium_value)/length(medium_value)))),
                               (mean(sample_high_value)-mean(sample_medium_value))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(high_value)/length(high_value)+pop.var(medium_value)/length(medium_value)))))

dif_m_value_high_low <- c((mean(sample_high_value)-mean(sample_low_value))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(high_value)/length(high_value)+pop.var(low_value)/length(low_value)))),
                            (mean(sample_high_value)-mean(sample_low_value))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(high_value)/length(high_value)+pop.var(low_value)/length(low_value)))))

dif_m_value_medium_low <- c((mean(sample_medium_value)-mean(sample_low_value))-(dnorm(confianza_dif_m/2)*sqrt((pop.var(medium_value)/length(medium_value)+pop.var(low_value)/length(low_value)))),
                              (mean(sample_medium_value)-mean(sample_low_value))+(dnorm(confianza_dif_m/2)*sqrt((pop.var(medium_value)/length(medium_value)+pop.var(low_value)/length(low_value)))))


# ** Intervalo de confianza de cociente de varianzas
confianza_coc_var = 1-0.95
# *** Overall-Position
coc_var_overall_gk_st = c((var(sample_gk_overall)/var(sample_st_overall))*pf(1-(confianza_coc_var/2),length(sample_st_overall)-1,length(sample_gk_overall)-1, lower.tail = FALSE),
                          (var(sample_gk_overall)/var(sample_st_overall))*pf((confianza_coc_var/2),length(sample_st_overall)-1,length(sample_gk_overall)-1, lower.tail = FALSE))

coc_var_overall_gk_other = c((var(sample_gk_overall)/var(sample_other_overall))*pf(1-(confianza_coc_var/2),length(sample_other_overall)-1,length(sample_gk_overall)-1, lower.tail = FALSE),
                          (var(sample_gk_overall)/var(sample_other_overall))*pf((confianza_coc_var/2),length(sample_other_overall)-1,length(sample_gk_overall)-1, lower.tail = FALSE))

coc_var_overall_st_other = c((var(sample_st_overall)/var(sample_other_overall))*pf(1-(confianza_coc_var/2),length(sample_other_overall)-1,length(sample_st_overall)-1, lower.tail = FALSE),
                             (var(sample_st_overall)/var(sample_other_overall))*pf((confianza_coc_var/2),length(sample_other_overall)-1,length(sample_st_overall)-1, lower.tail = FALSE))

# *** Overall-Attack
coc_var_overall_high_medium = c((var(sample_high_overall)/var(sample_medium_overall))*pf(1-(confianza_coc_var/2),length(sample_medium_overall)-1,length(sample_high_overall)-1, lower.tail = FALSE),
                          (var(sample_high_overall)/var(sample_medium_overall))*pf((confianza_coc_var/2),length(sample_medium_overall)-1,length(sample_high_overall)-1, lower.tail = FALSE))

coc_var_overall_high_low = c((var(sample_high_overall)/var(sample_low_overall))*pf(1-(confianza_coc_var/2),length(sample_low_overall)-1,length(sample_high_overall)-1, lower.tail = FALSE),
                             (var(sample_high_overall)/var(sample_low_overall))*pf((confianza_coc_var/2),length(sample_low_overall)-1,length(sample_high_overall)-1, lower.tail = FALSE))

coc_var_overall_medium_low = c((var(sample_medium_overall)/var(sample_low_overall))*pf(1-(confianza_coc_var/2),length(sample_low_overall)-1,length(sample_medium_overall)-1, lower.tail = FALSE),
                             (var(sample_medium_overall)/var(sample_low_overall))*pf((confianza_coc_var/2),length(sample_low_overall)-1,length(sample_medium_overall)-1, lower.tail = FALSE))

# *** Value-Position
coc_var_value_gk_st = c((var(sample_gk_value)/var(sample_st_value))*pf(1-(confianza_coc_var/2),length(sample_st_value)-1,length(sample_gk_value)-1, lower.tail = FALSE),
                          (var(sample_gk_value)/var(sample_st_value))*pf((confianza_coc_var/2),length(sample_st_value)-1,length(sample_gk_value)-1, lower.tail = FALSE))

coc_var_value_gk_other = c((var(sample_gk_value)/var(sample_other_value))*pf(1-(confianza_coc_var/2),length(sample_other_value)-1,length(sample_gk_value)-1, lower.tail = FALSE),
                             (var(sample_gk_value)/var(sample_other_value))*pf((confianza_coc_var/2),length(sample_other_value)-1,length(sample_gk_value)-1, lower.tail = FALSE))

coc_var_value_st_other = c((var(sample_st_value)/var(sample_other_value))*pf(1-(confianza_coc_var/2),length(sample_other_value)-1,length(sample_st_value)-1, lower.tail = FALSE),
                             (var(sample_st_value)/var(sample_other_value))*pf((confianza_coc_var/2),length(sample_other_value)-1,length(sample_st_value)-1, lower.tail = FALSE))

# *** Value-Attack
coc_var_value_high_medium = c((var(sample_high_value)/var(sample_medium_value))*pf(1-(confianza_coc_var/2),length(sample_medium_value)-1,length(sample_high_value)-1, lower.tail = FALSE),
                                (var(sample_high_value)/var(sample_medium_value))*pf((confianza_coc_var/2),length(sample_medium_value)-1,length(sample_high_value)-1, lower.tail = FALSE))

coc_var_value_high_low = c((var(sample_high_value)/var(sample_low_value))*pf(1-(confianza_coc_var/2),length(sample_low_value)-1,length(sample_high_value)-1, lower.tail = FALSE),
                             (var(sample_high_value)/var(sample_low_value))*pf((confianza_coc_var/2),length(sample_low_value)-1,length(sample_high_value)-1, lower.tail = FALSE))

coc_var_value_medium_low = c((var(sample_medium_value)/var(sample_low_value))*pf(1-(confianza_coc_var/2),length(sample_low_value)-1,length(sample_medium_value)-1, lower.tail = FALSE),
                               (var(sample_medium_value)/var(sample_low_value))*pf((confianza_coc_var/2),length(sample_low_value)-1,length(sample_medium_value)-1, lower.tail = FALSE))



# TEMA 6 

# * Contraste de hípotesis bilateral para Overall
# ** a)Planteamos hipótesis nula y alternativa
# *** Nula:Los que tienen high attack rate valen menos o igual que los que tienen medium atack rate, que a su vez valen menos o igual que los que tienen low
# *** Alternativa: Los que tienen high attack rate valen mas que los que tienen medium atack rate, que a su vez valen mas que los que tienen low
# *** Nula: mean_high_value <= mean_medium_value <= mean_low_value
# *** Alternativa: mean_high_value > mean_medium_value > mean_low_value

# **** Nivel de significancia 7%
significance_level =0.07

# *** Se divide en dos pruebas
# **** a-1) Los que tienen high attack rate valen mas o igual que los que tienen medium atack rate
# **** Nula: mean_high_value <= mean_medium_value ---- mean_high_value - mean_medium_value <= 0
# **** Alternativa: mean_high_value > mean_medium_value ---- mean_high_value - mean_medium_value > 0

# **** Estadístico de prueba Z=( (x_barra1 - x_barra2) - (mu1 - mu2)) / raiz( var1/tamaño_muestra1 + var2/tamaño_muestra2)
# **** Distribución del estadístico Z ~ N(0,1)

z_calc_high_med = ((mean(sample_high_value)-mean(sample_medium_value))-(0))/
                      sqrt((pop.var(high_value)/n2) + (pop.var(medium_value)/n2))
rc_high_med = pnorm(significance_level) 
if(z_calc_high_med > rc_high_med){
  print("Se acepta hipotesis alternativa")
} else {
  print("Se rechaza hipotesis alternativa")
}

# **** a-2) Los que tienen medium attack rate valen mas o igual que los que tienen low atack rate
# **** Nula: mean_medium_value <= mean_low_value ---- mean_medium_value - mean_low_value <= 0
# **** Alternativa: mean_medium_value > mean_low_value ---- mean_medium_value - mean_low_value > 0

z_calc_med_low = ((mean(sample_medium_value)-mean(sample_low_value))-(0))/
                    sqrt((pop.var(medium_value)/n2) + (pop.var(low_value)/n2))
rc_med_low = pnorm(significance_level) 
if(z_calc_med_low > rc_med_low){
  print("Se acepta hipotesis alternativa")
} else {
  print("Se rechaza hipotesis alternativa")
}
