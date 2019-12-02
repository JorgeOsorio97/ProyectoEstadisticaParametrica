data <- read.csv("fifa19.csv")
cuantitative <- subset(data, selecct = c(Value,Overall))
cualitative <- subset(data, select = c(Work.Rate, Position))

cuantitative$Value
cualitative$Work.Rate

