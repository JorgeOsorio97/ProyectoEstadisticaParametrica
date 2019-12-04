
cleanPlayerValue <- function(original_value){
  original_value = toString(original_value)
  #print(original_value)
  mult <- 0
  last_digit <- substr(original_value ,nchar(original_value), nchar(original_value))
  if(last_digit=="K"){
    mult <- 1000
  }
  if(last_digit=="M"){
    mult <- 1000000
  }
  if(last_digit==0){
    return(0)
  }
  return(as.numeric(substr(original_value, 2, nchar(original_value)-1)) * mult)
}

cleanPlayerPosition <- function(original_position, main_vals=c("ST","GK")){
  if(original_position %in% main_vals){
    return(toString(original_position))
  } else{
    return("Other")
  }
}

getTwoMaxValues <- function (list){
  c(names(sort(table(list),decreasing=TRUE)[1:2]))
}

# Population statistics
pop.var <- function(x) var(x) * (length(x)-1) / length(x)
pop.sd <- function(x) sqrt(pop.var(x))
pop.cov <- function(x,y) cov(x,y) * (length(x)-1) / length(x)
pop.cor <- function(x,y){ pop.cov(x,y)/(pop.sd(x) * pop.sd(y))}
