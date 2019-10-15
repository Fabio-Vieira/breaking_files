#Function to count the number of weeks from a vector with week days in Portuguese

week_number <- function(x){
  n <- length(x)
  number <- as.numeric(n)
  for(i in 1:n){
    if(x[i] == "segunda-feira"){ #week days in Portuguese
      number[i] <- 1
    } else if(x[i] == "terça-feira"){
      number[i] <- 2
    } else if(x[i] == "quarta-feira"){
      number[i] <- 4
    } else if(x[i] == "quinta-feira"){
      number[i] <- 5
    } else if(x[i] == "sexta-feira"){
      number[i] <- 6
    } 
  }
  
  aux <- as.numeric(n)
  aux[1] <- 1 #The first element will be always consider as the starting of first week
  
  for(i in 2:n){
    if(number[i] >= number[i-1]){
      aux[i] <- aux[i-1]
    } else {
      aux[i] <- aux[i-1] + 1
    }
  }
  return(aux)
}