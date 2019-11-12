growth_rate <- function(year1, year2, pop_year1, pop_year2){
  diff <- abs(year1 - year2)
  if(diff == 1){
    return((pop_year2/pop_year1) - 1)
  } else {
    return(((pop_year2/pop_year1)^(1/diff)) - 1)
  }
}