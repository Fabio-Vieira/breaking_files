growth_rate <- function(year1, year2, pop_year1, pop_year2, monthly = FALSE){
  diff <- abs(year1 - year2)
  if(monthly == TRUE){
    if(diff == 1){
      return((((pop_year2/pop_year1)) - 1)/12)
    } else {
      return((((pop_year2/pop_year1)^(1/diff)) - 1)/12)
    }
  } else {
      if(diff == 1){
        return((pop_year2/pop_year1) - 1)
      } else {
        return(((pop_year2/pop_year1)^(1/diff)) - 1)
      }
  } 
}
