#This function was written to break a .docx file with multiple tables within it and transform each table
#into a data frame, and every one of them will be stored into a list.

breaking_tables <- function(data){
  library(docxtractr)
  tabs <- docx_extract_all_tbls(data,1)[[1]]
  
  for(i in 1:nrow(tabs)){
    for(j in 1:ncol(tabs)){
      if(isTRUE(tabs[i,j] == "")){ tabs[i,j] <- NA}
    }
  }
  
  is.line.empty <- function(x){ #A function to test if an entire line or column is empty.
    n <- length(x)
    line <- NULL
    for(i in 1:n){
      if(is.na(x[i]) == T){
        line[i] <- 1
      } else {
        line[i] <- 0
      }
    }
    if(sum(line) == n){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  
  ind <- NULL
  for(i in 1:nrow(tabs)){
    if(is.line.empty(tabs[i,]) == T){
      ind[i] <- 1
    } else {
      ind[i] <- 0
    }
  }
  
  tabs$ind <- ind
  tables <- list()
  
  aux.table <- NULL; l <- 1
  for(j in 1:24){
    aux.table <- NULL
    for(i in l:nrow(tabs)){
      if(tabs$ind[i] == 1){
        l <- i + 1
        break
      } else{
        aux.table <- rbind(aux.table, tabs[i,1:10])
      }
      tables[[j]] <- aux.table
    }
  }
  
  tables <- lapply(tables, as.data.frame)
  aux.tables <- list()
  
  for(i in 1:length(tables)){
    cols <- NULL
    if(ncol(tables[[i]]) == 0){
      next
    } else {
      for(j in 1:ncol(tables[[i]])){
        if(is.line.empty(tables[[i]][,j]) == T){
          cols <- c(cols, j)
          #print(cols)
        }
      }
      aux.tables[[i]] <- tables[[i]][-c(cols)]
    }
  }
  return(aux.tables)
}