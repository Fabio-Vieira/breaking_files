#This function was written to break a .docx file with multiple tables within it and transform each table
#into a data frame, and every one of them will be stored into a list. There is a bug, because somehow it
#doesn't get the last table in the file and I still don't know why :'(

#It needs the packages "docxtractr", which does similar, but it is not necessarily what I was looking for.

breaking_tables <- function(data){
  suppressWarnings(library(docxtractr))
  tabs <- docx_extract_all_tbls(data)
  
  if(length(tabs) > 1){
    aux.tabs <- NULL
    for(i in 1:length(tabs)){
      aux.tabs[[i]] <- as.data.frame(tabs[[i]])
    }
  }
  
  for(i in 1:length(aux.tabs)){
    for(l in 1:nrow(aux.tabs[[i]])){
      for(j in 1:ncol(aux.tabs[[i]])){
      if(isTRUE(aux.tabs[[i]][l,j] == "")){ aux.tabs[[i]][l,j] <- NA}
      }
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

  tables <- list()
  for(i in 1:length(aux.tabs)){
    l <- 1
    aux.table <- NULL
    for(j in l:nrow(aux.tabs[[i]])){
      if(is.line.empty(aux.tabs[[i]][j,]) == FALSE){
        next
      } else {
        tables <- append(tables, list(aux.tabs[[i]][l:j,]))
        l <- j + 1
      }
    }
  }
  
  aux.tables <- vector(mode = "list", length(tables))
  for(i in 1:length(tables)){
    cols <- NULL
    for(j in 1:ncol(tables[[i]])){
      if(sum(!is.na(tables[[i]][,j])) == 0){
        cols <- c(cols, paste0("V",j))
      }
      aux.tables[[i]] <- tables[[i]][,which(cols %in% names(tables[[i]]))]
    }
  }
  return(list(tables = aux.tables))
}
