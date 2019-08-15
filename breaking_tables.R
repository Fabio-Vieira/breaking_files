#This function was written to break a .docx file with multiple tables within it and transform each table
#into a data frame, and every one of them will be stored into a list.

#It needs the packages "docxtractr", which does similar, but it is not necessarily what I was looking for.

breaking_tables <- function(data, dir){
  if(is.character(data) == FALSE){return(print("data has to be of type character!"))}
  if(is.character(dir) == FALSE){return(print("dir has to be of type character!"))}
  aux.dir <- strsplit(dir, split = as.character())
  #just to make sure that our dir has "/" as its last character to separate the dir's name of our data
  if(tail(aux.dir[[1]], n = 1L) != "/"){return(print("The last character of dir has to be '/'."))}
  suppressWarnings(suppressMessages(library(docxtractr)))
  suppressWarnings(suppressMessages(library(rlist)))
  data <- read_docx(paste0(dir, data))
  
  tbs <- docx_extract_all_tbls(data)
  tbs <- lapply(tbs, as.data.frame) #we need to transform everything in our list into data frames
  
  is.line.empty <- function(x){#A function to test if a line or a column is empty, because we'll need to breaking the data frames
    n <- length(x)             #inside the list into tables or because we'll need to erase empty columns in our tables
    if(sum(is.na(x)) == n){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  for(i in 1:length(tbs)){
    tbs[[i]][tbs[[i]] == ""] <- NA
  }
  
  tables <- list()
  for(i in 1:length(tbs)){
    l <- 1
    n <- rep(0,nrow(tbs[[i]]))
    for(j in l:nrow(tbs[[i]])){
      if(is.line.empty(tbs[[i]][j,]) == TRUE){
        tables <- list.append(tables, tbs[[i]][l:j-1,])
        l <- j + 1
      }
    }
  }
  
  for(i in 1:length(tables)){
    cols <- as.numeric(ncol(tables[[i]]))
    for(j in 1:ncol(tables[[i]])){
      if(is.line.empty(tables[[i]][,j]) == TRUE){
        cols[j] <- j
      }
    }
    tables[[i]] <- tables[[i]][,-cols[is.na(cols) == F]]
  }
 return(tables) #return a list with all tables 
}
