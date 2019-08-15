#It needs the packages "docxtractr", which does similar, but it is not necessarily what I was looking for.

#This function was written to break a .docx file with multiple tables within it and transform each table
#into a data frame, and every one of them will be stored into a list. I have finally corrected it, now it is able to get all
#the tables in the .docx file. However, it probably won't work if your .docx has anything but tables in it.

breaking_tables <- function(data, dir){
  if(is.character(data) == FALSE){return(print("data has to be of type character!"))}
  if(is.character(dir) == FALSE){return(print("dir has to be of type character!"))}
  aux.dir <- strsplit(dir, split = as.character())
  #You need to have a "/" at the end of your dir name, because R will need to be able to understand what is the dir and what is
  #the file name when we paste them together, which happens on line 16.
  if(tail(aux.dir[[1]], n = 1L) != "/"){return(print("The last character of dir has to be '/'."))}
  suppressWarnings(suppressMessages(library(docxtractr)))
  suppressWarnings(suppressMessages(library(rlist)))
  data <- read_docx(paste0(dir, data))
  
  tbs <- docx_extract_all_tbls(data)
  tbs <- lapply(tbs, as.data.frame)
  is.line.empty <- function(x){ #this function tests if a line or a column is empty, because we'll need to break a bigger table
    n <- length(x)              #extracted from the file on line 18 in case that bigger table is actually smaller ones that the
    if(sum(is.na(x)) == n){     #docx_extract_all_tbls function didn't undestand. We'll also need to clean empty columns in the end
      return(TRUE)              #to get cleaner tables.
    } else {
      return(FALSE)
    }
  }
  
  for(i in 1:length(tbs)){
    tbs[[i]][tbs[[i]] == ""] <- NA
  }
  
  tables <- list()
  for(i in 1:length(tbs)){ #this loop is when we actually break those bigger tables into the smaller ones we want.
    l <- 1
    n <- rep(0,nrow(tbs[[i]]))
    if(sum(apply(tbs[[i]], 1, is.line.empty)) == 0){
      tables <- list.append(tables, tbs[[i]])
    } else {
      for(j in l:nrow(tbs[[i]])){
        if(is.line.empty(tbs[[i]][j,]) == TRUE){
          tables <- list.append(tables, tbs[[i]][l:j-1,])
          l <- j + 1
        }
      }
    }
  }
  
  for(i in 1:length(tables)){ #this step just cleans additional columns that shouldn't be there.
    cols <- as.numeric(ncol(tables[[i]]))
    for(j in 1:ncol(tables[[i]])){
      if(is.line.empty(tables[[i]][,j]) == TRUE){
        cols[j] <- j
      }
    }
    tables[[i]] <- tables[[i]][,-cols[is.na(cols) == F]]
  }
  return(tables) 
}
