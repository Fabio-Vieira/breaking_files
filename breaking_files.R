#This function breaks down a file that is too big for you to work with. In my case, I use this function to break down huge .sav
#files, so I can open them in small parts using excel. All you have to do is specify the data you want to break down, then a 
#directory, the maximum number of rows you want in your final files, and the name you want to give them. Just be careful with the
#fact that your director name and your file names need to be strings. Also, the very last character of your directory name needs
#to be a "/", this way R will understand what you're trying to do, which is dir/file_name, that's why you need the "/", to separate
#the dir name from the file_name. The resulting types of your files will be .csv

breaking_files <- function(data, dir, n, file_name){
  library(data.table)
  time <- Sys.time()
  if(nrow(data) < n){
    name <- paste0(dir, file_name, ".csv")
    fwrite(data, file = name, row.names = FALSE, sep = ";")
    print(paste0(file_name, " is saved!"))
    setwd(dir)
    zip(zipfile = paste0(file_name, ".zip"), files = paste0(file_name, ".csv"))
    unlink(name)
  } else {
    list_files <- NULL
    k <- trunc(nrow(data)/n + 1)
    min <- 1
    max <- n
    for(i in 1:k){
      name <- paste0(dir, file_name, "_part_", i, ".csv")
      list_files[i] <- paste0(file_name, "_part_", i)
      if(max > nrow(data)){
        fwrite(data[min:nrow(data),], file = name, row.names = FALSE, sep = ";")
      } else {
        fwrite(data[min:max,], file = name, row.names = FALSE, sep = ";")
      }
      min <- max + 1
      max <- n * (i+1)
        print(paste0(file_name, "_part_", i))
    }
    setwd(dir)
    zip(zipfile = paste0(file_name, ".zip"), files = paste0(list_files, ".csv"))
    unlink(paste0(list_files, ".csv"))
  }
  print(difftime(Sys.time(), time, units = "mins"))
}