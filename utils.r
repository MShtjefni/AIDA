loadPackages <- function(filePath) {

  conn <- file(filePath,open="r")
  listOfPack <-readLines(conn)
  close(conn)
  remove(conn)
  for (i in which(! listOfPack %in% .packages())){
    tryCatch(library(listOfPack[i],character.only = T), #loads all the libraries needed
    error = function(e) {install.packages(toString(listOfPack[i]))} #if it can't load a library, it'll download it
    )
    }
  }
