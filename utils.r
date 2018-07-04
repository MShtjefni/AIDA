loadPackages <- function(filePath) {

  conn <- file(filePath,open="r")
  listOfPack <-trimws(readLines(conn))
  close(conn)
  remove(conn)
  for (i in which(! listOfPack %in% .packages())){
    tryCatch(library(listOfPack[i],character.only = T), #loads all the libraries needed
    error = function(e) {
      install.packages(toString(listOfPack[i])); #if can't load a library, it'll be downloaded
      library(listOfPack[i],character.only = T)
      } 
    )
  }
}

loadExistingData <- function() {
  return ()
}
