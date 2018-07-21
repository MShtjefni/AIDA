loadPackages <- function(filePath) {

  conn <- file(filePath,open="r")
  listOfPack <-trimws(readLines(conn))
  close(conn)
  remove(conn)
  print("Loading all needed packages into the current workspace...")
  for (i in which(! listOfPack %in% .packages())){
    tryCatch(library(listOfPack[i],character.only = T), #loads all the libraries needed
    error = function(e) {
      install.packages(toString(listOfPack[i])); #if can't load a library, it'll be downloaded
      library(listOfPack[i],character.only = T)
      } 
    )
  }
  print("Done. All packages currently loaded and ready to be used.")
}


loadDatasets <- function(dataDir) {
  " Preprocessing steps: changing column types for Ateco(int), TaxID(num), year(int). Removing TradingRegion and TradingProvince columns "
  print("Loading aidat from file")
  load(dataDir)
  aidat$Ateco <- as.integer(as.character(aidat$Ateco))
  aidat$TaxID <- as.numeric(as.character(aidat$TaxID))
  aidat$Year <- as.integer(as.character(aidat$Year))
  aidat[which(names(aidat) %in% c("TradingRegion","TradingProvince"))] <- NULL
  aidat<-applyInflation(aidat)
  aidat<-addGeoArea(aidat)
  aidat<-addSize(aidat)
  
  print("Getting sectors subsets")
  aida<<-subset(aidat, Year>2006 & Year<2016 & R>=0 & E>=0); remove(aidat)
  
  #manufacturing <- subset(aidat,Ateco>=101100 & Ateco<=332009 & Year>2006 & Year<2016) #subset containing all the manufacturing firms from original dataset
  manufacturing <- subset(aida,Ateco>=101100 & Ateco<=332009 & !is.na(R) & !is.na(E))  # removing all the rows having missing values for Revenue or Employee and keeping just 2007-2015 years records
  manufacturing <<- addSubsectorColumn(manufacturing)
  #manufacturing <- arrange(manufacturing,TaxID,Year) #sort manufacturing firms by TaxID and by year
  
  
  restaurants<<-subset(aida,Ateco>=550000 & Ateco<570000) #subset containing all the manufacturing firms from original dataset
  
  media<<-subset(aida,Ateco>=580000 & Ateco<640000) #subset containing all the manufacturing firms from original dataset
}

loadPackages(paste(wdir,packagesFile,sep = ""))
