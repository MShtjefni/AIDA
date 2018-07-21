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
  colNames = c("Ateco", "Company", "Form", "Province", "Region", "Status", "TaxID", "Year", 
            "B", "E", "P", "R", "Infl", "GeoArea", "Size", "Growth")
  
  tryCatch({
    suppressWarnings(warning(remove(aidat)))
    for (name in colNames) {
      load(paste0(dataDir, name, ".RData"))
      if (!exists("origData"))
        origData<-eval(parse(text = name))
      else
        origData<-cbind(origData, eval(parse(text = name)))
      remove(list=name)
    }
    print("Loading from distinct column files.")
  
  },
  
  error = function(e) {
    print("Loading aidat from file") 
    tryCatch( {
      load(paste0(dataDir,"aidat.RData"))
      origData<<-aidat
      for (name in colNames ) {
        assign(toString(name), aidat[name])
        save(list=name, file = paste0("data/",name,".RData"))
        remove(list=name)
      }
    }, 
    error = function(e) {
      stop("No aidat file neither distinct column files found to load datasets.")
    })
    
  },
  
  finally = {
    
    " Preprocessing steps: changing column types for Ateco(int), TaxID(num), year(int). 
    Removing TradingRegion and TradingProvince columns "
    origData$Ateco <- as.integer(as.character(origData$Ateco))
    print("qua2")
    origData$TaxID <- as.numeric(as.character(origData$TaxID))
    origData$Year <- as.integer(as.character(origData$Year))
    origData[which(names(origData) %in% c("TradingRegion","TradingProvince"))] <- NULL
    origData<-applyInflation(origData)
    origData<-addGeoArea(origData)
    origData<-addSize(origData)
    
    print("Getting sectors subsets")
    aida<<-subset(origData, Year>2006 & Year<2016 & R>=0 & E>=0); remove(origData)
    
    #manufacturing <- subset(aidat,Ateco>=101100 & Ateco<=332009 & Year>2006 & Year<2016) #subset containing all the manufacturing firms from original dataset
    manufacturing <- subset(aida,Ateco>=101100 & Ateco<=332009 & !is.na(R) & !is.na(E))  # removing all the rows having missing values for Revenue or Employee and keeping just 2007-2015 years records
    manufacturing <<- addSubsectorColumn(manufacturing)
    #manufacturing <- arrange(manufacturing,TaxID,Year) #sort manufacturing firms by TaxID and by year
    
    restaurants<<-subset(aida,Ateco>=550000 & Ateco<570000) #subset containing all the manufacturing firms from original dataset
    media<<-subset(aida,Ateco>=580000 & Ateco<640000) #subset containing all the manufacturing firms from original dataset
    
    })
  
  }

loadPackages(paste(wdir,packagesFile,sep = ""))
