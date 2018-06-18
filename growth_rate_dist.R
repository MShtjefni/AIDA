wdir <- ""
source(paste(wdir, "first_analysis.r", sep=""))
#summary(aidat)

View(nine_year_firms)
nine_year_firms2$Size <- NA
nine_year_firms2$Size[nine_year_firms2$E <= 49] <- "Small"
nine_year_firms2$Size[50 <= nine_year_firms2$E & nine_year_firms2$E <= 249] <- "Medium"
nine_year_firms2$Size[nine_year_firms2$E >= 250] <- "Large"



subsectorsGrowth <- function(data) {
  data$R<-data$R + 1
  data$SubsectorGrowth <- NA
  for (subs in unique(data$Subsector)) {
    subsectorFirms <- subset(data, Subsector == subs)
    by_firm <- group_by(subsectorFirms,TaxID, Year)
    by_firm$Growth<- NA

    revSum <- group_by(subsectorFirms,Year) %>%
      summarise(sumOfRevenues=sum(R))
    distinctYears <- unique(subsectorFirms$Year)
    subsAverages <- list()
    
    for (y in distinctYears) {
      average<-(log10(revSum$sumOfRevenues[revSum$Year==y]))/nrow(subsectorFirms[subsectorFirms$Year==y ,])
      subsAverages[y]=average
    }
    
    for (row in 2:nrow(by_firm)) {
      if (by_firm[row, "TaxID"] != by_firm[row-1, "TaxID"]) next
      #if (by_firm[row, "Year"] - by_firm[row-1, "Year"] != 1) next
      currYear <- by_firm$Year[row]
      currAverage <- subsAverages[currYear]
      prevAverage <- subsAverages[currYear-1]
      by_firm$Growth[row] <- (log10(by_firm[row, "R"])) - currAverage - (log10(by_firm[row-1, "R"])) - prevAverage
    }

    growth <-  by_firm$Growth[!is.na(by_firm$Growth)]
    growth <- as.numeric(growth)
    d <- density(growth) # returns the density data
    plot(d) # plots the results
    View(growth)    #View(by_firm)

    data [data$Subsector==subs,]<-group_by(data [data$Subsector==subs,],TaxID,Year)
    data$SubsectorGrowth [data$Subsector==subs]<-by_firm$Growth 
    break
  }
  return (data)
}

singleFirmGrowth <- function(data) {
  data$R<-data$R + 1
  data$SingleFirmGrowth <- NA
  for (subs in unique(data$Subsector)) {
    sectorFirms <- subset(data, Subsector == subs)
    
    by_firm <- group_by(sectorFirms,TaxID, Year)
    by_firm <- summarize(by_firm, Revenue=sum(R))

    by_firm$Growth<- NA

    #average<-(sum(log10(by_firm$Revenue))/nrow(by_firm))
    for (row in 2:nrow(by_firm)) {
      if (by_firm[row, "TaxID"] != by_firm[row-1, "TaxID"]) next
      #if ((by_firm[row, "Year"] - by_firm[row-1, "Year"]) != 1) next
      by_firm$Growth[row] <- log10(by_firm[row, "Revenue"]) - log10(by_firm[row-1, "Revenue"])
      #by_firm$Growth[row] <- (log10(by_firm[row, "Revenue"]) - year_average$tot[year_average$Year==as.numeric(by_firm[row, "Year"])]) - (log10(by_firm[row-1, "Revenue"]) - year_average$tot[year_average$Year==as.numeric(by_firm[row-1, "Year"])])
    }
    #View(by_firm)
    growth <-  by_firm$Growth[!is.na(by_firm$Growth)]
    growth <- as.numeric(growth)
    #mean(growth)
    
    d <- density(growth) # returns the density data 
    plot(d) # plots the results
    hist(growth, prob=T, breaks="Scott", xlim=c(-1, 1))
    
    data [data$Subsector==subs,]<-group_by(data [data$Subsector==subs,],TaxID,Year)
    data$SingleFirmGrowth [data$Subsector==subs]<-by_firm$Growth
    break
  }
  return(data)  
}
  
getGrowthDensities <- function(data, growthColumn) {
  densities <- list()
  for (subs in unique(data$Subsector)) {
    growth <- data[growthColumn] [data$Subsector == subs ,]
    densities[subs] = growth
  }
  return (densities)
}
  