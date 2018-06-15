# wdir <- ""

source(paste(wdir, "first_analysis.r", sep=""))
#summary(aidat)

View(nine_year_firms)
nine_year_firms2$Size = "NA"
nine_year_firms2$Size[nine_year_firms2$E <= 49] <- "Small"
nine_year_firms2$Size[50 <= nine_year_firms2$E & nine_year_firms2$E <= 249] <- "Medium"
nine_year_firms2$Size[nine_year_firms2$E >= 250] <- "Large"
nine_year_firms2$Growth <- NA

nine_year_firms2$R[nine_year_firms2$R <=1 ] <- 1
allDensities = list()

subsectorsGrowth <- function(data) {
  for (subs in unique(data$Subsector)[1:2]) {
    #View(nine_year_firms)
    sectorFirms <- subset(data, Subsector == subs)
    #View(sectorFirms)
    by_firm <- group_by(sectorFirms,TaxID, Year)
    by_firm <- summarize(by_firm, Revenue=R)

    by_subsector <- group_by(sectorFirms, Year) %>%
      summarize(sumOfRevenues=sum(R))

    by_firm$Growth<- NA
    #View(by_firm)
    average<-(log10(by_subsector$sumOfRevenues))/nrow(by_firm)
    for (row in 2:nrow(by_firm)) {
      if (by_firm[row, "TaxID"] != by_firm[row-1, "TaxID"]) next
      #if (by_firm[row, "Year"] - by_firm[row-1, "Year"] != 1) next
      by_firm$Growth[row] <- (log10(by_firm[row, "Revenue"]) - average) - (log10(by_firm[row-1, "Revenue"]) - average)
    }

    growth <-  by_firm$Growth[!is.na(by_firm$Growth)]
    growth <- as.numeric(growth)
    d <- density(growth) # returns the density data
    plot(d) # plots the results
    append(allDensities,d)
    #View(by_firm)

    data [data$Subsector==subs,]<-group_by(data [data$Subsector==subs,],TaxID,Year)
    data$Growth [data$Subsector==subs]<-by_firm$Growth 

  }
  return (data)
}


by_firm <- group_by(food_sector,TaxID, Year)
by_firm <- summarize(by_firm, Revenue=sum(R))

by_firm$Growth<- NA

#average<-(sum(log10(by_firm$Revenue))/nrow(by_firm))
for (row in 2:nrow(by_firm)) {
  if (by_firm[row, "TaxID"] != by_firm[row-1, "TaxID"]) next
  #if ((by_firm[row, "Year"] - by_firm[row-1, "Year"]) != 1) next
  by_firm$Growth[row] <- log10(by_firm[row, "Revenue"]) - log10(by_firm[row-1, "Revenue"])
  #by_firm$Growth[row] <- (log10(by_firm[row, "Revenue"]) - year_average$tot[year_average$Year==as.numeric(by_firm[row, "Year"])]) - (log10(by_firm[row-1, "Revenue"]) - year_average$tot[year_average$Year==as.numeric(by_firm[row-1, "Year"])])
}
View(by_firm)
growth <-  by_firm$Growth[!is.na(by_firm$Growth)]
growth <- as.numeric(growth)
mean(growth)
d <- density(growth) # returns the density data 
plot(d) # plots the results
hist(growth, prob=T, breaks="Scott", xlim=c(-1, 1))