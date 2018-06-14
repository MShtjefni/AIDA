library(dplyr)
data<-read.table("AIDA.txt", header=T)
#summary(data)
attach(data) 
manufacture<-subset(data,Ateco>= 101100 & Ateco<=332009 & Year>=2007 & Year<=2015 & R>=0 & !is.na(E))
manufacture <- manufacture[, -c(3,4,8,9)]
detach(data)
mfc <- group_by(manufacture, manufacture$TaxID)
mfc <- summarize(mfc, N_of_Years=n())
nine_year_firms <- filter(mfc, mfc$N_of_Years ==9)
nine_year_firms <- filter(manufacture, manufacture$TaxID %in% nine_year_firms$`manufacture$TaxID`)
View(nine_year_firms)
nine_year_firms$Size[nine_year_firms$E <= 49] <- "Small"
nine_year_firms$Size[50 <= nine_year_firms$E & nine_year_firms$E <= 249] <- "Medium"
nine_year_firms$Size[nine_year_firms$E >= 250] <- "Large"

nine_year_firms$R[nine_year_firms$R <=1 ] <- 1 

#View(nine_year_firms)
food_sector <- subset(nine_year_firms, Ateco>=101100 & Ateco <=109200)
#View(food_sector)
by_firm <- group_by(food_sector,TaxID, Year)
by_firm <- summarize(by_firm, Revenue=sum(R))

by_firm$Growth<- NA
#View(by_firm)
average<-(sum(log10(by_firm$Revenue))/nrow(by_firm))
for (row in 2:nrow(by_firm)) {
  if (by_firm[row, "TaxID"] != by_firm[row-1, "TaxID"]) next 
  by_firm$Growth[row] <- (log10(by_firm[row, "Revenue"]) - average) - (log10(by_firm[row-1, "Revenue"]) - average)
}

growth <-  by_firm$Growth[!is.na(by_firm$Growth)]
growth <- as.numeric(growth)
d <- density(growth) # returns the density data 
plot(d) # plots the results




