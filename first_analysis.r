#rm(list = ls()) #remove all the existing environment vars

packagesFile <- "packages.txt"
dataFile <- "data/aidat.RData"
utilsFile <- "utils.r"
#print(wdir)
source(paste(wdir, "utils.r", sep=""))
loadPackages(paste(wdir,packagesFile,sep = ""))


" Preprocessing steps: changing column types for Ateco(int), TaxID(num), year(int). Removing TradingRegion and TradingProvince columns "
if(!exists ("aidat")) load(paste(wdir,dataFile,sep = ""))
aidat$Ateco <- as.integer(as.character(aidat$Ateco))
aidat$TaxID <- as.numeric(as.character(aidat$TaxID))
aidat$Year <- as.integer(as.character(aidat$Year))
aidat[which(names(aidat) %in% c("TradingRegion","TradingProvince"))] <- NULL
inflactions <- c(1, .032, .007, .016, .027, .03, .011, .002, -0.001, -0.001, .011) #inflations from 2007 to 2017, by ISTAT
aidat$Infl <- NA

#for (i in seq(9)) {aidat$Infl[aidat$Year==2006+i]<-inflations[i]}

"industrieAlim<-[10,11)
industrieBev<-[11,12)
indTab<-[12,13)
indTess<-[13,14)
confezAbbigl<-[14,15)
pelle<-[15,16)
legno<-[16,17)
carta<-[17,18)
stampa<-[18,19)
fabbrCoke<-[19,20)
prodChimici<-[20,21)
prodFarm<-[21,22)
gomma<-[22,23)
minerali<-[23,24)
metallurgia<-[24,25)
prodMetallo<-[25,26)
computer<-[26,27)
appElettriche<-[27,28)
macchinari<-[28,29)
autoveicoli<-[29,30)
mezziTrasp<-[30,31)
mobili<-[31,32)
altreManuf<-[32,33)
riparaz<-[33,34)
"
"Apply inflaction function"
applyInflaction <- function(data, inflactions=c(1, .032, .007, .016, .027, .03, .011, .002, -0.001, -0.001, .011)) {
  
  if (!"Infl" %in% colnames(data)) 
    data$Infl <- NA
  for (i in seq(2,9)) {
    inflactions[i]<-(inflactions[i-1]*(1+inflactions[i]))
    print(inflactions[i])
    data$Infl[data$Year==2006+i]<-inflactions[i]
  }
  
  data$P <- data$P/data$Infl
  data$R <- data$R/data$Infl
  data$E <- data$E/mdata$Infl
  return(data)
}

"Subset of the only manufacturing firms. Adding subsector column"

addSubsectorColumn <- function(data) {
  subsectors<-list(subsector=c('alimentari','bevande','tabacco','tessile','confezAbbigl','pelle','legno','carta','stampa', 'fabbrCoke','prodChimici','prodFarm','gomma','minerali','metallurgia','prodMetallo','computer','appElettriche','macchinari','autoveicoli', 'mezziTrasp', 'mobili', 'altreManuf', 'riparaz'), min=c(100000,110000,120000,130000,140000,150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000,270000,280000,290000,300000,310000,320000,330000), max=c(110000,120000,130000,140000,150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000,270000,280000,290000,300000,310000,320000,330000,340000))
  data$Subsector='NA'
  for(i in seq(length(subsectors$subsector))) #mapping subsector to each row based on Ateco
  data$Subsector[data$Ateco>=subsectors$min[i] & data$Ateco<subsectors$max[i]] <- subsectors$subsector[i]
  return (data) }

  manufacturing <- subset(aidat,aidat$Ateco>=101100 & aidat$Ateco<=332009 & aidat$Year>=2007 & aidat$Year<2016) #subset containing all the manufacturing firms from original dataset
  manufacturing <- addSubsectorColumn(manufacturing)
  manufacturing <- filter(manufacturing,R>=0 | is.na(R)) #removing all the entries with negative(<0) revenue
  manufacturingNoMiss <- filter(manufacturing, !is.na(R) & !is.na(E))  # removing all the rows having missing values for Revenue or Employee
  #manufacturing <- arrange(manufacturing,TaxID,Year) #sort manufacturing firms by TaxID and by year


"Useful subsets regarding firms which have missing values for Employee column"
#unId<-unique(manufacturing$TaxID) #vector containing all the unique taxIDs from manufacturing firms
onlyMissEmployees <- subset(manufacturing,is.na(manufacturing$E)) #subset of manufacturing firms having at least one "NA" as Employee
missingEmpID <- unique(onlyMissEmployees$TaxID) #vector of unique taxIDs having at least one "NA" as Employee
#tmp <- group_by(onlyMissEmployees,TaxID)
#distinctEmpByID <- summarize(tmp,countE=n()) #grouping by TaxId and counting the number of NA for each TaxId
#distinctEmpByID <- arrange(distinctEmpByID, countE)

firmsMissingEmpRows <- filter(manufacturing, manufacturing$TaxID %in% missingEmpID) #subset of rows containing only the firms having at least one "NA" as Employee
#missingEmpSortByTaxID <- arrange(firmsMissingEmpRows,TaxID,E,Year) #sorting the last subset by TaxID, N°Employee and Year
#tmp <- group_by(missingEmpSortByTaxId,TaxID)
#distinctEmpByID2 <- summarize(tmp,countE=n()) #grouping by TaxId and counting the number of rows for each TaxId
#distinctEmpByID2 <- arrange(distinctEmpByID2, countE)

"Counting the Employee missing values for each firm"
countMissingByTaxID <- filter(onlyMissEmployees, is.na(onlyMissEmployees$E)) %>%
group_by(taxID = onlyMissEmployees$TaxID) %>%
summarise(numMissing = n())
countMissingByTaxID$numYears <- count(firmsMissingEmpRows, firmsMissingEmpRows$TaxID)$n # associating to each firm(taxID) the number of missing values and the number of years the firm appears in the whole dataset
countMissingByTaxID$percMissing <- countMissingByTaxID$numMissing / countMissingByTaxID$numYears #associating the percent of missing values for Employee to each taxID
countMissingByTaxID <- arrange(countMissingByTaxID, percMissing, numYears)

tmpC<-group_by(countMissingByTaxID, percMissing)
tmpC<-count(tmpC, percMissing)
plot(tmpC$percMissing,tmpC$n,type = "l", xlab = "percMissing", ylab = "count")


#### ROWS AND TAX IDs OF FIRMS HAVING AT LEAST 1 NA AS REVENUE ####
onlyMissRevenues <- subset(manufacturing,is.na(manufacturing$R)) #subset of manufacturing firms having at least one "NA" as Employee
missingRevID <- unique(onlyMissRevenues$TaxID) #vector of unique taxIDs having at least one "NA" as Employee
firmsMissingRevRows <- filter(manufacturing, manufacturing$TaxID %in% missingRevID) #subset containing only the firms having at least one "NA" as Employee

#### SELECTING ALL THE ROWS OF FIRMS HAVING ALL THE ENTRIES FOR YEARS 2006-2015 ####
nonMissingRevenues<-filter(manufacturing,! is.na(R)) #removing all the rows having missing values for Revenue
nine_years_TaxID<-group_by(nonMissingRevenues,TaxID) %>%
  count()
nine_years_TaxID<-filter(nine_years_TaxID,n==9)
nine_year_firms<-filter(nonMissingRevenues, nonMissingRevenues$TaxID %in% nine_years_TaxID$TaxID) %>%
arrange(TaxID,Year) #subset containing only the firms having entries for each year(between 2007 and 2016) and Revenue NOT missing


nine_years_TaxID2<-group_by(manufacturingNoMiss,TaxID) %>%
  count()
nine_years_TaxID2<-filter(nine_years_TaxID2,n==9)
nine_year_firms2<-filter(manufacturingNoMiss, manufacturingNoMiss$TaxID %in% nine_years_TaxID2$TaxID) %>%
arrange(TaxID,Year) #subset containing only the firms having entries for each year(between 2007 and 2016) with Revenue and Employee NOT missing
