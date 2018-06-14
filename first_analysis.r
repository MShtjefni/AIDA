# install.packages("dplyr")
# library("dplyr")
# wdir <- "/home/honestus/UNIVERSITÀ/SMD/PROGETTO/data/"
load(paste(wdir,"aidat.RData",sep = ""))
aidat$Ateco <- as.integer(as.character(aidat$Ateco))
aidat$TaxID <- as.numeric(as.character(aidat$TaxID))
aidat$Year <- as.integer(as.character(aidat$Year))
aidat[which(names(aidat) %in% c("TradingRegion","TradingProvince"))] <- NULL

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



manufacturing <- subset(aidat,aidat$Ateco>=101100 & aidat$Ateco<=332009 & aidat$Year>=2007 & aidat$Year<2016) #subset containing all the manufacturing firms from original dataset
manufacturing <- filter(manufacturing,R>=0 | is.na(R)) #removing all the entries with negative(<0) revenue
#manufacturing <- arrange(manufacturing,TaxID,Year) #sort manufacturing firms by TaxID and by year

subsectors<-list(subsector=c('alimentari','bevande','tabacco','tessile','confezAbbigl','pelle','legno','carta','stampa', 'fabbrCoke','prodChimici','prodFarm','gomma','minerali','metallurgia','prodMetallo','computer','appElettriche','macchinari','autoveicoli', 'mezziTrasp', 'mobili', 'altreManuf', 'riparaz'), min=c(100000,110000,120000,130000,140000,150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000,270000,280000,290000,300000,310000,320000,330000), max=c(110000,120000,130000,140000,150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000,270000,280000,290000,300000,310000,320000,330000,340000))
#manufacturing$Subsector='NA'
for(i in seq(length(subsectors$subsector))) manufacturing$Subsector[manufacturing$Ateco>=subsectors$min[i] & manufacturing$Ateco<subsectors$max[i]]<- subsectors$subsector[i]


#unId<-unique(manufacturing$TaxID) #vector containing all the unique taxIDs from manufacturing firms
onlyMissEmployees <- subset(manufacturing,is.na(manufacturing$E)) #subset of manufacturing firms having at least one "NA" as Employee
missingEmpID <- unique(onlyMissEmployees$TaxID) #vector of unique taxIDs having at least one "NA" as Employee
#tmp <- group_by(onlyMissEmployees,TaxID)
#distinctEmpByID <- summarize(tmp,countE=n()) #grouping by TaxId and counting the number of NA for each TaxId
#distinctEmpByID <- arrange(distinctEmpByID, countE)

firmsMissingEmpRows <- filter(manufacturing, manufacturing$TaxID %in% missingEmpID) #subset containing only the firms having at least one "NA" as Employee
#missingEmpSortByTaxID <- arrange(firmsMissingEmpRows,TaxID,E,Year) #sorting the last subset by TaxID, N°Employee and Year
#tmp <- group_by(missingEmpSortByTaxId,TaxID)
#distinctEmpByID2 <- summarize(tmp,countE=n()) #grouping by TaxId and counting the number of objects for each TaxId
#distinctEmpByID2 <- arrange(distinctEmpByID2, countE)


countMissingByTaxID <- filter(onlyMissEmployees, is.na(onlyMissEmployees$E)) %>%
group_by(taxID = onlyMissEmployees$TaxID) %>%
summarise(numMissing = n())
countMissingByTaxID$numYears <- count(firmsMissingEmp, firmsMissingEmpRows$TaxID)$n # associating to each firm(taxID) the number of missing values and the number of years the firm appears in the whole dataset
countMissingByTaxID$percMissing <- countMissingByTaxID$numMissing / countMissingByTaxID$numYears
countMissingByTaxID <- arrange(countMissingByTaxID, percMissing, numYears)

tmpC<-group_by(countMissingByTaxID, percMissing)
tmpC<-count(tmpC, percMissing)
plot(tmpC$percMissing,tmpC$n,type = "l", xlab = "percMissing", ylab = "count")


#### ROWS AND TAX IDs OF FIRMS HAVING AT LEAST 1 NA AS REVENUE ####
onlyMissRevenues <- subset(manufacturing,is.na(manufacturing$R)) #subset of manufacturing firms having at least one "NA" as Employee
missingRevID <- unique(onlyMissRevenues$TaxID) #vector of unique taxIDs having at least one "NA" as Employee
firmsMissingRevRows <- filter(manufacturing, manufacturing$TaxID %in% missingRevID) #subset containing only the firms having at least one "NA" as Employee

#### SELECTING ALL THE ROWS OF FIRMS HAVING ALL THE ENTRIES FOR YEARS 2006-2015 ####
nonMissingRevenues<-filter(manufacturing,! is.na(R))
allConsecutiveTaxID<-group_by(nonMissingRevenues,TaxID) %>%
  count()
allConsecutiveTaxID<-filter(allConsecutiveTaxID,n==9)
allConsecutiveFirms<-filter(nonMissingRevenues, nonMissingRevenues$TaxID %in% allConsecutiveTaxID$TaxID) %>%
arrange(TaxID,Year) #subset containing only the firms having entries for each year(between 2007 and 2016)

nonMissingRandE<-filter(manufacturing,! is.na(R) & !is.na(E))
allConsecutiveTaxID<-group_by(nonMissingRandE,TaxID) %>%
  count()
allConsecutiveTaxID<-filter(allConsecutiveTaxID,n==9)
allConsecutiveFirms2<-filter(nonMissingRandE, nonMissingRandE$TaxID %in% allConsecutiveTaxID$TaxID) %>%
arrange(TaxID,Year) #subset containing only the firms having entries for each year(between 2007 and 2016)
