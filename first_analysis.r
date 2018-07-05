#rm(list = ls()) #remove all the existing environment vars
wdir <- ""

packagesFile <- "packages.txt"
dataFile <- "data/aidat.RData"
utilsFile <- "utils.r"
source(paste(wdir, utilsFile, sep=""))
loadPackages(paste(wdir,packagesFile,sep = ""))


" Preprocessing steps: changing column types for Ateco(int), TaxID(num), year(int). Removing TradingRegion and TradingProvince columns "
if(!exists ("aidat")) load(paste(wdir,dataFile,sep = ""))
aidat$Ateco <- as.integer(as.character(aidat$Ateco))
aidat$TaxID <- as.numeric(as.character(aidat$TaxID))
aidat$Year <- as.integer(as.character(aidat$Year))
aidat[which(names(aidat) %in% c("TradingRegion","TradingProvince"))] <- NULL
inflations <- c(1, .032, .007, .016, .027, .03, .011, .002, -0.001, -0.001, .011) #inflations from 2007 to 2017, by ISTAT

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
applyInflation <- function(data, inflations=c(1, .032, .007, .016, .027, .03, .011, .002, -0.001, -0.001, .011)) {
  if ("Infl" %in% colnames(data)) 
    return (data)
    
  data$Infl <- NA
  for (i in seq(2,9)) {
    inflations[i]<-(inflations[i-1]*(1+inflations[i]))
    print(inflations[i])
    data$Infl[data$Year==2006+i]<-inflations[i]
  }
  data$Infl[is.na(data$Infl)]<-1
  data$P <- data$P/data$Infl
  data$R <- data$R/data$Infl
  data$B <- data$B/data$Infl
  return(data)
}

addGeoArea<-function(data) {
  if ("GeoArea" %in% colnames(data)) 
    return (data)
  #VARIABILE AREA GEOGRAFICA NORD, CENTRO, SUD
  #SUD--> Abruzzo, Basilicata, Calabria, Campania, Molise, Puglia, Sardegna, Sicilia
  data$GeoArea=factor("Sud", levels = c("Sud", "Centro", "Nord"))
  #NORD-->Liguria, Lombardia, Piemonte, Valle d'Aosta, Emilia-Romagna, Friuli-Venezia Giulia, Trentino-Alto Adige, Veneto
  data$GeoArea[data$Region=="Liguria" | data$Region=="Lombardia" |data$Region=="Piemonte" |data$Region=="Valle d'Aosta/Vall?e d'Aoste" |data$Region=="Emilia-Romagna"|data$Region=="Friuli-Venezia Giulia"|data$Region=="Trentino-Alto Adige"|data$Region=="Veneto" ]= "Nord"
  #CENTRO--> Lazio, Marche, Toscana ed Umbria
  data$GeoArea[data$Region=="Lazio" |data$Region=="Marche" |data$Region=="Toscana" |data$Region=="Umbria"  ]= "Centro"
  
  data$GeoArea = as.factor(data$GeoArea)
  return(data)
}

addSize <- function(data) {
  if ("Size" %in% colnames(data)) 
    return (data)
  data$Size <- factor("Medium", levels = c("Medium", "Small", "Large"))
  data$Size[data$E <= 49] <- "Small"
  data$Size[data$E >= 250] <- "Large"
  
  return(data)
}

"Subset of the only manufacturing firms. Adding subsector column"
addSubsectorColumn <- function(data) {
  subsectors<-list(subsector=c('alimentari','bevande','tabacco','tessile','confezAbbigl','pelle','legno','carta','stampa', 'fabbrCoke','prodChimici','prodFarm','gomma','minerali','metallurgia','prodMetallo','computer','appElettriche','macchinari','autoveicoli', 'mezziTrasp', 'mobili', 'altreManuf', 'riparaz'), min=c(100000,110000,120000,130000,140000,150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000,270000,280000,290000,300000,310000,320000,330000), max=c(110000,120000,130000,140000,150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000,270000,280000,290000,300000,310000,320000,330000,340000))
  data$SubSector=factor(levels = subsectors$subsector)
  for(i in seq(length(subsectors$subsector))) #mapping subsector to each row based on Ateco
  data$SubSector[data$Ateco>=subsectors$min[i] & data$Ateco<subsectors$max[i]] <- subsectors$subsector[i]
  return (data) 
  }

  
aidat<-applyInflation(aidat)
aidat<-addGeoArea(aidat)
aidat<-addSize(aidat)
aida<-subset(aidat, Year>2006 & Year<2016 & R>=0 & E>=0)
manufacturing <- subset(aidat,Ateco>=101100 & Ateco<=332009 & Year>2006 & Year<2016) #subset containing all the manufacturing firms from original dataset
manufacturing <- addSubsectorColumn(manufacturing)
manufacturing <- dplyr::filter(manufacturing,R>=0 | is.na(R)) #removing all the entries with negative(<0) revenue
manufacturingNoMiss <- dplyr::filter(manufacturing, !is.na(R) & !is.na(E))  # removing all the rows having missing values for Revenue or Employee
#manufacturing <- arrange(manufacturing,TaxID,Year) #sort manufacturing firms by TaxID and by year


"Basic Statistics and plots"  
par(mfrow=c(2,2))
tb.region = table(aidat$Region)
tb.region = tb.region[order(tb.region)]
tb.region
barplot(tb.region,xlab = "Region")
barplot(tb.region[(length(tb.region)-2):length(tb.region)],xlab = "Top Region")
#provinces distribution
tb.province = table(aidat$Province)
tb.province = tb.province[order(tb.province)]
barplot(tb.province,xlab = "Province")
barplot(tb.province[(length(tb.province)-2):length(tb.province)],xlab = "Top Province")
par(mfrow=c(1,1))


#NOTIAMO SUBITO CHE LE REGIONI CON PIU ALTO  NUMERO
#DI AZIENDE SONO AL NORD->Lombardia, VENETO, Emilia Romagna
#INVECE LE PROVINCE SONO MILANO, VICENZA, BRESCIA
#VEDIAMO LA DISTRIBUZIONE DI PROVINCE E REGIONI IN BASE ALLA ZONA GEOGRAFICA SUD, NORD, CENTRO
#geo area distibution
tb.geo = table(aidat$GeoArea) # SUD: 2000509 (23.8%), CENTRO: 2097369(25%), NORD(51.2%): 
#tb.geo = tb.geo[order(tb.geo)]
#tb.geo <- scale(tb.geo, FALSE, colSums(tb.geo)) * 100
barplot(tb.geo,xlab="Geo Area" )
ggplot(aidat, aes(x=GeoArea, fill=GeoArea)) + 
  geom_bar(aes(y=prop.table((..count..)/sum(..count..))), position = "dodge") + 
  ggtitle("Firms distribution by Geographic Area") + 
  xlab("Geographic Areas") + ylab ("Firms percentage") + 
  geom_text(aes(y = prop.table((..count..)/sum(..count..)) / 2, 
                label = paste0(round(prop.table(..count..) * 100, 1), '%')), 
            stat = 'count', position = position_dodge(.9), size = 3) + 
  scale_x_discrete(limits=c("Sud", "Nord", "Centro")) +
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) + 
  guides(fill=F) + theme(plot.title = element_text(hjust = .5))
by.geo = split(aidat,aidat$GeoArea)
plot.by.geo = function(x){
  par(mfrow=c(1,1))
  nomi = c(names(x))
  nomi[1]
  k=1
  for(i in x){
    #distribution by province
    tb.province = table(i$Province)
    tb.province = tb.province[order(tb.province)]
    title=paste("",nomi[k], sep=" ")
    barplot(tb.province[106:110],xlab = "Top Province" ,main =title )
    
    
    #distribution by region
    tb.region = table(i$Region)
    tb.region = tb.region[order(tb.region)]
    title=paste("",nomi[k], sep=" ")
    barplot(tb.region[15:20],xlab = "Top Region ",main = title)
    k=k+1
  }
  par(mfrow=c(1,1))
}
plot.by.geo(by.geo)
  

# DISTRIBUTION BY SIZE

tb.size<- table(aidat$Size)
tb.size<-sort(tb.size) # LARGE: 33681(0.4%), MEDIUM: 1061477(12.64%), SMALL: 86.96%
ggplot(aidat, aes(x=Size, fill=Size)) + 
  geom_bar(aes(y=prop.table((..count..)/sum(..count..))), position = "dodge") + 
  ggtitle("Firms distribution by Size") + 
  xlab("Firm Sizes") + ylab ("Firms percentage") + 
  geom_text(aes(y = prop.table((..count..)/sum(..count..)) / 2, 
                label = paste0(round(prop.table(..count..) * 100, 1), '%')), 
            stat = 'count', position = position_dodge(.9), size = 3) + 
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) + 
  guides(fill=F) + theme(plot.title = element_text(hjust = .5))
  
#plot with exp scale (exp=0.4, very similar to sqrt!)
ggplot(aidat, aes(x=Size, fill=Size)) + 
  geom_bar(aes(y=((prop.table((..count..)/sum(..count..)))*1000)^.4), position = "dodge") + 
  ggtitle("Firms distribution by Size") + 
  xlab("Firm Sizes") + ylab ("Firms percentage") + 
  geom_text(aes(y = (prop.table((..count..)/sum(..count..)) *1000)^.4 / 2, 
                label = paste0(round(prop.table(..count..) * 100, 1), '%')), 
            stat = 'count', position = position_dodge(.9), size = 3) + 
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) + 
  scale_y_continuous(labels = trans_format(function(y) y^2.5/1000, format = percent)) + 
  guides(fill=F) + theme(plot.title = element_text(hjust = .5))


### SIZE BY GEOAREA ### 
{
countBySud<-table(aidat$Size[aidat$GeoArea=='Sud'])
countByCentro<-table(aidat$Size[aidat$GeoArea=='Centro'])
countByNord<-table(aidat$Size[aidat$GeoArea=='Nord'])
myFreq<-rbind(countBySud/sum(countBySud),countByCentro/sum(countByCentro), countByNord/sum(countByNord))
tmpDf<-data.frame(cbind("GeoArea"=c("Sud", "Centro", "Nord"),myFreq))
# melt the data frame for plotting
melted <- melt(tmpDf, id.vars='GeoArea')
melted$value <- as.double(melted$value)
# ok, the percentages distribution of Small, Medium and Large are very similar within each GeoArea
ggplot(melted, aes(GeoArea, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")
}


### GEOAREA BY SIZE ###
{
countBySmall<-table(aidat$GeoArea[aidat$Size=='Small'])
countByMedium<-table(aidat$GeoArea[aidat$Size=='Medium'])
countByLarge<-table(aidat$GeoArea[aidat$Size=='Large'])
myFreq<-rbind(countBySmall/sum(countBySmall),countByMedium/sum(countByMedium), countByLarge/sum(countByLarge))
tmpDf<-data.frame(cbind("Size"=c("Small", "Medium", "Large"),myFreq))
# melt the data frame for plotting
melted <- melt(tmpDf, id.vars='Size')
melted$value <- as.double(melted$value)
# Large firms are much more "common" in Nord with respect to Centro and Sud
ggplot(melted, aes(Size, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  ggtitle("Geographic Area distribution by Size") + 
  xlab("Firm Sizes") + ylab ("Firms Percentage") + 
  geom_text(aes(fill=variable, label = paste0(round(100*value, 1),"%"), y=value/2),size = 3 , position = position_dodge(.9)) + 
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) +
  theme(plot.title = element_text(hjust = .5))
}


"Useful subsets regarding firms which have missing values for Employee column"
#unId<-unique(manufacturing$TaxID) #vector containing all the unique taxIDs from manufacturing firms
onlyMissEmployees <- subset(manufacturing,is.na(manufacturing$E)) #subset of manufacturing firms having at least one "NA" as Employee
missingEmpID <- unique(onlyMissEmployees$TaxID) #vector of unique taxIDs having at least one "NA" as Employee
#tmp <- group_by(onlyMissEmployees,TaxID)
#distinctEmpByID <- summarize(tmp,countE=n()) #grouping by TaxId and counting the number of NA for each TaxId
#distinctEmpByID <- arrange(distinctEmpByID, countE)

firmsMissingEmpRows <- dplyr::filter(manufacturing, manufacturing$TaxID %in% missingEmpID) #subset of rows containing only the firms having at least one "NA" as Employee
#missingEmpSortByTaxID <- arrange(firmsMissingEmpRows,TaxID,E,Year) #sorting the last subset by TaxID, N°Employee and Year
#tmp <- group_by(missingEmpSortByTaxId,TaxID)
#distinctEmpByID2 <- summarize(tmp,countE=n()) #grouping by TaxId and counting the number of rows for each TaxId
#distinctEmpByID2 <- arrange(distinctEmpByID2, countE)

"Counting the Employee missing values for each firm"
countMissingByTaxID <- dplyr::filter(onlyMissEmployees, is.na(onlyMissEmployees$E)) %>%
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
firmsMissingRevRows <- dplyr::filter(manufacturing, manufacturing$TaxID %in% missingRevID) #subset containing only the firms having at least one "NA" as Employee

#### SELECTING ALL THE ROWS OF FIRMS HAVING ALL THE ENTRIES FOR YEARS 2006-2015 ####
nonMissingRevenues<-dplyr::filter(manufacturing,! is.na(R)) #removing all the rows having missing values for Revenue
nine_years_TaxID<-group_by(nonMissingRevenues,TaxID) %>%
  count()
nine_years_TaxID<-dplyr::filter(nine_years_TaxID,n==9)
nine_year_firms<-dplyr::filter(nonMissingRevenues, nonMissingRevenues$TaxID %in% nine_years_TaxID$TaxID) %>%
arrange(TaxID,Year) #subset containing only the firms having entries for each year(between 2007 and 2016) and Revenue NOT missing


nine_years_TaxID2<-group_by(manufacturingNoMiss,TaxID) %>%
  count()
nine_years_TaxID2<-dplyr::filter(nine_years_TaxID2,n==9)
nine_year_firms2<-dplyr::filter(manufacturingNoMiss, manufacturingNoMiss$TaxID %in% nine_years_TaxID2$TaxID) %>%
arrange(TaxID,Year) #subset containing only the firms having entries for each year(between 2007 and 2016) with Revenue and Employee NOT missing
