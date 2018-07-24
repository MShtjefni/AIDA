#rm(list = ls()) #remove all the existing environment vars
wdir <- ""
dataDir <- "data/"
packagesFile<-"pack2.txt"
source(paste(wdir, "functions.R", sep="")) ### this also loads every needed package
#loadDatasets(paste(wdir,dataFile,sep="")) ###USE THIS IF YOU CURRENTLY HAVEN'T DATASETS IN WORKSPACE

"Basic Statistics and plots..."  
data<-aida
par(mfrow=c(2,2))
tb.region <- sort(table(data$Region))
tb.region
barplot(tb.region,xlab = "Region")
barplot(tb.region[(length(tb.region)-2):length(tb.region)],xlab = "Top Region")
#provinces distribution
tb.province <- sort(table(data$Province))
barplot(tb.province,xlab = "Province")
barplot(tb.province[(length(tb.province)-2):length(tb.province)],xlab = "Top Province")
par(mfrow=c(1,1))


#The regions with largest numbers of firms are from North(Lombardia, VENETO, Emilia Romagna)
#Instead the most common provinces are MILANO, VICENZA, BRESCIA
#geo area distibution
tb.geo = table(data$GeoArea) # SUD: 2000509 (23.8%), CENTRO: 2097369(25%), NORD(51.2%): 
#tb.geo = tb.geo[order(tb.geo)]
#tb.geo <- scale(tb.geo, FALSE, colSums(tb.geo)) * 100
barplot(tb.geo,xlab="Geo Area" )
ggplot(data, aes(x=GeoArea, fill=GeoArea)) + 
  geom_bar(aes(y=prop.table((..count..)/sum(..count..))), position = "dodge") + 
  ggtitle("Firms distribution by Geographic Area") + 
  xlab("Geographic Areas") + ylab ("Firms percentage") + 
  geom_text(aes(y = prop.table((..count..)/sum(..count..)) / 2, 
                label = paste0(round(prop.table(..count..) * 100, 1), '%')), 
            stat = 'count', position = position_dodge(.9), size = 3) + 
  scale_x_discrete(limits=c("Sud", "Nord", "Centro")) +
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) + 
  guides(fill=F) + theme(plot.title = element_text(hjust = .5))

by.geo = split(data,data$GeoArea)

plotByGeo = function(x){ #PLOTS PROVINCES AND REGIONS FOR EACH GEOAREA
  par(mfrow=c(1,1))
  nomi <- c(names(x))
  k<-1
  for(i in x){
    #distribution by province
    tb.province <- sort(table(i$Province))
    
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
plotByGeo(by.geo)
  

# DISTRIBUTION BY SIZE
{
tb.size<- sort(table(data$Size)) # LARGE: 33681(0.4%), MEDIUM: 1061477(12.64%), SMALL: 86.96%
'ggplot(data, aes(x=Size, fill=Size)) + 
  geom_bar(aes(y=prop.table((..count..)/sum(..count..))), position = "dodge") + 
  ggtitle("Firms distribution by Size") + 
  xlab("Firm Sizes") + ylab ("Firms percentage") + 
  geom_text(aes(y = prop.table((..count..)/sum(..count..)) / 2, 
                label = paste0(round(prop.table(..count..) * 100, 1), "%")), 
            stat = "count", position = position_dodge(.9), size = 3) + 
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) + 
  guides(fill=F) + theme(plot.title = element_text(hjust = .5))
  '
#plot with exp scale (exp=0.4, very similar to sqrt!)
ggplot(data, aes(x=Size, fill=Size)) + 
  geom_bar(aes(y=((prop.table((..count..)/sum(..count..)))*1000)^.4), position = "dodge") + 
  ggtitle("Firms distribution by Size") + 
  xlab("Firm Sizes") + ylab ("Firms percentage") + 
  geom_text(aes(y = (prop.table((..count..)/sum(..count..)) *1000)^.4 / 2, 
                label = paste0(round(prop.table(..count..) * 100, 1), '%')), 
            stat = 'count', position = position_dodge(.9), size = 3) + 
  scale_fill_manual(values=wes_palette(n=3, name="Darjeeling1")) + 
  scale_y_continuous(labels = trans_format(function(y) y^2.5/1000, format = percent)) + 
  guides(fill=F) + theme(plot.title = element_text(hjust = .5))
}

### SIZE BY GEOAREA ### 
{
countBySud<-table(data$Size[data$GeoArea=='Sud'])
countByCentro<-table(data$Size[data$GeoArea=='Centro'])
countByNord<-table(data$Size[data$GeoArea=='Nord'])
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
countBySmall<-table(data$GeoArea[data$Size=='Small'])
countByMedium<-table(data$GeoArea[data$Size=='Medium'])
countByLarge<-table(data$GeoArea[data$Size=='Large'])
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
remove(melted,tmpDf,myFreq,data, by.geo)

"Useful subsets regarding firms which have missing values for Employee column"
{
  if(!exists("aidat"))
    load(paste0(wdir,dataDir,"aidat.R"))


  manufacturing <- subset(aidat,Ateco>=101100 & Ateco<=332009 & Year>2006 & Year<2016) #subset containing all the manufacturing firms from original dataset
  manufacturingNoMiss <- subset(manufacturing, R>=0 & E>=0)
  #remove(aidat)

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
}

