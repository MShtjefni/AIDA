##### DATASET DESCRIPTION ####
 {
ls() #controllo le variabili di ambiente
rm(list = ls()) #rimuoviamo tutte le varaibili d'ambiente
ls() #controllo se sono state eliminate le variabili d'ambiente



#reucpero il dataset completo
aida = get(load("/Users/alessandroarmillotta/Desktop/Statistica/Progetto/aidat.RData")) #dataset aida
#View(aida)

#lunghezza del dataset iniziale
length(rownames(aida)) #lunghezza del dataset 8.397.955 records

#numerosità degli attributi
length(unique(aida$Ateco[!is.na(aida$Ateco)])) #numero di codici ateco senza NA uguale a  1660
length(unique(aida$Company[!is.na(aida$Company)])) #1.102.591 aziende differenti
length(unique(aida$TaxID[!is.na(aida$TaxID)]))#1.266.379 univoci taxID
length(unique(aida$Form[!is.na(aida$Form)])) #34 forme aziendali distinte
length(unique(aida$Province[!is.na(aida$Province)]))# 110 province
length(unique(aida$Region[!is.na(aida$Region)]))#20 regioni
length(unique(aida$Status[!is.na(aida$Status)]))#10 stati attività
length(unique(aida$Year[!is.na(aida$Year)])) #27 anni di visualizzazione

#gli attributi iniziali sono 14 ma tradingProvince e tradingRegion sono uguali a Province e Ragion
#eliminiamo gli attributi TradingProvince e TradingRegion in quanto uguali a Region e Province
aida$TradingProvince = NULL
aida$TradingRegion = NULL

#VARIABILE AREA GEOGRAFICA NORD, CENTRO, SUD
aida$GeoArea=0
#NORD-->Liguria, Lombardia, Piemonte, Valle d'Aosta, Emilia-Romagna, Friuli-Venezia Giulia, Trentino-Alto Adige, Veneto
aida$GeoArea[aida$Region=="Liguria" | aida$Region=="Lombardia" |aida$Region=="Piemonte" |aida$Region=="Valle d'Aosta/Vallée d'Aoste" |aida$Region=="Emilia-Romagna"|aida$Region=="Friuli-Venezia Giulia"|aida$Region=="Trentino-Alto Adige"|aida$Region=="Veneto" ]= "Nord"
#CENTRO--> Lazio, Marche, Toscana ed Umbria
aida$GeoArea[aida$Region=="Lazio" |aida$Region=="Marche" |aida$Region=="Toscana" |aida$Region=="Umbria"  ]= "Centro"
#SUD--> Abruzzo, Basilicata, Calabria, Campania, Molise, Puglia, Sardegna, Sicilia
aida$GeoArea[aida$Region=="Abruzzo" |aida$Region=="Basilicata" |aida$Region=="Calabria" |aida$Region=="Campania" |aida$Region=="Molise" |aida$Region=="Puglia" |aida$Region=="Sardegna" |aida$Region=="Sicilia"  ]= "Sud"

aida =  aida[order(aida$TaxID,aida$Year),]

save(aida, file="/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/aida.RData")

}

##### SAMPLING DATASET MANIFACTURING SECTOR 2007-2016 #####
aida = get(load("/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/aida.RData")) #dataset aida
View(aida)

#PER PROBLEMI COMPUTAZIONALI PRENDIAMO IN CONSIDERAZIONE IL SETTORE MANIFATTURIERI SETTORE MANIFATTURIERO
aida$Ateco =  as.numeric(as.character(aida$Ateco))
manifacturing = subset(aida,Ateco >= 101100 & Ateco <= 332009 )
nrow(manifacturing) #1.294.212 records per il settore manifatturiero
aida$Ateco =  as.factor(aida$Ateco)

# ESSENDO GLI ANNI SBILANCIATI, ABBIAMO DECISO DI SELEZIONARE LE FIRM DAL 2007
par(mfrow=c(1,2))
tb.year = table(manifacturing$Year)
tb.year = tb.year[order(names(tb.year))]
tb.year
barplot(tb.year,xlab = "Year")
barplot(tb.year[26:36],xlab = "Top Years")
par(mfrow=c(1,1))

#SETTORE MANIFATTURIERO  firms with year>=2007
manifacturing$Year =  as.numeric(as.character(manifacturing$Year))
manifacturing = subset(manifacturing,R>=0 & E>=0) 
manifacturing = subset(manifacturing,Year>=2007) 
manifacturing$Year =  as.factor(manifacturing$Year)
nrow(manifacturing) #1.029.156
View(manifacturing)

#select firm with year 2007 to 2015 = 9 years of data
tb = table(manifacturing$TaxID,manifacturing$Year)
tb = as.data.frame.matrix(tb)
from07to15 = tb[tb$`2007`==1 & tb$`2008`==1 & tb$`2009`==1 & tb$`2010`==1 & tb$`2011`==1 & tb$`2012`==1 & tb$`2013`==1 & tb$`2014`==1 & tb$`2015`==1,]
list.ID = c(as.character(rownames(from07to15)))
from07to15 = manifacturing[manifacturing$TaxID %in% list.ID,]
from07to15$Year = as.numeric(as.character(from07to15$Year))
from07to15 = subset(from07to15,Year<2016)
from07to15 <- from07to15[order(from07to15$TaxID,from07to15$Year),]
nrow(from07to15)# 378.612 records
length(unique(from07to15$TaxID))#42.068
manifacturing =from07to15

#save(manifacturing, file="/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/manifacturing.RData")



###### DISTRIBUTION VARIABLES ######
manifacturing = get(load("/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/manifacturing.RData"))
View(manifacturing)

#GUARDIAMO LA DISTRIBUZIONE DELLE REGIONI E VEDIAMO QUELLE PIU NUMEROSE
{
  #regions distribution
par(mfrow=c(2,2))
tb.region = table(manifacturing$Region)
tb.region = tb.region[order(tb.region)]
tb.region
barplot(tb.region,xlab = "Region")
barplot(tb.region[18:20],xlab = "Top Region")
#provinces distribution
tb.province = table(manifacturing$Province)
tb.province = tb.province[order(tb.province)]
barplot(tb.province,xlab = "Province")
barplot(tb.province[106:110],xlab = "Top Province")
par(mfrow=c(1,1))
}

#NOTIAMO SUBITO CHE LE REGIONI CON PIU ALTO  NUMERO
#DI AZIENDE SONO AL NORD->Lombardia, VENETO, Emilia Romagna
#INVECE LE PROVINCE SONO MILANO, VICENZA, BRESCIA
#VEDIAMO LA DISTRIBUZIONE DI PROVINCE E REGIONI IN BASE ALLA ZONA GEOGRAFICA SUD, NORD, CENTRO
#geo area distibution
tb.geo = table(manifacturing$GeoArea)
tb.geo = tb.geo[order(tb.geo)]
barplot(tb.geo,xlab="Geo Area")

by.geo = split(manifacturing,manifacturing$GeoArea)
plot.by.geo = function(x){
  par(mfrow=c(3,2))
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


#VEDIAMO ANCHE LA DISTRIBUZIONE DELLE VARAIBILI CONTINUE
#TUTTE E 4 LE VARIABILI E,B,P,R SEMBRANO CHE SEGUANO UNA DISTRIBUZIONE LOGARITMICA
{par(mfrow=c(4,1))
manifacturing$E[manifacturing$E==0] = 1
E = E[!is.na(E)]
hist(log(E),prob=TRUE,main = "Distribution of Employee",breaks=22)
lines(density(log(E)))

R = manifacturing$R[!is.na(manifacturing$R)]
hist(log(R),prob=TRUE,main = "Distribution of Employee")
lines(density(log(R)))

P = aida$P[aida$P>0]
P = P[!is.na(P)]
lines(density(log(P)))

B = aida$B[aida$B>0]
B = B[!is.na(B)]
lines(density(log(B)))
par(mfrow=c(1,1))
}

#COME FACCIO A FARE UN TEST SULLA DISTRIBUZIONE LOGNORM?
{
library(fitdistrplus)
library(forecast)
library(psych)
library(goft)

R = manifacturing$R[!is.na(manifacturing$R)]
length(R)
R = R[R>0]
length(R)

fit.weibull <- fitdist(R, "weibull")
fit.norm <- fitdist(R, "norm")
fit.lnorm <- fitdist(R,"lnorm")
fit.unif <- fitdist(R,"unif")
fit.logis <- fitdist(R,"logis")

gofstat(list(fit.weibull,fit.lnorm,fit.norm,fit.logis))

#bootstrap
bendo.B = bootdist(fit.lnorm, niter=100)
summary(bendo.B)
plot(bendo.B)

}




















###### CORRELATION ANALYSIS #####
manifacturing = get(load("/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/manifacturing.RData")) #dataset aida
View(manifacturing)
summary(manifacturing)

#effettuiamo la correlazione di pearson per ogni Year
manifacturing$Year =  as.numeric(as.character(manifacturing$Year))
x = manifacturing[!is.na(manifacturing)]
View(x)
x = x[c("Year","E","R","P","B")]

r<- by(x, x$Year, FUN = function(x) cor(x, use = "pairwise",method = "pearson"))
library(corrplot)

#matrice delle correlazioni per year
par(mfrow= c(3,3))
plotCorr = function(r){
  year=c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
  k=1
  for(i in r){
    i = i[,-1]
    i = i[-1,]
    
    corrplot(i,method = "number", type="upper", order="hclust")
    
    title(main = year[k],line = +3)
    k=k+1
    
  }
}
plotCorr(r)
par(mfrow= c(1,1))

# plot per E-R per ogni Year
par(mfrow= c(3,3))
plotByYear=function(x){
  a=split(x,x$Year)
  year=c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
  k=1
  for(i in a){
    plot(i$E,i$R,xlab = "Employee",ylab = "Revenue")
    title(main = year[k],line = +3)
    k=k+1
  }
}
plotByYear(x)
par(mfrow= c(1,1))

#test di ipotesi per correlazione considerando che sono distribuite log-norm.( chiedere a tantari)


#####vecchio#####
{


##### SUMMARY STATISTICS #####
#summary- general summary statistics
summary(aida)

#summary - by year

#total summary statistics by numerics attributes
#dev.sta-> variabilibiltà della popolazione dalla media
#var-> quanto si discosano quadraticamente dalla media
#EMPLOYEE
mean(aida$E,na.rm = TRUE) #media 10.85532
sd(aida$E,na.rm = TRUE) #standard deviation 385.9006
var(aida$E,na.rm=TRUE) #varianza 148919.3
median(aida$E,na.rm=TRUE) #mediana 1
min(aida$E,na.rm=TRUE) #minimo 0
max(aida$E,na.rm=TRUE) #max 791678
quantile(aida$E,na.rm = TRUE)

#REVENUE
mean(aida$R,na.rm = TRUE) #media 3265.228
sd(aida$R,na.rm = TRUE) #standard deviation 622574.8
var(aida$R,na.rm = TRUE) #varianza 387599363409
median(aida$R,na.rm=TRUE) #mediana 229.853
min(aida$R,na.rm=TRUE) #minimo -22068
max(aida$R,na.rm=TRUE) #max 1673628019

x<- rnorm(length(aida$R[!is.na(aida$R)]),3265.228,622574.8 )
hist(x,freq =F)
curve(dnorm(x),add = T)


#PROFIT
mean(aida$P,na.rm = TRUE) #media 38.96912
sd(aida$P,na.rm = TRUE) #standard deviation 18447.07
var(aida$P,na.rm = TRUE) #varianza 340294436
median(aida$P,na.rm=TRUE) #mediana 1.215
min(aida$P,na.rm=TRUE) #minimo -11601111
max(aida$P,na.rm=TRUE) #max 38413703

x<- rnorm(length(aida$P[!is.na(aida$P)]),38.96912,18447.07 )
hist(x,freq =F)
curve(dnorm(x),add = T)

#EBIT
mean(aida$B,na.rm = TRUE) #media 259.0506
sd(aida$B,na.rm = TRUE) # standard devation 99121.53
var(aida$B,na.rm = TRUE) #varianza 9825078029
median(aida$B,na.rm=TRUE) #mediana 19.168
min(aida$B,na.rm=TRUE) #minimo -14084077
max(aida$B,na.rm=TRUE) #max 265341741

x<- rnorm(length(aida$B[!is.na(aida$B)]),259.0506,99121.53 )
hist(x,freq =F)
curve(dnorm(x),add = T)


#ordino il dataset per TaxID e Year. In questo modo ogni azienda ha gli anni ordinati in ordine credescente
aida <- aida[order(aida$TaxID,aida$Year),] #order by TaxID e Year

#delete all rows with all NA value
aida = aida[ !is.na(aida$R),]
aida = aida[ !is.na(aida$E),]

#dataset iniziale pulito senza missing value
nrow(aida)#7.514.962 records

#save dataset without missing value
#save(aida, file="/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/aida2.RData")




#plot histogram and density






###### SELECT 2007 - 2015 FOR MANIFACTURING FIRMS #####
aida = get(load("/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/aida2.RData"))
nrow(aida) #7.514.962 records

#select firms with year>=2007
aida$Year=  as.numeric(as.character(aida$Year))
aida = subset(aida,R>=0) 
aida = subset(aida,Year>=2007) 
aida$Year =  as.factor(aida$Year)
nrow(aida) #7.062.820

#select firm with year 2007 to 2015 = 9 years of data
tb = table(aida$TaxID,aida$Year)
tb = as.data.frame.matrix(tb)
from07to15 = tb[tb$`2007`==1 & tb$`2008`==1 & tb$`2009`==1 & tb$`2010`==1 & tb$`2011`==1 & tb$`2012`==1 & tb$`2013`==1 & tb$`2014`==1 & tb$`2015`==1,]
list.ID = c(as.character(rownames(from07to15)))
from07to15 = aida[aida$TaxID %in% list.ID,]
from07to15$Year = as.numeric(as.character(from07to15$Year))
from07to15 = subset(from07to15,Year<2016)
nrow(from07to15)# 2.382.525 records

#ordino il dataset per Company, TaxID ed Year. In questo modo ogni azienda ha gli anni ordinati in ordine credescente
from07to15 <- from07to15[order(from07to15$TaxID,from07to15$Year),] #order by TaxID e Year
from07to15$Ateco = as.numeric(as.character(from07to15$Ateco))
length(unique(from07to15$TaxID)) # 264.725 aziende distinte

#selezioniamo le aziende del settore manifatturiero
manifacturing=subset(from07to15,Ateco >= 101100 & Ateco <= 332009)
nrow(manifacturing)#378.615 records totali
length(unique(manifacturing$TaxID))# 42.068 aziende distinte

summary(manifacturing)


#selezione sotto-settori
manifacturing$SubSector = 0
#1- alimentare
manifacturing$SubSector[manifacturing$Ateco>=100000 & manifacturing$Ateco <110000] ="Alimentare"
#2- bevande
manifacturing$SubSector[manifacturing$Ateco>=110000 & manifacturing$Ateco <120000] ="Bevande"
#3-tabacco
manifacturing$SubSector[manifacturing$Ateco>=120000 & manifacturing$Ateco <130000] ="Tabacco"
#4- tessile
manifacturing$SubSector[manifacturing$Ateco>=130000 & manifacturing$Ateco <140000] ="Tessile"
#5- confezioni
manifacturing$SubSector[manifacturing$Ateco>=140000 & manifacturing$Ateco <150000] ="Confezioni"
#6 -pelle
manifacturing$SubSector[manifacturing$Ateco>=150000 & manifacturing$Ateco <160000] ="Pelle"
#7 - legno
manifacturing$SubSector[manifacturing$Ateco>=160000 & manifacturing$Ateco <170000] ="Legno"
#8- carta
manifacturing$SubSector[manifacturing$Ateco>=170000 & manifacturing$Ateco <180000] ="Carta"
#9- stampa
manifacturing$SubSector[manifacturing$Ateco>=180000 & manifacturing$Ateco <190000] ="Stampa"
#10- coke.petrolio (coke==siderurgia)
manifacturing$SubSector[manifacturing$Ateco>=190000 & manifacturing$Ateco <200000] ="Petrolio e Siderurgia"
#11 - chimico
manifacturing$SubSector[manifacturing$Ateco>=200000 & manifacturing$Ateco <210000] ="Chimico"
#12 - farmaceutico
manifacturing$SubSector[manifacturing$Ateco>=210000 & manifacturing$Ateco <220000] ="Farmaceutico"
#13 -gomma e plastica 22 23
manifacturing$SubSector[manifacturing$Ateco>=220000 & manifacturing$Ateco <230000] ="Gomma e Plastica"
#14 - minerali 23 24
manifacturing$SubSector[manifacturing$Ateco>=230000 & manifacturing$Ateco <240000] ="Minerali"
#15 - mettallurgico 24 25
manifacturing$SubSector[manifacturing$Ateco>=240000 & manifacturing$Ateco <250000] ="Metallurgia"
#16 - metallo 25 26
manifacturing$SubSector[manifacturing$Ateco>=250000 & manifacturing$Ateco <260000] ="Metallo"
#17 -  26 27
manifacturing$SubSector[manifacturing$Ateco>=260000 & manifacturing$Ateco <270000] ="Apparecchiature E"
#18 - 27 28
manifacturing$SubSector[manifacturing$Ateco>=270000 & manifacturing$Ateco <280000] ="Apparecchiature NE"
#19 - NCA 28 29
manifacturing$SubSector[manifacturing$Ateco>=280000 & manifacturing$Ateco <290000] ="NCA"
#20 - autoveicoli 29 30
manifacturing$SubSector[manifacturing$Ateco>=290000 & manifacturing$Ateco <300000] ="Autoveicoli"
#21 - altri mezzi di trasporto 30 31
manifacturing$SubSector[manifacturing$Ateco>=300000 & manifacturing$Ateco <310000] ="Altro Trasporto"
#22 - mobili 31 32
manifacturing$SubSector[manifacturing$Ateco>=310000 & manifacturing$Ateco <320000] ="Mobili"
#23 - altro 32 33
manifacturing$SubSector[manifacturing$Ateco>=320000 & manifacturing$Ateco <330000] ="Altro"
#24 - riparazione 33 35
manifacturing$SubSector[manifacturing$Ateco>=330000 & manifacturing$Ateco <350000] ="Riparazione"


View(manifacturing)




###### MANIFACTURING AND SUB-SECTORS PLOTS ######
manifacturing = get(load("/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/manifacturing.RData"))
length(unique(manifacturing$TaxID))# 42.068 aziende distinte
nrow(manifacturing) #378.612

summary(manifacturing)
x =rlnorm(1000)
y =rlnorm(1000)
ks.test(x,y)
x = manifacturing[,9:12]

x =x[!is.na(x$P)]
View(x)

summary(x)
manifacturing$R[manifacturing$R<=1] = 1
manifacturing$R = log10(manifacturing$R)

manifacturing$E[manifacturing$E<=1] = 1
manifacturing$E = log10(manifacturing$E)

manifacturing$P[manifacturing$P<=1] = 1
manifacturing$P = log10(manifacturing$P)

manifacturing$B[manifacturing$B<=1] = 1
manifacturing$B = log10(manifacturing$B)

plot(manifacturing$R,manifacturing$E)
cor(manifacturing$R,manifacturing$E)

library(corrplot)

View(x)
x <- cor()
x
corrplot(x,method = "number", type="upper", order="hclust")

summary(manifacturing)
#barplot of sub-sector
tb.sector = unique(manifacturing[,c("TaxID", "SubSector")])
tb.sector = table(prova$SubSector)
tb.sector
tb.sector = tb.sector[order(names(tb.sector),decreasing = TRUE)]
barplot(tb.sector,las=2,cex.names=0.6,horiz=TRUE)




###### CORRELATION AND REGRESSION ANALYSIS ####
manifacturing = get(load("/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/manifacturing.RData"))

length(unique(manifacturing$TaxID))# 42.068 aziende distinte
nrow(manifacturing) #378.612


#la CORRELAZIONE e la REGRESSIONE LINEARE, vengono utilizzate per individuare la relazione tra variabili.
#CORRELAZIONE-> determina se una varaibile varia sistematicamente se varia un'altra variabile. Non specifica che una variabile è
#dipendente e l'altra indipendente'
#le forme di correlazione sono:
#Pearson p-value parametrico che assume che le variabili sono bivariate normali
#Kenadal e Spearman sono non parametriche 

#LINEAR REGRESSION -> specifica una variabile dipendente ed una variabile indipendente. Il modello risultante mette
#in relazione le variabili con una relazione lineare.
#IL test associato con la regressione lineare sono parametrici e assumono, normalità , homoschedasticità ed indipendenza

#CORRELAZIONE






###### GROWTH ######

#BY SUB-SECTOR
#calcolo il logaritmo per ogni anno  log 10
manifacturing$R[manifacturing$R<=1] = 1
manifacturing$logSijt = 0
manifacturing$logSijt = log10(manifacturing$R)
View(manifacturing)

#numero di aziende nel sotto-settore
N = as.data.frame(as.table(tb.sector))


#calcolo la sommatoria di log per settore ed anno
prova = manifacturing[c("TaxID","SubSector","Year","R","logSijt")]
prova = aggregate(prova$logSijt, by=list(SubSector=prova$SubSector,Year=prova$Year), FUN=sum)
prova = merge(prova,N, by.x = "SubSector",by.y = "Var1")

prova$SumSijt =prova$x/prova$Freq

prova = merge(manifacturing,prova, by.x = c("SubSector","Year"),by.y =c("SubSector","Year"))
prova$Freq=NULL
prova$x =NULL
prova = prova[c("TaxID","SubSector","Year","logSijt","SumSijt")]

prova$sijt = prova$logSijt - prova$SumSij
prova = prova[order(prova$TaxID,prova$Year),]


#growth
prova$growth <- ave(prova$sijt, c(prova$TaxID,prova$Year), FUN=function(x) c(NA,diff(x)))
View(prova)
mean(prova$growth,na.rm = TRUE) #1.222601e-17
plot(density(prova$growth,na.rm = TRUE), ylim=c(0,1),xlim=c(-4,4))

alimentare = subset(prova,SubSector=="Alimentare")
View(alimentare)
mean(alimentare$growth,na.rm = TRUE)#-1.81297e-17
plot(density(alimentare$growth,na.rm = TRUE))

altro = subset(prova,SubSector=="Altro")
mean(altro$growth,na.rm = TRUE) #2.555492e-17
density(altro$growth,na.rm = TRUE)









}
