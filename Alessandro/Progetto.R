##### DATASET DESCRIPTION ####
 {
ls() #controllo le variabili di ambiente
rm(list = ls()) #rimuoviamo tutte le varaibili d'ambiente
ls() #controllo se sono state eliminate le variabili d'ambiente



#reucpero il dataset completo
aida = get(load("aidat.RData")) #dataset aida
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

save(aida, file="aida.RData")

}

##### SAMPLING DATASET MANIFACTURING SECTOR 2007-2016 #####
aida = get(load("aida.RData")) #dataset aida

aida$Infl <- NA
"Apply inflaction function"
applyInflaction <- function(data, inflactions=c(1, .032, .007, .016, .027, .03, .011, .002, -0.001, -0.001, .011)) {
  
  if (!"Infl" %in% colnames(data)) 
    data$Infl <- NA
  for (i in seq(2,9)) {
    inflactions[i]<-(inflactions[i-1]/(1+inflactions[i]))
    print(inflactions[i])
    data$Infl[data$Year==2006+i]<-inflactions[i]
  }
  
  data$P <- data$P*data$Infl
  data$R <- data$R*data$Infl
  data$E <- data$E*data$Infl
  return(data)
}

#PRENDIAMO COME SAMPLE IL SETTORE MANIFATTURIERO
aida$Ateco =  as.numeric(as.character(aida$Ateco))
aida$Year =  as.numeric(as.character(aida$Year))
manufacturing = subset(aida,Ateco >= 101100 & Ateco <= 332009 & Year>=2007)
nrow(manufacturing) #1.290.032 records per il settore manifatturiero


# ESSENDO GLI ANNI SBILANCIATI, ABBIAMO DECISO DI SELEZIONARE LE FIRM DAL 2007
par(mfrow=c(1,2))
tb.year = table(manufacturing$Year)
tb.year = tb.year[order(names(tb.year))]
barplot(tb.year,xlab = "Year")
par(mfrow=c(1,1))

#SETTORE MANIFATTURIERO  firms with year>=2007
manufacturing$Year =  as.numeric(as.character(manufacturing$Year))
manufacturing = subset(manufacturing,R>=0 & E>=0) 
manufacturing$Year =  as.factor(manufacturing$Year)
nrow(manufacturing) #1.029.156
#View(manufacturing)

#select firm with year 2007 to 2015 = 9 years of data
tb = table(manufacturing$TaxID,manufacturing$Year)
tb = as.data.frame.matrix(tb)
from07to15 = tb[tb$`2007`==1 & tb$`2008`==1 & tb$`2009`==1 & tb$`2010`==1 & tb$`2011`==1 & tb$`2012`==1 & tb$`2013`==1 & tb$`2014`==1 & tb$`2015`==1,]
list.ID = c(as.character(rownames(from07to15)))
from07to15 = manufacturing[manufacturing$TaxID %in% list.ID,]
from07to15$Year = as.numeric(as.character(from07to15$Year))
from07to15 = subset(from07to15,Year<2016)
from07to15 <- from07to15[order(from07to15$TaxID,from07to15$Year),]
nrow(from07to15)# 378.612 records
length(unique(from07to15$TaxID))#42.068
manufacturing = from07to15


#manufacturing subsector
{
manufacturing$SubSector = 0
#1- alimentare
manufacturing$SubSector[manufacturing$Ateco>=100000 & manufacturing$Ateco <110000] ="Alimentare"
#2- bevande
manufacturing$SubSector[manufacturing$Ateco>=110000 & manufacturing$Ateco <120000] ="Bevande"
#3-tabacco
manufacturing$SubSector[manufacturing$Ateco>=120000 & manufacturing$Ateco <130000] ="Tabacco"
#4- tessile
manufacturing$SubSector[manufacturing$Ateco>=130000 & manufacturing$Ateco <140000] ="Tessile"
#5- confezioni
manufacturing$SubSector[manufacturing$Ateco>=140000 & manufacturing$Ateco <150000] ="Confezioni"
#6 -pelle
manufacturing$SubSector[manufacturing$Ateco>=150000 & manufacturing$Ateco <160000] ="Pelle"
#7 - legno
manufacturing$SubSector[manufacturing$Ateco>=160000 & manufacturing$Ateco <170000] ="Legno"
#8- carta
manufacturing$SubSector[manufacturing$Ateco>=170000 & manufacturing$Ateco <180000] ="Carta"
#9- stampa
manufacturing$SubSector[manufacturing$Ateco>=180000 & manufacturing$Ateco <190000] ="Stampa"
#10- coke.petrolio (coke==siderurgia)
manufacturing$SubSector[manufacturing$Ateco>=190000 & manufacturing$Ateco <200000] ="Petrolio e Siderurgia"
#11 - chimico
manufacturing$SubSector[manufacturing$Ateco>=200000 & manufacturing$Ateco <210000] ="Chimico"
#12 - farmaceutico
manufacturing$SubSector[manufacturing$Ateco>=210000 & manufacturing$Ateco <220000] ="Farmaceutico"
#13 -gomma e plastica 22 23
manufacturing$SubSector[manufacturing$Ateco>=220000 & manufacturing$Ateco <230000] ="Gomma e Plastica"
#14 - minerali 23 24
manufacturing$SubSector[manufacturing$Ateco>=230000 & manufacturing$Ateco <240000] ="Minerali"
#15 - mettallurgico 24 25
manufacturing$SubSector[manufacturing$Ateco>=240000 & manufacturing$Ateco <250000] ="Metallurgia"
#16 - metallo 25 26
manufacturing$SubSector[manufacturing$Ateco>=250000 & manufacturing$Ateco <260000] ="Metallo"
#17 -  26 27
manufacturing$SubSector[manufacturing$Ateco>=260000 & manufacturing$Ateco <270000] ="Apparecchiature E"
#18 - 27 28
manufacturing$SubSector[manufacturing$Ateco>=270000 & manufacturing$Ateco <280000] ="Apparecchiature NE"
#19 - NCA 28 29
manufacturing$SubSector[manufacturing$Ateco>=280000 & manufacturing$Ateco <290000] ="NCA"
#20 - autoveicoli 29 30
manufacturing$SubSector[manufacturing$Ateco>=290000 & manufacturing$Ateco <300000] ="Autoveicoli"
#21 - altri mezzi di trasporto 30 31
manufacturing$SubSector[manufacturing$Ateco>=300000 & manufacturing$Ateco <310000] ="Altro Trasporto"
#22 - mobili 31 32
manufacturing$SubSector[manufacturing$Ateco>=310000 & manufacturing$Ateco <320000] ="Mobili"
#23 - altro 32 33
manufacturing$SubSector[manufacturing$Ateco>=320000 & manufacturing$Ateco <330000] ="Altro"
#24 - riparazione 33 35
manufacturing$SubSector[manufacturing$Ateco>=330000 & manufacturing$Ateco <350000] ="Riparazione"

}


#SIZE FIRM- SMALL, MEDIUM, BIG
manufacturing$Size <- NA
manufacturing$Size[manufacturing$E <= 49] <- "Small"
manufacturing$Size[50 <= manufacturing$E & manufacturing$E <= 249] <- "Medium"
manufacturing$Size[manufacturing$E >= 250] <- "Large"

summary(manufacturing)
save(manufacturing, file="manufacturing.RData")




###### DISTRIBUTION VARIABLES ######
manufacturing = get(load("manufacturing.RData"))
summary(manufacturing)
View(manufacturing)

#DISTRIBUZIONE DELLE REGIONI E DELLE PROVINCE PIU NUMEROSE
#Le regioni con il più alto numero di aziende sono al Nord->Lombardia, Veneto, Emilia Romagna
#Le province con il più alto numero di aziende sono al Nord-> Milano, Vicenza, Brescia
{
  #regions distribution
par(mfrow=c(2,2))
tb.region = table(manufacturing$Region)
tb.region = tb.region[order(tb.region)]
tb.region
barplot(tb.region,xlab = "Region")
barplot(tb.region[18:20],xlab = "Top Region")
#provinces distribution
tb.province = table(manufacturing$Province)
tb.province = tb.province[order(tb.province)]
barplot(tb.province,xlab = "Province")
barplot(tb.province[106:110],xlab = "Top Province")
par(mfrow=c(1,1))
}

#DISTRIBUZIONE DI PROVINCE E REGION IN BASE ALLA ZONA GEOGRAFICA SUD, NORD, CENTRO
#Nord->265.410
#Centro->66.159
#Sud->47.043
tb.geo = table(manufacturing$GeoArea)
tb.geo = tb.geo[order(tb.geo)]
tb.geo
barplot(tb.geo,xlab="Geo Area")
by.geo = split(manufacturing,manufacturing$GeoArea)
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


#VEDIAMO ANCHE LA DISTRIBUZIONE DELLE VARAIBILI CONTINUE E, R, B, P
library(fitdistrplus)
R = manufacturing$R
E = manufacturing$E
P = manufacturing$P
B = manufacturing$B

descdist(R, discrete = FALSE)


{
par(mfrow=c(4,1))
E = manufacturing$E[!is.na(manufacturing$E)]
hist(log(E),prob=TRUE,main = "Distribution of Employee",breaks=20)
lines(density(log(E)))

R = manufacturing$R[!is.na(manufacturing$R)]
hist(log(R),prob=TRUE,main = "Distribution of Revenue",breaks = 20)
lines(density(log(R)))


P = manufacturing$P[!is.na(manufacturing$P)]
P=P[P>0]
hist(log(P),prob=TRUE,main = "Distribution of Profit",breaks = 20)
lines(density(log(P)))


B = manufacturing$B[manufacturing$B>1]
hist(log(B),prob=TRUE,main = "Distribution of EBIDTA",breaks = 20)
lines(density(log(B)))
par(mfrow=c(1,1))
}




#COME FACCIO A FARE  UN TEST SULLA DISTRIBUZIONE?
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
manufacturing = get(load("/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/manufacturing.RData")) #dataset aida
View(manufacturing)
summary(manufacturing)

#effettuiamo la correlazione di pearson per ogni Year
manufacturing$Year =  as.numeric(as.character(manufacturing$Year))

View(x)
x = manufacturing[c("Year","E","R","P","B")]

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


##### DOMANDE TANTARI#####
#1-Prima di procedere all'analisi della correlazione, dobbiamo analizzare la distribuzione
#2-Una volta analizzata ed individuato il tipo di distribuzione procediamo all'analisi della correlazione
#3-Dopo aver analizzato  la correlazione, effettuiamo un test di ipotesi come H0- non correlate e H1 correlate

