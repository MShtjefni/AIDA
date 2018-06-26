##### DATASET DESCRIPTION ####
#The dataset is long 8.397.955 records with 14 variables. 
#The varaibles are:
#Ateco: ateco code which indicates the firm sector
 {
ls() #Environment
rm(list = ls()) #remove all variables
ls() #check varaibles


aida = get(load("aidat.RData")) #dataset aida
length(rownames(aida)) # 8.397.955 records

#numbers of attribute 14
ncol(aida)

#single value attribute
length(unique(aida$Ateco[!is.na(aida$Ateco)])) #1.660 ateco code
length(unique(aida$Company[!is.na(aida$Company)])) #1.102.591 company
length(unique(aida$TaxID[!is.na(aida$TaxID)]))#1.266.379 firms
length(unique(aida$Form[!is.na(aida$Form)])) #34 type of firms
length(unique(aida$Province[!is.na(aida$Province)]))# 110 province
length(unique(aida$Region[!is.na(aida$Region)]))#20 regions
length(unique(aida$Status[!is.na(aida$Status)]))#10 activity states
length(unique(aida$Year[!is.na(aida$Year)])) #27 years

#delete attributes TradingProvince and TradingRegion because are equals of Province and Region
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

#order dataset by TaxID and Year
aida =  aida[order(aida$TaxID,aida$Year),]

#save dataset aida
save(aida, file="aida.RData")

}

##### MANUFACTORING SECTOR FROM 2007 TO 2016 #####
{
aida = get(load("aida.RData")) #dataset aida

#MANUFACTORING SCTOR FROM 101100 TO 332009
aida$Ateco =  as.numeric(as.character(aida$Ateco))
aida$Year =  as.numeric(as.character(aida$Year))
manufacturing = subset(aida,Ateco >= 101100 & Ateco <= 332009 & Year>=2007)
nrow(manufacturing) #1.290.032 records for manufactoring sector


# THE YEARS ARE UNBALANCED
tb.year = table(manufacturing$Year)
tb.year = tb.year[order(names(tb.year))]
barplot(tb.year,xlab = "Year")


#SETTORE MANIFATTURIERO  firms with year>=2007

manufacturing = subset(manufacturing,R>=0 & E>=0) 
nrow(manufacturing) #1.029.156

summary(manufacturing)

 manufacturing = subset(manufacturing,manufacturing$Year<2016)
nrow(manufacturing)

{
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
}

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


#SIZE FIRMs - SMALL, MEDIUM, BIG
manufacturing$Size <- NA
manufacturing$Size[manufacturing$E <= 49] <- "Small"
manufacturing$Size[50 <= manufacturing$E & manufacturing$E <= 249] <- "Medium"
manufacturing$Size[manufacturing$E >= 250] <- "Big"



length(unique(manufacturing$TaxID))#169434
nrow(manufacturing) #1027140


manufacturing$R = manufacturing$R+1
manufacturing$E = manufacturing$E+1

nrow(manufacturing)


#manufacturing$z.E= z <- (manufacturing$E - min(manufacturing$E)) /(max(manufacturing$E) - min(manufacturing$E))
#manufacturing$z.R= z <- (manufacturing$R - min(manufacturing$R)) /(max(manufacturing$R) - min(manufacturing$R))

#manufacturing$z.R = manufacturing$z.R-1
#manufacturing$z.E = manufacturing$z.E-1
#View(manufacturing)

save(manufacturing, file="manufacturing.RData")





}
###### VARIABLES DISTRIBUTION  ######
#delete environment
{
ls() #controllo le variabili di ambiente
rm(list = ls()) #rimuoviamo tutte le varaibili d'ambiente
ls() #controllo se sono state eliminate le variabili d'ambiente
}

manufacturing = get(load("manufacturing.RData"))

#summary(manufacturing)
View(manufacturing)

#REGIONS AND PROVINCE DISTRIBUTION
#The regions with the highest numbers of firms are in the North of Italy Nord->Lombardia with 12940, Veneto with 5840 firms and Emilia Romagna with 4989 firms
#The provinces with the highest numbers of firms are also in the North of Italy Nord-> Milano, Vicenza, Brescia
{
#regions distribution
par(mfrow=c(2,2))
a = data.frame(manufacturing$TaxID,manufacturing$Region,manufacturing$Province,manufacturing$GeoArea,manufacturing$Size)
a = unique(a)
tb.region = table(a$manufacturing.Region)
tb.region = tb.region[order(tb.region)]
barplot(tb.region,xlab = "Region")
barplot(tb.region[18:20],xlab = "Top Region")
#provinces distribution
tb.province = table(a$manufacturing.Province)
tb.province = tb.province[order(tb.province)]
tb.province
barplot(tb.province,xlab = "Province")
barplot(tb.province[106:110],xlab = "Top Province")
par(mfrow=c(1,1))
}


#DISTRIBUZIONE DI PROVINCE E REGION IN BASE ALLA ZONA GEOGRAFICA SUD, NORD, CENTRO
{
a = data.frame(manufacturing$TaxID,manufacturing$GeoArea)
a = unique(a)
tb.geo = table(a$manufacturing.GeoArea)
tb.geo = tb.geo[order(tb.geo)]
barplot(tb.geo,xlab="Geo Area")

a = data.frame(manufacturing$GeoArea,manufacturing$Region,manufacturing$Province,manufacturing$TaxID)
a = unique(a)
a
by.geo = split(a,a$manufacturing.GeoArea)
plot.by.geo = function(x){
  par(mfrow=c(3,2))
  nomi = c(names(x))
  nomi[1]
  k=1
   
  for(i in x){
    #distribution by province
    tb.province = table(i$manufacturing.Province)
    tb.province = tb.province[order(tb.province)]
    title=paste("",nomi[k], sep=" ")
    barplot(tb.province[100:110],xlab = "Top Province" ,main =title )
    #distribution by region
    tb.region = table(i$manufacturing.Region)
    tb.region = tb.region[order(tb.region)]
    title=paste("",nomi[k], sep=" ")
    barplot(tb.region[15:20],xlab = "Top Region ",main = title)
    k=k+1
  }
par(mfrow=c(1,1))
}
plot.by.geo(by.geo)
}

#WHAT IS THE DISTRIBUTION ABOUT OURS  FIRMS DATA?
#Now we start analyze our distribution and after we can test the Box Cox trasformation
#TEST IPOTESI DELLA DISTRIBUZIONE DELLE VARAIBILI CONTINUE E, R
#Histograms e density
{
  par(mfrow=c(2,2))
  ateco = split(manufacturing,manufacturing$Ateco)
  R = ateco$`101100`$R #potrebbe essere una distribuzione gamma, poisson o weibull
  E = ateco$`101100`$E # potrebbe essere una distribuzione gamma, poissona o weibull
  
  
  
  hist(R,freq=FALSE,main = "Revenue Distribution",breaks = 20,xlab = "Revenue")
  lines(density(R),col = "red")

  hist(log(R),freq = FALSE,main="Revenue Distribution log()",xlab = "Revenue")
  lines(density(log(R)),col="red")
  
  hist(E,freq=FALSE,main = "Employee Distribution",breaks = 20,xlab = "Employee")
  lines(density(E),col = "blue")
  
  hist(log(E),freq=FALSE,main = "Employee Distribution log()",xlab = "Employee")
  lines(density(log(E)),col = "blue")
  par(mfrow=c(1,1))
}

#prove sulla distribuzione, passi e risultati ottenuti
{ 
  #manufacturing-> 378,612 entries

  #### 1 - Analisi della distribuzione per la totalità di R ed E#####
    #Il dataset di 350.000 circa records, risulta impossibile da analizzare
    #per questo motivo si passa ad un'analisi della distribuzione iniziando a splittare il dataset.
    #Dal grafico è stato possibile osservare che i dati di R ed E seguono una distribuzione forse gamma
   #tutto zero
  #### 2- Analisi della distribuzione per SIZE (small,medium big).####
      #È inutile prendere in considerazione una granularità così grande perchè non otteniamo p.value validi per lo 0.001
                                          #### SMALL #####
    #R sembra seguire una distribuzione tra lognormal e gamma. Invece E sembra seguire una distribuzione centrata su beta
    # Normal: R-->mean=3270.692, sd=10662.140, AIC=6.817.256, p.value=0 
    #         E-->mean=11.61359, sd=11.67069,  AIC=2.471.044, p.value=0
    
    # LNorm:R-->meanlog=6.514234, sdlog=2.465079, AIC=5.632.746, P.VALUE=0 
    #       E-->meanlog=1.854746, sdlog=1.211695, AIC= 2.209.460,p.value=0
  
    # Gamma:R-->shape=0.4145238182, rate=0.0001267834, AIC=5.551.758, p.value=0 
    #       E-->shape=0.96973563, rate=0.08351021 , AIC=2.200.638, p.value=0
  
    # Beta: R-->shape1=0.4142952, shape2=1264.4387655, AIC= -4.723.479, p.value=0
    #       E-->shape1=9.695337e-01, shape2=8.349714e+05 , AIC= -8.074.971, p.value=0
  
    # Weibull: R--> shape=0.5588465, scale=1956.2636744 , AIC=5.540.933. p.value=0
    #          E--> p.value=0
  
    # Expon  : R-->p.value=0
    #          E-->p.value=0
                                          ### MEDIUM #####
    #R sembra seguire una distribuzione tra lognormal e gamma ma più vicina a gamma. Invece E sembra seguire una distribuzione centrata su beta uguale per small
    # Normal: R-->p.value=0
    #         E-->p.value=0
    
    # LNorm:  R-->p.value=0
    #         E-->p.value=0
    
    # Gamma:  R-->p.value=0
    #         E-->p.value=0
    
    # Beta:   R-->p.value=0
    #         E-->p.value=0
    
    # Weibu:  R-->p.value=0
    #         E-->p.value=0
    
    # Expon : R-->p.value=0
    #         E-->p.value=0
                                          #### MEDIUM BY ACTIVE -- 47669 ####
  
                                          #### MEDIUM BY ACTIVE, ALIMENTARE --3231 ####
  #LNORM R:->0.06593318

                                          #### MEDIUM BY ACTIVE, NCA,2014 -- 931 ####
  #LOGNORM R:->0.1811011
  #BETA R:->0.000224175
  #
  
                                          #### BIG #####
    # Secondo il grafico R si trova su beta, ed E si trova vicino log 
    # Normal: R-->p.value=0
    #         E-->p.value=0
    
    # LNorm:  R-->p.value=0
    #         E-->p.value=0
    
    # Gamma:  R-->p.value=0
    #         E-->p.value=0
    
    # Beta:   R-->p.value=0
    #         E-->p.value=0
    
    # Weibu:  R-->p.value=0
    #         E-->p.value=0
    
    # Expon : R-->p.value=0
    #         E-->p.value=0 #tutto zero
                                          #### BIG BY ACTIVE, NCA 2014####
  #LOGNORM E->0.006286393
  #LOGNORM R->0.09053475
  
  
  
                                          #### BIG BY ACTIVE NCA 2015 --171####
  
  #### 3 - Analisi della distribuzione per STATUS ####
                                          ##### ACTIVE -- 339,057 entries ######
  #troppo grande e troppa dispersione
                                    
  
 
                                    
  unique(manufacturing$Status)
                                    
  
                                          ##### ACTIVE by YEAR -- 37,673 entries for every year####
                                          ##### ACTIVE BY REGION Abruzzo, YEAR 2007 -- 691 entries ####
  #R -> beta, E-> gamma/beta
  # Normal: R-->0
  #         E-->0
  
  # LNorm:R--> 7.633145e-06
  #       E-->2.041734e-06
  
  # Gamma:R-->2.422297e-08
  #       E-->1.635403e-11
  
  # Beta: R-->3.718057e-08
  #       E-->3.043633e-10
  
  # Weibull:R-->0.03062575
  #         E-->1.374938e-08
  
  # Expon  :R-->0
  #         E-->1.48598e-10
  
  # Geom  :R-->0
  #         E-->0
  
  #Pareto :R-->0
  #         E--0
  
  #LOgistica :R-->0
  #           :E->0
  
  
                                          

                                          
                                          ##### BANKRU -- 2223 ####
  #LNORM R->0.004054871
  #### 4 - Analisi distribuzione per SUBSECTORS 
                                          #####  MOBILI  -- 17 troppo pochi#######
                      
                                          #####  BEVANDE  -- 5823 (Shit)#######
  #R -> beta, E-> beta
  # Normal: R-->0
  #         E-->0
  
  # LNorm:R--> 4.477032e-07
  #       E-->4.037856e-09
  
  # Gamma:R-->0
  #       E-->0
  
  # Beta: R-->
  #       E-->
  
  # Weibull:R-->0.001614602
  #         E-->0
  
  # Expon  :R-->0
  #         E-->0
  
  # Geom  :R-->0
  #         E-->0
  
  #Pareto :R-->0
  #         E--0
  
  #LOgistica :R-->0
  #           :E->0
  
  
  
  
                                          #####  BEVANDE BY SMALL -- 5176 #####
  #WEIUBULL R->0.0001544586
  
  
                                          #####  PELLE --12042 #####
                    #only shit
  
                                          
                                          ##### PELLE BY SMALL -- 10204#####
  #only only only shit
                                          #####  PELLE BY MEDIUM -- #####
  #LOGNORM R->0.2100501
  
  
  #### 5 - Analisi distribuzione per Province 
                                          ##### FOGGIA -- 819#####
  #R -> beta, E-> beta
  # Normal: R-->0
  #         E-->0
  
  # LNorm:R--> 4.921619e-13
  #       E-->0
  
  # Gamma:R-->
  #       E-->
  
  # Beta: R-->2.627657e-10
  #       E-->0
  
  # Weibull:R-->2.367251e-11
  #         E-->0
  
  # Expon  :R-->0
  #         E-->0
  
  # Geom  :R-->0
  #         E-->0
  
  #Pareto :R-->0
  #         E--0
  
  #LOgistica :R-->0
  #           :E->0
  
                                          ##### FOGGIA BY Small -- 819 non possiamo procedere su FOggia####  
                                          ##### FOGGIA BY YEAR 2007 -- 91 ####
  #R -> beta, E-> beta
  # Normal: R-->0.005839164
  #         E-->0.0001837799
  
  # LNorm:R--> 0.0006785059
  #       E-->0.8842354
  
  # Gamma:R-->
  #       E-->
  
  # Beta: R-->0.04743787
  #       E-->0.3059009
  
  # Weibull:R-->0.002372771
  #         E-->1.916052e-05
  
  # Expon  :R-->7.917e-13
  #         E--> 0.1090379
  
  # Geom  :R-->
  #         E-->0.01770227
  
  #Pareto :R-->
  #         E-->0.002840455
  
  #Logistica :R-->9.177104e-13
  #           :E->7.669976e-09
                                          
    
}

set.seed(10)
#distribution and hypotesis test ks
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  library(boot)
  
  a = split(manufacturing,manufacturing$Size)
  a = a$Big
  nrow(a)
  food=a
  #View(food)

  E = food$E
  R = food$R
  
  par(mfrow=c(2,1))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  R = food$E[food$E>1]
  R = food$R[food$R>1]
  
  {
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
   
    n.sims <- 1000
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  E = food$E[food$E>1]
  E = food$R[food$R>1]
  {
    
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$estimate
    fit.lnorm$aic
    
  
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION 
  E = food$E[food$E>1]
  E = food$R[food$R>1]
  {
    
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(r, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    }
  
  #BETA
  R = food$E[food$E>1]*0.00000001
  R = food$R[food$R>1]*0.00000001
  {
    fit.beta<-fitdist(R, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 2000
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)

  }
  
  #WEIBULL
  R = food$E[food$E>1]
  R = food$R[food$R>1]
  {
    
    
    fit.weibull <- fitdist(R, "weibull")
    fit.weibull$estimate
    fit.weibull$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
  #ESPONENZIALE
  R = food$E[food$E>1]
  R = food$R[food$R>1]
  {
 
  
  fit.exp<-fitdist(R,"exp",method = c("mle") 
  #plot(fit.exp)
  fit.exp$estimate
  fit.exp$aic
  
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rexp(n = length(R), rate = fit.exp$estimate[1]  )
      as.numeric(ks.test(r, "pexp", rate = fit.exp$estimate[1])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
   1- plogspline(ks.test(R,"pexp",rate = fit.exp$estimate[1] )$statistic
               , fit
    )
  
  
  }
  
  #GEOMETRICA
  R = food$E[food$E>1]
  R = food$R[food$R>1]
  {
    
    fit.geom<-fitdist(round(R),"geom",method = c("mle"))
    #plot(fit.exp)
    fit.geom$estimate
    fit.geom$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rgeom(n = length(R), prob = fit.geom$estimate[1]  )
      as.numeric(ks.test(r, "pgeom", prob = fit.geom$estimate[1])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1- plogspline(ks.test(R,"pgeom",prob = fit.geom$estimate[1] )$statistic
                  , fit
    )
    
    
  }
  

  #LOG LOGISTICA
  R = food$E[food$E>1]
  R = food$R[food$R>1]
  {
  #nella distribuzione lloglogistica, i parametri devo essere positivi
  fit.llogis<- fitdist(R ,"llogis", start = list(shape = 1))
    #plot(fit.llogis)
    fit.llogis$estimate
    fit.llogis$aic
    
    n.sims <- 2000
    stats <- replicate(n.sims, {   
      r <- rllogis(n = length(R), shape = fit.llogis$estimate[1]  )
      as.numeric(ks.test(r, "pllogis",  shape = fit.llogis$estimate[1])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1- plogspline(ks.test(R,"pllogis", shape = fit.llogis$estimate[1] )$statistic
                  , fit
    )
  
  }
  

  #POWERLAW FOR CONTINUOUS ATTRIBUTE
  R = food$E
  R = food$R
  {
    
    fp <- fitdist(R, "pareto", lower = c(0, 0), start = list(scale = 1, shape = 1))
    fp$estimate
    fp$aic
    
    
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rpareto(n = length(R), scale=fp$estimate[1] , shape = fp$estimate[2] )
      as.numeric(ks.test(r, "ppareto", scale=fp$estimate[1] , shape = fp$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1- plogspline(ks.test(R,"ppareto", scale=fp$estimate[1] , shape = fp$estimate[2])$statistic
                  , fit
    )
  
  }
  
  
 
  
}


###### POWER LAW ######
#POWERLAW FOR CONTINUOUS ATTRIBUTE
#For computational problems, I fit power law for Alimentare/Food subsector for 2007-2008
#2007-> E-p.value=0.609      alpha=2.205376  xmin=78       time=6min
#2007-> R-p.value=1          alpha=2.332329  xmin=99240.45 time=6min

#2008->E-p.value=0.68      alpha=2.244039   xmin=74              time=2min
#2008->R-p.value=1        alpha=2.247034   xmin=98234.11        time=6min

#2009->E-p.value=      alpha=   xmin=              time=2min
#2009->R-p.value=      alpha=   xmin=        time=6min

{
  manufacturing = get(load("manufacturing.RData"))
  
  food = manufacturing[manufacturing$SubSector=="Alimentare",]
  
  View(manufacturing)
  nrow(food)
  food.E = food$E
  plot(food.E)
  View(food.E)
  e=table(food.E)
  names(e)
  plot(e)
  plot(names(e),e)
  plot(names(e), e , log="xy",xlab="N. Employee",ylab="Freq")
  plot(density(moby), log="xy") # density does not work
  
  food.R = food$R[food$Year==2007]
  
  plot(R)
  length(r)
  r=food.R
  m_r = conpl$new(r) #we can estimate the same value for continuous var
  (est = estimate_xmin(m_r))
  m_r$setXmin(est)
  
  #plot data from xmin
  plot(m_r,ylab="Freq")
  View(m_r$dat)
  m_r
  
  #add in the fitted value
  lines(m_r, col=2)
  
  parallel::detectCores() #for know how cores we have
  bs_p = bootstrap_p(m_r, no_of_sims=200, threads=4)
  bs_p$p
  plot(bs_p)
  hist(bs_p$bootstraps[,1])
  

}





###### CORRELATION ANALYSIS #####
manufacturing = get(load("/Users/alessandroarmillotta/Desktop/Statistica/Progetto/Progetto_aida_2/manufacturing.RData")) #dataset aida


#effettuiamo la correlazione di pearson per ogni Year
manufacturing$Year =  as.numeric(as.character(manufacturing$Year))

#View(x)
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
#least square. la varianza di una delle due è molto più piccola
#intervalli di confidenza su least square





######
{
  statistic <- function(x, inds) {fitdist(x[inds],"norm")$estimate}
  bs <- boot(R, statistic, R = 40)
  print(boot.ci(bs, conf=0.95, type="bca"))
  
#####vecchio#####
#DISTRIBUTION

  
  #DISTRIBUZIONE POISSON
  fit.pois.R<-fitdistr(R+1, densfun="poisson")
  fit.pois.R$estimate
  AIC(fit.pois.R)
  {
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rpois(n = length(R), lambda = fit.pois.R$estimate[1] )
      as.numeric(ks.test(r, "ppois", lambda = fit.pois.R$estimate[1])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(R,"ppois",lambda = fit.pois.R$estimate[1])$statistic
               , fit
    )
  }
  #LAPLACE
  m = median(R)
  t = mean(abs(R-m))
  {
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rlaplace(n = length(R), location = m,scale = t )
      as.numeric(ks.test(r, "plaplace", location = m,scale = t)$statistic)      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(R,"plaplace",location = m,scale = t)$statistic,fit)
  }
  
  
  
  

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




###### distribution #######

#distribution and hypotesis test for R and E of 222900 and 13940 by year
#Sono usciti tutti p.value alti ma per ogni ateco-anno si producono pochi record. 
#Questo campione mostra norm vicino lo zero e le altre più alte. La box Cox Potrebbe funzionare.
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  ateco = split(manufacturing,manufacturing$Ateco)
  R = ateco$`222900`$R[ateco$`222900`$Year=="2007"]
  E = ateco$`222900`$E[ateco$`222900`$Year=="2007"]
  
  R = ateco$`139400`$R[ateco$`139400`$Year=="2007"]
  E = ateco$`139400`$E[ateco$`139400`$Year=="2007"]
  
  
  
  par(mfrow=c(2,2))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R = ateco$`222900`$R[ateco$`222900`$Year=="2007"]
    R = ateco$`222900`$E[ateco$`222900`$Year=="2007"]
    
    
    R = ateco$`139400`$R[ateco$`139400`$Year=="2007"]
    R = ateco$`139400`$E[ateco$`139400`$Year=="2007"]
    
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 5e4
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  {
    E = ateco$`222900`
    E = E$R[E$Year=="2007"]#p.value = 0.8872644
    E = ateco$`222900`
    E = E$E[E$Year=="2007"]#p.value = 0.7336559
    
    E = ateco$`139400`
    E = E$R[E$Year=="2007"]#p.value = 0.5411786
    E = ateco$`139400`
    E = E$E[E$Year=="2007"]#p.value = 0.4813196
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 5e4
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION OK
  {
    E = ateco$`222900`
    E = E$R[E$Year=="2007"]#p.value = 0.1706319
    E = ateco$`222900`
    E = E$E[E$Year=="2007"]#p.value = 0.2922023
    
    E = ateco$`139400`
    E = E$R[E$Year=="2007"]#p.value = 0.9911243
    E = ateco$`139400`
    E = E$E[E$Year=="2007"]#p.value = 0.7108185
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 5e4
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = ateco$`222900`
    R=R$R[R$Year=="2007"]*0.00001 #p.value = 0.4234946
    R = ateco$`222900`
    R=R$E[R$Year=="2007"]*0.00001 #p.value = 0.8352383
    
    R = ateco$`139400`
    R=R$R[R$Year=="2007"]*0.00001#p.value = 0.9871316
    R = ateco$`139400`
    R=R$E[R$Year=="2007"]*0.00001#p.value = 0.7113059
    
    fit.beta<-fitdist(R, "beta")
    plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 5e4
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = ateco$`222900`
    R=R$R[R$Year=="2007"] #p.value = 0.8071699
    R = ateco$`222900`
    R=R$E[R$Year=="2007"] #p.value = 0.8150588
    
    R = ateco$`139400`
    R=R$R[R$Year=="2007"]#p.value = 0.9957922
    R = ateco$`139400`
    R=R$E[R$Year=="2007"]#p.value = 0.7858101
    
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 5e4
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
  
}

#distribution and hypotesis test for R and E of 332009 and 13940 by year by BOX COX --per me questa
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  ateco = split(manufacturing,manufacturing$Ateco)
  R = ateco$`332009`$R[ateco$`332009`$Year=="2007"] 
  E = ateco$`332009`$E[ateco$`332009`$Year=="2007"]
  
  
  R = ateco$`332003`$R[ateco$`332003`$Year=="2007"]
  E = ateco$`33`$E[ateco$`139400`$Year=="2007"]
  
  
  
  par(mfrow=c(2,2))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R = boxcoxnc(ateco$`332009`$R[ateco$`332009`$Year=="2007"],method = "mle")$tf.data #0.5180935
    R = boxcoxnc(ateco$`332003`$R[ateco$`332003`$Year=="2007"],method = "mle")$tf.data #0.01713807
    R = ateco$`332009`$E[ateco$`332009`$Year=="2007"]
    
    
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 5e4
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  {
    E = ateco$`222900`
    E = E$R[E$Year=="2007"]#p.value = 0.8872644
    E = ateco$`222900`
    E = E$E[E$Year=="2007"]#p.value = 0.7336559
    
    E = ateco$`139400`
    E = E$R[E$Year=="2007"]#p.value = 0.5411786
    E = ateco$`139400`
    E = E$E[E$Year=="2007"]#p.value = 0.4813196
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 5e4
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION OK
  {
    E = ateco$`222900`
    E = E$R[E$Year=="2007"]#p.value = 0.1706319
    E = ateco$`222900`
    E = E$E[E$Year=="2007"]#p.value = 0.2922023
    
    E = ateco$`139400`
    E = E$R[E$Year=="2007"]#p.value = 0.9911243
    E = ateco$`139400`
    E = E$E[E$Year=="2007"]#p.value = 0.7108185
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 5e4
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = ateco$`222900`
    R=R$R[R$Year=="2007"]*0.00001 #p.value = 0.4234946
    R = ateco$`222900`
    R=R$E[R$Year=="2007"]*0.00001 #p.value = 0.8352383
    
    R = ateco$`139400`
    R=R$R[R$Year=="2007"]*0.00001#p.value = 0.9871316
    R = ateco$`139400`
    R=R$E[R$Year=="2007"]*0.00001#p.value = 0.7113059
    
    fit.beta<-fitdist(R, "beta")
    plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 5e4
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = ateco$`222900`
    R=R$R[R$Year=="2007"] #p.value = 0.8071699
    R = ateco$`222900`
    R=R$E[R$Year=="2007"] #p.value = 0.8150588
    
    R = ateco$`139400`
    R=R$R[R$Year=="2007"]#p.value = 0.9957922
    R = ateco$`139400`
    R=R$E[R$Year=="2007"]#p.value = 0.7858101
    
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 5e4
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
  
}

#distribution and hypotesis test for R and E of 222900 and 332003 with  box-cox transform
#qualche p value positivo è uscito ma da scartare come test
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  ateco = split(manufacturing,manufacturing$Ateco)
  
  
  R = boxcoxnc(ateco$`222900`$R, method = "mle")
  R = R$tf.data
  E = boxcoxnc(ateco$`222900`$E, method = "mle")
  E = E$tf.data
  
  
  R = boxcoxnc(ateco$`332003`$R, method = "mle")
  R = R$tf.data
  E = boxcoxnc(ateco$`332003`$E, method = "mle")
  E = E$tf.data
  
  par(mfrow=c(2,1))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R = boxcoxnc(ateco$`222900`$R, method = "mle") #p.value = 0.08546113
    R = R$tf.data
    R = boxcoxnc(ateco$`222900`$E, method = "mle") #p.value = 0.1633514
    R = R$tf.data
    
    R = boxcoxnc(ateco$`332003`$R, method = "mle") #p.value = 0.0001524639
    R = R$tf.data
    R = boxcoxnc(ateco$`332003`$E, method = "mle") #p.value = 5.614398e-13
    R = R$tf.data
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 5e4
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  
  {
    E = boxcoxnc(ateco$`222900`$R, method = "mle") #p.value = 2.87373e-05
    E = E$tf.data+1
    E = boxcoxnc(ateco$`222900`$E, method = "mle") #p.value = 0.005356138
    E = E$tf.data+1
    
    E = boxcoxnc(ateco$`332003`$R, method = "mle") #p.value = 0
    E = E$tf.data+1
    E = boxcoxnc(ateco$`332003`$E, method = "mle") #p.value = 0
    E = E$tf.data+1
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 5e4
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION OK
  {
    E = boxcoxnc(ateco$`222900`$R, method = "mle") #p.value = 
    E = E$tf.data
    E = boxcoxnc(ateco$`222900`$E, method = "mle") #p.value = 
    E = E$tf.data
    
    E = boxcoxnc(ateco$`332003`$R, method = "mle") #p.value = 
    E = E$tf.data
    E = boxcoxnc(ateco$`332003`$E, method = "mle") #p.value = 
    E = E$tf.data
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"))
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 5e4
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = ateco$`222900`$E #R_pvalue->0.07460977 #E_pvalue->0.1504125
    R=R*0.00001
    
    R = ateco$`332003`$R #R_pvalue->0 #E_pvalue->0
    R=R*0.000001
    
    fit.beta<-fitdist(R, "beta")
    plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 5e4
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = ateco$`222900`$R #0.1537166
    R = ateco$`222900`$E #0.1353928-0.13
    
    R = ateco$`332003`$R # 2.804009e-08
    R = ateco$`332003`$E # 0
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 5e4
    
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
  
}

#distribution and hypotesis test for R and E for year.
#eseguito ma tutti p.value 0
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  year = split(manufacturing,manufacturing$Year)
  R = year$`2007`$R
  E = year$`2007`$E
  
  par(mfrow=c(1,1))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R = year$`2007`$R #p.value = 0
    R = year$`2007`$E #p.value = 0
    
    R = year$`2012`$R #p.value = 0
    R = year$`2012`$E #p.value = 0
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  {
    E = year$`2007`$R #p.value = 0
    E = year$`2007`$E #p.value = 0
    
    E = year$`2012`$R #p.value = 0
    E = year$`2012`$E #p.value = 0
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA  OK
  {
    E = year$`2007`$R #p.value = 0
    E = year$`2007`$E #p.value = 0
    
    E = year$`2012`$R #p.value = 0
    E = year$`2012`$E #p.value = 0
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = year$`2007`$R*0.00000001 #p.value = 0
    R = year$`2007`$E*0.00000001 #p.value = 0
    
    E = year$`2012`$R*0.00000001 #p.value = 0
    E = year$`2012`$E*0.00000001 #p.value = 0
    
    
    fit.beta<-fitdist(R, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = year$`2007`$R #p.value = 0
    R = year$`2007`$E #p.value = 0
    
    R = year$`2012`$R #p.value = 0
    R = year$`2012`$E #p.value = 0
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 100
    
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
  
}

#distribution and hypotesis test for R and E for year with box-cox transform.
#non riesco ad eseguirlo perchè troppi valori per la trasformazione
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  year = split(manufacturing,manufacturing$Year)
  R = boxcoxnc(year$`2007`$R, method = "mle")
  R = 
    E = boxcoxnc(year$`2007`$E, method = "mle")
  E = E$tf.data
  
  
  R = boxcoxnc(year$`2012`$E, method = "mle")
  R = R$tf.data
  E = boxcoxnc(year$`2012`$E, method = "mle")
  E = E$tf.data
  
  
  
  par(mfrow=c(1,1))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R = year$`2007`$R #p.value = 0
    R = year$`2007`$E #p.value = 0
    
    R = year$`2012`$R #p.value = 0
    R = year$`2012`$E #p.value = 0
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  {
    E = year$`2007`$R #p.value = 0
    E = year$`2007`$E #p.value = 0
    
    E = year$`2012`$R #p.value = 0
    E = year$`2012`$E #p.value = 0
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA  OK
  {
    E = year$`2007`$R #p.value = 0
    E = year$`2007`$E #p.value = 0
    
    E = year$`2012`$R #p.value = 0
    E = year$`2012`$E #p.value = 0
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = year$`2007`$R*0.00000001 #p.value = 0
    R = year$`2007`$E*0.00000001 #p.value = 0
    
    E = year$`2012`$R*0.00000001 #p.value = 0
    E = year$`2012`$E*0.00000001 #p.value = 0
    
    
    fit.beta<-fitdist(R, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = year$`2007`$R #p.value = 0
    R = year$`2007`$E #p.value = 0
    
    R = year$`2012`$R #p.value = 0
    R = year$`2012`$E #p.value = 0
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 100
    
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
  
}

#distribution and hypotesis test for R and E by Alimentare and Pelle SubSector
#Tutti i  pvalue vicino a zero
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  subSec = split(manufacturing,manufacturing$SubSector)
  R = subSec$Alimentare$R
  E = subSec$Alimentare$E
  
  R = subSec$Pelle$R
  E = subSec$Pelle$E
  
  par(mfrow=c(2,2))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R = subSec$Alimentare$R #0
    R = subSec$Alimentare$E #0
    
    R = subSec$Pelle$R #0
    R = subSec$Pelle$E #0
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  
  {
    E = subSec$Alimentare$R #0
    E = subSec$Alimentare$E #0
    
    E = subSec$Pelle$R #0
    E = subSec$Pelle$E #0
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION OK
  {
    E = subSec$Alimentare$R #6.661338e-15
    E = subSec$Alimentare$E #0
    
    E = subSec$Pelle$R #9.6266e-11
    E = subSec$Pelle$E #0
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = subSec$Alimentare$R*0.0000001 #0
    R = subSec$Alimentare$E*0.0000001 #0
    
    R = subSec$Pelle$R*0.0000001 #1.217915e-13
    R = subSec$Pelle$E*0.0000001 #0
    
    fit.beta<-fitdist(R, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = subSec$Alimentare$R #0
    R = subSec$Alimentare$E #0
    
    R = subSec$Pelle$R #3.796004e-09
    R = subSec$Pelle$E #0
    
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 100
    
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
  
}

#distribution and hypotesis test for R and E by Alimentare by Year 2007-2008
#Tutte su pvalue pari a zero
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  subSec = split(manufacturing,manufacturing$SubSector)
  
  R=subSec$Alimentare$R[subSec$Alimentare$Year=="2007"]
  E=subSec$Alimentare$E[subSec$Alimentare$Year=="2007"]
  
  R=subSec$Alimentare$R[subSec$Alimentare$Year=="2008"]
  E=subSec$Alimentare$E[subSec$Alimentare$Year=="2008"]
  
  
  par(mfrow=c(2,2))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #0
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #0
    
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #0
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #0
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  {
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #2.654543e-13
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #3.18634e-14
    
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #0
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #0
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION OK
  {
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #2.298162e-14
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #0
    
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #3.252953e-14
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #0
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"]*0.0000001 #8.456518e-08
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"]*0.0000001 #0
    
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"]*0.0000001 #3.833449e-09
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"]*0.0000001 #1.096879e-09
    
    fit.beta<-fitdist(R, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #2.497084e-08
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #2.416956e-13
    
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #3.006896e-05
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #7.355228e-13
    
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 100
    
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
}

#distribution and hypotesis test for R and E by Alimentare by Year 2007-2008 wiyh box-cox
#Box Cox dovrebbe convergere i valori verso  distr NORMALe ma nkn succedere
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  subSec = split(manufacturing,manufacturing$SubSector)
  
  
  R = boxcoxnc(subSec$Alimentare$R[subSec$Alimentare$Year=="2007"], method = "mle",plot = FALSE)$tf.data
  E = boxcoxnc(subSec$Alimentare$E[subSec$Alimentare$Year=="2007"], method = "mle",plot = FALSE)$tf.data
  
  R = boxcoxnc(subSec$Alimentare$R[subSec$Alimentare$Year=="2008"], method = "mle",plot = FALSE)$tf.data
  E = boxcoxnc(subSec$Alimentare$E[subSec$Alimentare$Year=="2008"], method = "mle",plot = FALSE)$tf.data
  
  
  par(mfrow=c(2,2))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R = boxcoxnc(subSec$Alimentare$R[subSec$Alimentare$Year=="2007"], method = "mle",plot = FALSE)$tf.data
    R = boxcoxnc(subSec$Alimentare$E[subSec$Alimentare$Year=="2007"], method = "mle",plot = FALSE)$tf.data
    
    R = boxcoxnc(subSec$Alimentare$R[subSec$Alimentare$Year=="2008"], method = "mle",plot = FALSE)$tf.data
    R = boxcoxnc(subSec$Alimentare$E[subSec$Alimentare$Year=="2008"], method = "mle",plot = FALSE)$tf.data
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  {
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #2.654543e-13
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #3.18634e-14
    
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #0
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #0
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION OK
  {
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #2.298162e-14
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #0
    
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #3.252953e-14
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #0
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"]*0.0000001 #8.456518e-08
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"]*0.0000001 #0
    
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"]*0.0000001 #3.833449e-09
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"]*0.0000001 #1.096879e-09
    
    fit.beta<-fitdist(R, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #2.497084e-08
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #2.416956e-13
    
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #3.006896e-05
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #7.355228e-13
    
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 100
    
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
}

#distribution and hypotesis test for R and E by Tessile by Year 2007-2008
#nessun pvalue è superiore a 0.002
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  subSec = split(manufacturing,manufacturing$SubSector)
  
  R=subSec$Tessile$R[subSec$Tessile$Year=="2007"]
  E=subSec$Tessile$E[subSec$Tessile$Year=="2007"]
  
  R=subSec$Tessile$R[subSec$Tessile$Year=="2008"]
  E=subSec$Tessile$E[subSec$Tessile$Year=="2008"]
  
  
  par(mfrow=c(2,2))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R =subSec$Tessile$R[subSec$Tessile$Year=="2007"] #0
    R =subSec$Tessile$E[subSec$Tessile$Year=="2007"] #0
    
    R =subSec$Tessile$R[subSec$Tessile$Year=="2008"] #0
    R =subSec$Tessile$E[subSec$Tessile$Year=="2008"] #0
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  {
    E =subSec$Tessile$R[subSec$Tessile$Year=="2007"] #1.88028e-06
    E =subSec$Tessile$E[subSec$Tessile$Year=="2007"] #1.705671e-06
    
    E =subSec$Tessile$R[subSec$Tessile$Year=="2008"] #2.103412e-10
    E =subSec$Tessile$E[subSec$Tessile$Year=="2008"] #5.658082e-09
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION OK
  {
    E =subSec$Tessile$R[subSec$Tessile$Year=="2007"] #0.0005302532
    E =subSec$Tessile$E[subSec$Tessile$Year=="2007"] #6.042389e-12
    
    E =subSec$Tessile$R[subSec$Tessile$Year=="2008"] #0.0009887784
    E =subSec$Tessile$E[subSec$Tessile$Year=="2008"] #6.100009e-12
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = subSec$Tessile$R[subSec$Tessile$Year=="2007"]*0.0000001 #0.007051019
    R = subSec$Tessile$E[subSec$Tessile$Year=="2007"]*0.0000001 #1.332268e-15
    
    R = subSec$Tessile$R[subSec$Tessile$Year=="2008"]*0.0000001 #0.003505322
    R = subSec$Tessile$E[subSec$Tessile$Year=="2008"]*0.0000001 #0
    
    fit.beta<-fitdist(R, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = subSec$Tessile$R[subSec$Tessile$Year=="2007"] #0.00247917
    R = subSec$Tessile$E[subSec$Tessile$Year=="2007"] #0.001737885
    
    R = subSec$Tessile$R[subSec$Tessile$Year=="2008"] #0.001678193
    R = subSec$Tessile$E[subSec$Tessile$Year=="2008"] #4.669598e-13
    
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 100
    
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
}

#distribution and hypotesis test for R and E by Tessile by Year 2007-2008 wiyh box-cox
#Box Cox dovrebbe convergere i valori verso  tutti i valori
{
  library(fitdistrplus)
  library(logspline)
  library(moments)
  library(bbmle)
  library(actuar)
  library(VGAM)
  library(poweRlaw)
  library(fExtremes)
  library(AID)
  subSec = split(manufacturing,manufacturing$SubSector)
  
  
  R = boxcoxnc(subSec$Tessile$R[subSec$Tessile$Year=="2007"], method = "mle",plot = FALSE)$tf.data
  E = boxcoxnc(subSec$Tessile$E[subSec$Tessile$Year=="2007"], method = "mle",plot = FALSE)$tf.data
  
  R = boxcoxnc(subSec$Tessile$R[subSec$Tessile$Year=="2008"], method = "mle",plot = FALSE)$tf.data
  E = boxcoxnc(subSec$Tessile$E[subSec$Tessile$Year=="2008"], method = "mle",plot = FALSE)$tf.data
  
  
  par(mfrow=c(2,2))
  descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
  descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE
  {
    R = boxcoxnc(subSec$Tessile$R[subSec$Tessile$Year=="2007"], method = "mle",plot = FALSE)$tf.data #0.009630624
    R = boxcoxnc(subSec$Tessile$E[subSec$Tessile$Year=="2007"], method = "mle",plot = FALSE)$tf.data #9.384861e-08
    
    R = boxcoxnc(subSec$Tessile$R[subSec$Tessile$Year=="2008"], method = "mle",plot = FALSE)$tf.data
    E = boxcoxnc(subSec$Tessile$E[subSec$Tessile$Year=="2008"], method = "mle",plot = FALSE)$tf.data
    
    fit.norm <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(R), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  {
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #2.654543e-13
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #3.18634e-14
    
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #0
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #0
    
    fit.lnorm<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    1 - plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION OK
  {
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #2.298162e-14
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #0
    
    E = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #3.252953e-14
    E = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #0
    
    fit.gamma<-fitdist(E,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(E, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(E,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic,
                   fit)
    
    
  }
  
  
  #BETA
  {
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"]*0.0000001 #8.456518e-08
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"]*0.0000001 #0
    
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"]*0.0000001 #3.833449e-09
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"]*0.0000001 #1.096879e-09
    
    fit.beta<-fitdist(R, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    1 - plogspline(ks.test(R,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic, fit)
    
    
  }
  
  
  #WEIBULL
  {
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2007"] #2.497084e-08
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2007"] #2.416956e-13
    
    R = subSec$Alimentare$R[subSec$Alimentare$Year=="2008"] #3.006896e-05
    R = subSec$Alimentare$E[subSec$Alimentare$Year=="2008"] #7.355228e-13
    
    fit.weibull <- fitdist(R, "weibull")
    n.sims <- 100
    
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(R)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    1 - plogspline(ks.test(R, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])$statistic, fit
    )
    
  }
  
}
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









##### DOMANDE TANTARI#####
#1-Prima di procedere all'analisi della correlazione, dobbiamo analizzare la distribuzione
#2-Una volta analizzata ed individuato il tipo di distribuzione procediamo all'analisi della correlazione
#3-Dopo aver analizzato  la correlazione, effettuiamo un test di ipotesi come H0- non correlate e H1 correlate

}