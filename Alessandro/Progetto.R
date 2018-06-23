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
manufacturing$Year =  as.numeric(as.character(manufacturing$Year))
manufacturing = subset(manufacturing,R>=0 & E>=0) 
manufacturing$Year =  as.factor(manufacturing$Year)
nrow(manufacturing) #1.029.156


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


#SIZE FIRMs - SMALL, MEDIUM, BIG
manufacturing$Size <- NA
manufacturing$Size[manufacturing$E <= 49] <- "Small"
manufacturing$Size[50 <= manufacturing$E & manufacturing$E <= 249] <- "Medium"
manufacturing$Size[manufacturing$E >= 250] <- "Big"



length(unique(manufacturing$TaxID))#42.068 firms
nrow(manufacturing) #378.612 unique records
save(manufacturing, file="manufacturing.RData")



}
###### DISTRIBUTION VARIABLES ######
#delete environment
{ls() #controllo le variabili di ambiente
rm(list = ls()) #rimuoviamo tutte le varaibili d'ambiente
ls() #controllo se sono state eliminate le variabili d'ambiente
}
manufacturing = get(load("manufacturing.RData"))
summary(manufacturing)

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
#Ting Ting Chen and Tetsuya Takaishi in the 2nd International Conference on Mathematical Modeling in Physical Sciences 
#claim that Firm size data usually do not show the normality that is often assumed in statistical analysis 
#such as regression analysis. Firm size data are important variables to find relationships among financial indicators. 
#However it is well-known that firm size data are not normally distributed and often suggested to follow a log-normal distribution.
#In this study they focus on two firm size data: the number of employees and sale. 
#Those data deviate considerably from a normal distribution. 
#To improve the normality of those data they transform them by the Box-Cox transformation 
#with appropriate parameters. It is found that the two firm size data transformed by the Box-Cox 
#transformation show strong linearity. This indicates that the number of employees and sale have the 
#similar property as a firm size indicator.

#Now we start analyze our distribution and after we can test the Box Cox trasformation
#TEST IPOTESI DELLA DISTRIBUZIONE DELLE VARAIBILI CONTINUE E, R

#istogrammi e density
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

#distribution and hypotesis test for total R and total E
{
  
  
  #R maybe is close to BETA distribution with some sample of bootstrap in GAMMA distribution
  #E maybe is close to gamma distribution
  # For the evaluation,  we use p-value and AIC. Usualy in this samples, the p-value is always high and the AIC 
  #is the best fits value.  We test: 
  #Normal Distribution : 
  #LogNormal Distribution 
  #Gamma Distribution :
  #Beta distribution : 
  
  #without box-cox transformation
  #R distribution is Beta Distribuited
  #E distribution
  {
    R = manufacturing$R
    E = manufacturing$E
    #distribution for E and R
    par(mfrow=c(2,1))
    descdist(R, discrete = FALSE,obs.col="red", boot=100,obs.pch = 15, boot.col="blue")
    descdist(E, discrete = FALSE,obs.col="red", boot=100,obs.pch = 15, boot.col="green")
    par(mfrow=c(1,1))
    
  #DISTRIBUZIONE NORMALE 
  # R: mean = 13213.56, sd=133312.64  , AIC: 10.010.044
  {
  fit.norm.R <-fitdist(R, "norm",method = c("mle"))
  #plot(fit.norm.R)
  fit.norm.R$estimate
  fit.norm.R$aic
  n.sims <- 100
  stats <- replicate(n.sims, {      
    r <- rnorm(n = length(R), mean = fit.norm.R$estimate["mean"], sd = fit.norm.R$estimate["sd"])
    as.numeric(ks.test(r, "pnorm", mean = fit.norm.R$estimate["mean"], sd = fit.norm.R$estimate["sd"])$statistic
    )      
  })
  fit <- logspline(stats)
  plogspline(ks.test(R,"pnorm", mean = fit.norm.R$estimate["mean"], sd = fit.norm.R$estimate["sd"])$statistic, fit)
  }
  # E: mean = 38.65826, sd=207.28282  , AIC: 5.113.555
  {
    fit.norm.E <-fitdist(E, "norm",method = c("mle"))
    #plot(fit.norm.E)
    fit.norm.E$estimate
    fit.norm.E$aic
    n.sims <- 100
    stats <- replicate(n.sims, {      
      r <- rnorm(n = length(E), mean = fit.norm.E$estimate["mean"], sd = fit.norm.E$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm.E$estimate["mean"], sd = fit.norm.E$estimate["sd"])$statistic
      )      
    })
    fit <- logspline(stats)
    plogspline(ks.test(E,"pnorm", mean = fit.norm.E$estimate["mean"], sd = fit.norm.E$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  # R: meanlog=7.097309, sdlog=2.671154  , AIC = 7.192.692
  {
  fit.lnorm.R<-fitdist(R+1,"lnorm",method = c("mle"))
  #plot(fit.lnorm.R)
  fit.lnorm.R$estimate
  fit.lnorm.R$aic
  
  n.sims <- 100
  stats <- replicate(n.sims, {   
    r <- rlnorm(n = length(R), meanlog = fit.lnorm.R$estimate[1] , sdlog = fit.lnorm.R$estimate[2]  )
    as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm.R$estimate[1] , sdlog = fit.lnorm.R$estimate[2])$statistic
    )      
  })

  fit <- logspline(stats)

  plogspline(ks.test(R,"plnorm",meanlog = fit.lnorm.R$estimate[1] , sdlog = fit.lnorm.R$estimate[2])$statistic, fit)
  }
  # E: meanlog=-2.316315 , sdlog=1.569184   , AIC = 3.169.600
  {
    fit.lnorm.E<-fitdist(E+1,"lnorm",method = c("mle"))
    #plot(fit.lnorm.E)
    fit.lnorm.E$estimate
    fit.lnorm.E$aic
    
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm.E$estimate[1] , sdlog = fit.lnorm.E$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm.E$estimate[1] , sdlog = fit.lnorm.E$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm.E$estimate[1] , sdlog = fit.lnorm.E$estimate[2])$statistic, fit)
  }

  #GAMMA DISTRIBUTION
  # R: shape=0.2501059 ,rate=431.5276096 , AIC= -5.829.999
  {
  R.norm = R
  fit.gamma.R<-fitdist(R.norm,"gamma",method = c("mle"),lower=0,upper=1)
  #plot(fit.gamma.R)
  fit.gamma.R$estimate
  fit.gamma.R$aic
  n.sims <- 10
  stats <- replicate(n.sims, {   
    r <- rgamma(n = length(R.norm), shape = fit.gamma.R$estimate[1] , rate = fit.gamma.R$estimate[2]  )
    as.numeric(ks.test(r, "pgamma", shape = fit.gamma.R$estimate[1] , rate = fit.gamma.R$estimate[2])$statistic
    )      
  })
  
  fit <- logspline(stats)
  
  plogspline(ks.test(R.norm,"pgamma",shape = fit.gamma.R$estimate[1] , rate = fit.gamma.R$estimate[2])$statistic, fit)
  
  
  }
  # E: shape=0.2625213 ,rate=228.2919192, AIC =  -5.223.628
  {
    E.norm <- E
    fit.gamma.E<-fitdist(E.norm,"gamma",method = c("mle"))
    #plot(fit.gamma.R)
    fit.gamma.E$estimate
    fit.gamma.E$aic
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E.norm), shape = fit.gamma.E$estimate[1] , rate = fit.gamma.E$estimate[2]  )
      as.numeric(ks.test(r, "pgamma", shape = fit.gamma.E$estimate[1] , rate = fit.gamma.E$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    plogspline(ks.test(E.norm,"pgamma",shape = fit.gamma.E$estimate[1] , rate = fit.gamma.E$estimate[2])$statistic, fit)
    
    
  }
  
  #BETA
  # R: shape1 = 0.2397152, shape2 = 358.7267210 , AIC= -5.803.650
  {
  R.norm <- R
  fit.beta.R<-fitdist(R.norm,"beta")
  fit.beta.R$estimate
  fit.beta.R$aic
  
  n.sims <- 10
  stats <- replicate(n.sims, {   
    r <- rbeta(n = length(R.norm), shape1 = fit.beta.R$estimate[1] , shape2 = fit.beta.R$estimate[2]  )
    as.numeric(ks.test(r, "pbeta", shape1 = fit.beta.R$estimate[1] , shape2 = fit.beta.R$estimate[2])$statistic
    )      
  })
  
  fit <- logspline(stats)
  plogspline(ks.test(R.norm,"pbeta",shape1 = fit.beta.R$estimate[1] , shape2 = fit.beta.R$estimate[2])$statistic, fit)
  
  
  }
  # E: shape1 = 0.257029 , shape2 = 209.469943   , AIC = -5.211.078 
  {
    E.norm <- E
    fit.beta.E<-fitdist(E.norm,"beta")
    fit.beta.E$estimate
    fit.beta.E$aic
    
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(E.norm), shape1 = fit.beta.E$estimate[1] , shape2 = fit.beta.E$estimate[2]  )
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta.E$estimate[1] , shape2 = fit.beta.E$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    plogspline(ks.test(E.norm,"pbeta",shape1 = fit.beta.E$estimate[1] , shape2 = fit.beta.E$estimate[2])$statistic, fit)
    
    
  }

  }
  
  #with box-cox transformation
  {
  R = boxcoxnc(manufacturing$R,lambda2 = 1)
  E = boxcoxnc(manufacturing$E,lambda2 = 1)
  
  #DISTRIBUZIONE NORMALE (i valori sono normalizzati [0,1])
  # R: mean = 0.0005793282, sd=.0058448883 , AIC: -2.819.330
  {
    fit.norm.R <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm.R)
    fit.norm.R$estimate
    fit.norm.R$aic
    n.sims <- 100
    stats <- replicate(n.sims, {      
      r <- rnorm(n = length(R), mean = fit.norm.R$estimate["mean"], sd = fit.norm.R$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm.R$estimate["mean"], sd = fit.norm.R$estimate["sd"])$statistic
      )      
    })
    fit <- logspline(stats)
    plogspline(ks.test(R,"pnorm", mean = fit.norm.R$estimate["mean"], sd = fit.norm.R$estimate["sd"])$statistic, fit)
  }
  # E: mean = 0.001149342, sd=0.006162528 , AIC: -2.779.258
  {
    fit.norm.E <-fitdist(E, "norm",method = c("mle"))
    #plot(fit.norm.E)
    fit.norm.E$estimate
    fit.norm.E$aic
    n.sims <- 100
    stats <- replicate(n.sims, {      
      r <- rnorm(n = length(E), mean = fit.norm.E$estimate["mean"], sd = fit.norm.E$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm.E$estimate["mean"], sd = fit.norm.E$estimate["sd"])$statistic
      )      
    })
    fit <- logspline(stats)
    plogspline(ks.test(E,"pnorm", mean = fit.norm.E$estimate["mean"], sd = fit.norm.E$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  # R: meanlog=-10.293819, sdlog=4.040119 , AIC = -5.662.976
  {
    fit.lnorm.R<-fitdist(R,"lnorm",method = c("mle"))
    #plot(fit.lnorm.R)
    fit.lnorm.R$estimate
    fit.lnorm.R$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(R), meanlog = fit.lnorm.R$estimate[1] , sdlog = fit.lnorm.R$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm.R$estimate[1] , sdlog = fit.lnorm.R$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(R,"plnorm",meanlog = fit.lnorm.R$estimate[1] , sdlog = fit.lnorm.R$estimate[2])$statistic, fit)
  }
  # E: meanlog=-9.453188 , sdlog=3.893421  , AIC = -5.054.437
  {
    fit.lnorm.E<-fitdist(E,"lnorm",method = c("mle"))
    #plot(fit.lnorm.E)
    fit.lnorm.E$estimate
    fit.lnorm.E$aic
    
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm.E$estimate[1] , sdlog = fit.lnorm.E$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm.E$estimate[1] , sdlog = fit.lnorm.E$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(E,"plnorm",meanlog = fit.lnorm.E$estimate[1] , sdlog = fit.lnorm.E$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION
  # R: shape=0.2501059 ,rate=431.5276096 , AIC= -5.829.999
  {
    R.norm = R
    fit.gamma.R<-fitdist(R.norm,"gamma",method = c("mle"))
    #plot(fit.gamma.R)
    fit.gamma.R$estimate
    fit.gamma.R$aic
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(R.norm), shape = fit.gamma.R$estimate[1] , rate = fit.gamma.R$estimate[2]  )
      as.numeric(ks.test(r, "pgamma", shape = fit.gamma.R$estimate[1] , rate = fit.gamma.R$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(R.norm,"pgamma",shape = fit.gamma.R$estimate[1] , rate = fit.gamma.R$estimate[2])$statistic, fit)
    
    
  }
  # E: shape=0.2625213 ,rate=228.2919192, AIC =  -5.223.628
  {
    E.norm <- E
    fit.gamma.E<-fitdist(E.norm,"gamma",method = c("mle"))
    #plot(fit.gamma.R)
    fit.gamma.E$estimate
    fit.gamma.E$aic
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E.norm), shape = fit.gamma.E$estimate[1] , rate = fit.gamma.E$estimate[2]  )
      as.numeric(ks.test(r, "pgamma", shape = fit.gamma.E$estimate[1] , rate = fit.gamma.E$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    plogspline(ks.test(E.norm,"pgamma",shape = fit.gamma.E$estimate[1] , rate = fit.gamma.E$estimate[2])$statistic, fit)
    
    
  }
  
  #BETA
  # R: shape1 = 0.2397152, shape2 = 358.7267210 , AIC= -5.803.650
  {
    R.norm <- R
    fit.beta.R<-fitdist(R.norm,"beta")
    fit.beta.R$estimate
    fit.beta.R$aic
    
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R.norm), shape1 = fit.beta.R$estimate[1] , shape2 = fit.beta.R$estimate[2]  )
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta.R$estimate[1] , shape2 = fit.beta.R$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    plogspline(ks.test(R.norm,"pbeta",shape1 = fit.beta.R$estimate[1] , shape2 = fit.beta.R$estimate[2])$statistic, fit)
    
    
  }
  # E: shape1 = 0.257029 , shape2 = 209.469943   , AIC = -5.211.078 
  {
    E.norm <- E
    fit.beta.E<-fitdist(E.norm,"beta")
    fit.beta.E$estimate
    fit.beta.E$aic
    
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(E.norm), shape1 = fit.beta.E$estimate[1] , shape2 = fit.beta.E$estimate[2]  )
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta.E$estimate[1] , shape2 = fit.beta.E$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    plogspline(ks.test(E.norm,"pbeta",shape1 = fit.beta.E$estimate[1] , shape2 = fit.beta.E$estimate[2])$statistic, fit)
    
    
  }
  
}

}

#distribution and hypotesis test for R and E of 101100
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
  R = ateco$`101100`$R
  E = ateco$`101100`$E
  
  #guardando i grafici sotto è facile notare che entrambe le distribuzioni sono
  #delle distribuzioni beta.
  par(mfrow=c(2,1))
  descdist(R, discrete = FALSE,obs.col="red", boot=1000,obs.pch = 15, boot.col="red")
  descdist(E, discrete = FALSE,obs.col="red", boot=1000,obs.pch = 15, boot.col="blue")
  par(mfrow=c(1,1))
  
  #DISTRIBUZIONE NORMALE 
  # R: mean = 11471.51, sd=31461.72  , AIC: 10.010.044
  {
    fit.norm.R <-fitdist(R, "norm",method = c("mle"))
    #plot(fit.norm.R)
    fit.norm.R$estimate
    fit.norm.R$aic
    n.sims <- 100
    stats <- replicate(n.sims, {r <- rnorm(n = length(R), mean = fit.norm.R$estimate["mean"], sd = fit.norm.R$estimate["sd"])
    as.numeric(ks.test(r, "pnorm", mean = fit.norm.R$estimate["mean"], sd = fit.norm.R$estimate["sd"])$statistic
      )      
    })
    fit <- logspline(stats)
    plogspline(ks.test(R,"pnorm", mean = fit.norm.R$estimate["mean"], sd = fit.norm.R$estimate["sd"])$statistic, fit)
  }
  # E: mean = 38.65826, sd=207.28282  , AIC: 5.113.555
  {
    fit.norm.E <-fitdist(E, "norm",method = c("mle"))
    #plot(fit.norm.E)
    fit.norm.E$estimate
    fit.norm.E$aic
    n.sims <- 10
    stats <- replicate(n.sims, {      
      r <- rnorm(n = length(E), mean = fit.norm.E$estimate["mean"], sd = fit.norm.E$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm.E$estimate["mean"], sd = fit.norm.E$estimate["sd"])$statistic
      )      
    })
    fit <- logspline(stats)
    plogspline(ks.test(E,"pnorm", mean = fit.norm.E$estimate["mean"], sd = fit.norm.E$estimate["sd"])$statistic, fit)
  }
  
  #DISTRIBUZIONE LOGNORM
  # R: meanlog=7.097309, sdlog=2.671154  , AIC = 7.192.692
  {
    fit.lnorm.R<-fitdist(R+1,"lnorm",method = c("mle"))
    #plot(fit.lnorm.R)
    fit.lnorm.R$estimate
    fit.lnorm.R$aic
    
    n.sims <- 100
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(R), meanlog = fit.lnorm.R$estimate[1] , sdlog = fit.lnorm.R$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm.R$estimate[1] , sdlog = fit.lnorm.R$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(R+1,"plnorm",meanlog = fit.lnorm.R$estimate[1] , sdlog = fit.lnorm.R$estimate[2])$statistic, fit)
  }
  # E: meanlog=-2.316315 , sdlog=1.569184   , AIC = 3.169.600
  {
    fit.lnorm.E<-fitdist(E+1,"lnorm",method = c("mle"))
    #plot(fit.lnorm.E)
    fit.lnorm.E$estimate
    fit.lnorm.E$aic
    
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(E), meanlog = fit.lnorm.E$estimate[1] , sdlog = fit.lnorm.E$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm.E$estimate[1] , sdlog = fit.lnorm.E$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(E+1,"plnorm",meanlog = fit.lnorm.E$estimate[1] , sdlog = fit.lnorm.E$estimate[2])$statistic, fit)
  }
  
  #GAMMA DISTRIBUTION
  # R: shape=0.2501059 ,rate=431.5276096 , AIC= -5.829.999
  {
    R.norm = 
    fit.gamma.R<-fitdist(R.norm,"gamma",method = c("mle"),lower=0,upper=1)
    #plot(fit.gamma.R)
    fit.gamma.R$estimate
    fit.gamma.R$aic
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(R.norm), shape = fit.gamma.R$estimate[1] , rate = fit.gamma.R$estimate[2]  )
      as.numeric(ks.test(r, "pgamma", shape = fit.gamma.R$estimate[1] , rate = fit.gamma.R$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(R.norm,"pgamma",shape = fit.gamma.R$estimate[1] , rate = fit.gamma.R$estimate[2])$statistic, fit)
    
    
  }
  # E: shape=0.2625213 ,rate=228.2919192, AIC =  -5.223.628
  {
    E.norm <- E
    fit.gamma.E<-fitdist(E.norm,"gamma",method = c("mle"))
    #plot(fit.gamma.R)
    fit.gamma.E$estimate
    fit.gamma.E$aic
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(E.norm), shape = fit.gamma.E$estimate[1] , rate = fit.gamma.E$estimate[2]  )
      as.numeric(ks.test(r, "pgamma", shape = fit.gamma.E$estimate[1] , rate = fit.gamma.E$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    plogspline(ks.test(E.norm,"pgamma",shape = fit.gamma.E$estimate[1] , rate = fit.gamma.E$estimate[2])$statistic, fit)
    
    
  }
  
  #BETA
  # R: shape1 = 0.2397152, shape2 = 358.7267210 , AIC= -5.803.650
  {
    R.norm <- R
    fit.beta.R<-fitdist(R.norm,"beta")
    fit.beta.R$estimate
    fit.beta.R$aic
    
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(R.norm), shape1 = fit.beta.R$estimate[1] , shape2 = fit.beta.R$estimate[2]  )
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta.R$estimate[1] , shape2 = fit.beta.R$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    plogspline(ks.test(R.norm,"pbeta",shape1 = fit.beta.R$estimate[1] , shape2 = fit.beta.R$estimate[2])$statistic, fit)
    
    
  }
  # E: shape1 = 0.257029 , shape2 = 209.469943   , AIC = -5.211.078 
  {
    E.norm <- E
    fit.beta.E<-fitdist(E.norm,"beta")
    fit.beta.E$estimate
    fit.beta.E$aic
    
    n.sims <- 10
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(E.norm), shape1 = fit.beta.E$estimate[1] , shape2 = fit.beta.E$estimate[2]  )
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta.E$estimate[1] , shape2 = fit.beta.E$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    plogspline(ks.test(E.norm,"pbeta",shape1 = fit.beta.E$estimate[1] , shape2 = fit.beta.E$estimate[2])$statistic, fit)
    
    
  }
  
  
  
  
  
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
#####vecchio#####
#DISTRIBUTION
{
  #DISTRIBUZIONE UNIFORME
  fit.unif.R<-fitdist(R,"unif")
  plot(fit.unif.R)
  fit.unif.R$estimate
  fit.unif.R$aic
  {
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- runif(n = length(R), min = fit.unif.R$estimate[1] , max = fit.unif.R$estimate[2]  )
      as.numeric(ks.test(r, "punif", min = fit.unif.R$estimate[1] , max = fit.unif.R$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(R,"punif",min = fit.unif.R$estimate[1] , max = fit.unif.R$estimate[2])$statistic
               , fit
    )
  }
  #DISTRIBUZIONE ESPONENZIALE
  fit.exp.R<-fitdist(R,"exp")
  plot(fit.exp.R)
  fit.exp.R$estimate
  fit.exp.R$aic
  {
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rexp(n = length(R), rate = fit.exp.R$estimate[1]  )
      as.numeric(ks.test(r, "pexp", rate = fit.exp.R$estimate[1])$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(R,"pexp",rate = fit.exp.R$estimate[1] )$statistic
               , fit
    )
  }
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
  
  
  #DISTRIBUZIONE DI PARETO 
  m_plwords = displ$new(round(R))
  est_plwords = estimate_xmin(m_plwords)
  m_plwords$setXmin(est_plwords$xmin)
  m_plwords$setPars(est_plwords$pars)
  bs_p = bootstrap_p(m_plwords,no_of_sims = 100,threads = 4)   
  bs_p$p
  plot(m_plwords)
  {
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rpareto(n = length(R), scale=scale ,shape = shape)
      as.numeric(ks.test(r, "ppareto", scale=scale ,shape = shape)$statistic
      )      
    })
    
    fit <- logspline(stats)
    
    plogspline(ks.test(R,"ppareto",scale=4.013177e+04 ,shape = 7.342282e-01)$statistic
               , fit
    )
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

