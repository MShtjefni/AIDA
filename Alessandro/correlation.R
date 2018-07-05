#Abbiamo definito che E segue una distribuzione log normal, mentre R segue una distribuzione Weibull
#EMPLOYEE log-normal distribution
#REVENUE Weibull distribution

#Passiamo ora all'analisi della correlazione e della regressione tra le size scelte da noi E ed R.
#Scriveremo sulla relazione che abbiamo scelto E ed R perchè abbiamo seguito i papers trovati online
#B e P sono varaibili influenzate dal mondo esterno (tasse, prezzi ecc) quindi abbiamo deciso di concentrare
#la nostra analisi su E ed R

#Cosa possiamo aggiungere nella relazione.
#Correlation and linear regression each explore the relationship between two quantitative variables.
#Correlation determines if one variable varies systematically as another variable changes.  
#It does not specify that one variable is the dependent variable and the other is the 
#independent variable.  Often, it is useful to look at which variables are correlated to others 
#in a data set, and it is especially useful to see which variables correlate to a particular variable
#of interest.

#In contrast, linear regression specifies one variable as the independent variable and another 
#as the dependent variable. The resultant model relates the variables with a linear relationship. 


ls() #Environment
rm(list = ls()) #remove all variables
ls() #check varaibles
library(kimisc)
library(corrplot)
library(boot)

set.seed(1000)

##### AIDA 
##### CORRELATION ANALYSIS ####
#recupero il dataset di aida
aida = get(load("aida2.RData")) #dataset aida
#controllo i records per vedere se è il dataset giusto
nrow(aida) #7.050.620

#seleziono sono le variabili di nostro interesse
x = aida[c("E","R")]

#MATRICE DI CORRELAZIONE TRA E ED R DI TUTTO IL DATASET AIDA
#Sia con Pearson che con Spearman, le correlazioni sono alte,
#ma non è possibile effettuarla con Kendall per problemi comoutazionali
corr.pear = cor(x, use = "pairwise",method = "pearson") # r=0.3397841 NON ESEGUIRE DI NUOVO
corr.sper = cor(x, use = "pairwise",method = "spearman") #rho=0.7157978 NON ESEGUIRE DI NUOVO
#corr.kend = cor(x, use = "pairwise",method = "kendall") # IMPOSSIBILE ESEGUIRE


#PLOT CORRELAZIONI
par(mfrow=c(1,2))
corrplot(corr.pear,method = "number", type="upper", order="hclust")
title(main="Pearson",line = -2)
corrplot(corr.sper,method = "number", type="upper", order="hclust")
title(main="Spearman",line= -2)
par(mfrow=c(1,1))

#TEST CORRELAZIONI 
#attraverso la funzione cor.test possiamo effettuare il test della correlazione tra le variabili
# le ipotesi sono : H0: corr=0 ;H1: corr!=0
#fissato un alpha 0.05, entrambi i test (Spearman - Pearson) restituiscono un p-value<0.05 quindi
#rifiutiamo l'ipotesi nulla ed accettiamo  l'ipotesi alternativa che corr!=0
corr.sper.test = cor.test( ~ x$E + x$R,method = "spearman")
corr.pear.test = cor.test( ~ x$E + x$R,method = "pearson")


#per via di problemi computazionali, per plottare i Employee e Revenue ho usato un sample molto piccolo
s = sample.rows(aida,1000,replace = FALSE)
plot(s$E,s$R,xlim=c(0, 80),ylim=c(0, 20000),xlab = "Employee",ylab = "Revenue")
model = lm(s$R ~ s$E,data = x)
summary(model)
abline(model,col = "blue",lwd = 2)


#CONFIDENCE INTERVAL 
cor.boot <- function(data, k) cor(data[k,])[1,2]
cor.res <- boot(data=with(x, cbind(x$E, x$R)),statistic=cor.boot, R=500)
#tutti i 500 valori stimati
plot(cor.res)
q = quantile(cor.res$t, c(0.025, 0.975))

#PLOT CONFIDENCE INTERVAL
plot(density(cor.res$t),main="Confidence Interval Correlation")
text(round(corr[2],3),0,labels=corr[2], pos=3)
segments(x0=q[1],y1=-1,1.0,col="red")
segments(x0=q[2],y1=-1,0.7,col="red")




##### MANUFACTURING 
##### CORRELATION ANALYSIS ####
manufacturing = get(load("manufacturing.RData"))
nrow(manufacturing)# 1.027.140 records















#### vecchio ####
#trasformo year in numeric per la selezione
manufacturing$Year =  as.numeric(as.character(manufacturing$Year))

#trasformo year in numeric per la selezione
manufacturing$Year =  as.numeric(as.character(manufacturing$Year))

manufacturing = sample.rows(manufacturing,700,replace = FALSE)
x = manufacturing[c("Year","E","R","B","P")]

plot(x$E,x$R)
abline(lm(x$R[x$Year==2007]~x$E[x$Year==2007]),col='red',lwd=2)


r<- by(x, x$Year, FUN = function(x) cor(x, use = "pairwise",method = "pearson"))


#matrice delle correlazioni per year per E, R, P, B
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