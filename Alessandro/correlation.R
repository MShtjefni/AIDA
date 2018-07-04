#Abbiamo definito che E segue una distribuzione log normal, mentre R segue una distribuzione Weibull
#EMPLOYEE log-normal distribution
#REVENUE Weibull distribution

#Passiamo ora all'analisi della correlazione tra le size scelte da noi E ed R.
#Essendo che i dati non provengono da una distribuzione normale, non possiamo effettuare
#la correlazione usando Pearson  ma solo Kendall Spearman" 

ls() #Environment
rm(list = ls()) #remove all variables
ls() #check varaibles
library(kimisc)
library(corrplot)

set.seed(1000)

manufacturing = get(load("manufacturing.RData"))
nrow(manufacturing)# 1.027.140 records

#trasformo year in numeric per la selezione
manufacturing$Year =  as.numeric(as.character(manufacturing$Year))

manufacturing = sample.rows(manufacturing,700,replace = FALSE)
x = manufacturing[c("Year","E","R")]

plot(x$E,x$R,xlim = )
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
