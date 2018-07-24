#Correlation and linear regression each explore the relationship between two quantitative variables.
#Correlation determines if one variable varies systematically as another variable changes.  
#It does not specify that one variable is the dependent variable and the other is the 
#independent variable.  

#In contrast, linear regression specifies one variable as the independent variable and another 
#as the dependent variable. The resultant model relates the variables with a linear relationship. 
wdir <- ""
dataDir <- "data/"
packagesFile <- "pack2.txt"
source(paste(wdir, "functions.R", sep="")) ### this also loads every needed package
#loadDatasets(paste(wdir,dataDir,sep="")) ###USE THIS IF YOU CURRENTLY HAVEN'T DATASETS IN WORKSPACE

#library(dplyr)
#library(kimisc)
#library(corrplot)
#library(boot)
#library(ggplot2)
#library(Hmisc)
#library(ggpubr)

set.seed(1000)

##### AIDA ####
##### CORRELATION ANALYSIS ####


#Correlation CI to be sure we're avoiding bias due to the (sub)sample used
correlazioni <-c()
for(i in 1:1000){
  print(paste("Iteration",i,"over 1000"))
  samples = sample_n(aida,35000,replace = FALSE)
  samples=samples[c("E","R")]
  corr<-cor(samples, use = "pairwise",method = "pearson")[1,2]
  correlazioni[i]<-corr
}
q = quantile(correlazioni,c(0.025,0.975))
x.aida = sample_n(aida,35000,replace = FALSE) #sample di aida
x.aida$R =round(x.aida$R)
x.aida = x.aida[c("E","R")]
corrEmp = cor(x.aida, use = "pairwise",method = "pearson")[1,2]
plotConfidInterv(correlazioni,corrEmp)

# A SORT OF LINEAR CORRELATION BETWEEN E AND R
ggplot(data = x.aida, aes(x = E, y = R)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE,color="red")+
  labs(x = "Employee")+
  labs(y = "Revenue")+
  ylim(0,500000)+
  xlim(0,2200)
  #labs(title = paste("Y =",signif(linearMod.aida$coef[[1]],5),"+",signif(linearMod.aida$coef[[2]], 5),"X"))
  #labs(title = "Plot Employee - Revenue", subtitle = "A subtitle")

qqplot(x.aida$E, x.aida$R)


Employee = x.aida$E[1:5000]
Revenue = x.aida$R[1:5000]
ad.test(x.aida$E)
ad.test(x.aida$R)

#CORRELATION MATRIX
#With both Pearson and Spearman coefficients, correlation values are pretty high
corr.p.aida = cor(x.aida, use = "pairwise",method = "pearson") # r=0.3397841 
corr.p.aida
corr.s.aida = cor(x.aida, use = "pairwise",method = "spearman") #rho=0.7157978
corr.s.aida

#PLOTTING
par(mfrow=c(1,2))
corrplot(corr.p.aida,method = "number", type="upper", order="hclust")
title(main="Pearson",line = -2)
corrplot(corr.s.aida,method = "number", type="upper", order="hclust")
title(main="Spearman",line= -2)
par(mfrow=c(1,1))

#HYPOTHESIS TESTING
#per assicurarci che ci sia correlazione, dobbiamo effettuare un test
#attraverso la funzione cor.test possiamo effettuare il test della correlazione tra le variabili
# le ipotesi sono : H0: corr=0 ;H1: corr!=0
#fissato un alpha 0.05, entrambi i test (Spearman - Pearson) restituiscono un p-value<0.05 quindi
#rifiutiamo l'ipotesi nulla ed accettiamo  l'ipotesi alternativa che corr!=0
corr.s.test.aida = cor.test( ~ x.aida$E + x.aida$R,method = "spearman")
corr.s.test.aida #rifiutiamo H0 ed accettiamo H1 quindi c'è correlazione
corr.p.test.aida = cor.test( ~ x.aida$E + x.aida$R,method = "pearson")
corr.p.test.aida #rifiutiamo H0 ed accettiamo H1 quindi c'è correlazione



##### CORRELATION BY YEARS ####
prova = sample_n(aida,35000,replace = FALSE)
prova = prova[c("Year","E","R")]

r<- by(prova, prova$Year, FUN = function(x) cor(x, use = "pairwise",method = "spearman"))

#PLOT MATRIX BY YEAR
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

#PLOT E-R doesn't work
par(mfrow= c(3,3))
plotByYear=function(x){
  a=split(x,x$Year)
  year=c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
  k=1
  plots = list()
  for(i in a){
    #plots[[k]]<-ggplot(data = i, aes(x = i$E, y = i$R))+ 
     # geom_point(color='blue')+
      #geom_smooth(method = "lm", se = FALSE,color="red")+
      #labs(x = "Employee")+
      #labs(y = "Revenue")+
      #ylim(0,500000)+
      #xlim(0,2200)+
      labs(title = year[k])
    plot(i$E,i$R,xlab = "Employee",ylab = "Revenue")
    title(main = year[k],line = +3)
    
    k=k+1
  }
  
}
plotByYear(prova)
par(mfrow= c(1,1))

#CORR TEST all pvalues are <0.05. reject null hypothesis
  a=split(prova,prova$Year)
  year=c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
  for(i in a){
    print(cor.test( ~ i$E + i$R,method = "spearman"))
  }
  





##### LINEAR REGRESSION #####
# Y=Intercept +(β ∗ X)
linearMod.aida <- lm(E ~ R, data=x.aida)

print(linearMod.aida) #intercept e beta/slope

ggplot(data = x.aida, aes(x = E, y = R)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE,color="red")+
  labs(x = "Employee")+
  labs(y = "Revenue")+
  ylim(0,500000)+
  xlim(0,2200)+
labs(title = paste("Y =",signif(linearMod.aida$coef[[1]],5),"+",signif(linearMod.aida$coef[[2]], 5),"X"))
labs(title = "Plot Employee - Revenue", subtitle = "A subtitle")

summary(linearMod.aida)

#Testing the hypothesis for regression model

{
#library(nortest)
gvlma(linearMod.aida)

#1-la media degli errori non sia significativamente diversa da zero attuando il test t di Student:
residui<-residuals(linearMod.aida)
t.test(residui)
mean(residui) #molto vicino a zero

#2-la normalità della distribuzione degli errori con il test di Shapiro-Wilk
#non potendo applicare il test per via di troppi records, possiamo utilizzare il qqplot
ad.test(residui)
qqnorm(scale(residui[1:5000]))
abline(0,1)

#3-Proseguiamo con il verificare l’omoschedasticità dei residui utilizzando il test di Breusch-Pagan e
#l’assenza di correlazione seriale tramite il test di Durbin-Watson. 
#Entrambi i test sono largamente impiegati nelle analisi econometriche.
modello<-formula(linearMod.aida)
bptest(modello,data=x.aida) #omoschedasticità
dwtest(modello,data=x.aida) #autocorrelazione


#confidence interval
CI.lm =confint(linearMod.aida)
CI.lm
}

#Before using a regression model, you have to ensure that it is statistically significant.
#When there is a p-value, there is a hull and alternative hypothesis associated with it. 
#In Linear Regression, the Null Hypothesis is that the coefficients associated with the variables 
#is equal to zero. The alternate hypothesis is that the coefficients are not equal to zero 
#(i.e. there exists a relationship between the independent variable in question and the dependent 
#variable)

#da quello che ho capito il nostro modello dovrebbe essere valido. I p-value sono minori di alphs
#Per testarlo possiamo usare o la Cross Validation oppure usare training e test set
# setting seed to reproduce results of random sampling

#STEP 1: TRAINING E TEST SET
d = sort(sample(nrow(x.aida), nrow(x.aida)*.8)) # select training sample, sort is optional 
train.aida = x.aida[d,] 
test.aida = x.aida[-d,] 


#STEP 2: Fit the model on training data and predict  on test data
# Build the model on training data

lmMod.aida <- lm(R ~ E, data=train.aida)  # build the model
distPred.aida <- predict(lmMod.aida, test.aida)  # predict distance

#STEP 3: Review diagnostic measures.
#CHECK THE PVALUES
summary (lmMod.aida)  # model summary

#STEP 4: Calculate prediction accuracy and error rates
actuals_preds.aida <- data.frame(cbind(actuals=test.aida$R, predicteds=distPred.aida))  # make actuals_predicteds dataframe.
correlation_accuracy.aida <- cor(actuals_preds.aida)  # 91.26%
min_max_accuracy.aida <- mean(apply(actuals_preds.aida, 1, min) / apply(actuals_preds.aida, 1, max))  
min_max_accuracy.aida



##### MANUFACTURING ######
##### CORRELATION ANALYSIS ####

x.man = sample_n(manufacturing,35000,replace = FALSE) #sample di manufacturing
#x.man$R =round(x.man$R)
x.man = x.man[c("E","R")]

# PLOT DI E ED R
ggplot(data = x.man, aes(x = E, y = R)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE, color="red")+
  labs(x = "Employee")+
  labs(y = "Revenue")+
  ylim(0,500000)+
  xlim(0,2200)
#labs(title = "Plot Employee - Revenue", subtitle = "A subtitle")

Employee = x.man$E[1:5000]
Revenue = x.man$R[1:5000]
shapiro.test(Employee)
shapiro.test(Revenue)

#MATRICE DI CORRELAZIONE TRA E ED R DI TUTTO IL DATASET manufacturing
#Sia con Pearson che con Spearman, le correlazioni sono alte,
#ma non è possibile effettuarla con Kendall per problemi computazionali
corr.p.man = cor(x.man, use = "pairwise",method = "pearson") # r= 0.3397841 NON ESEGUIRE DI NUOVO
corr.p.man
corr.s.man = cor(x.man, use = "pairwise",method = "spearman") #rho= 0.7157978 NON ESEGUIRE DI NUOVO
corr.s.man

#PLOT CORRELAZIONI
par(mfrow=c(1,2))
corrplot(corr.p.man,method = "number", type="upper", order="hclust")
title(main="Pearson",line = -2)
corrplot(corr.s.man,method = "number", type="upper", order="hclust")
title(main="Spearman",line= -1)
par(mfrow=c(1,1))

#TEST CORRELAZIONI 
#per assicurarci che ci sia correlazione, dobbiamo effettuare un test
#attraverso la funzione cor.test possiamo effettuare il test della correlazione tra le variabili
# le ipotesi sono : H0: corr=0 ;H1: corr!=0
#fissato un alpha 0.05, entrambi i test (Spearman - Pearson) restituiscono un p-value<0.05 quindi
#rifiutiamo l'ipotesi nulla ed accettiamo  l'ipotesi alternativa che corr!=0
corr.s.test.man = cor.test( ~ x.man$E + x.man$R,method = "spearman")
corr.s.test.man #rifiutiamo H0 ed accettiamo H1 quindi c'è correlazione
corr.p.test.man = cor.test( ~ x.man$E + x.man$R,method = "pearson")
corr.p.test.man #rifiutiamo H0 ed accettiamo H1 quindi c'è correlazione


##### CORRELATION BY YEARS ####
prova = sample_n(manufacturing,35000,replace = FALSE) #sample di aida
prova = prova[c("Year","E","R")]

r<- by(prova, prova$Year, FUN = function(x) cor(x, use = "pairwise",method = "spearman"))

#PLOT MATRIX BY YEAR
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

#PLOT E-R doesn't work
par(mfrow= c(3,3))
plotByYear=function(x){
  a=split(x,x$Year)
  year=c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
  k=1
  plots = list()
  for(i in a){
    #plots[[k]]<-ggplot(data = i, aes(x = i$E, y = i$R))+ 
    # geom_point(color='blue')+
    #geom_smooth(method = "lm", se = FALSE,color="red")+
    #labs(x = "Employee")+
    #labs(y = "Revenue")+
    #ylim(0,500000)+
    #xlim(0,2200)+
    labs(title = year[k])
    plot(i$E,i$R,xlab = "Employee",ylab = "Revenue")
    title(main = year[k],line = +3)
    
    k=k+1
  }
  
}
plotByYear(prova)
par(mfrow= c(1,1))

#CORR TEST all pvalues are <0.05. reject null hypothesis
a=split(prova,prova$Year)
year=c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
for(i in a){
  print(cor.test( ~ i$E + i$R,method = "spearman"))
}









#### MEDIA #####
##### CORRELATION ANALYSIS ####

x.media = sample_n(media,35000,replace = FALSE) #sample di aida
#seleziono sono le variabili di nostro interesse. 
#Spieghiamo che abbiamo scelto Employee e Revenue e perchè
x.media$R =round(x.media$R)
x.media = x.media[c("E","R")]

# PLOT DI E ED R
ggplot(data = x.media, aes(x = E, y = R)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE,color="red")+
  labs(x = "Employee")+
  labs(y = "Revenue")+
  ylim(0,500000)+
  xlim(0,2200)
#labs(title = paste("Y =",signif(linearMod.aida$coef[[1]],5),"+",signif(linearMod.aida$coef[[2]], 5),"X"))
#labs(title = "Plot Employee - Revenue", subtitle = "A subtitle")



#MATRICE DI CORRELAZIONE TRA E ED R DI TUTTO IL DATASET AIDA
#Sia con Pearson che con Spearman, le correlazioni sono alte,
#ma non è possibile effettuarla con Kendall per problemi computazionali
corr.p.media = cor(x.media, use = "pairwise",method = "pearson") # r=0.3397841 NON ESEGUIRE DI NUOVO
corr.p.media
corr.s.media = cor(x.media, use = "pairwise",method = "spearman") #rho=0.7157978 NON ESEGUIRE DI NUOVO
corr.s.media

#PLOT CORRELAZIONI
par(mfrow=c(1,2))
corrplot(corr.p.media,method = "number", type="upper", order="hclust")
title(main="Pearson",line = -2)
corrplot(corr.s.media,method = "number", type="upper", order="hclust")
title(main="Spearman",line= -2)
par(mfrow=c(1,1))

#TEST CORRELAZIONI 
#per assicurarci che ci sia correlazione, dobbiamo effettuare un test
#attraverso la funzione cor.test possiamo effettuare il test della correlazione tra le variabili
# le ipotesi sono : H0: corr=0 ;H1: corr!=0
#fissato un alpha 0.05, entrambi i test (Spearman - Pearson) restituiscono un p-value<0.05 quindi
#rifiutiamo l'ipotesi nulla ed accettiamo  l'ipotesi alternativa che corr!=0
corr.s.test.media = cor.test( ~ x.media$E + x.media$R,method = "spearman")
corr.s.test.media #rifiutiamo H0 ed accettiamo H1 quindi c'è correlazione
corr.p.test.aida = cor.test( ~ x.media$E + x.media$R,method = "pearson")
corr.p.test.aida #rifiutiamo H0 ed accettiamo H1 quindi c'è correlazione




#### RESTAURANT #####
##### CORRELATION ANALYSIS ####

x.res = sample_n(restaurants,35000,replace = FALSE) #sample di aida
#seleziono sono le variabili di nostro interesse. 
#Spieghiamo che abbiamo scelto Employee e Revenue e perchè
x.res$R =round(x.res$R)
x.res = x.res[c("E","R")]

# PLOT DI E ED R
ggplot(data = x.media, aes(x = E, y = R)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE,color="red")+
  labs(x = "Employee")+
  labs(y = "Revenue")+
  ylim(0,500000)+
  xlim(0,2200)
#labs(title = paste("Y =",signif(linearMod.aida$coef[[1]],5),"+",signif(linearMod.aida$coef[[2]], 5),"X"))
#labs(title = "Plot Employee - Revenue", subtitle = "A subtitle")



#MATRICE DI CORRELAZIONE TRA E ED R DI TUTTO IL DATASET AIDA
#Sia con Pearson che con Spearman, le correlazioni sono alte,
#ma non è possibile effettuarla con Kendall per problemi computazionali
corr.p.res = cor(x.res, use = "pairwise",method = "pearson") # r=0.3397841 NON ESEGUIRE DI NUOVO
corr.p.res
corr.s.res = cor(x.res, use = "pairwise",method = "spearman") #rho=0.7157978 NON ESEGUIRE DI NUOVO
corr.s.res

#PLOT CORRELAZIONI
par(mfrow=c(1,2))
corrplot(corr.p.res,method = "number", type="upper", order="hclust")
title(main="Pearson",line = -2)
corrplot(corr.s.res,method = "number", type="upper", order="hclust")
title(main="Spearman",line= -2)
par(mfrow=c(1,1))

#TEST CORRELAZIONI 
#per assicurarci che ci sia correlazione, dobbiamo effettuare un test
#attraverso la funzione cor.test possiamo effettuare il test della correlazione tra le variabili
# le ipotesi sono : H0: corr=0 ;H1: corr!=0
#fissato un alpha 0.05, entrambi i test (Spearman - Pearson) restituiscono un p-value<0.05 quindi
#rifiutiamo l'ipotesi nulla ed accettiamo  l'ipotesi alternativa che corr!=0
corr.s.test.res = cor.test( ~ x.res$E + x.res$R,method = "spearman")
corr.s.test.res #rifiutiamo H0 ed accettiamo H1 quindi c'è correlazione
corr.p.test.res = cor.test( ~ x.res$E + x.res$R,method = "pearson")
corr.p.test.res #rifiutiamo H0 ed accettiamo H1 quindi c'è correlazione













##### SUMMARY CORR BY SECTOR #####
par(mfrow=c(3,2))
#MANUFACTURING
layout(matrix(c(2,3,1,1), 2, 2, byrow = TRUE))
corrplot(corr.p.man,method = "number", type="upper", order="hclust")
title(main="Pearson",sub="Manufacturing",line = -2)
corrplot(corr.s.man,method = "number", type="upper", order="hclust")
title(main="Spearman",sub="Manufacturing",line= -2)
#RESTAURANT
corrplot(corr.p.res,method = "number", type="upper", order="hclust")
title(main="Pearson",sub="Restaurant",line = -2)
corrplot(corr.s.res,method = "number", type="upper", order="hclust")
title(main="Spearman",sub="Restaurant",line= -2)
#MEDIA
corrplot(corr.p.media,method = "number", type="upper", order="hclust")
title(main="Pearson",sub="Media",line = -2)
corrplot(corr.s.media,method = "number", type="upper", order="hclust")
title(main="Spearman",sub="Media",line= -2)
par(mfrow=c(1,1))









#AIDA
cor.boot.aida <- function(data, k) cor(data[k,],method = "spearman")[1,2]
cor.res.aida <- boot(data=with(x.aida, cbind(E, R)),statistic=cor.boot.aida, R=10000)
cor.res.aida$t0
q = quantile(cor.res.aida$t,c(0.025,0.975))
q
plotConfidInterv(cor.res.aida$t,cor.res.aida$t0)
cor.res.aida$t0

#MANUFACTURING
cor.boot.man <- function(data, k) cor(data[k,],method = "spearman")[1,2]
cor.res.man <- boot(data=with(x.man, cbind(E, R)),statistic=cor.boot.man, R=10000)
cor.res.man$t0
q = quantile(cor.res.man$t,c(0.025,0.975))
q
plotConfidInterv(cor.res.man$t,cor.res.man$t0)
mean(cor.res.man$t0)


#RESTAURANT
cor.boot.res <- function(data, k) cor(data[k,],method = "spearman")[1,2]
cor.res.res <- boot(data=with(x.res, cbind(E, R)),statistic=cor.boot.res, R=10000)
cor.res.res$t0
q = quantile(cor.res.res$t,c(0.025,0.975))
q
plotConfidInterv(cor.res.res$t,cor.res.res$t0)

#MEDIA
cor.boot.media <- function(data, k) cor(data[k,],method = "spearman")[1,2]
cor.res.media <- boot(data=with(x.media, cbind(E, R)),statistic=cor.boot.media, R=10000)
cor.res.media$t0
q = quantile(cor.res.media$t,c(0.025,0.975))
q
plotConfidInterv(cor.res.media$t,cor.res.media$t0)

