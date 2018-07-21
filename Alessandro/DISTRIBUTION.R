ls() #Environment
rm(list = ls()) #remove all variables
ls() #check varaibles

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
library(kimisc)

set.seed(1000)
#secondo il metodo del MLE, valutando il valore AIC, 
#la distribuzione di Employee segue una distribuzione paretiana mentre
#Revenue segue una distribuzione di Weibull

#prendo il dataset manufacturing
manufacturing = get(load("aida2.RData"))
nrow(manufacturing)# 1.027.140 records

E = manufacturing$E
R = manufacturing$R

#SAMPLES 700.000 TO 120
{
#sample 700.000 records
#dat.1 = sample.rows(manufacturing,700000,replace = FALSE)
E = dat.1$E
R = dat.1$R

#sample 350.000 records
#dat.2 = sample.rows(manufacturing,350000,replace = FALSE)
E = dat.2$E
R = dat.2$R

#sample 150.000 records
#dat.3 = sample.rows(manufacturing,150000,replace = FALSE)
E = dat.3$E
R = dat.3$R

#sample 80.000 records
dat.3 = sample.rows(manufacturing,80000,replace = FALSE)
E = dat.3$E
R = dat.3$R

#sample 40.000 records
dat.4 = sample.rows(manufacturing,40000,replace = FALSE)
E = dat.4$E
R = dat.4$R

#sample 20.000 records
dat.5 = sample.rows(manufacturing,20000,replace = FALSE)
E = dat.5$E
R = dat.5$R

#sample 10.000 records
dat.6 = sample.rows(manufacturing,10000,replace = FALSE)
E = dat.6$E
R = dat.6$R

#sample 5.000 records
dat.7 = sample.rows(manufacturing,5000,replace = FALSE)
E = dat.7$E
R = dat.7$R

#sample 2.500 records
dat.8 = sample.rows(manufacturing,2500,replace = FALSE)
E = dat.8$E
R = dat.8$R

#sample 1.500 records
dat.9 = sample.rows(manufacturing,1500,replace = FALSE)
E = dat.9$E
R = dat.9$R

#sample 1.000 records
dat.10 = sample.rows(manufacturing,1000,replace = FALSE)
E = dat.10$E
R = dat.10$R

#sample 800 records
dat.11 = sample.rows(manufacturing,800,replace = FALSE)
E = dat.11$E
R = dat.11$R

#sample 700 records
#dat.12 = sample.rows(manufacturing,700,replace = FALSE)
E = dat.12$E
R = dat.12$R

#sample 500 records
#dat.13 = sample.rows(manufacturing,120,replace = FALSE)
E = dat.13$E
R = dat.13$R

#sample 500 records
#dat.14 = sample.rows(manufacturing,500,replace = FALSE)
E = dat.14$E
R = dat.14$R
}

#PLOT CULLEN AND FREY GRAPH
par(mfrow=c(2,1))
descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue",boot = 1000)
descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue",boot = 1000)
par(mfrow=c(1,1))

# EMPLOYEE SEGUE UNA DISTRIBUZIONE LOG NORMALE
# REVENUE SEGUE UNA DISTRIBUZIONE WEIBULL

#DISTRIBUTION ANALYSIS
#### NORMAL DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.norm.E = fitdist(E,"norm") #fit our distribution for a normal distribution
  ks.test(E,"pnorm",mean=fit.norm.E$estimate[1],sd=fit.norm.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.norm.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.norm.R = fitdist(R,"norm") #fit our distribution for a normal distribution
  ks.test(R,"pnorm",mean=fit.norm.R$estimate[1],sd=fit.norm.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.norm.R)
  
  
#### LOG-NORM DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.lnorm.E = fitdist(E,"lnorm") #fit our distribution for a log normal distribution
  fit.lnorm.E$estimate
  plot(fit.lnorm.E)
  
  boot.lnorm.E =  bootdist(fit.lnorm.E, niter = 1500000,bootmethod="param",parallel="multicore",ncpus = 4)
  summary(boot.lnorm.E)
  CI.meanlog.norm.E = quantile(boot.lnorm.E$estim[,1], c(0.025, 0.975))
  CI.sdlog.norm.E = quantile(boot.lnorm.E$estim[,2], c(0.025, 0.975))
  
  #plot confidence intervall
  par(mfrow=c(2,1))
  plot(density(boot.lnorm.E$estim[,1]),main="Confidence Interval meanlog")
  text(fit.lnorm.E$estimate[1],0,labels=round(fit.lnorm.E$estimate[1],4), pos=3)
  segments(x0=boot.lnorm.E$CI[3],y1=-1,0.5,col="red")
  text(boot.lnorm.E$CI[3],0,labels=round(boot.lnorm.E$CI[3],4), pos=3)
  segments(x0=boot.lnorm.E$CI[5],y1=-1,0.5,col="red")
  text(boot.lnorm.E$CI[5],0,labels=round(boot.lnorm.E$CI[5],3), pos=3)
  
  
  plot(density(boot.lnorm.E$estim[,2]),main="Confidence Interval sdlog")
  text(fit.lnorm.E$estimate[2],0,labels=round(fit.lnorm.E$estimate[2],4), pos=3)
  segments(x0=boot.lnorm.E$CI[4],y1=-1,0.8,col="red")
  text(boot.lnorm.E$CI[4],0,labels=round(boot.lnorm.E$CI[4],4), pos=3)
  segments(x0=boot.lnorm.E$CI[6],y1=-1,0.7,col="red")
  text(boot.lnorm.E$CI[6],0,labels=round(boot.lnorm.E$CI[6],4), pos=3)
  par(mfrow=c(1,1))
  
  ks.test(E,"plnorm",meanlog=fit.lnorm.E$estimate[1],sdlog=fit.lnorm.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.lnorm.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.lnorm.R = fitdist(R,"lnorm") #fit our distribution for a normal distribution
  ks.test(R,"plnorm",meanlog=fit.lnorm.R$estimate[1],sdlog=fit.lnorm.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.lnorm.R)
  
  
#### GAMMA DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.gamma.E = fitdist(E,"gamma",lower=0) #fit our distribution for a gamma  distribution
  ks.test(E,"pgamma",shape=fit.gamma.E$estimate[1],rate=fit.gamma.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.gamma.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.gamma.R = fitdist(R,"gamma",lower=0)#fit our distribution for a normal distribution
  ks.test(R,"pgamma",shape=fit.gamma.R$estimate[1],rate=fit.gamma.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.gamma.R)
  
  
#### BETA DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.beta.E = fitdist(E*0.00001,"beta") #fit our distribution for a log normal distribution
  ks.test(E*0.00001,"pbeta",shape1=fit.beta.E$estimate[1],shape2=fit.beta.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.beta.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.beta.R = fitdist(R*0.00000001,"beta")#fit our distribution for a normal distribution
  ks.test(R*0.00000001,"pbeta",shape1=fit.beta.R$estimate[1],shape2=fit.beta.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.beta.R)
  
#### WEIBULL DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.weibull.E = fitdist(E,"weibull") #fit our distribution for a weibull distribution
  ks.test(E,"pweibull",shape=fit.weibull.E$estimate[1],scale=fit.weibull.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.weibull.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.weibull.R = fitdist(R,"weibull")#fit our distribution for a normal distribution
  summary(fit.weibull.R)
  boot.weibull.R =  bootdist(fit.weibull.R, niter = 150000,bootmethod="nonparam",parallel="multicore",ncpus = 4)
  summary(boot.weibull.R)
  CI.mean.norm.E = quantile(boot.weibull.R$estim[,1], c(0.025, 0.975))
  CI.sd.norm.E = quantile(boot.weibull.R$estim[,2], c(0.025, 0.975))
  
  #plot confidence intervall
  par(mfrow=c(2,1))
  plot(density(boot.weibull.R$estim[,1]),main="Confidence Interval shape")
  text(fit.weibull.R$estimate[1],0,labels=round(fit.weibull.R$estimate[1],4), pos=3)
  segments(x0=boot.weibull.R$CI[3],y1=-1,1.6,col="red")
  text(boot.weibull.R$CI[3],0,labels=round(boot.weibull.R$CI[3],4), pos=3)
  segments(x0=boot.weibull.R$CI[5],y1=-1,1.0,col="red")
  text(boot.weibull.R$CI[5],0,labels=round(boot.weibull.R$CI[5],3), pos=3)
  
  
  plot(density(boot.weibull.R$estim[,2]),main="Confidence Interval scale")
  text(fit.weibull.R$estimate[2],0,labels=round(fit.weibull.R$estimate[2],3), pos=3)
  segments(x0=boot.weibull.R$CI[4],y1=-1,0.00023,col="red")
  text(boot.weibull.R$CI[4],0,labels=round(boot.weibull.R$CI[4],3), pos=3)
  segments(x0=boot.weibull.R$CI[6],y1=-1,0.00015,col="red")
  text(boot.weibull.R$CI[6],0,labels=round(boot.weibull.R$CI[6],3), pos=3)
  par(mfrow=c(1,1))
  
  
  ks.test(R,"pweibull",shape=fit.weibull.R$estimate[1],scale=fit.weibull.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.weibull.R)
  
#### EXPONENTIAL DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.exp.E = fitdist(E,"exp") #fit our distribution for a log normal distribution
  ks.test(E,"pexp",rate=fit.exp.E$estimate[1])# test that fitted parameter are statistically significant
  gofstat(fit.exp.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.exp.R = fitdist(R,"exp",lower=0.1)#fit our distribution for a normal distribution
  ks.test(R,"pexp",rate=fit.exp.R$estimate[1])# test that fitted parameter are statistically significant
  gofstat(fit.exp.R)
  
#### LOGISTICAL DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.logis.E = fitdist(E,"logis") #fit our distribution for a log normal distribution
  ks.test(E,"plogis",location=fit.logis.E$estimate[1],scale=fit.logis.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.logis.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.logis.R = fitdist(R,"logis")#fit our distribution for a normal distribution
  fit.logis.R$estimate
  ks.test(R,"plogis",location=fit.logis.R$estimate[1],scale=fit.logis.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.logis.R)
  

#### LOG-LOGISTICAL DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.llogis.E = fitdist(E,"llogis") #fit our distribution for a log normal distribution
  #plot(fit.llogis.R)
  fit.llogis.E$estimate
  ks.test(E,"pllogis",shape=fit.llogis.E$estimate[1],scale=fit.llogis.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.llogis.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.llogis.R = fitdist(R,"llogis")#fit our distribution for a normal distribution
  fit.llogis.R$estimate
  ks.test(R,"pllogis",shape=fit.llogis.R$estimate[1],scale=fit.llogis.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.llogis.R)
  
  
#### POWER-LAW DISTRIBUTION #####
  fit.pareto.E = fitdist(E,"pareto",lower = c(0, 0), start = list(scale = 1, shape = 1)) #fit our distribution fpareto
  ks.test(E,"ppareto",scale=fit.pareto.E$estimate[1],shape=fit.pareto.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.pareto.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.pareto.R = fitdist(R,"pareto",lower = c(0, 0), start = list(scale = 1, shape = 1))
  ks.test(R,"ppareto",scale=fit.pareto.R$estimate[1],shape=fit.pareto.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.pareto.R)
  
#### CAUCHY DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.cauchy.E = fitdist(E,"cauchy") #fit our distribution for a log normal distribution
  ks.test(E,"pcauchy",location=fit.cauchy.E$estimate[1],scale=fit.cauchy.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.cauchy.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.cauchy.R = fitdist(R,"cauchy")#fit our distribution for a normal distribution
  ks.test(R,"pcauchy",location=fit.cauchy.R$estimate[1],scale=fit.cauchy.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.cauchy.R)
  
##### RISULTATI ####
  colors =list("")
  legend =list("LNormal","Gamma","Weibull","Exponential","Llogis","Pareto")
  #
  distr.E = list(
                 aidaS[["200 E"]][["fits"]][["lnorm"]],
                 aidaS[["200 E"]][["fits"]][["gamma"]],
                 aidaS[["200 E"]][["fits"]][["weibull"]],
                 aidaS[["200 E"]][["fits"]][["exp"]],
                 aidaS[["200 E"]][["fits"]][["llogis"]],
                 aidaS[["200 E"]][["fits"]][["pareto"]])
  distr.R = list(
                 aidaS[["200 R"]][["fits"]][["lnorm"]],
                 aidaS[["200 R"]][["fits"]][["gamma"]],
                 aidaS[["200 R"]][["fits"]][["weibull"]],
                 aidaS[["200 R"]][["fits"]][["exp"]],
                 aidaS[["200 R"]][["fits"]][["llogis"]],
                 aidaS[["200 R"]][["fits"]][["pareto"]])
  
  
  par(mfrow=c(2,2))
  cdfcomp(distr.R,legendtext = legend,xlab = "Revenue",lwd=2) # compare cumulative
  cdfcomp(distr.R,xlim=c(2,60000),ylim=c(0.7,1.0),legendtext = legend,xlab = "Revenue",lwd=2) # compare cumulative
  cdfcomp(restaurS[["1000 R"]][["fits"]][["weibull"]],legendtext = "Weibull",xlab = "Theoretical Distribution",main = NULL) # compare cumulative
  cdfcomp(distr.E,xlim=c(2,200),ylim=c(0.7,1.0),legendtext = legend,xlab = "Employee",lwd=2) # compare cumulative
  par(mfrow=c(1,1))
  
  par(mfrow=c(1,2))
  qqcomp(distr.R,legendtext = legend,xlab = "Revenue",xlim = c(0,100000),ylim = c(0,70000)) # compare quartiles
  qqcomp(distr.E,legendtext = legend,xlab = "Employee",xlim = c(0,150),ylim = c(0,750))
  par(mfrow=c(1,1))
  #
  

#### INTERVALLI DI CONFIDENZA####
  #EMPLOYEE 
  boot =  bootdist(aidaS[["200 E"]][["fits"]][["weibull"]], niter = 1000,parallel="multicore",ncpus = 4)
  q = quantile(boot$estim[,2], c(0.025, 0.975))
  plotConfidInterv(boot$estim[,1],aidaS[["200 E"]][["fits"]][["weibull"]][["estimate"]][["shape"]])
  plotConfidInterv(boot$estim[,2],aidaS[["200 E"]][["fits"]][["weibull"]][["estimate"]][["scale"]])
   plotConfidInterv<-function(data, myValue=F, conf=.05) {
    "plot(density(data))
    abline(v=quantile(data, conf/2), col='red')
    abline(v=quantile(data, 1-(conf/2)), col='red')"
    # subset region and plot
    plt<-ggplot(as.data.frame(data), aes(x=data)) + geom_density(colour = "black", size=1.2) 
    d <- ggplot_build(plt)$data[[1]]
    ypos1 <- d$y[match(x=T, d$x>=quantile(data,conf/2))]
    ypos2 <- d$y[match(x=T, d$x>=quantile(data,1-conf/2))]
    
    plt <- plt + 
      geom_area(data = subset(d, x >= quantile(data,.025) & x <= quantile(data,.975)), aes(x=x, y=y), fill="grey69") +
      geom_segment(aes(x=quantile(data, .025), xend=quantile(data, .025), y=0, yend=ypos1), color="navyblue", size=1.2) + 
      geom_segment(aes(x=quantile(data, .975), xend=quantile(data, .975), y=0, yend=ypos2), color="navyblue", size=1.2)
    if(myValue) {
      myColor='red'
      if (myValue<quantile(data,conf/2) | myValue>quantile(data,1-conf/2))
        myColor='red'
      ypos <- d$y[match(x=T, d$x>=myValue)]
      plt <- plt + geom_segment(aes(x=myValue, xend=myValue, y=0, yend=ypos), color=myColor, size=1.2)
    }
    plt + geom_density(colour = "black", size=1.2)
    #return(d)
  }
  
  
  

   
   
#### prova