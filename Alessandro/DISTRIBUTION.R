ls() #Environment
rm("prova") #remove all variables
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

set.seed(1000)
#secondo il metodo del MLE, valutando il valore AIC, la distribuzione di Employee segue una distribuzione paretiana mentre
#Revenue segue una distribuzione di Weibull

#prendo il dataset manufacturing
manufacturing = get(load("manufacturing.RData"))
nrow(manufacturing)# 1.027.140 records

library(kimisc)

dat.1 = sample.rows(manufacturing,700000,replace = FALSE)

E = dat.1$E
skewness(E)# 95.79515 simmetria
kurtosis(E)# 16186.75

R = dat.1$R
skewness(R)# 131.3853 simmetria
kurtosis(R)# 23960.05


par(mfrow=c(2,1))
descdist(R, discrete = FALSE,obs.col="red",obs.pch = 15, boot.col="blue")
descdist(E, discrete = FALSE,obs.col="red", obs.pch = 15, boot.col="blue")
par(mfrow=c(1,1))



#AIC
{
  #DISTRIBUZIONI 
  #### NORMAL DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.norm.E = fitdist(E,"norm") #fit our distribution for a normal distribution
  fit.norm.E$estimate[1] # mean sample
  fit.norm.E$estimate[2] # sd sample
  boot.norm.E =  bootdist(fit.norm.E, niter = 150,bootmethod="param",parallel="multicore",ncpus = 4)
  summary(boot.norm.E)
  #oppure
  #CI.mean.norm.E = quantile(boot.norm.E$estim[,1], c(0.025, 0.975))
  #CI.sd.norm.E = quantile(boot.norm.E$estim[,2], c(0.025, 0.975))
  #plot confidence intervall
  plot(density(boot.norm.E$estim[,1]))
  abline(v = mean(boot.norm.E$estim[,1]), col="black")
  abline(v=fit.norm.E$estimate[1],col="blue")
  abline(v=boot.norm.E$CI[3],col="red")
  abline(v=boot.norm.E$CI[5],col="red")
  
  plot(density(boot.norm.E$estim[,2]))
  abline(v = mean(boot.norm.E$estim[,2]), col="black")
  abline(v=fit.norm.E$estimate[2],col="blue")
  abline(v=boot.norm.E$CI[4],col="red")
  abline(v=boot.norm.E$CI[6],col="red")
  
 
  ks.test(E,"pnorm",mean=fit.norm.E$estimate[1],sd=fit.norm.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.norm.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.norm.R = fitdist(R,"norm") #fit our distribution for a normal distribution
  fit.norm.R$estimate[1] # mean sample
  fit.norm.R$estimate[2] # sd sample
  boot.norm.R =  bootdist(fit.norm.R, niter = 150,bootmethod="param",parallel="multicore",ncpus = 4)
  summary(boot.norm.R)
  #oppure
  #CI.mean.norm.E = quantile(boot.norm.E$estim[,1], c(0.025, 0.975))
  #CI.sd.norm.E = quantile(boot.norm.E$estim[,2], c(0.025, 0.975))
  
  #plot confidence intervall mean
  plot(density(boot.norm.R$estim[,1]))
  abline(v = mean(boot.norm.R$estim[,1]), col="black")
  abline(v=fit.norm.R$estimate[1],col="blue")
  abline(v=boot.norm.R$CI[3],col="red")
  abline(v=boot.norm.R$CI[5],col="red")
  
  plot(density(boot.norm.R$estim[,2]))
  abline(v = mean(boot.norm.R$estim[,2]), col="black")
  abline(v=fit.norm.R$estimate[2],col="blue")
  abline(v=boot.norm.R$CI[4],col="red")
  abline(v=boot.norm.R$CI[6],col="red")
  
  ks.test(R,"pnorm",mean=fit.norm.R$estimate[1],sd=fit.norm.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.norm.R)
  
  
  #### LOG-NORM DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.lnorm.E = fitdist(E,"lnorm") #fit our distribution for a log normal distribution
  fit.lnorm.E$estimate[1] # meanlog sample
  fit.lnorm.E$estimate[2] # sdlog sample
  boot.lnorm.E =  bootdist(fit.lnorm.E, niter = 150,bootmethod="nonparam",parallel="multicore",ncpus = 4)
  summary(boot.lnorm.E)
  #oppure
  #CI.mean.norm.E = quantile(boot.norm.E$estim[,1], c(0.025, 0.975))
  #CI.sd.norm.E = quantile(boot.norm.E$estim[,2], c(0.025, 0.975))
  #plot confidence intervall
  plot(density(boot.lnorm.E$estim[,1]))
  abline(v = mean(boot.lnorm.E$estim[,1]), col="black")
  abline(v=fit.lnorm.E$estimate[1],col="blue")
  abline(v=boot.lnorm.E$CI[3],col="red")
  abline(v=boot.lnorm.E$CI[5],col="red")
  
  plot(density(boot.lnorm.E$estim[,2]))
  abline(v = mean(boot.lnorm.E$estim[,2]), col="black")
  abline(v=fit.lnorm.E$estimate[2],col="blue")
  abline(v=boot.lnorm.E$CI[4],col="red")
  abline(v=boot.lnorm.E$CI[6],col="red")
  
  
  ks.test(E,"plnorm",meanlog=fit.lnorm.E$estimate[1],sdlog=fit.lnorm.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.lnorm.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.lnorm.R = fitdist(R,"lnorm") #fit our distribution for a normal distribution
  fit.lnorm.R$estimate[1] # meanlog sample
  fit.lnorm.R$estimate[2] # sdlog sample
  boot.lnorm.R =  bootdist(fit.lnorm.E, niter = 150,bootmethod="nonparam",parallel="multicore",ncpus = 4)
  summary(boot.lnorm.R)
  #oppure
  #CI.mean.lnorm.R = quantile(boot.lnorm.R$estim[,1], c(0.025, 0.975))
  #CI.sd.lnorm.R = quantile(boot.lnorm.R$estim[,2], c(0.025, 0.975))
  #plot confidence intervall
  plot(density(boot.lnorm.R$estim[,1]))
  abline(v = mean(boot.lnorm.R$estim[,1]), col="black")
  abline(v=fit.lnorm.R$estimate[1],col="blue")
  abline(v=boot.lnorm.R$CI[3],col="red")
  abline(v=boot.lnorm.R$CI[5],col="red")
  
  plot(density(boot.lnorm.R$estim[,2]))
  abline(v = mean(boot.lnorm.R$estim[,2]), col="black")
  abline(v=fit.lnorm.R$estimate[2],col="blue")
  abline(v=boot.lnorm.R$CI[4],col="red")
  abline(v=boot.lnorm.R$CI[6],col="red")
  
  
  ks.test(R,"plnorm",meanlog=fit.lnorm.R$estimate[1],sdlog=fit.lnorm.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.lnorm.R)
  
  
  #### GAMMA DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.gamma.E = fitdist(E,"gamma") #fit our distribution for a gamma  distribution
  fit.gamma.E$estimate[1] # shape sample
  fit.gamma.E$estimate[2] # rate sample
  boot.gamma.E =  bootdist(fit.gamma.E, niter = 100,bootmethod="nonparam",parallel="multicore",ncpus = 4)
  summary(boot.gamma.E)
  #oppure
  #CI.mean.norm.E = quantile(boot.norm.E$estim[,1], c(0.025, 0.975))
  #CI.sd.norm.E = quantile(boot.norm.E$estim[,2], c(0.025, 0.975))
  #plot confidence intervall
  plot(density(boot.gamma.E$estim[,1]))
  abline(v = mean(boot.gamma.E$estim[,1]), col="black")
  abline(v=fit.gamma.E$estimate[1],col="blue")
  abline(v=boot.gamma.E$CI[3],col="red")
  abline(v=boot.gamma.E$CI[5],col="red")
  
  plot(density(boot.gamma.E$estim[,2]))
  abline(v = mean(boot.gamma.E$estim[,2]), col="black")
  abline(v=fit.gamma.E$estimate[2],col="blue")
  abline(v=boot.gamma.E$CI[4],col="red")
  abline(v=boot.gamma.E$CI[6],col="red")
  
  
  ks.test(E,"pgamma",shape=fit.gamma.E$estimate[1],rate=fit.gamma.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.gamma.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.gamma.R = fitdist(R,"gamma",lower=0)#fit our distribution for a normal distribution
  fit.gamma.R$estimate[1] # shape sample
  fit.gamma.R$estimate[2] # rate sample
  boot.gamma.R =  bootdist(fit.gamma.R, niter = 100,bootmethod="nonparam",parallel="multicore",ncpus = 4)
  summary(boot.gamma.R)
  #oppure
  #CI.mean.norm.E = quantile(boot.norm.E$estim[,1], c(0.025, 0.975))
  #CI.sd.norm.E = quantile(boot.norm.E$estim[,2], c(0.025, 0.975))
  #plot confidence intervall
  plot(density(boot.gamma.R$estim[,1]))
  abline(v = mean(boot.gamma.R$estim[,1]), col="black")
  abline(v=fit.gamma.R$estimate[1],col="blue")
  abline(v=boot.gamma.R$CI[3],col="red")
  abline(v=boot.gamma.R$CI[5],col="red")
  
  plot(density(boot.gamma.R$estim[,2]))
  abline(v = mean(boot.gamma.R$estim[,2]), col="black")
  abline(v=fit.gamma.R$estimate[2],col="blue")
  abline(v=boot.gamma.R$CI[4],col="red")
  abline(v=boot.gamma.R$CI[6],col="red")
  
  ks.test(R,"pgamma",shape=fit.gamma.R$estimate[1],rate=fit.gamma.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.gamma.R)
  
  
  #### BETA DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.beta.E = fitdist(E*0.00001,"beta") #fit our distribution for a log normal distribution
  fit.beta.E$estimate[1] # shape1 sample
  fit.beta.E$estimate[2] # shape2 sample
  boot.beta.E =  bootdist(fit.beta.E, niter = 100,bootmethod="nonparam",parallel="multicore",ncpus = 4)
  summary(boot.beta.E)
  #oppure
  #CI.mean.norm.E = quantile(boot.norm.E$estim[,1], c(0.025, 0.975))
  #CI.sd.norm.E = quantile(boot.norm.E$estim[,2], c(0.025, 0.975))
  #plot confidence intervall
  plot(density(boot.beta.E$estim[,1]))
  abline(v = mean(boot.beta.E$estim[,1]), col="black")
  abline(v=fit.beta.E$estimate[1],col="blue")
  abline(v=boot.beta.E$CI[3],col="red")
  abline(v=boot.beta.E$CI[5],col="red")
  
  plot(density(boot.beta.E$estim[,2]))
  abline(v = mean(boot.beta.E$estim[,2]), col="black")
  abline(v=fit.beta.E$estimate[2],col="blue")
  abline(v=boot.beta.E$CI[4],col="red")
  abline(v=boot.beta.E$CI[6],col="red")
  
  
  ks.test(E,"pbeta",shape1=fit.beta.E$estimate[1],shape2=fit.beta.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.beta.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.beta.R = fitdist(R*0.00000001,"beta")#fit our distribution for a normal distribution
  fit.beta.R$estimate[1] # shape1 sample
  fit.beta.R$estimate[2] # shape2 sample
  boot.beta.R =  bootdist(fit.beta.R, niter = 100,bootmethod="nonparam",parallel="multicore",ncpus = 4)
  summary(boot.beta.R)
  #oppure
  #CI.mean.norm.E = quantile(boot.norm.E$estim[,1], c(0.025, 0.975))
  #CI.sd.norm.E = quantile(boot.norm.E$estim[,2], c(0.025, 0.975))
  #plot confidence intervall
  plot(density(boot.beta.R$estim[,1]))
  abline(v = mean(boot.beta.R$estim[,1]), col="black")
  abline(v=fit.beta.R$estimate[1],col="blue")
  abline(v=boot.beta.R$CI[3],col="red")
  abline(v=boot.beta.R$CI[5],col="red")
  
  plot(density(boot.beta.R$estim[,2]))
  abline(v = mean(boot.beta.R$estim[,2]), col="black")
  abline(v=fit.beta.R$estimate[2],col="blue")
  abline(v=boot.beta.R$CI[4],col="red")
  abline(v=boot.beta.R$CI[6],col="red")
  
  
  ks.test(R,"pbeta",shape1=fit.beta.R$estimate[1],shape2=fit.beta.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.beta.R)
  
  #### WEIBULL DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.weibull.E = fitdist(E,"weibull") #fit our distribution for a weibull distribution
  fit.weibull.E$estimate[1] # shape sample
  fit.weibull.E$estimate[2] # scale sample
  boot.weibull.E =  bootdist(fit.weibull.E, niter = 100,bootmethod="nonparam",parallel="multicore",ncpus = 4)
  summary(boot.weibull.E)
  #oppure
  #CI.mean.norm.E = quantile(boot.norm.E$estim[,1], c(0.025, 0.975))
  #CI.sd.norm.E = quantile(boot.norm.E$estim[,2], c(0.025, 0.975))
  #plot confidence intervall
  plot(density(boot.weibull.E$estim[,1]))
  abline(v = mean(boot.weibull.E$estim[,1]), col="black")
  abline(v=fit.weibull.E$estimate[1],col="blue")
  abline(v=boot.weibull.E$CI[3],col="red")
  abline(v=boot.weibull.E$CI[5],col="red")
  
  plot(density(boot.weibull.E$estim[,2]))
  abline(v = mean(boot.weibull.E$estim[,2]), col="black")
  abline(v=fit.weibull.E$estimate[2],col="blue")
  abline(v=boot.weibull.E$CI[4],col="red")
  abline(v=boot.weibull.E$CI[6],col="red")
  
  
  ks.test(E,"pweibull",shape=fit.weibull.E$estimate[1],scale=fit.weibull.E$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.weibull.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.weibull.R = fitdist(R,"weibull")#fit our distribution for a normal distribution
  fit.weibull.R$estimate[1] # shape sample
  fit.weibull.R$estimate[2] # scale sample
  boot.weibull.R =  bootdist(fit.weibull.R, niter = 100,bootmethod="nonparam",parallel="multicore",ncpus = 4)
  summary(boot.weibull.R)
  #oppure
  #CI.mean.norm.E = quantile(boot.norm.E$estim[,1], c(0.025, 0.975))
  #CI.sd.norm.E = quantile(boot.norm.E$estim[,2], c(0.025, 0.975))
  #plot confidence intervall
  plot(density(boot.weibull.R$estim[,1]))
  abline(v = mean(boot.weibull.R$estim[,1]), col="black")
  abline(v=fit.weibull.R$estimate[1],col="blue")
  abline(v=boot.weibull.R$CI[3],col="red")
  abline(v=boot.weibull.R$CI[5],col="red")
  
  plot(density(boot.weibull.R$estim[,2]))
  abline(v = mean(boot.weibull.R$estim[,2]), col="black")
  abline(v=fit.weibull.R$estimate[2],col="blue")
  abline(v=boot.weibull.R$CI[4],col="red")
  abline(v=boot.weibull.R$CI[6],col="red")
  
  
  ks.test(R,"pweibull",shape=fit.weibull.R$estimate[1],scale=fit.weibull.R$estimate[2])# test that fitted parameter are statistically significant
  gofstat(fit.weibull.R)
  
  #### EXPONENTIAL DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.exp.E = fitdist(E,"exp") #fit our distribution for a log normal distribution
  gof.exp.E = gofstat(fit.exp.E)
  gof.exp.E
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.exp.R = fitdist(R,"exp",lower=0.1)#fit our distribution for a normal distribution
  gof.exp.R = gofstat(fit.exp.R)
  gof.exp.R
  
  #### LOG-LOGISTICAL DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.llogis.E = fitdist(E,"logis") #fit our distribution for a log normal distribution
  gof.llogis.E = gofstat(fit.llogis.E)
  gof.llogis.E
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.llogis.R = fitdist(R,"logis")#fit our distribution for a normal distribution
  gof.llogis.R = gofstat(fit.llogis.R)
  gof.llogis.R
  
  #### POWER-LAW DISTRIBUTION #####
  fit.pareto.E = fitdist(E,"pareto",lower = c(0, 0), start = list(scale = 1, shape = 1)) #fit our distribution for a log normal distribution
  gof.pareto.E = gofstat(fit.pareto.E)
  gof.pareto.E
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.pareto.R = fitdist(R,"pareto",lower = c(0, 0), start = list(scale = 1, shape = 1))
  gof.pareto.R = gofstat(fit.pareto.R)
  gof.pareto.R
  
  #### POISSON DISTRIBUTION #####
  fit.pois.E = fitdist(E,"pois") #fit our distribution for a log normal distribution
  gof.pois.E = gofstat(fit.pois.E)
  gof.pois.E
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.pois.R = fitdist(round(R),"pois")
  gof.pois.R = gofstat(fit.pois.R)
  gof.pois.R
  
  
  #### CAUCHY DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.cauchy.E = fitdist(E,"cauchy") #fit our distribution for a log normal distribution
  gof.cauchy.E = gofstat(fit.cauchy.E)
  gof.cauchy.E
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.cauchy.R = fitdist(R,"cauchy")#fit our distribution for a normal distribution
  gof.cauchy.R = gofstat(fit.cauchy.R)
  gof.cauchy.R
  
  
  
  ##### RISULTATI ####
  ##EMPLOYEE ->follow pareto distribution
  plot.legend <- c("normal", "lognormal","gamma","weibull","exp","logis","pareto","poisson","cauchy")
  lest = list(fit.norm.E,fit.lnorm.E,fit.gamma.E,fit.weibull.E,fit.exp.E,fit.llogis.E,fit.pareto.E,fit.pois.E,fit.cauchy.E)
  denscomp(lest, legendtext = plot.legend) # compare densities
  cdfcomp(lest, legendtext = plot.legend) # compare cumulative
  qqcomp(lest, legendtext = plot.legend) # compare quartiles
  gofstat(lest, fitnames=plot.legend)
  
  gofstat(fit.beta.E)# LO POSSIAMO LEGGERE IN POSITIVO, NON CAMBIA NULLA
  
  ##REVENUE->follow weibull distribution
  plot.legend <- c("normal", "lognormal","gamma","weibull","exp","logis","pareto","cauchy")
  lest = list(fit.norm.R,fit.lnorm.R,fit.gamma.R,fit.weibull.R,fit.exp.R,fit.llogis.R,fit.pareto.R,fit.cauchy.R)
  denscomp(lest, legendtext = plot.legend) # compare densities
  cdfcomp(lest, legendtext = plot.legend) # compare cumulative
  qqcomp(lest, legendtext = plot.legend) # compare quartiles
  gofstat(lest, fitnames=plot.legend)
  
  par=c("poison")
  gofstat(fit.pois.R,fitnames = par)
  
  
  
  
  
  
  
  
  
  
  
  #### INTERVALLI DI CONFIDENZA####
  #EMPLOYEE PARETO DISTRIBUTION
  fit = fitdist(food.R,"",lower = c(0, 0), start = list(scale = 1, shape = 1))
  fit$estimate
  fit$aic 
  boot =  bootdist(fit, niter = 150,parallel="multicore",ncpus = 4)
  
  q = quantile(boot$estim[,1], c(0.025, 0.975))
  q
  int1 = boot$estim[,1]
  int = int1[int1 >=q[1] & int1<=q[2]]
  t.test(int,mu=fit$estimate[1],conf.level = 0.01,alternative = c("two.sided"))
  
  
  
  
  
}


#prova
{
x =rnorm(1000,1) #definisco una distribuzione normale
x = rlnorm(100)
x =rbeta(100,1,1)
mean(x)#1.017948

fit = fitdist(x,distr="norm",method = "mle") #fit a distribution to extract the parameters for normal distribution  mean,sd
fit$estimate[1] #mean
fit$estimate[2] #sd
fit$data #are the same data from x. 

#in boot i pass a fit object. Fit in fit$data cantains the data from x. 
#If we use boothmethod =nonparam, bootdist take random value from x and the estimate mean and sd
boot =  bootdist(fit,bootmethod = "param",niter = 1000, parallel = "multicore",ncpus = 4)
boot$estim #these are the estimate for niter dataset

gof = gofstat(fit,chisqbreaks = sort(boot$estim[,1])) #in gof we pass the value for mean and the fit distribution
gof$chisqpvalue
gof$kstest#after that, here there is the kstest (reject or not-reject).

#If you use fit norm for x norm i not-reject. If you use fit norm for lnorm is reject, Obviusly
}








