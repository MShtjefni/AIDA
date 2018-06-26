ls() #Environment
rm(list = ls()) #remove all variables
ls() #check varaibles

set.seed(1000)
#secondo il metodo del MLE, valutando il valore AIC, la distribuzione di Employee segue una distribuzione paretiana mentre
#Revenue segue una distribuzione di Weibull

#prendo il dataset manufacturing
manufacturing = get(load("manufacturing.RData"))
nrow(manufacturing)# 1.027.140 records


install.packages('kimisc')
library(kimisc)

prova = sample.rows(manufacturing,700000,replace = FALSE)

E = prova$E
R = prova$R


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
  gof.norm.E = gofstat(fit.norm.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.norm.R = fitdist(R,"norm") #fit our distribution for a normal distribution
  gof.norm.R = gofstat(fit.norm.R)
  
  
  #### LOG-NORM DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.lnorm.E = fitdist(E,"lnorm") #fit our distribution for a log normal distribution
  gof.lnorm.E = gofstat(fit.lnorm.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.lnorm.R = fitdist(R,"lnorm") #fit our distribution for a normal distribution
  gof.lnorm.R = gofstat(fit.lnorm.R)
  
  
  #### GAMMA DISTRIBUTION ####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.gamma.E = fitdist(E,"gamma") #fit our distribution for a log normal distribution
  gof.gamma.E = gofstat(fit.gamma.E)
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.gamma.R = fitdist(R,"gamma",lower=0)#fit our distribution for a normal distribution
  gof.gamma.R = gofstat(fit.gamma.R)
  gof.gamma.R
  
  
  
  
  #### BETA DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.beta.E = fitdist(E*0.0001,"beta") #fit our distribution for a log normal distribution
  gof.beta.E = gofstat(fit.beta.E)
  gof.beta.E
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.beta.R = fitdist(R*0.0000001,"beta")#fit our distribution for a normal distribution
  gof.beta.R = gofstat(fit.beta.R)
  gof.gamma.R
  
  #### WEIBULL DISTRIBUTION #####
  #### EMPLOYEE DISTRIBUTION (rejected)
  fit.weibull.E = fitdist(E,"weibull") #fit our distribution for a log normal distribution
  gof.weibull.E = gofstat(fit.weibull.E)
  gof.weibull.E
  
  #### REVENUEE DISTRIBUTION (rejected)
  fit.weibull.R = fitdist(R,"weibull")#fit our distribution for a normal distribution
  gof.weibull.R = gofstat(fit.weibull.R)
  gof.weibull.R
  
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
  
  
  
  
  
  
  
  
  
  
  
  #### INTERVALLI DI CONFIDENZA
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

#P-Value
{
  ### DISTRIBUZIONE NORMALE ####
  x = rnorm(100)
  x = rlnorm(100)
  fit = fitdist(x,"norm")
  fit$estimate
  
  ks.test(x,"pnorm",mean=fit$estimate[1],sd=fit$estimate[2])# test that fitted parameter are statistically significant
  ### DISTRIBU
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








