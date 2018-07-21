#AIDA
{
#AIC
aidaS[["350000 E"]][["gof"]][[1]][["aic"]]
aidaS[["350000 R"]][["gof"]][[1]][["aic"]]

aidaS[["10000 E"]][["gof"]][[1]][["aic"]]
aidaS[["10000 R"]][["gof"]][[1]][["aic"]]

aidaS[["1000 E"]][["gof"]][[1]][["aic"]]
aidaS[["1000 R"]][["gof"]][[1]][["aic"]]

aidaS[["500 E"]][["gof"]][[1]][["aic"]]
aidaS[["500 R"]][["gof"]][[1]][["aic"]]

aidaS[["300 E"]][["gof"]][[1]][["aic"]]
aidaS[["300 R"]][["gof"]][[1]][["aic"]]

aidaS[["200 E"]][["gof"]][[1]][["aic"]]
aidaS[["200 R"]][["gof"]][[1]][["aic"]]

#P-VALUE
getSortedPValue(aidaS[["350000 E"]]) #0
getSortedPValue(aidaS[["350000 R"]])  #0

getSortedPValue(aidaS[["10000 E"]]) #0
getSortedPValue(aidaS[["10000 R"]]) #0

getSortedPValue(aidaS[["1000 E"]])  #0
getSortedPValue(aidaS[["1000 R"]])  #WEIBULL

getSortedPValue(aidaS[["500 E"]])   #0
getSortedPValue(aidaS[["500 R"]])  #WEIBULL

getSortedPValue(aidaS[["300 E"]])  #lnorm
getSortedPValue(aidaS[["300 R"]])  #weibull

getSortedPValue(aidaS[["200 E"]])  #gamma
getSortedPValue(aidaS[["200 R"]])  #weibull


#D - TEST
getSortedD(aidaS[["350000 E"]])
getSortedD(aidaS[["350000 R"]])

getSortedD(aidaS[["10000 E"]])
getSortedD(aidaS[["10000 R"]])

getSortedD(aidaS[["1000 E"]])
getSortedD(aidaS[["1000 R"]])

getSortedD(aidaS[["500 E"]])
getSortedD(aidaS[["500 R"]])

getSortedD(aidaS[["300 E"]])
getSortedD(aidaS[["300 R"]])

getSortedD(aidaS[["200 E"]])  #gamma
getSortedD(aidaS[["200 R"]])  #weibull
}

#MANUFACTURING
{
#AIC
manS[["350000 E"]][["gof"]][[1]][["aic"]]
manS[["350000 R"]][["gof"]][[1]][["aic"]]

manS[["10000 E"]][["gof"]][[1]][["aic"]]
manS[["10000 R"]][["gof"]][[1]][["aic"]]

manS[["1000 E"]][["gof"]][[1]][["aic"]]
manS[["1000 R"]][["gof"]][[1]][["aic"]]

manS[["500 E"]][["gof"]][[1]][["aic"]]
manS[["500 R"]][["gof"]][[1]][["aic"]]

manS[["300 E"]][["gof"]][[1]][["aic"]]
manS[["300 R"]][["gof"]][[1]][["aic"]]

manS[["200 E"]][["gof"]][[1]][["aic"]]
manS[["200 R"]][["gof"]][[1]][["aic"]]

#P-VALUE
getSortedPValue(manS[["350000 E"]]) #0
getSortedPValue(manS[["350000 R"]]) #0
                
getSortedPValue(manS[["10000 E"]]) #0
getSortedPValue(manS[["10000 R"]]) #0
                
getSortedPValue(manS[["1000 E"]])  #lnorm
getSortedPValue(manS[["1000 R"]])  #WEIBULL
                
getSortedPValue(manS[["500 E"]])   #gamma
getSortedPValue(manS[["500 R"]])  #WEIBULL
                
getSortedPValue(manS[["300 E"]])  #lnorm
getSortedPValue(manS[["300 R"]])  #weibull
                
getSortedPValue(manS[["200 E"]])  #lnorm
getSortedPValue(manS[["200 R"]])  #weibull

#D
getSortedD(manS[["350000 E"]]) #0
getSortedD(manS[["350000 R"]]) #0

getSortedD(manS[["10000 E"]]) #0
getSortedD(manS[["10000 R"]]) #0

getSortedD(manS[["1000 E"]])  #lnorm
getSortedD(manS[["1000 R"]])  #WEIBULL

getSortedD(manS[["500 E"]])   #gamma
getSortedD(manS[["500 R"]])  #WEIBULL

getSortedD(manS[["300 E"]])  #lnorm
getSortedD(manS[["300 R"]])  #weibull

getSortedD(manS[["200 E"]])  #lnorm
getSortedD(manS[["200 R"]])  #weibull
}

#MEDIA
{
  #AIC
  mediaS[["150000 E"]][["gof"]][[1]][["aic"]]
  mediaS[["150000 R"]][["gof"]][[1]][["aic"]]
  
  mediaS[["10000 E"]][["gof"]][[1]][["aic"]]
  mediaS[["10000 R"]][["gof"]][[1]][["aic"]]
  
  mediaS[["1000 E"]][["gof"]][[1]][["aic"]]
  mediaS[["1000 R"]][["gof"]][[1]][["aic"]]
  
  mediaS[["500 E"]][["gof"]][[1]][["aic"]]
  mediaS[["500 R"]][["gof"]][[1]][["aic"]]
  
  mediaS[["300 E"]][["gof"]][[1]][["aic"]]
  mediaS[["300 R"]][["gof"]][[1]][["aic"]]
  
  mediaS[["200 E"]][["gof"]][[1]][["aic"]]
  mediaS[["200 R"]][["gof"]][[1]][["aic"]]
  
  #P-VALUE
  getSortedPValue(mediaS[["150000 E"]]) #0
  getSortedPValue(mediaS[["150000 R"]]) #0
  
  getSortedPValue(mediaS[["10000 E"]]) #0
  getSortedPValue(mediaS[["10000 R"]]) #0
  
  getSortedPValue(mediaS[["1000 E"]])  #lnorm
  getSortedPValue(mediaS[["1000 R"]])  #WEIBULL
  
  getSortedPValue(mediaS[["500 E"]])   #gamma
  getSortedPValue(mediaS[["500 R"]])  #WEIBULL
  
  getSortedPValue(mediaS[["300 E"]])  #lnorm
  getSortedPValue(mediaS[["300 R"]])  #weibull
  
  getSortedPValue(mediaS[["200 E"]])  #lnorm
  getSortedPValue(mediaS[["200 R"]])  #weibull
  
  #D
  getSortedD(mediaS[["150000 E"]]) #0
  getSortedD(mediaS[["150000 R"]]) #0
  
  getSortedD(mediaS[["10000 E"]]) #0
  getSortedD(mediaS[["10000 R"]]) #0
  
  getSortedD(mediaS[["1000 E"]])  #lnorm
  getSortedD(mediaS[["1000 R"]])  #WEIBULL
  
  getSortedD(mediaS[["500 E"]])   #gamma
  getSortedD(mediaS[["500 R"]])  #WEIBULL
  
  getSortedD(mediaS[["300 E"]])  #lnorm
  getSortedD(mediaS[["300 R"]])  #weibull
  
  getSortedD(mediaS[["200 E"]])  #lnorm
  getSortedD(mediaS[["200 R"]])  #weibull
}

#RESTAURANT
{
  #AIC
  restaurS[["150000 E"]][["gof"]][[1]][["aic"]]
  restaurS[["150000 R"]][["gof"]][[1]][["aic"]]
  
  restaurS[["10000 E"]][["gof"]][[1]][["aic"]]
  restaurS[["10000 R"]][["gof"]][[1]][["aic"]]
  
  restaurS[["1000 E"]][["gof"]][[1]][["aic"]]
  restaurS[["1000 R"]][["gof"]][[1]][["aic"]]
  
  restaurS[["500 E"]][["gof"]][[1]][["aic"]]
  restaurS[["500 R"]][["gof"]][[1]][["aic"]]
  
  restaurS[["300 E"]][["gof"]][[1]][["aic"]]
  restaurS[["300 R"]][["gof"]][[1]][["aic"]]
  
  restaurS[["200 E"]][["gof"]][[1]][["aic"]]
  restaurS[["200 R"]][["gof"]][[1]][["aic"]]
  
  #P-VALUE
  getSortedPValue(restaurS[["150000 E"]]) #0
  getSortedPValue(restaurS[["150000 R"]]) #0
  
  getSortedPValue(restaurS[["10000 E"]]) #0
  getSortedPValue(restaurS[["10000 R"]]) #0
  
  getSortedPValue(restaurS[["1000 E"]])  #lnorm
  getSortedPValue(restaurS[["1000 R"]])  #WEIBULL
  
  getSortedPValue(restaurS[["500 E"]])   #gamma
  getSortedPValue(restaurS[["500 R"]])  #WEIBULL
  
  getSortedPValue(restaurS[["300 E"]])  #lnorm
  getSortedPValue(restaurS[["300 R"]])  #weibull
  
  getSortedPValue(restaurS[["200 E"]])  #lnorm
  getSortedPValue(restaurS[["200 R"]])  #weibull
  
  #D
  getSortedD(mediaS[["150000 E"]]) #0
  getSortedD(mediaS[["150000 R"]]) #0
  
  getSortedD(mediaS[["10000 E"]]) #0
  getSortedD(mediaS[["10000 R"]]) #0
  
  getSortedD(mediaS[["1000 E"]])  #lnorm
  getSortedD(mediaS[["1000 R"]])  #WEIBULL
  
  getSortedD(mediaS[["500 E"]])   #gamma
  getSortedD(mediaS[["500 R"]])  #WEIBULL
  
  getSortedD(mediaS[["300 E"]])  #lnorm
  getSortedD(mediaS[["300 R"]])  #weibull
  
  getSortedD(mediaS[["200 E"]])  #lnorm
  getSortedD(mediaS[["200 R"]])  #weibull
}

#### PLOT DENSITY AND  HISTOGRAM  #####
par(mfrow=c(1,2))
#REVENUE : su 100 sembra fittare per gamma
legendLabel=c("logNormal","Gamma","Weibull")
x = restaurS[["1000 R"]][["sample"]]
na=mediaS[["40000 R"]][["fits"]][["norm"]][["estimate"]]
ln=mediaS[["40000 R"]][["fits"]][["lnorm"]][["estimate"]]
ga=mediaS[["40000 R"]][["fits"]][["gamma"]][["estimate"]]
weib=mediaS[["1000 R"]][["fits"]][["weibull"]][["estimate"]]
ex=mediaS[["40000 R"]][["fits"]][["exp"]][["estimate"]]
logis=mediaS[["40000 R"]][["fits"]][["llogis"]][["estimate"]]
pa=mediaS[["40000 R"]][["fits"]][["pareto"]][["estimate"]]


hist(x, freq=F,breaks ="fd",main =NULL,xlab = "Revenue",xlim = c(0,5000))
#lines(density(x))
#curve(dnorm(x, na[1], na[2]), col='blue',add=T,lwd=2)
curve(dlnorm(x,ln[1],ln[2]),col="red",add=T,lwd=2)
curve(dgamma(x,ga[1],ga[2]),col="green",add=T,lwd=2)
curve(dweibull(x,weib[1],weib[2]),col="blue",add=T,lwd=2)
#curve(dexp(x,ex[1]),col="yellow",add=T,lwd=2)
curve(dllogis(x,logis[1],logis[2]),col="grey",add=T,lwd=2)
curve(dpareto(x,pa[1],pa[2]),col="yellow",add=T,lwd=2)
legend("topright",legendLabel,fill=c("red","green","blue"))

#employee: sembra essere più PARETO
legendLabel=c("logNormal","Gamma","Weibull","Pareto")
x = restaurS[["1000 E"]][["sample"]]
na=mediaS[["40000 E"]][["fits"]][["norm"]][["estimate"]]
ln=mediaS[["40000 E"]][["fits"]][["lnorm"]][["estimate"]]
ga=mediaS[["40000 E"]][["fits"]][["gamma"]][["estimate"]]
weib=restaurS[["1000 E"]][["fits"]][["weibull"]][["estimate"]]
ex=mediaS[["40000 E"]][["fits"]][["exp"]][["estimate"]]
logis=mediaS[["40000 E"]][["fits"]][["llogis"]][["estimate"]]
pa=mediaS[["40000 E"]][["fits"]][["pareto"]][["estimate"]]

hist(x, freq=F,breaks = "fd",main = NULL,xlab = "Employee",xlim = c(0,60))
legend("topright",legend = "Exponential",fill="green")
#lines(density(x))
#curve(dnorm(x, na[1], na[2]), col='blue',add=T,lwd=2)
curve(dlnorm(x,ln[1],ln[2]),col="red",add=T,lwd=2)
curve(dgamma(x,ga[1],ga[2]),col="green",add=T,lwd=2)
curve(dweibull(x,weib[1],weib[2]),col="blue",add=T,lwd=2)
curve(dexp(x,ex[1]),col="green",add=T,lwd=2)
#curve(dllogis(x,logis[1],logis[2]),col="grey",add=T,lwd=2)
curve(dpareto(x,pa[1],pa[2]),col="yellow",add=T,lwd=2)
legend("topright",legendLabel,fill=c("red","green","blue","yellow"))
par(mfrow=c(1,1))


#### INTERVALLI DI CONFIDENZA PER REVENUE (GAMMA,WEIBULL) ED EMPLOYEE PARETO ####
library(ggplot2)
library(fitdistrplus)
require(cowplot)
plotConfidInterv<-function(data, myValue=F, conf=.05) {
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
#BOOTSTRAP WEIBULL
boot.Weibull =  bootdist(restaurS[["200 R"]][["fits"]][["weibull"]], niter = 20000,parallel="multicore",ncpus = 4,bootmethod = "nonparam")
q1 = quantile(boot.Weibull$estim[,1], c(0.025, 0.975))
q1 #0.3344109 0.4230618 
q2 = quantile(boot.Weibull$estim[,2], c(0.025, 0.975))
q2 #256.1458 567.1988 
a = plotConfidInterv(boot.Weibull$estim[,1],restaurS[["200 R"]][["fits"]][["weibull"]][["estimate"]][["shape"]])
b = plotConfidInterv(boot.Weibull$estim[,2],restaurS[["200 R"]][["fits"]][["weibull"]][["estimate"]][["scale"]])
plot_grid(a,b,labels = c('Shape', 'Scale'))


#BOOTSTRAP GAMMA
boot.gamma =  bootdist(aidaS[["200 R"]][["fits"]][["gamma"]], niter = 20000,parallel="multicore",ncpus = 4,bootmethod = "nonparam")
q1 = quantile(boot.gamma$estim[,1], c(0.025, 0.975)) #shape
q1 #0.1882148 0.2848943 
q2 = quantile(boot.gamma$estim[,2], c(0.025, 0.975)) #rate
q2 #0.0000390436 0.0002793919
c = plotConfidInterv(boot.gamma$estim[,1],aidaS[["200 R"]][["fits"]][["gamma"]][["estimate"]][["shape"]])
d = plotConfidInterv(boot.gamma$estim[,2],aidaS[["200 R"]][["fits"]][["gamma"]][["estimate"]][["rate"]])
plot_grid(c,d,labels = c('Shape', 'Rate'))

