library(dplyr)
library(stats4)
library(fitdistrplus) # macimum likelihood fitting (excluding laplace dist) 
library(bbmle) # maximum likelihood fitting with laplace distribution # needed for dlaplace
library(logspline)
library(poweRlaw)
library(lmtest)
library(boot)
library(BSDA)
library(lawstat)
library(sads)
library(e1071)
library(ggplot2)
data<-as.data.frame(AIDA, header=T)
#summary(data)
attach(data) 

# Adjust for inflation
applyInflation <- function(data, inflations=c(1, .032, .007, .016, .027, .03, .011, .002, -0.001, -0.001, .011)) {
  if ("Infl" %in% colnames(data)) 
    return (data)
  
  data$Infl <- NA
  for (i in seq(2,9)) {
    inflations[i]<-(inflations[i-1]*(1+inflations[i]))
    print(inflations[i])
    data$Infl[data$Year==2006+i]<-inflations[i]
  }
  data$Infl[is.na(data$Infl)]<-1
  data$P <- data$P/data$Infl
  data$R <- data$R/data$Infl
  data$B <- data$B/data$Infl
  return(data)
}
###########################################################################################################
plotConfidInterv<-function(data, myValue=F, conf=.05,title, xlb) {
  "plot(density(data))
  abline(v=quantile(data, conf/2), col='red')
  abline(v=quantile(data, 1-(conf/2)), col='red')"
  # subset region and plot
  plt<-ggplot(as.data.frame(data), aes(x=data)) + geom_density(colour = "black", size=0.5) + xlab(xlb) + ggtitle(title)
  d <- ggplot_build(plt)$data[[1]]
  ypos1 <- d$y[match(x=T, d$x>=quantile(data,conf/2))]
  ypos2 <- d$y[match(x=T, d$x>=quantile(data,1-conf/2))]
  
  plt <- plt + 
    geom_area(data = subset(d, x >= quantile(data,.025) & x <= quantile(data,.975)), aes(x=x, y=y), fill="red", alpha=0.1) +
    geom_segment(aes(x=quantile(data, .025), xend=quantile(data, .025), y=0, yend=ypos1), color="navyblue", size=0.8) + 
    geom_segment(aes(x=quantile(data, .975), xend=quantile(data, .975), y=0, yend=ypos2), color="navyblue", size=0.8)
  if(myValue) {
    myColor='red'
    if (myValue<quantile(data,conf/2) | myValue>quantile(data,1-conf/2))
      myColor='red'
    ypos <- d$y[match(x=T, d$x>=myValue)]
    plt <- plt + geom_segment(aes(x=myValue, xend=myValue, y=0, yend=ypos), color=myColor, size=0.8)
  }
  plt + geom_density(colour = "black", size=0.5)
  #return(d)
}
#############################################################################################################
aida<-subset(data, Year>=2007 & Year<=2015 & R>=0 & E>=0)
detach(data)

aida <-applyInflation(aida)
aida$R[aida$R <=1] <-1

by_firm_aida <- group_by(aida, TaxID, Year, Ateco)
by_firm_aida <- summarize(by_firm_aida, Revenue=sum(R))

manufacturing<-subset(aida,Ateco>= 101100 & Ateco<=332009)
manufacturing <- manufacturing[, -c(3,4,8,9)]


#mfc <- group_by(manufacturing, manufacturing$TaxID)
#mfc <- summarize(mfc, N_of_Years=n())
# nine_year_firms <- filter(mfc, mfc$N_of_Years ==9)
# nine_year_firms <- filter(manufacturing, manufacturing$TaxID %in% nine_year_firms$`manufacturing$TaxID`)
# View(nine_year_firms)
# nine_year_firms$Size[nine_year_firms$E <= 49] <- "Small"
# nine_year_firms$Size[50 <= nine_year_firms$E & nine_year_firms$E <= 249] <- "Medium"
# nine_year_firms$Size[nine_year_firms$E >= 250] <- "Large"
#nine_year_firms$R[nine_year_firms$R <=1 ] <- 1

#selezione sotto-settori
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

# 2)NCA -> 117926
# 1) Metallo -> 201092
# 3)Alimentare -> 77556

manufacturing$Size[manufacturing$E <= 49] <- "Small"
manufacturing$Size[50 <= manufacturing$E & manufacturing$E <= 249] <- "Medium"
manufacturing$Size[manufacturing$E >= 250] <- "Large"

#VARIABILE AREA GEOGRAFICA NORD, CENTRO, SUD
manufacturing$GeoArea=0
#NORD-->Liguria, Lombardia, Piemonte, Valle d'Aosta, Emilia-Romagna, Friuli-Venezia Giulia, Trentino-Alto Adige, Veneto
manufacturing$GeoArea[manufacturing$Region=="Liguria" | manufacturing$Region=="Lombardia" |manufacturing$Region=="Piemonte" |manufacturing$Region=="Valle d'Aosta/Vallée d'Aoste" |manufacturing$Region=="Emilia-Romagna"|manufacturing$Region=="Friuli-Venezia Giulia"|manufacturing$Region=="Trentino-Alto Adige"|manufacturing$Region=="Veneto" ]= "Nord"
#CENTRO--> Lazio, Marche, Toscana ed Umbria
manufacturing$GeoArea[manufacturing$Region=="Lazio" |manufacturing$Region=="Marche" |manufacturing$Region=="Toscana" |manufacturing$Region=="Umbria"  ]= "Centro"
#SUD--> Abruzzo, Basilicata, Calabria, Campania, Molise, Puglia, Sardegna, Sicilia
manufacturing$GeoArea[manufacturing$Region=="Abruzzo" |manufacturing$Region=="Basilicata" |manufacturing$Region=="Calabria" |manufacturing$Region=="Campania" |manufacturing$Region=="Molise" |manufacturing$Region=="Puglia" |manufacturing$Region=="Sardegna" |manufacturing$Region=="Sicilia"  ]= "Sud"


small_firms <- subset(manufacturing, manufacturing$Size == "Small")
medium_firms <- subset(manufacturing, manufacturing$Size == "Medium")
large_firms <- subset(manufacturing, manufacturing$Size == "Large")


#View(nine_year_firms)

#test_df <- merge(x = manufacturing, y = gmf, by = c("TaxID", "Year"), all.x = TRUE, sort = F)
#write.csv(test_df, "manufacturing_growth.csv", row.names = F)

#tobacco_sector <- subset(nine_year_firms, Ateco == 120000)
# year_average <- group_by (tobacco_sector, Year)
# year_average <- summarize(year_average, tot= sum(log10(R)/n()))
# View(year_average)

# by_firm_aida <- group_by(aida, TaxID, Year, Ateco)
# by_firm_aida <- summarize(by_firm_aida, Revenue=sum(R))

by_firm_manufacturing <- group_by(manufacturing,TaxID, Year, SubSector, Region, GeoArea, Size)
by_firm_manufacturing <- summarize(by_firm_manufacturing, Revenue=sum(R))

by_firm_small <- group_by(small_firms,TaxID, Year, SubSector, Region, GeoArea, Size)
by_firm_small <- summarize(by_firm_small, Revenue=sum(R))

by_firm_medium <- group_by(medium_firms,TaxID, Year, SubSector,Region, GeoArea, Size)
by_firm_medium <- summarize(by_firm_medium, Revenue=sum(R))

by_firm_large <- group_by(large_firms,TaxID, Year, SubSector,Region, GeoArea, Size)
by_firm_large <- summarize(by_firm_large, Revenue=sum(R))

#for aida
by_firm_aida_ID <- as.numeric(by_firm_aida$TaxID)
by_firm_aida_year <- as.numeric(by_firm_aida$Year)
by_firm_aida_R <- as.numeric(by_firm_aida$Revenue)
by_firm_aida_growth <- c()


for (i in 2:length(by_firm_aida_ID)) { # much faster computation
  if (by_firm_aida_ID[i] != by_firm_aida_ID[i-1]) next
  if (by_firm_aida_year[i] - by_firm_aida_year[i-1] != 1) next
  by_firm_aida_growth[i] <- log(by_firm_aida_R[i]/by_firm_aida_R[i-1])
}

by_firm_aida["Growth"]<-by_firm_aida_growth


by_firm_aida <- by_firm_aida[sample(nrow(by_firm_aida)),] #shuffle

#########################################################################################
restaurants = subset(by_firm_aida,Ateco>=550000 & Ateco<570000) #restaurants (alloggio e ristoranti)

media = subset(by_firm_aida,Ateco>=580000 & aida$Ateco<640000) #media
#########################################################################################

aida_growth <- by_firm_aida$Growth[!is.na(by_firm_aida$Growth)]


sample_size <- 10000

growth_aida <- sample(aida_growth, sample_size)

fit_norm <- fitdist(growth_aida, distr="norm", method="mle")
fit_cauchy <- fitdist(growth_aida, distr="cauchy", method="mle")
fit_laplace1 <-fitdist(growth_aida, distr = "laplace",  method = "mle", start=list(location=-0.1, scale=0.001))

est1 <- c()
est2 <- c()

for (i in 1:1000) {
  fnorm<-fitdist(sample(aida_growth, sample_size), method="mle", distr = "norm")
  est1[i] <- fnorm$estimate[1]
  est2[i] <- fnorm$estimate[2]
}


plotConfidInterv(est2, fit_norm$estimate[2], title="Std dev distribution of a thousand 500k samples", xlb="Standard deviation")

#growth_random_aida <- growth_random_aida[growth_random_aida != 0]
mean(growth_aida)
length(growth_aida)
shifted<- growth_aida +10 # to fit distributions that require values to be positive
min(shifted)
scaled <- (growth_aida - min(growth_aida) +0.000001) /(max(growth_aida) - min(growth_aida)+ 0.000002)
#fit distributions that require values to be within ]0, 1[

LL2 <- function(m, s) { 
  -sum(dlaplace(growth_aida, m, s, log=TRUE)) 
  }


LL3 <- function(m, s) {
  -sum(dpareto(shifted, m, s, log=TRUE)) # for dpareto, the first parameter is alpha, the second is xmin
}

#Powerlaw fit using the PowerLaw library
# m_m2 = conpl$new(shifted)
# est = estimate_xmin(m_m)
# m_m2$setXmin(coef(fit_pareto)[2])
# bs_p2 <- bootstrap_p(m_m2, no_of_sims=100, threads=8 )
# bs_p$p

#Get an idea of the possible distribution for the empirical data
descdist(growth_aida, discrete = FALSE)

#################################################################################################
#Fit distributions using fitdist and mle2
#norm, exp, gamma, beta, weibull, cauchy, pareto, logis, lnorm, laplace

#fit_laplace2 <- mle2(LL2, start=list(m=-0.1, s=0.001), method = "L-BFGS-B", lower = c(m=-0.1, s=0.001))
fit_logis <-fitdist(growth_aida, distr="logis", method="mle")
fit_lnorm <- fitdist(shifted, distr="lnorm", method="mle")
fit_pareto <- mle2(LL3, start = list(m = 0.001, s = min(shifted)), method = "L-BFGS-B", lower = c(m=0.001), fixed = list(s=(min(shifted))))
fit_exp <- fitdist(shifted, distr="exp", method="mle")
fit_exp <- fitdist(shifted, distr="exp", method="mle")
fit_weib <- fitdist(shifted, distr="weibull", method="mle")
fit_gamma <- fitdist(shifted, distr="gamma", method="mle")
fit_beta <- fitdist(scaled, distr="beta", method="mle")

##################################################################################################

#Compute confidence intervals for the estimated parameters using boostrap

# For mle2 objects:
confint(fit_laplace2) # gives the confidence intervals of the estimated parameters (default is 95% c.l.)
plot(profile(fit_laplace2)) # plots profile (confidence intervals) of the parameters


# For fitdistr objects:
boot <- bootdist(fit_cauchy, bootmethod = "param", niter = 1000) #uses parametric bootsrap to generate 
# 1000 samples and compute their parameters according to the given distribution

plotConfidInterv(boot$estim[,1], fit_cauchy$estimate[1], title= "Distribution of Location parameter", xlb= "Bootstraped Location")
plotConfidInterv(boot$estim[,2], fit_cauchy$estimate[2], title= "Distribution of Scale parameter", xlb= "Bootstraped Scale")
boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters

##################################################################################################
summary(fit_norm)
summary(fit_cauchy)
summary(fit_laplace1)
summary(fit_laplace2)
summary(fit_logis)
summary(fit_lnorm)
summary(fit_pareto)
summary(fit_exp)
summary(fit_weib)
summary(fit_gamma)
summary(fit_beta)

#KS test

ks.test(growth_aida, "pcauchy", fit_cauchy$estimate[1], fit_cauchy$estimate[2])
ks.test(growth_random_manufacturing, "plaplace", fit_laplace1$estimate[1], fit_laplace1$estimate[2])
#ks.test(growth_random_manufacturing, "plaplace", coef(fit_laplace2)[1], coef(fit_laplace2)[2])
ks.test(growth_aida, "plogis",fit_logis$estimate[1], fit_logis$estimate[2])
ks.test(growth_aida, "pnorm", fit_norm$estimate[1], fit_norm$estimate[2])
ks.test(shifted, "plnorm", fit_lnorm$estimate[1] , fit_lnorm$estimate[2])
ks.test(shifted, "ppareto", coef(fit_pareto)[1], coef(fit_pareto)[2])
ks.test(shifted, "pexp", fit_exp$estimate[1])
ks.test(shifted, "pweibull", fit_weib$estimate[1], fit_weib$estimate[2])
ks.test(shifted, "pgamma", fit_gamma$estimate[1], fit_gamma$estimate[2])
ks.test(scaled, "pbeta",fit_beta$estimate[1], fit_beta$estimate[2])


#############################################################################

# Generates 1000 ks test statistics using parametric bootstrap and returns 
# a p-value based on a logspline distribution fit
n.sims <- 1000
stats <- replicate(n.sims, {
  r <- rcauchy(length(growth_aida)
               , fit_cauchy$estimate[1]
               , fit_cauchy$estimate[2])
  as.numeric(ks.test(r
                     , "pcauchy"
                     , fit_cauchy$estimate[1]
                     , fit_cauchy$estimate[2])$statistic)})

fit_log <- logspline(stats)

1-plogspline(ks.test(growth_aida
                     , "pcauchy"
                     , fit_cauchy$estimate[1]
                     , fit_cauchy$estimate[2])$statistic
             , fit_log
)
##############################################################################

# Take a look at the AICs and BICs among other gof statistics
gof_original <- gofstat(list(fit_norm, fit_cauchy,fit_logis, fit_laplace1))
gof_shifted <- gofstat(list(fit_lnorm, fit_exp, fit_weib, fit_gamma))
gof_scaled <-gofstat(fit_beta)
gof_laplace<-c(AIC(fit_laplace), BIC(fit_laplace)) # BIC is NA. It can be retrieved
gof_pareto<-c(AIC(fit_pareto), BIC(fit_pareto)) # using mle of {stats4} instead of mle2 


gof_original
gof_original$chisqpvalue # returns the Chi-square p-values for the given fits. (They are surprisingly low) 
gof_laplace
gof_pareto
gof_shifted
gof_scaled

skewness(mfc_growth)
#####################################################################################

#Chi-square test for goodness of fit
seqlast <- function (from, to, by) 
{
  vec <- do.call(what = seq, args = list(from, to, by))
  if ( tail(vec, 1) != to ) {
    return(c(vec, to))
  } else {
    return(vec)
  }
}

breaks <- -Inf
breaks <- append (breaks, c(seqlast(min(growth_aida),max(growth_aida),0.2), Inf))
growth_aida.cut<-cut(growth_aida,breaks=breaks, include.lowest = TRUE) #binning data
#table(growth_aida.cut) # frequencies of empirical data in the respective bins
p<-c()
for (i in 1:(length(breaks)-1)) {
  p[i] <- pcauchy(breaks[i+1],fit_cauchy$estimate[1], fit_cauchy$estimate[2])-pcauchy(breaks[i],fit_cauchy$estimate[1], fit_cauchy$estimate[2])
}

f.os<-c()
for (j in 1:(length(breaks)-1)) {
  f.os[j]<- table(growth_aida.cut)[[j]]
  }

chisq.test(x=f.os, p=p)# p-value is very low, probably due to binning choice
chisq.test(x=f.os, simulate.p.value = T) # test with Monte Carlo simulated p-values
#################################################################################




#################################################################################

#Visualise the obtained distributions
x<-aida_growth
skewness(aida_growth)
mean(aida_growth)


x<-growth_aida #(normal, cauchy, laplace, logis)

x<-shifted #(lnorm, pareto, exp, weib, gamma)
x<-scaled #(beta)

par(mfrow=c(1,1))

d <- density(x)
plot(d, main="KDE for the AIDA growth rate distribution", xlab="Growth rate",  xlim=c(-10, 10))
grid(5,5)
plot(d, add=TRUE, main="KDE for the AIDA growth rate distribution", xlab="Growth rate",  xlim=c(-10, 10))
polygon(d, col=alpha("red", 0.3), border="black")


hist(x, prob=T, breaks=c(seqlast(min(aida_growth),max(aida_growth),0.2)), xlab="Growth rate", xlim=c(-10, 10),col="light grey", main="Histogram of AIDA growth rate distribution")

grid(6,6)
hist(x, add = TRUE,  prob=T, breaks=c(seqlast(min(aida_growth),max(aida_growth),0.2)),xlim=c(-10, 10), xlab="Growth rate", col="light grey", main="Empirical growth rate distribution in AIDA")
curve(dnorm(x, fit_norm$estimate[1], fit_norm$estimate[2]), add=T,col = 1, lwd=2)
curve(dcauchy(x, fit_cauchy$estimate[1], fit_cauchy$estimate[2]), add=T,col = 2, lwd=2)
curve(dlaplace(x,  fit_laplace1$estimate[1] , fit_laplace1$estimate[2]), add=T, col=4, lwd=2)
#curve(dlaplace(x,  coef(fit_laplace2)[1] , coef(fit_laplace2)[2]), add=T, col=3, lwd=2)
curve(dlogis(x, fit_logis$estimate[1], fit_logis$estimate[2]), add=T,col = 7, lwd=2)
curve(dlnorm(x, fit_lnorm$estimate[1], fit_lnorm$estimate[2]), add=T,col =6, lwd=2)
curve(dpareto(x,  coef(fit_pareto)[1] , coef(fit_pareto)[2]), add=T,col =7 , lwd=2)
curve(dexp(x, fit_exp$estimate[1]), add=T,col = 8, lwd=2)
curve(dweibull(x, fit_weib$estimate[1], fit_weib$estimate[2]), add=T,col = 9, lwd=2)
curve(dgamma(x, fit_gamma$estimate[1], fit_gamma$estimate[2]), add=T,col = 10, lwd=2)
curve(dbeta(x, fit_beta$estimate[1], fit_beta$estimate[2]), add=T,col = 11, lwd=2)
legend("topright", c("Normal", "Cauchy", "Laplace", "Logistic"), col=c(1,2,4,7), lwd=3)


# Visualise some Q-Q and P-P plots
plot(fit_cauchy)
?qqcomp
# For fitdistr objects:
# QQ plot
qqcomp(fit_norm, main="Q-Q plot for the Gaussian fit", legendtext = "Gaussian")

ppcomp(fit_norm,main="P-P plot for the Gaussian fit", legendtext = "Gaussian")

# PP plot
qqcomp(fit_cauchy, main="Q-Q plot for the Cauchy fit", legendtext = "Cauchy")
ppcomp(fit_cauchy, main="P-P plot for the Cauchy fit", legendtext = "Cauchy")


#For other objects:
#QQ plot
lengr <- length(growth_aida)
y <- rlaplace(lengr, coef(fit_laplace2)[1], coef(fit_laplace2)[2])
qqplot(y, growth_aida, xlab="Theoretical Quantiles", ylab = "Empirical Quantiles")
qqline(rlaplace(lengr), col = 2,lwd=2,lty=2, distribution = qlaplace)
#qnorm(0.5) # for a given surface under the std normal dist (50% in this case) give me the corresponding z score

#CDF plot:
#lines(ecdf(rcauchy(lengr, fit_cauchy$estimate[1], fit_cauchy$estimate[2])), col="blue", cex=0.5, xlim=c(-1,1))
#plot(ecdf(rlaplace(lengr,  0.0315464, 0.0571342)), col="blue", cex=0.5)
plot(ecdf(growth_aida),lwd=4, col="blue", ylab="Probability", xlab="Data", main="Cumulative distribution function")
grid(5,5)
plot(ecdf(growth_aida),cex=4, col="blue", add=T, ylab="Probability", xlab="Data", main="Cumulative distribution function")
curve(pcauchy(x,fit_cauchy$estimate[1], fit_cauchy$estimate[2]), add = T , lwd=2, col="red")
curve(plaplace(x,fit_norm$estimate[1], fit_norm$estimate[2]), add = T , lwd=2, col="black")
curve(plaplace(x,fit_laplace1$estimate[1], fit_laplace1$estimate[2]), add = T , cex=0.5, col="blue")
legend("bottomright", c("Empirical","Normal", "Laplace"), col=c("blue","black","red"), lwd=2)
###########################################################################################################



#nonparametric bootstrap example
samplemean <- function(x, d) {
  return((mean(x[d])))
}

nd = rnorm(10000,2,3)
mean(nd)
samplemean(nd, 1:10000) # estimator
b = boot(nd, samplemean, R=10000, sim="ordinary")

#bootstrap p-value approximation, in case of a symmetric distribution
mean(abs(b$t) >= abs(b$t0))

#bootstrap confidence interval
quantile(b$t, c(0.025, 0.975))




#parametric bootstrap example
samplemean <- function(x, d) {
  return((mean(x[d])))
}


nd = rnorm(10000,2,3)
fit_nd <- fitdist(nd, distr = "norm", method = "mle")
est<-as.numeric(fit_nd$estimate)

samplemean(nd, 1:5000) # estimator

rg <- function(nd, mle) {
  out <- rnorm(length(nd), mle[1], mle[2])
  out
}

b = boot(nd, samplemean, R=10000, sim="parametric", ran.gen = rg , mle=est)
plot(b)
#bootstrap p-value approximation
mean(abs(b$t - b$t0) > abs(b$t0 - 1.99))

#bootstrap confidence interval
quantile(b$t, c(0.025, 0.975))


# rw.small <- rweibull(5,shape=1.5, scale=1)
# xs <- seq(10, 20, len=10)
# 
# 
# boot.pdf <- sapply(1:2, function(i) {
#   xi <- sample(rw.small, size=length(rw.small), replace=TRUE)
#   MLE.est <- suppressWarnings(fitdist(xi, distr="weibull"))  
#   dweibull(xs, shape=MLE.est$estimate["shape"],  scale = MLE.est$estimate["scale"])
# }
# )
# boot.pdf
# dweibull(xs, shape=1.5,  scale = 1)
# quants <- apply(boot.pdf, 1, quantile, c(0.025, 0.5, 0.975))
# quants
#the three lower, middle and upper quantiles are computed for every row generated. This is done because
#the quantiles should also have a weib distribution. Basically what we are doing is extracting the lowest,
#middle and highest values for every row and when they are put together respectively we get a weib dist in each case.

