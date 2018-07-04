library(dplyr)
library(stats4)
library(fitdistrplus) # macimum likelihood fitting (excluding laplace dist) 
library(bbmle) # maximum likelihood fitting with laplace distribution
library(rmutil) # needed for dlaplace
library(logspline)
library(BSDA)
library(ggplot2)
library(poweRlaw)
library(boot)
library(lawstat)
data<-as.data.frame(AIDA, header=T)
#summary(data)
attach(data) 
aida<-subset(data, Year>=2007 & Year<=2015 & R>=0 & !is.na(E))
aida <- aida[, -c(3,4,8,9)]
random_TaxID_aida<-sample(unique(aida$TaxID), 10000)
random_firms_aida <- filter(aida, aida$TaxID %in% random_TaxID_aida)

manufacturing<-subset(data,Ateco>= 101100 & Ateco<=332009 & Year>=2007 & Year<=2015 & R>=0 & !is.na(E))
manufacturing <- manufacturing[, -c(3,4,8,9)]
detach(data)
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


# Adjust for inflation

applyInflation <- function(data, inflations=c(1, .032, .007, .016, .027, .03, .011, .002, -0.001, -0.001, .011)) {
  for (i in seq(2,9)) {
    inflations[i]<-(inflations[i-1]*(1+inflations[i]))
    print(inflations[i])
    data$Infl[data$Year==2006+i]<-inflations[i]
  }
  data$P <- data$P/data$Infl
  data$R <- data$R/data$Infl
  data$E <- data$E/data$Infl
  return(data)
}
manufactuing <- applyInflation(manufacturing)

manufacturing$R[manufacturing$R <=1] <-1
random_firms_aida$R[random_firms_aida$R <=1] <-1

View(manufacturing)

small_firms <- subset(manufacturing, manufacturing$Size == "Small")
medium_firms <- subset(manufacturing, manufacturing$Size == "Medium")
large_firms <- subset(manufacturing, manufacturing$Size == "Large")


#View(nine_year_firms)

#tobacco_sector <- subset(nine_year_firms, Ateco == 120000)
# year_average <- group_by (tobacco_sector, Year)
# year_average <- summarize(year_average, tot= sum(log10(R)/n()))
# View(year_average)

by_random_firms_aida <- group_by(random_firms_aida,TaxID, Year)
by_random_firms_aida <- summarize(by_random_firms_aida, Revenue=sum(R))


by_manufacturing <- group_by(manufacturing,TaxID, Year, SubSector, Region, GeoArea, Size)
by_manufacturing <- summarize(by_manufacturing, Revenue=sum(R))

by_firm_small <- group_by(small_firms,TaxID, Year, SubSector, Region, GeoArea, Size)
by_firm_small <- summarize(by_firm_small, Revenue=sum(R))

by_firm_medium <- group_by(medium_firms,TaxID, Year, SubSector,Region, GeoArea, Size)
by_firm_medium <- summarize(by_firm_medium, Revenue=sum(R))

by_firm_large <- group_by(large_firms,TaxID, Year, SubSector,Region, GeoArea, Size)
by_firm_large <- summarize(by_firm_large, Revenue=sum(R))

by_random_firms_aida$Growth <- NA
by_manufacturing$Growth <- NA
by_firm_small$Growth<- NA
by_firm_medium$Growth<- NA
by_firm_large$Growth<- NA

# manufacturing$Growth[large_firm_growth$TaxID == manufacturing$TaxID & large_firm_growth$Year == manufacturing$Year] <-large_firm_growth$Growth
# manufacturing$Growth[medium_firm_growth$TaxID == manufacturing$TaxID & medium_firm_growth$Year == manufacturing$Year] <-medium_firm_growth$Growth
# manufacturing$Growth[small_firm_growth$TaxID == manufacturing$TaxID & small_firm_growth$Year == manufacturing$Year] <-small_firm_growth$Growth

for (row in 2:nrow(by_random_firms_aida)) {
  if (by_random_firms_aida[row, "TaxID"] != by_random_firms_aida[row-1, "TaxID"]) next
  if ((by_random_firms_aida[row, "Year"] - by_random_firms_aida[row-1, "Year"]) != 1) next
  #random_group_by$Growth[row] <- log10(random_group_by[row, "Revenue"]) - log10(random_group_by[row-1, "Revenue"])
  by_random_firms_aida$Growth[row] <- log10(by_random_firms_aida[row, "Revenue"]/by_random_firms_aida[row-1, "Revenue"])
  #by_firm$growth_medium[row] <- (log10(by_firm[row, "Revenue"]) - year_average$tot[year_average$Year==as.numeric(by_firm[row, "Year"])]) - (log10(by_firm[row-1, "Revenue"]) - year_average$tot[year_average$Year==as.numeric(by_firm[row-1, "Year"])])
}
by_random_firms_aida <- by_random_firms_aida[sample(nrow(by_random_firms_aida)),]
View(by_random_firms_aida)
#test_df <- merge(x = manufacturing, y = gmf, by = c("TaxID", "Year"), all.x = TRUE, sort = F)
#write.csv(test_df, "manufacturing_growth.csv", row.names = F)
growth_manufacturing <-as.data.frame(manufacturing_growth)

growth_large_firms <- as.data.frame(large_firm_growth)

growth_medium_firms <- as.data.frame(medium_firm_growth)

growth_small_firms <- as.data.frame(small_firm_growth)


random_sample_man<- growth_manufacturing[sample(nrow(growth_manufacturing), 500), ]
random_sample_aida <-by_random_firms_aida[sample(nrow(by_random_firms_aida), 400), ]
#View(random_sample)
#growth_large <- growth_large_firms$Growth[!is.na(growth_large_firms$Growth) & growth_large_firms$Year==2008]
#growth_medium<- growth_medium_firms$Growth[!is.na(growth_medium_firms$Growth) & growth_medium_firms$Year==2008 & growth_medium_firms$SubSector=="Alimentare"]
#growth_small <- growth_small_firms$Growth[!is.na(growth_small_firms$Growth) & growth_small_firms$Year==2008 & growth_small_firms$SubSector=="Metallo"]
#growth_random_man <- random_sample_man$Growth[!is.na(random_sample_man$Growth)]

growth_random <- random_sample_aida$Growth[!is.na(random_sample_aida$Growth)]
growth_random <- as.numeric(growth_random)
mean(growth_random)
length(growth_random)
shifted<- growth_random +10 # to fit distributions that require values to be

scaled <- (growth_random - min(growth_random) +0.000001) /(max(growth_random) - min(growth_random)+ 0.000002)
#fit distributions that require values to be within ]0, 1[

LL2 <- function(m, s) { 
  -sum(dlaplace(growth_random, m, s, log=TRUE)) 
  }


LL3 <- function(m, s) {
  -sum(dpareto(shifted, m, s, log=TRUE)) # for dpareto, the first parameter is alpha, the second is xmin
}

#Powerlaw fit using the PowerLaw library
m_m2 = conpl$new(shifted)
est = estimate_xmin(m_m)
m_m2$setXmin(coef(fit_pareto1)[2])
bs_p2 <- bootstrap_p(m_m2, no_of_sims=100, threads=8 )
bs_p$p

#Get an idea of the possible distribution for the empirical data
descdist(growth_random, discrete = FALSE)

#################################################################################################
#Fit distributions using fitdist and mle2
#norm, exp, gamma, beta, weibull, cauchy, pareto, logis, lnorm, laplace
fit_norm <- fitdist(growth_random, distr="norm", method="mle")
fit_cauchy <- fitdist(growth_random, distr="cauchy", method="mle")
fit_laplace<-mle2(LL2, start=list(m=1, s=0.001), method = "L-BFGS-B", lower = c(m=-0.1, s=0.001))
fit_logis <-fitdist(growth_random, distr="logis", method="mle")
fit_lnorm <- fitdist(shifted, distr="lnorm", method="mle")
fit_pareto1 <- mle2(LL3, start=list(m=0.001, s=1.001), method = "L-BFGS-B", lower = c(m=0.001, s=1.001))
fit_exp <- fitdist(shifted, distr="exp", method="mle")
fit_weib <- fitdist(shifted, distr="weibull", method="mle")
fit_gamma <- fitdist(shifted, distr="gamma", method="mle")
fit_beta <- fitdist(scaled, distr="beta", method="mle")

##################################################################################################

#Compute confidence intervals for the estimated parameters using boostrap

# For mle2 objects:
confint(fit_laplace) # gives the confidence intervals of the estimated parameters (default is 95% c.l.)
plot(profile(fit_laplace)) # plots profile (confidence intervals) of the parameters


# For fitdistr objects:
boot <- bootdist(fit_cauchy, bootmethod = "param", niter = 1000) #uses parametric bootsrap to generate 
# 1000 samples and compute their parameters according to the given distribution
boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters

##################################################################################################
summary(fit_norm)
summary(fit_cauchy)
summary(fit_laplace)
summary(fit_logis)
summary(fit_lnorm)
summary(fit_pareto1)
summary(fit_exp)
summary(fit_weib)
summary(fit_gamma)
summary(fit_beta)

#KS test
ks.test(growth_random, "pnorm", fit_norm$estimate[1], fit_norm$estimate[2])
ks.test(growth_random, "pcauchy", fit_cauchy$estimate[1], fit_cauchy$estimate[2])
ks.test(growth_random, "plaplace", coef(fit_laplace)[1] , coef(fit_laplace)[2])
ks.test(growth_random, "plogis",fit_logis$estimate[1], fit_logis$estimate[2])
ks.test(shifted, "plnorm", fit_lnorm$estimate[1] , fit_lnorm$estimate[2])
ks.test(shifted, "ppareto", coef(fit_pareto1)[1], coef(fit_pareto1)[2])
ks.test(shifted, "pexp", fit_exp$estimate[1])
ks.test(shifted, "pweibull", fit_weib$estimate[1], fit_weib$estimate[2])
ks.test(shifted, "pgamma", fit_gamma$estimate[1], fit_gamma$estimate[2])
ks.test(scaled, "pbeta",fit_beta$estimate[1], fit_beta$estimate[2])


#############################################################################

# Generates 1000 ks test statistics using parametric bootstrap and returns 
# a p-value based on a logspline distribution fit
n.sims <- 1000
stats <- replicate(n.sims, {
  r <- rcauchy(length(growth_random)
               , fit_cauchy$estimate[1]
               , fit_cauchy$estimate[2])
  as.numeric(ks.test(r
                     , "pcauchy"
                     , fit_cauchy$estimate[1]
                     , fit_cauchy$estimate[2])$statistic)})

fit_log <- logspline(stats)

1-plogspline(ks.test(growth_random
                     , "pcauchy"
                     , fit_cauchy$estimate[1]
                     , fit_cauchy$estimate[2])$statistic
             , fit_log
)
##############################################################################

# Take a look at the AICs and BICs among other gof statistics
gof_original <- gofstat(list(fit_norm, fit_cauchy,fit_logis))
gof_shifted <- gofstat(list(fit_lnorm, fit_exp, fit_weib, fit_gamma))
gof_scaled <-gofstat(fit_beta)
gof_laplace<-c(AIC(fit_laplace), BIC(fit_laplace)) # BIC is NA. It can be retrieved
gof_pareto1<-c(AIC(fit_pareto1), BIC(fit_pareto1)) # using mle of {stats4} instead of mle2 


gof_original
gof_original$chisqpvalue # returns the Chi-square p-values for the given fits. (They are surprisingly low) 
gof_laplace
gof_pareto1
gof_shifted
gof_scaled


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
breaks <- append (breaks, c(seqlast(min(growth_random),max(growth_random),0.2), Inf))
growth_random.cut<-cut(growth_random,breaks=breaks, include.lowest = TRUE) #binning data
#table(growth_random.cut) # frequencies of empirical data in the respective bins
p<-c()
for (i in 1:(length(breaks)-1)) {
  p[i] <- pcauchy(bins[i+1],fit_cauchy$estimate[1], fit_cauchy$estimate[2])-pcauchy(bins[i],fit_cauchy$estimate[1], fit_cauchy$estimate[2])
}

f.os<-c()
for (j in 1:(length(breaks)-1)) {
  f.os[j]<- table(growth_random.cut)[[j]]
  }

chisq.test(x=f.os, p=p)# p-value is very low, probably due to binning choice
chisq.test(x=f.os, simulate.p.value = T) # test with Monte Carlo simulated p-values
#################################################################################




#################################################################################

#Visualise the obtained distributions

x<-growth_random #(normal, cauchy, laplace, logis)
x<-shifted #(lnorm, pareto, exp, weib, gamma)
x<-scaled #(beta)

par(mfrow=c(1,1))

hist(x, prob=T, breaks="fd", xlab="growth_random rate",xlim = c(-3, 4), col="grey", main="Empirical small growth Rate Distribution")

curve(dnorm(x, fit_norm$estimate[1], fit_norm$estimate[2]), add=T,col = 1, lwd=2)
curve(dcauchy(x, fit_cauchy$estimate[1], fit_cauchy$estimate[2]), add=T,col = 2, lwd=2)
curve(dlaplace(x,  coef(fit_laplace)[1] , coef(fit_laplace)[2]), add=T, col=3, lwd=2)
curve(dlogis(x, fit_logis$estimate[1], fit_logis$estimate[2]), add=T,col = 4, lwd=2)
curve(dlnorm(x, fit_lnorm$estimate[1], fit_lnorm$estimate[2]), add=T,col = 5, lwd=2)
curve(dpareto(x,  coef(fit_pareto1)[1] , coef(fit_pareto2)[2]), add=T,col =6 , lwd=2)
curve(dexp(x, fit_exp$estimate[1]), add=T,col = 7, lwd=2)
curve(dweibull(x, fit_weib$estimate[1], fit_weib$estimate[2]), add=T,col = 8, lwd=2)
curve(dgamma(x, fit_gamma$estimate[1], fit_gamma$estimate[2]), add=T,col = 9, lwd=2)
curve(dbeta(x, fit_beta$estimate[1], fit_beta$estimate[2]), add=T,col = 10, lwd=2)
legend("topright", c("Normal fit", "Cauchy fit", "Laplace fit"), col=c(1,2,3), lwd=3)


# Visualise some Q-Q and P-P plots

# For fitdistr objects:
# QQ plot
qqcomp(fit_norm)
ppcomp(fit_norm)

# PP plot
qqcomp(fit_cauchy)
ppcomp(fit_cauchy)


#For other objects:
#QQ plot
lengr <- length(growth_random)
y <- rlaplace(lengr, coef(fit_laplace)[1], coef(fit_laplace)[2])
qqplot(y, growth_random, xlab="Theoretical Quantiles", ylab = "Empirical Quantiles")
qqline(rlaplace(lengr), col = 2,lwd=2,lty=2, distribution = qlaplace)
#qnorm(0.5) # for a given surface under the std normal dist (50% in this case) give me the corresponding z score

#CDF plot:
#lines(ecdf(rcauchy(lengr, fit_cauchy$estimate[1], fit_cauchy$estimate[2])), col="blue", cex=0.5, xlim=c(-1,1))
#plot(ecdf(rlaplace(lengr,  0.0315464, 0.0571342)), col="blue", cex=0.5)
plot(ecdf(growth_random),cex=0.5, col="red")
curve(pcauchy(x,fit_cauchy$estimate[1], fit_cauchy$estimate[2]), add = T , cex=0.5, col="blue")
curve(plaplace(x,coef(fit_laplace)[1], coef(fit_laplace)[2]), add = T ,  cex=0.5,col="green")

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

