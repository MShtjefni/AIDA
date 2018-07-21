sample_size <- 800
length(mfc_growth)
mean (mfc_growth)
by_firm_manufacturing_ID <- as.numeric(by_firm_manufacturing$TaxID)
by_firm_manufacturing_year <- as.numeric(by_firm_manufacturing$Year)
by_firm_manufacturing_R <- as.numeric(by_firm_manufacturing$Revenue)
by_firm_manufacturing_growth <- c()


for (i in 2:length(by_firm_manufacturing_ID)) { 
  if (by_firm_manufacturing_ID[i] != by_firm_manufacturing_ID[i-1]) next
  if (by_firm_manufacturing_year[i] - by_firm_manufacturing_year[i-1] != 1) next
  by_firm_manufacturing_growth[i] <- log(by_firm_manufacturing_R[i]/by_firm_manufacturing_R[i-1])
}

by_firm_manufacturing["Growth"]<-by_firm_manufacturing_growth
 

manufacturing_growth <-by_firm_manufacturing

manufacturing_2008 <- subset(manufacturing_growth, manufacturing_growth$Year=="2008" & !is.na(manufacturing_growth$Growth))

manufacturing_2009 <- subset(manufacturing_growth, manufacturing_growth$Year=="2009" & !is.na(manufacturing_growth$Growth))

manufacturing_2010 <- subset(manufacturing_growth, manufacturing_growth$Year=="2010" & !is.na(manufacturing_growth$Growth))

manufacturing_2011 <- subset(manufacturing_growth, manufacturing_growth$Year=="2011" & !is.na(manufacturing_growth$Growth))

manufacturing_2012 <- subset(manufacturing_growth, manufacturing_growth$Year=="2012" & !is.na(manufacturing_growth$Growth))

manufacturing_2013 <- subset(manufacturing_growth, manufacturing_growth$Year=="2013" & !is.na(manufacturing_growth$Growth))

manufacturing_2014 <- subset(manufacturing_growth, manufacturing_growth$Year=="2014" & !is.na(manufacturing_growth$Growth))

manufacturing_2015 <- subset(manufacturing_growth, manufacturing_growth$Year=="2015" & !is.na(manufacturing_growth$Growth))

manufacturing_growth <- manufacturing_growth[sample(nrow(manufacturing_growth)),] #shuffle


#########################################################################################################
odd_years <- subset(by_firm_manufacturing, by_firm_manufacturing$Year %% 2 ==1)
odd_years <- odd_years[, -c(8)]

odd_years_ID <- as.numeric(odd_years$TaxID)
odd_years_year <- as.numeric(odd_years$Year)
odd_years_R <- as.numeric(odd_years$Revenue)
odd_years_growth <- c()

for (i in 2:length(odd_years_ID)) { 
  if (odd_years_ID[i] != odd_years_ID[i-1]) next
  if (odd_years_year[i] - odd_years_year[i-1] != 2) next
  odd_years_growth[i] <- log(odd_years_R[i]/odd_years_R[i-1])
}


############################################################################################################


even_years <- subset(by_firm_manufacturing, by_firm_manufacturing$Year %% 2 ==0)
even_years <- even_years[, -c(8)]


even_years_ID <- as.numeric(even_years$TaxID)
even_years_year <- as.numeric(even_years$Year)
even_years_R <- as.numeric(even_years$Revenue)
even_years_growth <- c()

for (i in 2:length(even_years_ID)) { 
  if (even_years_ID[i] != even_years_ID[i-1]) next
  if (even_years_year[i] - even_years_year[i-1] != 2) next
  even_years_growth[i] <- log(even_years_R[i]/even_years_R[i-1])
}



biannual_growth <- c(odd_years_growth,even_years_growth)
biannual_growth <- biannual_growth[!is.na(biannual_growth)]

biannual_growth <- sample(biannual_growth) #shuffle
length(biannual_growth)
growth_biannual <- sample(biannual_growth, sample_size)

############################################################################################################


############################################################################################################
quinquennal <- subset(by_firm_manufacturing, by_firm_manufacturing$Year ==2010 | by_firm_manufacturing$Year ==2015)
quinquennal <- quinquennal[, -c(8)]


quinquennal_ID <- as.numeric(quinquennal$TaxID)
quinquennal_year <- as.numeric(quinquennal$Year)
quinquennal_R <- as.numeric(quinquennal$Revenue)
quinquennal_growth_10 <- c()

for (i in 2:length(quinquennal_ID)) { 
  if (quinquennal_ID[i] != quinquennal_ID[i-1]) next
  quinquennal_growth_10[i] <- log(quinquennal_R[i]/quinquennal_R[i-1])
}

for (i in 1:(nrow(quinquennal) - length(quinquennal_growth_10))) {
  quinquennal_growth_10 <- append(quinquennal_growth_10, NA)
}

#quinquennalgrowth7
#quinquennalgrowth8
#quinquennalgrowth9
#quinquennalgrowth10


quinquennal_growth_7_8_9_10 <- c(quinquennal_growth_7,quinquennal_growth_8,quinquennal_growth_9,quinquennal_growth_10)

write.csv(quinquennal_growth_7_8_9_10, "quinquennal.csv", row.names = F)

quinquennal_growth<-quinquennal_growth_7_8_9_10[!is.na(quinquennal_growth_7_8_9_10)]

length(quinquennal_growth_7_8_9_10)
quinquennal_growth <- sample(quinquennal_growth) #shuffle

growth_quinquennal <- sample(quinquennal_growth, sample_size)


############################################################################################################


############################################################################################################

lag_years <- subset(by_firm_manufacturing, by_firm_manufacturing$Year ==2007 | by_firm_manufacturing$Year ==2015)
lag_years <- lag_years[, -c(8)]

lag_years_ID <- as.numeric(lag_years$TaxID)
lag_years_year <- as.numeric(lag_years$Year)
lag_years_R <- as.numeric(lag_years$Revenue)
lag_years_growth <- c()

for (i in 2:length(lag_years_ID)) { 
  if (lag_years_ID[i] != lag_years_ID[i-1]) next
  lag_years_growth[i] <- log(lag_years_R[i]/lag_years_R[i-1])
}

for (i in 1:(nrow(lag_years) - length(lag_years_growth))) {
  lag_years_growth <- append(lag_years_growth, NA)
}

lag_years["Growth"]<-lag_years_growth

lag_years <- lag_years[sample(nrow(lag_years)),] #shuffle

lag_years_growth <- lag_years$Growth[!is.na(lag_years$Growth)]

growth_lag_years <- sample(lag_years_growth, sample_size)

############################################################################################################

mfc_growth <- manufacturing_growth$Growth[!is.na(manufacturing_growth$Growth)]
growth_manufacturing <- sample(mfc_growth, sample_size)

############################################################################################################

growth_2008 <- sample(manufacturing_2008$Growth)
growth_2009 <- sample(manufacturing_2009$Growth)
growth_2010 <- sample(manufacturing_2010$Growth)
growth_2011 <- sample(manufacturing_2011$Growth)
growth_2012 <- sample(manufacturing_2012$Growth)
growth_2013 <- sample(manufacturing_2013$Growth)
growth_2014 <- sample(manufacturing_2014$Growth)
growth_2015 <- sample(manufacturing_2015$Growth)


############################################################################################################
fit_norm <- fitdist(mfc_growth, distr="norm", method="mle")
fit_cauchy_growth_biannual <- fitdist(biannual_growth, distr="cauchy", method="mle")
fit_cauchy_growth_quinquennal <- fitdist(quinquennal_growth, distr="cauchy", method="mle")
fit_cauchy_growth_lag_years <- fitdist(lag_years_growth, distr="cauchy", method="mle")

fit_cauchy_manufacturing <- fitdist(growth_manufacturing, distr="cauchy", method="mle")
fit_laplace1 <-fitdist(mfc_growth, distr = "laplace",  method = "mle", start=list(location=-0.1, scale=0.001))
fit_logis <- fitdist(mfc_growth, distr="logis", method="mle")

est1 <- c()
est2 <- c()

for (i in 1:1000) {
  fcauchy<-fitdist(sample(mfc_growth, sample_size), method="mle", distr = "cauchy")
  est1[i] <- fcauchy$estimate[1]
  est2[i] <- fcauchy$estimate[2]
}


plotConfidInterv(est2, fit_cauchy$estimate[2], xlb="Scale parameter distribution")


############################################################################################################

mean(growth_manufacturing)
length(growth_manufacturing)
shifted<- growth_manufacturing +10 # to fit distributions that require values to be positive
min(shifted)
scaled <- (growth_manufacturing - min(growth_manufacturing) +0.000001) /(max(growth_manufacturing) - min(growth_manufacturing)+ 0.000002)

kurtosis(growth_manufacturing)

skewness(growth_manufacturing) # positive skewness -> the tail on the right side is longer or fatter than the left side


#fit distributions that require values to be within ]0, 1[

LL2 <- function(m, s) {
  -sum(dlaplace(growth_manufacturing, m, s, log=T))
}


LL3 <- function(m, s) {
  -sum(dpareto(shifted ,m, s, log=T)) # for dpareto, the first parameter is alpha, the second is xmin
}
#Powerlaw fit using the PowerLaw library
# m_m = conpl$new(shifted)
# est = estimate_xmin(m_m)
# m_m$setXmin(est)
# bs_p <- bootstrap_p(m_m, no_of_sims=1000, threads=8)
# bs_p$p

#Get an idea of the possible distribution for the empirical data
descdist(growth_manufacturing, discrete = FALSE)
#################################################################################################
#Fit distributions using fitdist and mle2
#norm, exp, gamma, beta, weibull, cauchy, pareto, logis, lnorm, laplace
#fit_laplace2 <- mle2(LL2, start=list(m=-0.1, s=0.001), method = "L-BFGS-B", lower = c(m=-0.1, s=0.001))
fit_lnorm <- fitdist(shifted, distr="lnorm", method="mle")
fit_pareto <- mle2(LL3, start = list(m = 1, s = min(shifted)), method = "L-BFGS-B", lower = c(m=0.001), fixed = list(s=(min(shifted))))
fit_exp <- fitdist(shifted, distr="exp", method="mle")
fit_weib <- fitdist(shifted, distr="weibull", method="mle")
fit_gamma <- fitdist(shifted, distr="gamma", method="mle")
fit_beta <- fitdist(scaled, distr="beta", method="mle")
##################################################################################################

#Compute confidence intervals for the estimated parameters using boostrap
par(mfrow=c(1,1))
# For mle2 objects:
confint(fit_laplace2, method="spline") # gives the confidence intervals of the estimated parameters (default is 95% c.l.)
plot(profile(fit_laplace2)) # plots profile (confidence intervals) of the parameters


# For fitdistr objects:
boot <- bootdist(fit_cauchy_manufacturing, bootmethod = "param", niter = 1000) #uses parametric bootsrap to generate 
# 1000 samples and compute their parameters according to the given fitted distribution
boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters

######################################################################################################################
#GG plot for confidence intervals
plotConfidInterv<-function(data, myValue=F, conf=.05) {
  "plot(density(data))
  abline(v=quantile(data, conf/2), col='red')
  abline(v=quantile(data, 1-(conf/2)), col='red')"
  # subset region and plot
  plt<-ggplot(as.data.frame(data), aes(x=data)) + geom_density(colour = "black", size=0.5) 
  d <- ggplot_build(plt)$data[[1]]
  ypos1 <- d$y[match(x=T, d$x>=quantile(data,conf/2))]
  ypos2 <- d$y[match(x=T, d$x>=quantile(data,1-conf/2))]
  
  plt <- plt + 
    geom_area(data = subset(d, x >= quantile(data,.025) & x <= quantile(data,.975)), aes(x=x, y=y), fill="red", alpha=0.3) +
    geom_segment(aes(x=quantile(data, .025), xend=quantile(data, .025), y=0, yend=ypos1), color="navyblue", size=0.5) + 
    geom_segment(aes(x=quantile(data, .975), xend=quantile(data, .975), y=0, yend=ypos2), color="navyblue", size=0.5)
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

plotConfidInterv2<-function(data, myValue=F, conf=.05, title, xlb) {
  "plot(density(data))
  abline(v=quantile(data, conf/2), col='red')
  abline(v=quantile(data, 1-(conf/2)), col='red')"
  # subset region and plot
  plt<-ggplot(as.data.frame(data), aes(x=data)) + geom_density(colour = "black", size=0.8) + xlab(xlb) + ggtitle(title)
  d <- ggplot_build(plt)$data[[1]]
  ypos1 <- d$y[match(x=T, d$x>=quantile(data,1-conf))]
  
  plt <- plt + 
    geom_area(data = subset(d, x <= quantile(data,.95)), aes(x=x, y=y), fill="red", alpha=0.3) +
    geom_segment(aes(x=quantile(data, .95), xend=quantile(data, .95), y=0, yend=ypos1), color="navyblue", size=0.8)
  if(myValue) {
    myColor='red'
    if (myValue>quantile(data,1-conf))
      myColor='red'
    ypos2 <- d$y[match(x=T, d$x>=myValue)]
    plt <- plt + geom_segment(aes(x=myValue, xend=myValue, y=0, yend=ypos2), color=myColor, size=0.8)
  }
  plt + geom_density(colour = "black", size=0.5)
  #return(d)
}

########################################################################################################################

plotConfidInterv(boot$estim[,1], fit_cauchy_manufacturing$estimate[1])
boot$estim

##################################################################################################
summary(fit_norm)
summary(fit_cauchy_manufacturing)
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
ks.test(mfc_growth, "pnorm", fit_norm$estimate[1], fit_norm$estimate[2])
ks.test(growth_2008, growth_2009)
ks.test(growth_2009, growth_2010)
ks.test(growth_2010, growth_2011)
ks.test(growth_2011, growth_2012)
ks.test(growth_2012, growth_2013)
ks.test(growth_2013, growth_2014)
ks.test(growth_2014, growth_2015)
ks.test(growth_manufacturing, "pcauchy", fit_cauchy_manufacturing$estimate[1], fit_cauchy_manufacturing$estimate[2])
ks.test(mfc_growth, "plaplace", fit_laplace1$estimate[1], fit_laplace1$estimate[2])
#ks.test(growth_manufacturing, "plaplace", coef(fit_laplace2)[1], coef(fit_laplace2)[2])
ks.test(mfc_growth, "plogis",fit_logis$estimate[1], fit_logis$estimate[2])
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
  r <- rcauchy(length(growth_manufacturing)
               , fit_cauchy_manufacturing$estimate[1]
               , fit_cauchy_manufacturing$estimate[2])
  as.numeric(ks.test(r
                     , "pcauchy"
                     , fit_cauchy_manufacturing$estimate[1]
                     , fit_cauchy_manufacturing$estimate[2])$statistic)})

fit_log <- logspline(stats)

1-plogspline(ks.test(growth_manufacturing
                     , "pcauchy"
                     , fit_cauchy_manufacturing$estimate[1]
                     , fit_cauchy_manufacturing$estimate[2])$statistic
             , fit_log
)

plotConfidInterv2(stats,ks.test(growth_manufacturing
                                , "pcauchy"
                                , fit_cauchy_manufacturing$estimate[1]
                                , fit_cauchy_manufacturing$estimate[2])$statistic, title="Log spline density of D-statistic for sample size=1000", xlb="D")
##############################################################################

# Take a look at the AICs and BICs among other gof statistics
gof_original <- gofstat(list(fit_norm, fit_cauchy_manufacturing,fit_logis, fit_laplace1))
gof_shifted <- gofstat(list(fit_lnorm, fit_exp, fit_weib, fit_gamma))
gof_scaled <-gofstat(fit_beta)
gof_laplace<-c(AIC(fit_laplace2), BIC(fit_laplace2)) # BIC is NA. It can be retrieved
gof_pareto<-c(AIC(fit_pareto), BIC(fit_pareto)) # using mle of {stats4} instead of mle2 


gof_original
gof_laplace
gof_pareto
gof_shifted
gof_scaled
gof_original$chisqpvalue # returns the Chi-square p-values for the given fits. (They are surprisingly low) 

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
breaks <- append (breaks, c(seqlast(min(growth_manufacturing),max(growth_manufacturing),0.1), Inf))
#breaks
growth_manufacturing.cut<-cut(growth_manufacturing,breaks=breaks, include.lowest = TRUE) #binning data

#table(growth_manufacturing.cut) # frequencies of empirical data in the respective bins
p<-c()

for (i in 1:(length(breaks)-1)) {
  p[i] <- pcauchy(breaks[i+1],fit_cauchy_manufacturing$estimate[1], fit_cauchy_manufacturing$estimate[2])-pcauchy(breaks[i],fit_cauchy_manufacturing$estimate[1], fit_cauchy_manufacturing$estimate[2])
  }

f.os<-c()
for (j in 1:(length(breaks)-1)) {
  f.os[j]<- table(growth_manufacturing.cut)[[j]]
}

chisq.test(x=f.os, p=p)# p-value is very low, probably due to binning choice
chisq.test(x=f.os, simulate.p.value = T) # test with Monte Carlo simulated p-values
#################################################################################



#################################################################################

#Visualise the obtained distributions

x<-lag_years_growth #(normal, cauchy, laplace, logis)
x<-shifted #(lnorm, pareto, exp, weib, gamma, llogis)
x<-scaled #(beta)

kurtosis(lag_years_growth)
skewness(lag_years_growth)
mean(lag_years_growth)
var(lag_years_growth)

ks.test(quinquennal_growth, lag_years_growth)


hist(x, prob=T, breaks=c(seqlast(min(lag_years_growth),max(lag_years_growth),0.3)),xlim=c(-15, 15), xlab="Growth", col="grey", main="Nine year growth distribution")
grid(5,5)
hist(x, add=T,prob=T, breaks=c(seqlast(min(lag_years_growth),max(lag_years_growth),0.3)), xlim=c(-15,15),xlab="Growth", col="grey", main="Nine year growth distribution")
#curve(dnorm(x, fit_norm$estimate[1], fit_norm$estimate[2]), add=T,col = 1, lwd=2)
#curve(dcauchy(x, fit_cauchy_growth_2015$estimate[1], fit_cauchy_growth_2015$estimate[2]), add=T,col = 2, lwd=2)
curve(dcauchy(x, fit_cauchy_growth_lag_years$estimate[1], fit_cauchy_growth_lag_years$estimate[2]), add=T,col = 2, lwd=2)
curve(dlaplace(x,  fit_laplace1$estimate[1] , fit_laplace1$estimate[2]), add=T, col=4, lwd=2)
#curve(dlaplace(x,  coef(fit_laplace2)[1] , coef(fit_laplace2)[2]), add=T, col=7, lwd=2)
curve(dlogis(x, fit_logis$estimate[1], fit_logis$estimate[2]), add=T,col = 7, lwd=2)
#curve(dllogis(x, fit_llogis$estimate[1], fit_llogis$estimate[2]), add=T,col = 5, lwd=2)
curve(dlnorm(x, fit_lnorm$estimate[1], fit_lnorm$estimate[2]), add=T,col = 6, lwd=2)
curve(dpareto(x,  coef(fit_pareto)[1] , coef(fit_pareto)[2]), add=T,col =7 , lwd=2)
curve(dexp(x, fit_exp$estimate[1]), add=T,col = 8, lwd=2)
curve(dweibull(x, fit_weib$estimate[1], fit_weib$estimate[2]), add=T,col = 9, lwd=2)
curve(dgamma(x, fit_gamma$estimate[1], fit_gamma$estimate[2]), add=T,col = 10, lwd=2)
curve(dbeta(x, fit_beta$estimate[1], fit_beta$estimate[2]), add=T,col = 11, lwd=2)
legend("topright", c("Normal", "Cauchy"), col=c(1,2), lwd=3)


# Visualise some Q-Q and P-P plots

# For fitdistr objects:
# QQ and PP plot
qqcomp(fit_norm)
ppcomp(fit_norm)

qqcomp(fit_logis)
ppcomp(fit_logis)

qqcomp(fit_cauchy_manufacturing)
ppcomp(fit_cauchy_manufacturing)

qqcomp(fit_laplace1)
ppcomp(fit_laplace1)

#For other objects:
#QQ plot
lengr <- length(growth_manufacturing)
y <- rlaplace(lengr, coef(fit_laplace2)[1], coef(fit_laplace2)[2])
qqplot(y, growth_manufacturing, xlab="Theoretical Quantiles", ylab = "Empirical Quantiles")
qqline(rlaplace(lengr), col = 2,lwd=2,lty=2, distribution = qlaplace)
#qnorm(0.5) # for a given surface under the std normal dist (50% in this case) give me the corresponding z score

#CDF plot:
#lines(ecdf(rcauchy(lengr, fit_cauchy_manufacturing$estimate[1], fit_cauchy_manufacturing$estimate[2])), col="blue", cex=0.5, xlim=c(-1,1))
#plot(ecdf(rlaplace(lengr,  0.0315464, 0.0571342)), col="blue", cex=0.5)
plot(ecdf(growth_manufacturing),cex=1, col="red")
curve(pcauchy(x,fit_cauchy_manufacturing$estimate[1], fit_cauchy_manufacturing$estimate[2]), add = T , cex=0.5, col="blue")
curve(plaplace(x,fit_laplace1$estimate[1], fit_laplace1$estimate[2]), add = T , cex=0.5, col="green")
curve(plaplace(x,coef(fit_laplace2)[1], coef(fit_laplace2)[2]), add = T ,  cex=0.5,col="purple")

###########################################################################################################

length(growth_2015)
#Hypothesis testing on the mean
symmetry.test(sample(growth_2008, 16000))
symmetry.test(sample(growth_2009, 16000))
symmetry.test(sample(growth_2010, 16000))
symmetry.test(sample(growth_2011, 16000))
symmetry.test(sample(growth_2012, 16000))
symmetry.test(sample(growth_2013, 16000))
symmetry.test(sample(growth_2014, 16000))
symmetry.test(sample(growth_2015, 16000))
#Test data for normality
qqnorm(growth_manufacturing_2008) #visually the distribution is far from normal
qqline(growth_manufacturing_2008)
shapiro.test(growth_manufacturing) # p-value close to 0 so we reject the null hypothesis

#Test of mean, non-normal data, distribution free but has to be symmetrical:
wilcox.test(growth_manufacturing_2008, mu=0 , conf.int = T) # We reject H0 i.e. the true mean is != 0

#Very large sample size, we can take advantage of the clt by performing a z-test:
 # Again we reject H0
mean(growth_2008)

 # variances are not equal, so we should use the welch t-test
#Let's try the t-test::
t.test(growth_2012, mu= 0) # We reject H0

#Parametric bootstrapping:
samplemean <- function(x, i) {
  return((mean(x[i])))
}

est<-as.numeric(fit_cauchy_manufacturing$estimate)

rg <- function(growth_manufacturing_2008, mle) {
  out <- rcauchy(length(growth_manufacturing_2008), mle[1], mle[2])
  out
}

p_boot = boot(growth_manufacturing_2008, samplemean, R=1000, sim="parametric", ran.gen = rg , mle=est)

quantile(p_boot$t, c(0.025, 0.975)) # we cannot reject the null hypothesis
plot(p_boot)
p_boot$t0

mean(growth_manufacturing_2008)
#bootstrap p-value approximation:
#centered:
mean(abs(p_boot$t - p_boot$t0) >= abs(p_boot$t0 - 0)) # we can see the p-value is very high 

#uncentered (practically the same result):
mean(abs(p_boot$t) >= 0) # we cannot reject H0, so the mean is =0
z.test(growth_2015, sigma.x=sd(growth_2015), mu=0)

#Non-parametric bootstrapping:
samplemean <- function(x, i) {
  return((mean(x[i])))
}

np_boot = boot(growth_2015, samplemean, R=1000, sim="ordinary")
# derive ci
quantile(np_boot$t, c(0.025, 0.975)) # we reject the null hypothesis
plotConfidInterv(np_boot$t, np_boot$t0, title="Bootstrapped mean distribution", xlb = "Mean")
# or like this
boot.ci(np_boot, type = "norm")
#bootstrap p-value approximation
#centered distribution of parameters:
mean(abs(np_boot$t - np_boot$t0) >= abs(np_boot$t0 - 0)) # the p-value is practically 0

###################################################################################################




#Hypothesis testing on two means

#z-test: we reject H0, so the true mean difference is not =0
z.test(growth_2008, sigma.x=sd(growth_2008), growth_2009, sigma.y=sd(growth_2009), mu=0)

#Wilcox test: we reject H0
wilcox.test(growth_manufacturing_2008, growth_manufacturing_2009, mu=0)

#t-test, unequal variances: again we reject H0
t.test(growth_manufacturing_2008, growth_manufacturing_2009, mu=0)


#Non-parametric bootstrapping:
growthDf <- data.frame(growth=c(growth_2014, growth_2015),
                       year=factor(rep(c("2014", "2015"), c(length(growth_2014), length(growth_2015)))))

getMD <- function(dat, idx) {
  MD <- aggregate(growth ~ year, data=dat, subset=idx,FUN=mean)
  -diff(MD$growth)
}

np_boot = boot(growthDf, statistic=getMD,strata=growthDf$year,  R=400)
plot(np_boot)

quantile(np_boot$t, c(0.025, 0.975)) # we reject the null hypothesis, so there is a difference between the two means

#bootstrap p-value approximation
#centered distribution of parameters:
mean(abs(np_boot$t - np_boot$t0) >= abs(np_boot$t0 - 0)) # the p-value is practically 0

plotConfidInterv(np_boot$t, np_boot$t0, title="Bootstrapped mean difference distribution", xlb = "Mean difference")


#Parametric bootstrapping:
#The good old fashioned way
mean_differences <- replicate(1000, {
  r1 <- rcauchy(length(growth_manufacturing_2008)
               , fit_cauchy_manufacturing$estimate[1] # Whatever the estimated cauchy parameters for the 2008 growth are
               , fit_cauchy_manufacturing$estimate[2])
  r2<- rcauchy(length(growth_manufacturing_2009)
               , fit_cauchy_manufacturing$estimate[1]  # Whatever the estimated cauchy parameters for the 2009 growth are
               , fit_cauchy_manufacturing$estimate[2])
  return (mean(r1) - mean(r2))
    })

hist(mean_differences, prob=T, breaks="scott", col="grey", main="Mean difference distribution")

#p-value approximation
mean(abs(mean_differences) >= 0)
####################################################################################################

#A symmetry test
symmetry.test(growth_manufacturing_2008)

###################################################################################################
#Regression

MDf <- merge(x = manufacturing_2008, y = manufacturing_2009, by = "TaxID")
growth_2008 <- MDf$Growth.x
growth_2009 <- MDf$Growth.y


shifted_8<- growth_2008 +10
shifted_9<- growth_2009 +10

fit_growth <- lm(growth_2009 ~ growth_2008)


summary(fit_growth)
plot(fit_growth)

bptest(fit_growth)


#predict(fit, data.frame(x = c(4, 5, 6)))
plot(sorted_growth_2008, sorted_growth_2009, ylab="Growth in 2009", xlab="Growth in 2008", main="Simple linear regression model")

lines(sorted_growth_2008,fit_growth$fitted.values,lwd=1, col="red")

newdata = data.frame(sorted_growth_2008=sorted_growth_2008[420])

predict(fit_growth, newdata, interval="confidence") 
sorted_growth_2009[420]
##################################################################################################
fit_growth$df.residual
length(sorted_growth_2008)
rse <- sqrt( sum(residuals(fit_growth)^2) / fit_growth$df.residual ) 
1-rse
