wdir <- ""
dataDir <- "data/"
packagesFile <- "packages.txt"
source(paste(wdir, "functions.R", sep="")) ### this also loads every needed package
loadDatasets(paste(wdir,dataDir,sep="")) ###USE THIS IF YOU CURRENTLY HAVEN'T DATASETS IN WORKSPACE
source(paste(wdir, "growth_rate_dist.R", sep=""))

if(!"Growth" %in% names(aida))
  aida<-getgrowth

aida_growth <- aida$Growth[!is.na(aida$Growth)]
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


plotConfidInterv(est2, fit_norm$estimate[2], xtitle = "Bootstrapped Standard deviation")

#growth_random_aida <- growth_random_aida[growth_random_aida != 0]
mean(growth_aida)
length(growth_aida)
shifted<- growth_aida + abs(min(growth_aida)) + .000001 # to fit distributions that require values to be positive
min(shifted)
scaled <- (growth_aida - min(growth_aida) +0.000001) /(max(growth_aida) - min(growth_aida)+ 0.000002)
#fit distributions that require values to be within ]0, 1[

#Get an idea of the possible distribution for the empirical data
descdist(growth_aida, discrete = FALSE)

#################################################################################################
#Fit distributions using fitdist and mle2
#norm, exp, gamma, beta, weibull, cauchy, pareto, logis, lnorm, laplace
growth<-growth_aida; fit_laplace2 <- mle2(LL2, start=list(m=-0.1, s=0.001), method = "L-BFGS-B", lower = c(m=-0.1, s=0.001))
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

plotConfidInterv(boot$estim[,1], fit_cauchy$estimate[1], xtitle = "Bootstraped Location")
plotConfidInterv(boot$estim[,2], fit_cauchy$estimate[2], xtitle =  "Bootstraped Scale")
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
ks.test(growth_aida, "plaplace", fit_laplace1$estimate[1], fit_laplace1$estimate[2])
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
gof_laplace<-c(AIC(fit_laplace2), BIC(fit_laplace2)) # BIC is NA. It can be retrieved
gof_pareto<-c(AIC(fit_pareto), BIC(fit_pareto)) # using mle of {stats4} instead of mle2


gof_original
gof_original$chisqpvalue # returns the Chi-square p-values for the given fits. (They are surprisingly low)
gof_laplace
gof_pareto
gof_shifted
gof_scaled

skewness(growth_aida)
#####################################################################################

#Chi-square test for goodness of fit

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

par(mfrow=c(1,1))

d <- density(x)
plot(d, main="KDE for the AIDA growth rate distribution", xlab="Growth rate",  xlim=c(-10, 10))
grid(5,5)
plot(d, add=TRUE, main="KDE for the AIDA growth rate distribution", xlab="Growth rate",  xlim=c(-10, 10))
polygon(d, col=alpha("red", 0.3), border="black")


hist(x, prob=T, breaks=c(seqlast(min(aida_growth),max(aida_growth),0.2)), xlab="Growth rate", xlim=c(-10, 10),col="light grey", main="Histogram of AIDA growth rate distribution")

grid(6,6)
x<-growth_aida #(normal, cauchy, laplace, logis)
hist(x, add = F,  prob=T, breaks=c(seqlast(min(aida_growth),max(aida_growth),0.2)),xlim=c(-10, 10), xlab="Growth rate", col="light grey", main="Empirical growth rate distribution in AIDA")
curve(dnorm(x, fit_norm$estimate[1], fit_norm$estimate[2]), add=T,col = 1, lwd=2)
curve(dcauchy(x, fit_cauchy$estimate[1], fit_cauchy$estimate[2]), add=T,col = 2, lwd=2)
curve(dlaplace(x,  fit_laplace1$estimate[1] , fit_laplace1$estimate[2]), add=T, col=4, lwd=2)
#curve(dlaplace(x,  coef(fit_laplace2)[1] , coef(fit_laplace2)[2]), add=T, col=3, lwd=2)
curve(dlogis(x, fit_logis$estimate[1], fit_logis$estimate[2]), add=T,col = 7, lwd=2)
legend("topright", c("Normal", "Cauchy", "Laplace", "Logistic"), col=c(1,2,4,7), lwd=3)
x<-shifted #(lnorm, pareto, exp, weib, gamma)
hist(x, add = F,  prob=T, breaks=c(seqlast(min(shifted),max(shifted),0.2)),xlim=c(0, 20), xlab="Growth rate", col="light grey", main="Empirical growth rate distribution in AIDA")
curve(dlnorm(x, fit_lnorm$estimate[1], fit_lnorm$estimate[2]), add=T,col =6, lwd=2)
curve(dpareto(x,  coef(fit_pareto)[1] , coef(fit_pareto)[2]), add=T,col =7 , lwd=2)
curve(dexp(x, fit_exp$estimate[1]), add=T,col = 8, lwd=2)
curve(dweibull(x, fit_weib$estimate[1], fit_weib$estimate[2]), add=T,col = 9, lwd=2)
curve(dgamma(x, fit_gamma$estimate[1], fit_gamma$estimate[2]), add=T,col = 10, lwd=2)
x<-scaled #(beta)
hist(x, add = F,  prob=T, breaks=c(seqlast(min(scaled),max(scaled),0.02)),xlim=c(0, 1), xlab="Growth rate", col="light grey", main="Empirical growth rate distribution in AIDA")
curve(dbeta(x, fit_beta$estimate[1], fit_beta$estimate[2]), add=T,col = 11, lwd=2)


# Visualise some Q-Q and P-P plots
plot(fit_cauchy)
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
