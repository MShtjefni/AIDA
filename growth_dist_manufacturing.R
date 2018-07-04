growth_manufacturing <-as.data.frame(manufacturing_growth)

manufacturing_2008 <- subset(growth_manufacturing, growth_manufacturing$Year=="2008")

manufacturing_2009 <- subset(growth_manufacturing, growth_manufacturing$Year=="2009")

random_sample_manufacturing<- growth_manufacturing[sample(nrow(growth_manufacturing), 10000), ]

random_sample_manufacturing_2008 <- subset(random_sample_manufacturing, random_sample_manufacturing$Year=="2008")

growth_random_manufacturing_2008 <- as.numeric(random_sample_manufacturing_2008$Growth[!is.na(random_sample_manufacturing_2008$Growth)])

growth_manufacturing_2008 <- as.numeric(manufacturing_2008$Growth[!is.na(manufacturing_2008$Growth)])
growth_manufacturing_2009 <- as.numeric(manufacturing_2009$Growth[!is.na(manufacturing_2009$Growth)])

mean(growth_random_manufacturing_2008)
length(growth_random_manufacturing_2008)
shifted<- growth_random_manufacturing_2008 +10 # to fit distributions that require values to be positive

scaled <- (growth_random_manufacturing_2008 - min(growth_random_manufacturing_2008) +0.000001) /(max(growth_random_manufacturing_2008) - min(growth_random_manufacturing_2008)+ 0.000002)
#fit distributions that require values to be within ]0, 1[

LL2 <- function(m, s) { 
  -sum(dlaplace(growth_random_manufacturing_2008, m, s, log=TRUE)) 
}


LL3 <- function(m, s) {
  -sum(dpareto(shifted, m, s, log=TRUE)) # for dpareto, the first parameter is alpha, the second is xmin
}

#Powerlaw fit using the PowerLaw library
m_m = conpl$new(shifted)
est = estimate_xmin(m_m)
m_m$setXmin(est)
bs_p <- bootstrap_p(m_m, no_of_sims=1000, threads=8)
bs_p$p

#Get an idea of the possible distribution for the empirical data
descdist(growth_random_manufacturing_2008, discrete = FALSE)

#################################################################################################
#Fit distributions using fitdist and mle2
#norm, exp, gamma, beta, weibull, cauchy, pareto, logis, lnorm, laplace
fit_norm <- fitdist(growth_random_manufacturing_2008, distr="norm", method="mle")
fit_cauchy <- fitdist(growth_random_manufacturing_2008, distr="cauchy", method="mle")
fit_laplace<-mle2(LL2, start=list(m=1, s=0.001), method = "L-BFGS-B", lower = c(m=-0.1, s=0.001))
fit_logis <-fitdist(growth_random_manufacturing_2008, distr="logis", method="mle")
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
summary(boot)# 1000 samples and compute their parameters according to the given fitted distribution
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
ks.test(growth_random_manufacturing_2008, "pnorm", fit_norm$estimate[1], fit_norm$estimate[2])
ks.test(growth_random_manufacturing_2008, "pcauchy", fit_cauchy$estimate[1], fit_cauchy$estimate[2])
ks.test(growth_random_manufacturing_2008, "plaplace", coef(fit_laplace)[1] , coef(fit_laplace)[2])
ks.test(growth_random_manufacturing_2008, "plogis",fit_logis$estimate[1], fit_logis$estimate[2])
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
  r <- rcauchy(length(growth_random_manufacturing_2008)
               , fit_cauchy$estimate[1]
               , fit_cauchy$estimate[2])
  as.numeric(ks.test(r
                     , "pcauchy"
                     , fit_cauchy$estimate[1]
                     , fit_cauchy$estimate[2])$statistic)})

fit_log <- logspline(stats)

1-plogspline(ks.test(growth_random_manufacturing_2008
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
breaks <- append (breaks, c(seqlast(min(growth_random_manufacturing_2008),max(growth_random_manufacturing_2008),0.2), Inf))
growth_random_manufacturing_2008.cut<-cut(growth_random_manufacturing_2008,breaks=breaks, include.lowest = TRUE) #binning data
#table(growth_random_manufacturing_2008.cut) # frequencies of empirical data in the respective bins
p<-c()
for (i in 1:(length(breaks)-1)) {
  p[i] <- pcauchy(bins[i+1],fit_cauchy$estimate[1], fit_cauchy$estimate[2])-pcauchy(bins[i],fit_cauchy$estimate[1], fit_cauchy$estimate[2])
}

f.os<-c()
for (j in 1:(length(breaks)-1)) {
  f.os[j]<- table(growth_random_manufacturing_2008.cut)[[j]]
}

chisq.test(x=f.os, p=p)# p-value is very low, probably due to binning choice
chisq.test(x=f.os, simulate.p.value = T) # test with Monte Carlo simulated p-values
#################################################################################




#################################################################################

#Visualise the obtained distributions

x<-growth_random_manufacturing_2008 #(normal, cauchy, laplace, logis)
x<-shifted #(lnorm, pareto, exp, weib, gamma)
x<-scaled #(beta)

par(mfrow=c(1,1))

hist(x, prob=T, breaks="fd", xlab="growth_random_manufacturing_2008 rate",xlim = c(-3, 4), col="grey", main="Empirical small growth Rate Distribution")

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
lengr <- length(growth_random_manufacturing_2008)
y <- rlaplace(lengr, coef(fit_laplace)[1], coef(fit_laplace)[2])
qqplot(y, growth_random_manufacturing_2008, xlab="Theoretical Quantiles", ylab = "Empirical Quantiles")
qqline(rlaplace(lengr), col = 2,lwd=2,lty=2, distribution = qlaplace)
#qnorm(0.5) # for a given surface under the std normal dist (50% in this case) give me the corresponding z score

#CDF plot:
#lines(ecdf(rcauchy(lengr, fit_cauchy$estimate[1], fit_cauchy$estimate[2])), col="blue", cex=0.5, xlim=c(-1,1))
#plot(ecdf(rlaplace(lengr,  0.0315464, 0.0571342)), col="blue", cex=0.5)
plot(ecdf(growth_random_manufacturing_2008),cex=0.5, col="red")
curve(pcauchy(x,fit_cauchy$estimate[1], fit_cauchy$estimate[2]), add = T , cex=0.5, col="blue")
curve(plaplace(x,coef(fit_laplace)[1], coef(fit_laplace)[2]), add = T ,  cex=0.5,col="green")

###########################################################################################################

#Hypothesis testing on the mean

#Test data for normality
qqnorm(growth_manufacturing_2008) #visually the distribution is far from normal
qqline(growth_manufacturing_2008)
shapiro.test(growth_random_manufacturing_2008) # p-value close to 0 so we reject the null hypothesis

#Test of mean, non-normal data, distribution free but has to be symmetrical:
wilcox.test(growth_manufacturing_2008, mu=0 , conf.int = T) # We reject H0 i.e. the true mean is != 0

#Very large sample size, we can take advantage of the clt by performing a z-test:
z.test(growth_manufacturing_2008, sigma.x=sd(growth_manufacturing_2008), mu=0) # Again we reject H0

#Let's try the t-test::
t.test(growth_manufacturing_2008, mu= 0) # We reject H0

#Parametric bootstrapping:
samplemean <- function(x, i) {
  return((mean(x[i])))
}

est<-as.numeric(fit_cauchy$estimate)

rg <- function(growth_manufacturing_2008, mle) {
  out <- rcauchy(length(growth_manufacturing_2008), mle[1], mle[2])
  out
}

p_boot = boot(growth_manufacturing_2008, samplemean, R=1000, sim="parametric", ran.gen = rg , mle=est)

quantile(p_boot$t, c(0.025, 0.975)) # we cannot reject the null hypothesis
plot(p_boot)
#bootstrap p-value approximation:
#centered:
mean(abs(p_boot$t - p_boot$t0) >= abs(np_boot$t0 - 0)) # we can see the p-value is very high 

#uncentered (practically the same result):
mean(abs(p_boot$t) >= 0) # we cannot reject H0, so the mean is =0

#Non-parametric bootstrapping:
samplemean <- function(x, i) {
  return((mean(x[i])))
}

np_boot = boot(growth_manufacturing_2008, samplemean, R=1000, sim="ordinary")
plot(np_boot)

# derive ci
quantile(np_boot$t, c(0.025, 0.975)) # we reject the null hypothesis

# or like this
boot.ci(np_boot, type = "norm")
#bootstrap p-value approximation
#centered distribution of parameters:
mean(abs(np_boot$t - np_boot$t0) >= abs(np_boot$t0 - 0)) # the p-value is practically 0

###################################################################################################

#Hypothesis testing on two means

#z-test: we reject H0, so the true mean difference is not =0
z.test(growth_manufacturing_2008, sigma.x=sd(growth_manufacturing_2008), growth_manufacturing_2009, sigma.y=sd(growth_manufacturing_2009), mu=0)

#Wilcox test: we reject H0
wilcox.test(growth_manufacturing_2008, growth_manufacturing_2009, mu=0)

#t-test, unequal variances: again we reject H0
t.test(growth_manufacturing_2008, growth_manufacturing_2009, mu=0)


#Non-parametric bootstrapping:
growthDf <- data.frame(growth=c(growth_manufacturing_2008, growth_manufacturing_2009),
                       year=factor(rep(c("2008", "2009"), c(length(growth_manufacturing_2008), length(growth_manufacturing_2009)))))

getMD <- function(dat, idx) {
  MD2008_2009 <- aggregate(growth ~ year, data=dat, subset=idx,FUN=mean)
  -diff(MD2008_2009$growth)
}

np_boot = boot(growthDf, statistic=getMD,strata=growthDf$year,  R=500)
plot(np_boot)

quantile(np_boot$t, c(0.025, 0.975)) # we reject the null hypothesis, so there is a difference between the two means

#bootstrap p-value approximation
#centered distribution of parameters:
mean(abs(np_boot$t - np_boot$t0) >= abs(np_boot$t0 - 0)) # the p-value is practically 0

#Parametric bootstrapping:
#The good old fashioned way
mean_differences <- replicate(1000, {
  r1 <- rcauchy(length(growth_manufacturing_2008)
               , fit_cauchy$estimate[1] # Whatever the estimated cauchy parameters for the 2008 growth are
               , fit_cauchy$estimate[2])
  r2<- rcauchy(length(growth_manufacturing_2009)
               , fit_cauchy$estimate[1]  # Whatever the estimated cauchy parameters for the 2009 growth are
               , fit_cauchy$estimate[2])
  return (mean(r1) - mean(r2))
    })

hist(mean_differences, prob=T, breaks="scott", col="grey", main="Mean difference distribution")

#p-value approximation
mean(abs(mean_differences) >= 0)
####################################################################################################

# Regression model fitting for the growth rate
symmetry.test(growth_manufacturing_2008)
