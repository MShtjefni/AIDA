sample_size <-5000

by_firm_small_ID <- as.numeric(by_firm_small$TaxID)
by_firm_small_year <- as.numeric(by_firm_small$Year)
by_firm_small_R <- as.numeric(by_firm_small$Revenue)
by_firm_small_growth <- c()

for (i in 2:length(by_firm_small_ID)) {
  if (by_firm_small_ID[i] != by_firm_small_ID[i-1]) next
  if (by_firm_small_year[i] - by_firm_small_year[i-1] != 1) next
  by_firm_small_growth[i] <- log(by_firm_small_R[i]/by_firm_small_R[i-1])
}
by_firm_small_growth<-append(by_firm_small_growth,NA)

by_firm_small["Growth"]<-by_firm_small_growth


growth_small_firms <-as.data.frame(by_firm_small)

growth_small_firms <- growth_small_firms[sample(nrow(growth_small_firms)),] #shuffle

small_growth <- growth_small_firms$Growth[!is.na(growth_small_firms$Growth)]
anyNA(small_growth)
growth_small <- sample(small_growth, sample_size)

scaled_x <- (medium_growth - min(medium_growth)) /(max(medium_growth) - min(medium_growth))
scaled_y <- (large_growth - min(large_growth)) /(max(large_growth) - min(large_growth))
ks.test(medium_growth, large_growth)
############################################################################################################
fit_norm <- fitdist(growth_small, distr="norm", method="mle")
fit_cauchy <- fitdist(small_growth, distr="cauchy", method="mle")
fit_laplace1 <-fitdist(growth_small, distr = "laplace",  method = "mle", start=list(location=-0.1, scale=0.001))

est1 <- c()
est2 <- c()

for (i in 1:1000) {
  fcauchy<-fitdist(sample(small_growth, sample_size), method="mle", distr = "cauchy")
  est1[i] <- fcauchy$estimate[1]
  est2[i] <- fcauchy$estimate[2]
}


plotConfidInterv(est2, fit_cauchy$estimate[2], xlb="Scale parameter distribution")
############################################################################################################

###################################################################################################
mean(growth_small)
length(growth_small)
shifted<- growth_small +10 # to fit distributions that require values to be positive
min(shifted)
scaled <- (growth_small - min(growth_small) +0.000001) /(max(growth_small) - min(growth_small)+ 0.000002)
#fit distributions that require values to be within ]0, 1[

LL2 <- function(m, s) { 
  -sum(dlaplace(growth_small, m, s, log=TRUE)) 
}


LL3 <- function(m, s) {
  -sum(dpareto(shifted, m, s, log=TRUE)) # for dpareto, the first parameter is alpha, the second is xmin
}

#Powerlaw fit using the PowerLaw library
# m_m = conpl$new(shifted)
# est = estimate_xmin(m_m)
# m_m$setXmin(est)
# bs_p <- bootstrap_p(m_m, no_of_sims=1000, threads=8)
# bs_p$p

#Get an idea of the possible distribution for the empirical data
descdist(growth_small, discrete = FALSE)

#################################################################################################
#Fit distributions using fitdist and mle2
#norm, exp, gamma, beta, weibull, cauchy, pareto, logis, lnorm, laplace
fit_laplace2 <- mle2(LL2, start=list(m=-0.1, s=0.001), method = "L-BFGS-B", lower = c(m=-0.1, s=0.001))
fit_logis <-fitdist(growth_small, distr="logis", method="mle")
fit_lnorm <- fitdist(shifted, distr="lnorm", method="mle")
fit_pareto <- mle2(LL3, start = list(m =1, s = min(shifted)), method = "L-BFGS-B", lower = c(m=0.001), fixed = list(s=(min(shifted))))
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
ks.test(growth_small, "pnorm", fit_norm$estimate[1], fit_norm$estimate[2])
ks.test(growth_small, "pcauchy", fit_cauchy$estimate[1], fit_cauchy$estimate[2])
ks.test(growth_small, "plaplace", fit_laplace1$estimate[1], fit_laplace1$estimate[2])
ks.test(growth_small, "plaplace", coef(fit_laplace2)[1], coef(fit_laplace2)[2])
ks.test(growth_small, "plogis",fit_logis$estimate[1], fit_logis$estimate[2])
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
  r <- rcauchy(length(growth_small)
               , fit_cauchy$estimate[1]
               , fit_cauchy$estimate[2])
  as.numeric(ks.test(r
                     , "pcauchy"
                     , fit_cauchy$estimate[1]
                     , fit_cauchy$estimate[2])$statistic)})

fit_log <- logspline(stats)

1-plogspline(ks.test(growth_small
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
breaks <- append (breaks, c(seqlast(min(growth_small),max(growth_small),0.2), Inf))
growth_small.cut<-cut(growth_small,breaks=breaks, include.lowest = TRUE) #binning data
#table(growth_small.cut) # frequencies of empirical data in the respective bins
p<-c()
for (i in 1:(length(breaks)-1)) {
  p[i] <- pcauchy(breaks[i+1],fit_cauchy$estimate[1], fit_cauchy$estimate[2])-pcauchy(breaks[i],fit_cauchy$estimate[1], fit_cauchy$estimate[2])
}

f.os<-c()
for (j in 1:(length(breaks)-1)) {
  f.os[j]<- table(growth_small.cut)[[j]]
}

chisq.test(x=f.os, p=p)# p-value is very low, probably due to binning choice
chisq.test(x=f.os, simulate.p.value = T) # test with Monte Carlo simulated p-values
#################################################################################




kurtosis(small_growth)
skewness(small_growth)
mean(small_growth)
var(small_growth)

#################################################################################

#Visualise the obtained distributions

x<-small_growth #(normal, cauchy, laplace, logis)
x<-shifted #(lnorm, pareto, exp, weib, gamma)
x<-scaled #(beta)

par(mfrow=c(1,1))

hist(x, prob=T, breaks=c(seqlast(min(small_growth),max(small_growth),0.1)), xlab="Growth",xlim = c(-3, 4), col="grey", main="Fit for small firm growth distribution")
grid(5,5)
hist(x, prob=T, breaks=c(seqlast(min(small_growth),max(small_growth),0.1)), xlab="Growth",xlim = c(-3, 4), col="grey", main="Fit for small firm growth distribution")
curve(dnorm(x, fit_norm$estimate[1], fit_norm$estimate[2]), add=T,col = 1, lwd=2)
curve(dcauchy(x, fit_cauchy$estimate[1], fit_cauchy$estimate[2]), add=T,col = 2, lwd=2)
curve(dlaplace(x,  fit_laplace1$estimate[1] , fit_laplace1$estimate[2]), add=T, col=3, lwd=2)
curve(dlaplace(x,  coef(fit_laplace2)[1] , coef(fit_laplace2)[2]), add=T, col=4, lwd=2)
curve(dlogis(x, fit_logis$estimate[1], fit_logis$estimate[2]), add=T,col = 5, lwd=2)
curve(dlnorm(x, fit_lnorm$estimate[1], fit_lnorm$estimate[2]), add=T,col = 6, lwd=2)
curve(dpareto(x,  coef(fit_pareto)[1] , coef(fit_pareto)[2]), add=T,col =7 , lwd=2)
curve(dexp(x, fit_exp$estimate[1]), add=T,col = 8, lwd=2)
curve(dweibull(x, fit_weib$estimate[1], fit_weib$estimate[2]), add=T,col = 9, lwd=2)
curve(dgamma(x, fit_gamma$estimate[1], fit_gamma$estimate[2]), add=T,col = 10, lwd=2)
curve(dbeta(x, fit_beta$estimate[1], fit_beta$estimate[2]), add=T,col = 11, lwd=2)
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
lengr <- length(growth_small)
y <- rlaplace(lengr, coef(fit_laplace2)[1], coef(fit_laplace2)[2])
qqplot(y, growth_small, xlab="Theoretical Quantiles", ylab = "Empirical Quantiles")
qqline(rlaplace(lengr), col = 2,lwd=2,lty=2, distribution = qlaplace)
#qnorm(0.5) # for a given surface under the std normal dist (50% in this case) give me the corresponding z score

#CDF plot:
#lines(ecdf(rcauchy(lengr, fit_cauchy$estimate[1], fit_cauchy$estimate[2])), col="blue", cex=0.5, xlim=c(-1,1))
#plot(ecdf(rlaplace(lengr,  0.0315464, 0.0571342)), col="blue", cex=0.5)
plot(ecdf(growth_small),cex=0.5, col="red")
curve(pcauchy(x,fit_cauchy$estimate[1], fit_cauchy$estimate[2]), add = T , cex=0.5, col="blue")
curve(plaplace(x,fit_laplace1$estimate[1], fit_laplace1$estimate[2]), add = T , cex=0.5, col="green")
curve(plaplace(x,coef(fit_laplace2)[1], coef(fit_laplace2)[2]), add = T ,  cex=0.5,col="purple")

###########################################################################################################

