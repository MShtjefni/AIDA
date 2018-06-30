growth_large_firms <-as.data.frame(large_firm_growth)

random_sample_large_firms<- growth_large_firms[sample(nrow(growth_large_firms), 500), ]

growth_random_large <- as.numeric(random_sample_large_firms$Growth[!is.na(random_sample_large_firms$Growth)])

mean(growth_random_large)
length(growth_random_large)
shifted<- growth_random_large +10 # to fit distributions that require values to be positive

scaled <- (growth_random_large - min(growth_random_large) +0.000001) /(max(growth_random_large) - min(growth_random_large)+ 0.000002)
#fit distributions that require values to be within ]0, 1[

LL2 <- function(m, s) { 
  -sum(dlaplace(growth_random_large, m, s, log=TRUE)) 
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
descdist(growth_random_large, discrete = FALSE)

#################################################################################################
#Fit distributions using fitdist and mle2
#norm, exp, gamma, beta, weibull, cauchy, pareto, logis, lnorm, laplace
fit_norm <- fitdist(growth_random_large, distr="norm", method="mle")
fit_cauchy <- fitdist(growth_random_large, distr="cauchy", method="mle")
fit_laplace<-mle2(LL2, start=list(m=1, s=0.001), method = "L-BFGS-B", lower = c(m=-0.1, s=0.001))
fit_logis <-fitdist(growth_random_large, distr="logis", method="mle")
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
ks.test(growth_random_large, "pnorm", fit_norm$estimate[1], fit_norm$estimate[2])
ks.test(growth_random_large, "pcauchy", fit_cauchy$estimate[1], fit_cauchy$estimate[2])
ks.test(growth_random_large, "plaplace", coef(fit_laplace)[1] , coef(fit_laplace)[2])
ks.test(growth_random_large, "plogis",fit_logis$estimate[1], fit_logis$estimate[2])
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
  r <- rcauchy(length(growth_random_large)
               , fit_cauchy$estimate[1]
               , fit_cauchy$estimate[2])
  as.numeric(ks.test(r
                     , "pcauchy"
                     , fit_cauchy$estimate[1]
                     , fit_cauchy$estimate[2])$statistic)})

fit_log <- logspline(stats)

1-plogspline(ks.test(growth_random_large
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
breaks <- append (breaks, c(seqlast(min(growth_random_large),max(growth_random_large),0.2), Inf))
growth_random_large.cut<-cut(growth_random_large,breaks=breaks, include.lowest = TRUE) #binning data
#table(growth_random_large.cut) # frequencies of empirical data in the respective bins
p<-c()
for (i in 1:(length(breaks)-1)) {
  p[i] <- pcauchy(bins[i+1],fit_cauchy$estimate[1], fit_cauchy$estimate[2])-pcauchy(bins[i],fit_cauchy$estimate[1], fit_cauchy$estimate[2])
}

f.os<-c()
for (j in 1:(length(breaks)-1)) {
  f.os[j]<- table(growth_random_large.cut)[[j]]
}

chisq.test(x=f.os, p=p)# p-value is very low, probably due to binning choice
chisq.test(x=f.os, simulate.p.value = T) # test with Monte Carlo simulated p-values
#################################################################################




#################################################################################

#Visualise the obtained distributions

x<-growth_random_large #(normal, cauchy, laplace, logis)
x<-shifted #(lnorm, pareto, exp, weib, gamma)
x<-scaled #(beta)

par(mfrow=c(1,1))

hist(x, prob=T, breaks="fd", xlab="growth_random_large rate", col="grey", main="Empirical small growth Rate Distribution")

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
lengr <- length(growth_random_large)
y <- rlaplace(lengr, coef(fit_laplace)[1], coef(fit_laplace)[2])
qqplot(y, growth_random_large, xlab="Theoretical Quantiles", ylab = "Empirical Quantiles")
qqline(rlaplace(lengr), col = 2,lwd=2,lty=2, distribution = qlaplace)
#qnorm(0.5) # for a given surface under the std normal dist (50% in this case) give me the corresponding z score

#CDF plot:
#lines(ecdf(rcauchy(lengr, fit_cauchy$estimate[1], fit_cauchy$estimate[2])), col="blue", cex=0.5, xlim=c(-1,1))
#plot(ecdf(rlaplace(lengr,  0.0315464, 0.0571342)), col="blue", cex=0.5)
plot(ecdf(growth_random_large),cex=0.5, col="red")
curve(pcauchy(x,fit_cauchy$estimate[1], fit_cauchy$estimate[2]), add = T , cex=0.5, col="blue")
curve(plaplace(x,coef(fit_laplace)[1], coef(fit_laplace)[2]), add = T ,  cex=0.5,col="green")

###########################################################################################################
