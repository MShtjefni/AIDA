# wdir <- ""

source(paste(wdir, "first_analysis.r", sep=""))
#summary(aidat)

View(nine_year_firms)
nine_year_firms2$Size <- NA
nine_year_firms2$Size[nine_year_firms2$E <= 49] <- "Small"
nine_year_firms2$Size[50 <= nine_year_firms2$E & nine_year_firms2$E <= 249] <- "Medium"
nine_year_firms2$Size[nine_year_firms2$E >= 250] <- "Large"


nine_year_firms2$R[nine_year_firms2$R <=1 ] <- 1
allDensities = list()

subsectorsGrowth <- function(data) {
  data$SubsectorGrowth <- NA
  for (subs in unique(data$Subsector)) {
    #View(nine_year_firms)
    sectorFirms <- subset(data, Subsector == subs)
    #View(sectorFirms)
    by_firm <- group_by(sectorFirms,TaxID, Year)
    by_firm <- summarize(by_firm, Revenue=R)

    by_subsector <- group_by(sectorFirms, Year) %>%
      summarize(sumOfRevenues=sum(R))

    by_firm$Growth<- NA
    #View(by_firm)
    average<-(log10(by_subsector$sumOfRevenues))/nrow(by_firm)
    for (row in 2:nrow(by_firm)) {
      if (by_firm[row, "TaxID"] != by_firm[row-1, "TaxID"]) next
      #if (by_firm[row, "Year"] - by_firm[row-1, "Year"] != 1) next
      by_firm$Growth[row] <- (log10(by_firm[row, "Revenue"]) - average) - (log10(by_firm[row-1, "Revenue"]) - average)
    }

    growth <-  by_firm$Growth[!is.na(by_firm$Growth)]
    growth <- as.numeric(growth)
    d <- density(growth) # returns the density data
    plot(d) # plots the results
    append(allDensities,d)
    #View(by_firm)

    data [data$Subsector==subs,]<-group_by(data [data$Subsector==subs,],TaxID,Year)
    data$SubsectorGrowth [data$Subsector==subs]<-by_firm$Growth 

  }
  return (data)
}

singleFirmGrowth <- function(data) {
  data$SingleFirmGrowth <- NA
  for (subs in unique(data$Subsector)) {
    sectorFirms <- subset(data, Subsector == subs)
    
    by_firm <- group_by(sectorFirms,TaxID, Year)
    by_firm <- summarize(by_firm, Revenue=sum(R))

    by_firm$Growth<- NA

    #average<-(sum(log10(by_firm$Revenue))/nrow(by_firm))
    for (row in 2:nrow(by_firm)) {
      if (by_firm[row, "TaxID"] != by_firm[row-1, "TaxID"]) next
      #if ((by_firm[row, "Year"] - by_firm[row-1, "Year"]) != 1) next
      by_firm$Growth[row] <- log10(by_firm[row, "Revenue"]) - log10(by_firm[row-1, "Revenue"])
      #by_firm$Growth[row] <- (log10(by_firm[row, "Revenue"]) - year_average$tot[year_average$Year==as.numeric(by_firm[row, "Year"])]) - (log10(by_firm[row-1, "Revenue"]) - year_average$tot[year_average$Year==as.numeric(by_firm[row-1, "Year"])])
    }
    #View(by_firm)
    growth <-  by_firm$Growth[!is.na(by_firm$Growth)]
    growth <- as.numeric(growth)
    #mean(growth)
    d <- density(growth) # returns the density data 
    plot(d) # plots the results
    hist(growth, prob=T, breaks="Scott", xlim=c(-1, 1))
    
    data [data$Subsector==subs,]<-group_by(data [data$Subsector==subs,],TaxID,Year)
    data$SingleFirmGrowth [data$Subsector==subs]<-by_firm$Growth
  }
  return(data)  
}


# LL1 <- function(x) {
#       x1 <- x[1]
#       x2 <- x[2]
#       -sum(dlaplace(growth, x1, x2, log=TRUE))
#     # -sum(log(R))
# }

#optim(c(0,1), LL1, NULL, method="L-BFGS-B", lower = c(0,2))



# the function below returns the sum of negative log likelihoods that 
# will be passed to the mle2 function (bbmle package) to be minimized. 
# The dlaplace function will return the probability of ocurrence for 
# every value in the growth vector, given the two parameters m and s
LL2 <- function(m, s) { 
  -sum(dlaplace(growth, m, s, log=TRUE))
}

# same as the LL2 function, only this time the dnorm function
# for the normal distribution is used.

LL3 <- function(mu, sigma) {
  -sum(dnorm(growth, mu, sigma, log=TRUE))
}

# We have to give a couple starting values to the m and s parameters for the 
# minimization to start. It is reasonable to equal the starting values with 
# the empirical values of the m and s parameters. The lower bound is used
# since the s parameter in laplace dist will take only positive values.
fit2<-mle2(LL2, start=list(m=0, s=1), method = "L-BFGS-B", lower = c(m=-1, s=0.01), upper = c(m=1,s=2))

# Similar to fit2. We need less restriction for a mle with normal distribution.
fit3<-mle2(LL3, start=list(mu=mean(growth), sigma=sd(growth)))

# The mle2 fucntion of bbmle was prefered to the mle function of the stats4 base package
# because it gives a little more information about the fit when using summary()

summary(fit2) # the std error is very small
AIC(fit2) 

summary(fit3) # the std error is small but bigger than that of fit2.
AIC(fit3)

# We see that the difference between the Akaike information criterion (AIC) 
# between fit2 and fit3 is considerable, indicating that the laplace fit
# better explains the growth rate distribution that the normal one. 

# The plots below give a likelihood profile for both m and s parameters
# including multiple confidence intervals. The y-axis is abs(Z).
plot(profile(fit2)) 
plot(profile(fit3))


# the function below (fitdistrplus package) estimates the distribution function
# of the empirical data, so it gives us an idea about the distribution of the 
# growth rate. As can be seen from the plot, the observation represented by the
# blue dot is far away from the normal, weibull or exponential distributions
descdist(growth, discrete = FALSE)

# Finally we do a fit using the fitdist function. It does not require much 
# tweaking for well known distributions such as normal, poisson or exponential. 
fit4 <- fitdist(growth, distr="norm", method="mle")

# The good thing about the fitdist function is that we can plot
# the obtained fit and we get four graphical representations of
# the comparison between the empirical data and fitted values.
summary(fit4)
plot(fit4)
# It can be concluded, judging by the Q-Q  and P-P plots, that the normal fit is not very
# suitable for our empirical data on the growth rate.

#Further testing with various distribution functions have to be conducted.
  
  