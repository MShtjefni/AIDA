#wdir <- ""
wdir <- ""
dataDir <- "data/"
packagesFile <- "packages.txt"
source(paste(wdir, "functions.R", sep="")) ### this also loads every needed package

getGrowth <- function(data) {
  data <- arrange(data, data$TaxID, data$Year)
  data$R[data$R<1]<-1
  grouped <- group_by(data,TaxID, Year)
  grouped <- summarize(grouped, Revenue=sum(R))
  if (nrow(grouped)<2) 
    return(data)
  
  by_firm_ID <- as.numeric(grouped$TaxID)
  by_firm_year <- as.numeric(grouped$Year)
  by_firm_R <- as.numeric(grouped$Revenue)
  growth <- c()
  for (i in 2:nrow(grouped)) { # much faster computation
    if (by_firm_ID[i] != by_firm_ID[i-1]) next
    if (by_firm_year[i] - by_firm_year[i-1] != 1) next
    growth[i] <- as.numeric(log(by_firm_R[i]/by_firm_R[i-1]))
  }
  data$Growth<-growth
  return (data)
}

subsectorsGrowth <- function(data) {
  data$R<-data$R + 1
  data$SubsectorGrowth <- NA
  for (subs in unique(data$Subsector)) {
    subsectorFirms <- subset(data, Subsector == subs)
    by_firm <- group_by(subsectorFirms,TaxID, Year)
    by_firm$Growth<- NA

    revSum <- group_by(subsectorFirms,Year) %>%
      summarise(sumOfRevenues=sum(R))
    distinctYears <- unique(subsectorFirms$Year)
    subsAverages <- list()
    
    for (y in distinctYears) {
      average<-(log10(revSum$sumOfRevenues[revSum$Year==y]))/nrow(subsectorFirms[subsectorFirms$Year==y ,])
      subsAverages[y]=average
    }
    
    for (row in 2:nrow(by_firm)) {
      if (by_firm[row, "TaxID"] != by_firm[row-1, "TaxID"]) next
      #if (by_firm[row, "Year"] - by_firm[row-1, "Year"] != 1) next
      currYear <- by_firm$Year[row]
      currAverage <- subsAverages[currYear]
      prevAverage <- subsAverages[currYear-1]
      by_firm$Growth[row] <- (log10(by_firm[row, "R"])) - currAverage - (log10(by_firm[row-1, "R"])) - prevAverage
    }

    growth <-  by_firm$Growth[!is.na(by_firm$Growth)]
    growth <- as.numeric(growth)
    d <- density(growth) # returns the density data
    plot(d) # plots the results
    View(growth)    #View(by_firm)

    data [data$Subsector==subs,]<-group_by(data [data$Subsector==subs,],TaxID,Year)
    data$SubsectorGrowth [data$Subsector==subs]<-by_firm$Growth 
    break
  }
  return (data)
}

singleFirmGrowth <- function(data) {
  data$R<-data$R + 1
  data$SingleFirmGrowth <- NA
  for (subs in unique(data$Subsector)) {
    sectorFirms <- subset(data, Subsector == subs)
    
    by_firm <- group_by(sectorFirms,TaxID, Year)
    by_firm <- summarize(by_firm, Revenue=sum(R))

    by_firm$Growth<- NA

    #average<-(sum(log10(by_firm$Revenue))/nrow(by_firm))
    if (nrow(by_firm)<2) next
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
    break
  }
  return(data)  
}


getGrowthDensities <- function(data, growthColumn) {
  densities <- list()
  for (subs in unique(data$Subsector)) {
    growth <- data[growthColumn] [data$Subsector == subs ,]
    densities[subs] = growth
  }
  return (densities)
}

groupByGrowth <- function(data) {
  if("Growth" %in% names(data)) {
    by_firm<- group_by(data,TaxID, Year, SubSector,Region, GeoArea, Size, Growth)
  } else {
      data <- getGrowth(data)
      by_firm<- group_by(data,TaxID, Year, SubSector,Region, GeoArea, Size, Growth)
  }
  
  growth_firms <-as.data.frame(by_firm)
  growth_firms <- growth_firms[sample(nrow(growth_firms)),] #shuffle
  growth_no_missing <- growth_firms[!is.na(growth_firms$Growth) ,]
  return(growth_no_missing)
}
#library(stats4)
#library(fitdistrplus) # maximum likelihood fitting (excluding laplace dist) 
#library(bbmle) # maximum likelihood fitting with laplace distribution
#library(rmutil) # needed for dlaplace


# LL1 <- function(x) {
#       x1 <- x[1]
#       x2 <- x[2]
#       -sum(dlaplace(growth, x1, x2, log=TRUE))
#     # -sum(log(R))
# }

#optim(c(0,1), LL1, NULL, method="L-BFGS-B", lower = c(0,2))

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

# the function below returns the sum of negative log likelihoods that 
# will be passed to the mle2 function (bbmle package) to be minimized. 
# The dlaplace function will return the probability of ocurrence for 
# every value in the growth vector, given the two parameters m and s
LL2 <- function(m, s) { 
  -sum(dlaplace(growth, m, s, log=TRUE))
}

# same as the LL2 function, only this time the dpareto function
# for the pareto distribution is used.

LL3 <- function(m, s) {
  -sum(dpareto(shifted, m, s, log=TRUE)) # for dpareto, the first parameter is alpha, the second is xmin
}

small_firms <- subset(manufacturing, manufacturing$Size == "Small")
medium_firms <- subset(manufacturing, manufacturing$Size == "Medium")
large_firms <- subset(manufacturing, manufacturing$Size == "Large")

# We have to give a couple starting values to the m and s parameters for the 
# minimization to start. It is reasonable to equal the starting values with 
# the empirical values of the m and s parameters. The lower bound is used
# since the s parameter in laplace dist will take only positive values.
#fit2<-mle2(LL2, start=list(m=0, s=1), method = "L-BFGS-B", lower = c(m=-1, s=0.01), upper = c(m=1,s=2))

# Similar to fit2. We need less restriction for a mle with normal distribution.
#fit3<-mle2(LL3, start=list(mu=mean(growth), sigma=sd(growth)))

# The mle2 fucntion of bbmle was prefered to the mle function of the stats4 base package
# because it gives a little more information about the fit when using summary()

#summary(fit2) # the std error is very small
#AIC(fit2) 

#summary(fit3) # the std error is small but bigger than that of fit2.
#AIC(fit3)

# We see that the difference between the Akaike information criterion (AIC) 
# between fit2 and fit3 is considerable, indicating that the laplace fit
# better explains the growth rate distribution that the normal one. 

# The plots below give a likelihood profile for both m and s parameters
# including multiple confidence intervals. The y-axis is abs(Z).
#plot(profile(fit2)) 
#plot(profile(fit3))


# the function below (fitdistrplus package) estimates the distribution function
# of the empirical data, so it gives us an idea about the distribution of the 
# growth rate. As can be seen from the plot, the observation represented by the
# blue dot is far away from the normal, weibull or exponential distributions
#descdist(growth, discrete = FALSE)

# Finally we do a fit using the fitdist function. It does not require much 
# tweaking for well known distributions such as normal, poisson or exponential. 
#fit4 <- fitdist(growth, distr="norm", method="mle")

# The good thing about the fitdist function is that we can plot
# the obtained fit and we get four graphical representations of
# the comparison between the empirical data and fitted values.
#summary(fit4)
#plot(fit4)
# It can be concluded, judging by the Q-Q  and P-P plots, that the normal fit is not very
# suitable for our empirical data on the growth rate.

#Further testing with various distribution functions have to be conducted.

#hist(x, prob=T, breaks="Scott", xlim=c(-1.5, 1.5), xlab="Growth rate", col="grey", main="Empirical Growth Rate Distribution")
#curve(dlaplace(x,  0.00438608, 0.10486774), add=T, col="blue", lwd=2)
#curve(dcauchy(x, 0.007378846, 0.1091681), add=T,col = "red", lwd=2)
#legend("topright", c("Laplace fit", "Cauchy fit"), col=c("blue", "red"), lwd=3)

#gofstat(fit4)


#ks.test(growth, "pcauchy", 0, 0.0623094)
#ks.test(growth, "pnorm", 0.0056724, 0.3310969)
#plot(growth, pcauchy(growth, 0.0056724, 0.3310969))
#plot(ecdf(rcauchy(length(growth), 0.0056724, 0.3310969)), col="blue", cex=0.5)
#plot(ecdf(rlaplace(length(growth),  0.00302667, 0.07369791)), col="blue", cex=0.5)
#lines(ecdf(growth), lwd=3, col="red")

"
n.sims <- 500
stats <- replicate(n.sims, {
  r <- rlaplace(length(x)
                , 0.00438608
                , 0.10486774)
  as.numeric(ks.test(r
                     , 'plaplace'
                     , 0.00438608
                     , 0.10486774)$statistic)})

fit5 <- logspline(stats)

1 - plogspline(ks.test(x
                       , 'plaplace'
                       , 0.00438608
                       , 0.10486774)$statistic
               , fit5
)

# yx <- rnorm(100000)
# xy <- rnorm(100000)
# mean(yx)
#z.test(yx, sigma.x = 1, yx, sigma.y = 1, mu=0, 'two.sided', TRUE)
#z.test(growth,sigma.x=sd(growth))
#Ask about paired z-test with two dependent samples
#Ask about the chi-sqaure test (is it suitable to determine goodness of fit?)
min(growth)
bins <-(seq(min(growth),max(growth),0.05))
bins
#Chi-square test

growth.cut<-cut(growth,breaks=seq(min(growth),max(growth),0.05), include.lowest = TRUE) ##binning data
table(growth.cut) ## binned data table
growth.cut

p=c()
for (i in 1:(length (seq(min(growth),max(growth),0.05)))-1) {
  p[i] <- plaplace(bins[i+1],0,0.0623094)-plaplace(bins[i],0,0.0623094)
}

f.os<-c()
for (j in 1:50) {f.os[j]<- table(growth.cut)[[j]]}

chisq.test(x=f.os,p=p)

"

  