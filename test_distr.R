distr <- c("norm", "lnorm", "gamma", "beta", "exp", "llogis", "weibull")

fitDistributions <- function(sample, distributionList=distr){
  fits <- kstests <- adtests <- Ds <- list()
  if ("norm" %in% distributionList) {
    fit.norm <-fitdist(sample, "norm",method = c("mle"))
  
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    n.sims <- 1000
    stats <- replicate(n.sims, {
      r <- rnorm(n = length(sample), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })
  
    fit <- logspline(stats)
    ks<-ks.test(sample,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
    ad<-ad.test(sample,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
    D<-1 - plogspline(ks$statistic, fit)
    fits["norm"]<-list(fit.norm)
    kstests["norm"]<-list(ks)
    adtests["norm"]<-list(ad)
    Ds["norm"]<-D
    
  }
  #DISTRIBUZIONE LOGNORM
  if ("lnorm" %in% distributionList) {
    
    fit.lnorm<-fitdist(sample,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$estimate
    fit.lnorm$aic
    
    #a = bootdist(sample.lnorm, niter=100, ncpus  = 4,bootmethod="param")
    #quantile(a,prob=0.5)
    
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rlnorm(n = length(sample), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    fit <- logspline(stats)
    ks <- ks.test(sample,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])
    ad <- ad.test(sample,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])
    D <- 1 - plogspline(ks$statistic, fit)
    fits["lnorm"]<-list(fit.lnorm)
    kstests["lnorm"]<-list(ks)
    adtests["lnorm"]<-list(ad)
    Ds["lnorm"]<-D
    
  }
  
  #GAMMA DISTRIBUTION 
  if ("gamma" %in% distributionList)
  {
    fit.gamma<-fitdist(sample,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rgamma(n = length(sample), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(r, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    ks <- ks.test(sample,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])
    ad <- ad.test(sample,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])
    D <- 1 - plogspline(ks$statistic, fit)
    fits["gamma"]<-list(fit.gamma)
    kstests["gamma"]<-list(ks)
    adtests["gamma"]<-list(ad)
    Ds["gamma"]<-D
  }
  
  #BETA
  if ("beta" %in% distributionList)
  {
    
    betaSample<-sample/(max(sample)+.1)
    fit.beta<-fitdist(betaSample, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
    
    n.sims <- 2000
    stats <- replicate(n.sims, {   
      r <- rbeta(n = length(sample), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    ks <- ks.test(sample,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
    ad <- ad.test(sample,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
    D <- 1 - plogspline(ks$statistic, fit)
    fits["beta"]<-list(fit.beta)
    kstests["beta"]<-list(ks)
    adtests["beta"]<-list(ad)
    Ds["beta"]<-D
  }
  
  #WEIBULL
  if ("weibull" %in% distributionList)
  {
    
    
    fit.weibull <- fitdist(sample, "weibull")
    fit.weibull$estimate
    fit.weibull$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {      
      r <- rweibull(n = length(sample)
                    , shape= fit.weibull$estimate["shape"]
                    , scale = fit.weibull$estimate["scale"]
      )
      as.numeric(ks.test(r
                         , "pweibull"
                         , shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])$statistic
      )      
    })
    
    fit <- logspline(stats)
    ks <- ks.test(sample, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])
    ad <- ad.test(sample, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])
    D <- 1 - plogspline(ks$statistic, fit)
    fits["weibull"]<-list(fit.weibull)
    kstests["weibull"]<-list(ks)
    adtests["weibull"]<-list(ad)
    Ds["weibull"]<-D
    
  }
  
  #ESPONENZIALE
  if ("exp" %in% distributionList)
  {
    fit.exp<-fitdist(sample,"exp",method = c("mle"),lower=0.1)
    #plot(fit.exp)
    fit.exp$estimate
    fit.exp$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rexp(n = length(sample), rate = fit.exp$estimate[1]  )
      as.numeric(ks.test(r, "pexp", rate = fit.exp$estimate[1])$statistic
      )      
    })
    
    fit <- logspline(stats)
    ks <- ks.test(sample,"pexp",rate = fit.exp$estimate[1] )
    ad <- ad.test(sample,"pexp",rate = fit.exp$estimate[1] )
    D <- 1 - plogspline(ks$statistic, fit)
    fits["exp"]<-list(fit.exp)
    kstests["exp"]<-list(ks)
    Ds["exp"]<-D
    
  }
  
  #GEOMETRICA
  if ("geom" %in% distributionList & all(sample==floor(sample)))
  {
    fit.geom<-fitdist(sample,"geom",method = c("mle"))
    #plot(fit.exp)
    fit.geom$estimate
    fit.geom$aic
    
    n.sims <- 1000
    stats <- replicate(n.sims, {   
      r <- rgeom(n = length(sample), prob = fit.geom$estimate[1]  )
      as.numeric(ks.test(r, "pgeom", prob = fit.geom$estimate[1])$statistic
      )      
    })
    
    fit <- logspline(stats)
    ks <- ks.test(sample,"pgeom",prob = fit.geom$estimate[1] )
    ad <- ad.test(sample,"pgeom",prob = fit.geom$estimate[1] )
    D <- 1- plogspline(ks$statistic, fit)
    fits["geom"]<-list(fit.geom)
    kstests["geom"]<-list(ks)
    adtests["geom"]<-list(ad)
    Ds["geom"]<-D
  }
  
  
  #LOG LOGISTICA
  if ("llogis" %in% distributionList)
  {
    #nella distribuzione lloglogistica, i parametri devo essere positivi
    fit.llogis<- fitdist(sample ,"llogis", start = list(shape = 1))
    #plot(fit.llogis)
    fit.llogis$estimate
    fit.llogis$aic
    
    n.sims <- 2000
    stats <- replicate(n.sims, {   
      r <- rllogis(n = length(sample), shape = fit.llogis$estimate[1]  )
      as.numeric(ks.test(r, "pllogis",  shape = fit.llogis$estimate[1])$statistic
      )      
    })
    
    fit <- logspline(stats)
    ks <- ks.test(sample,"pllogis", shape = fit.llogis$estimate[1] )
    ad <- ad.test(sample,"pllogis", shape = fit.llogis$estimate[1] )
    D <- 1- plogspline(ks$statistic, fit)
    fits["llogis"]<-list(fit.llogis)
    kstests["llogis"]<-list(ks)
    adtests["llogis"]<-list(ad)
    Ds["llogis"]<-D
  }
  
  
  #POWERLAW FOR CONTINUOUS ATTRIBUTE
  if ("pareto" %in% distributionList)
  {
    fit.pareto <- fitdist(sample, "pareto1", lower = c(0, 0), start = list(min = unname(quantile(sample, .1)), shape = 1))
    fit.pareto$estimate
    fit.pareto$aic
    
    ks.test(sample,"ppareto1", min=fit.pareto$estimate[1] , shape = fit.pareto$estimate[2])
    
    n.sims <- 200
    stats <- replicate(n.sims, {   
      r <- rpareto(n = length(sample), shape = fit.pareto$estimate[1]  )
      as.numeric(ks.test(r, "ppareto", scale=fit.pareto$estimate[1] , shape = fit.pareto$estimate[2])$statistic
      )      
    })
    
    fit <- logspline(stats)
    ks <- ks.test(sample,"ppareto", scale=fit.pareto$estimate[1] , shape = fit.pareto$estimate[2])
    ad <- ad.test(sample,"ppareto", scale=fit.pareto$estimate[1] , shape = fit.pareto$estimate[2])
    D <- 1- plogspline(ks$statistic, fit)
    
    fits["pareto"]<-list(fit.pareto)
    kstests["pareto"]<-list(ks)
    adtests["pareto"]<-list(ad)
    Ds["pareto"]<-D
  }
  if ("beta" %in% distributionList)
    gof<-list(gofstat(fits[- which(names(fits)=="beta")]))
  else
    gof<-list(gofstat(fits))
  return (list("fits"=fits,"ks"= kstests,"ad" = adtests, "D" = Ds, "gof"=gof))

  
}


sampleAndTest <-function(data) {
  #data is a data structure(eg: list, dataframe) containing R and E columns
  samples <- results <- list()
  for(i in seq(100, 1000, 100)) {
    samples[paste(toString(i), "R")]<-list(sample(subset(data$R+.1, data$R>=0), i))
    samples[paste(toString(i), "E")]<-list(sample(subset(data$E+1, data$E>=0), i))
    results[paste(toString(i), "R")]<-list(fitDistributions(samples[paste(toString(i), "R")][[1]]))
    results[paste(toString(i), "E")]<-list(fitDistributions(samples[paste(toString(i), "E")][[1]]))
  }
  for(i in seq(1500, 10000, 500)) {
    samples[paste(toString(i), "R")]<-list(sample(subset(data$R+.1, data$R>=0), i))
    samples[paste(toString(i), "E")]<-list(sample(subset(data$E+1, data$E>=0), i))
    results[paste(toString(i), "R")]<-list(fitDistributions(samples[paste(toString(i), "R")][[1]]))
    results[paste(toString(i), "E")]<-list(fitDistributions(samples[paste(toString(i), "E")][[1]]))
  }
  
  return (results)
}
