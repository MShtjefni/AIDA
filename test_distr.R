distr <- c("norm", "lnorm", "gamma", "beta", "weibull","exp","logis",  "pareto","cauchy" ,"llogis")

fitDistributions <- function(sample, distributionList=distr, nSims=0){
  if(! length(distributionList))
    return (NULL)
  fits <- kstests <- adtests <- Ds <- list()
  if ("norm" %in% distributionList) {
    print ("fitting norm...")
    fit.norm <-fitdist(sample, "norm",method = c("mle"))
    #plot(fit.norm)
    fit.norm$estimate
    fit.norm$aic
    
    stats <- replicate(nSims, {
      r <- rnorm(n = length(sample), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
      as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
      )      
    })

    ks<-ks.test(sample,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
    ad<-ad.test(sample,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])    
    
    if(nSims) {
      fit <- logspline(stats)
      D<-1 - plogspline(ks$statistic, fit)
      Ds["norm"]<-D
    }
    
    #boot <- bootdist(fit.norm, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.norm$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    
    
    fits["norm"]<-list(fit.norm)
    kstests["norm"]<-list(ks)
    adtests["norm"]<-list(ad)
    
    
  }
  #DISTRIBUZIONE LOGNORM
  if ("lnorm" %in% distributionList) {
    print ("fitting lognorm...")
    fit.lnorm<-fitdist(sample,"lnorm",method = c("mle"))
    #plot(fit.lnorm)
    fit.lnorm$estimate
    fit.lnorm$aic
    
    #a = bootdist(sample.lnorm, niter=100, ncpus  = 4,bootmethod="param")
    #quantile(a,prob=0.5)
    
    stats <- replicate(nSims, {   
      r <- rlnorm(n = length(sample), meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2]  )
      as.numeric(ks.test(r, "plnorm", meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])$statistic
      )      
    })
    
    ks <- ks.test(sample,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])
    ad <- ad.test(sample,"plnorm",meanlog = fit.lnorm$estimate[1] , sdlog = fit.lnorm$estimate[2])
    if(nSims) {
      fit <- logspline(stats)
      D <- 1 - plogspline(ks$statistic, fit)
      Ds["lnorm"]<-D
    }
    
    #boot <- bootdist(fit.lnorm, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.lnorm$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    fits["lnorm"]<-list(fit.lnorm)
    kstests["lnorm"]<-list(ks)
    adtests["lnorm"]<-list(ad)
   
    
  }
  
  #GAMMA DISTRIBUTION 
  if ("gamma" %in% distributionList)
  {
    print ("fitting gamma...")
    fit.gamma<-fitdist(sample,"gamma",method = c("mle"),lower=0)
    #plot(fit.gamma)
    fit.gamma$estimate
    fit.gamma$aic
    stats <- replicate(nSims, {   
      r <- rgamma(n = length(sample), shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2]  )
      as.numeric(ks.test(r, "pgamma", shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])$statistic
      )      
    })
    
    ks <- ks.test(sample,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])
    ad <- ad.test(sample,"pgamma",shape = fit.gamma$estimate[1] , rate = fit.gamma$estimate[2])
    if(nSims) {
      fit <- logspline(stats)
      D <- 1 - plogspline(ks$statistic, fit)
      Ds["gamma"]<-D
    }
    
    #boot <- bootdist(fit.gamma, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.gamma$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    fits["gamma"]<-list(fit.gamma)
    kstests["gamma"]<-list(ks)
    adtests["gamma"]<-list(ad)
  }
  
  #BETA
  if ("beta" %in% distributionList)
  {
    print ("fitting beta...")
    betaSample<-sample/(max(sample)+.1)
    fit.beta<-fitdist(betaSample, "beta")
    #plot(fit.beta)
    fit.beta$estimate
    fit.beta$aic
   
    stats <- replicate(nSims, {   
      r <- rbeta(n = length(sample), shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
      as.numeric(ks.test(r, "pbeta", shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])$statistic
      )      
    })
    
    ks <- ks.test(sample,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
    ad <- ad.test(sample,"pbeta",shape1 = fit.beta$estimate[1] , shape2 = fit.beta$estimate[2])
    if(nSims) {
      fit <- logspline(stats)
      D <- 1 - plogspline(ks$statistic, fit)
      Ds["beta"]<-D
    }
    #boot <- bootdist(fit.beta, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.beta$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    fits["beta"]<-list(fit.beta)
    kstests["beta"]<-list(ks)
    adtests["beta"]<-list(ad)
    
  }
  
  #WEIBULL
  if ("weibull" %in% distributionList)
  {
    print ("fitting weibull...")
    
    fit.weibull <- fitdist(sample, "weibull")
    fit.weibull$estimate
    fit.weibull$aic
    
    stats <- replicate(nSims, {      
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
    
    ks <- ks.test(sample, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])
    ad <- ad.test(sample, "pweibull", shape= fit.weibull$estimate["shape"],scale = fit.weibull$estimate["scale"])
    if(nSims) {
      fit <- logspline(stats)
      D <- 1 - plogspline(ks$statistic, fit)
      Ds["weibull"]<-D
    }
    #boot <- bootdist(fit.weibull, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.weibull$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    fits["weibull"]<-list(fit.weibull)
    kstests["weibull"]<-list(ks)
    adtests["weibull"]<-list(ad)
    
    
  }
  
  #ESPONENZIALE
  if ("exp" %in% distributionList)
  {
    print ("fitting exp...")
    fit.exp<-fitdist(sample,"exp",method = c("mle"),lower=0.01)
    #plot(fit.exp)
    
    stats <- replicate(nSims, {   
      r <- rexp(n = length(sample), rate = fit.exp$estimate[1]  )
      as.numeric(ks.test(r, "pexp", rate = fit.exp$estimate[1])$statistic
      )      
    })
    
    ks <- ks.test(sample,"pexp",rate = fit.exp$estimate[1] )
    ad <- ad.test(sample,"pexp",rate = fit.exp$estimate[1] )
    if(nSims) {
      fit <- logspline(stats)
      D <- 1 - plogspline(ks$statistic, fit)
      Ds["exp"]<-D
    }
    
    #boot <- bootdist(fit.exp, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.exp$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    fits["exp"]<-list(fit.exp)
    kstests["exp"]<-list(ks)
  }
  
  #GEOMETRICA
  if ("geom" %in% distributionList & all(sample==floor(sample)))
  {
    print ("fitting geom...")
    fit.geom<-fitdist(sample,"geom",method = c("mle"))
    #plot(fit.exp)
    fit.geom$estimate
    fit.geom$aic
    
    stats <- replicate(nSims, {   
      r <- rgeom(n = length(sample), prob = fit.geom$estimate[1]  )
      as.numeric(ks.test(r, "pgeom", prob = fit.geom$estimate[1])$statistic
      )      
    })
    
    ks <- ks.test(sample,"pgeom",prob = fit.geom$estimate[1] )
    ad <- ad.test(sample,"pgeom",prob = fit.geom$estimate[1] )
    if(nSims) {
      fit <- logspline(stats)
      D <- 1- plogspline(ks$statistic, fit)
      Ds["geom"]<-D
    }
    
    #boot <- bootdist(fit.geom, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.geom$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    fits["geom"]<-list(fit.geom)
    kstests["geom"]<-list(ks)
    adtests["geom"]<-list(ad)
  }
  
  
  #LOG LOGISTICA
  if ("llogis" %in% distributionList)
  {
    print ("fitting Log logis...")
    #nella distribuzione lloglogistica, i parametri devo essere positivi
    fit.llogis<- fitdist(sample ,"llogis", start = list(shape = 1))
    #plot(fit.llogis)
    fit.llogis$estimate
    fit.llogis$aic
    
    stats <- replicate(nSims, {   
      r <- rllogis(n = length(sample), shape = fit.llogis$estimate[1]  )
      as.numeric(ks.test(r, "pllogis",  shape = fit.llogis$estimate[1])$statistic
      )      
    })
    
    ks <- ks.test(sample,"pllogis", shape = fit.llogis$estimate[1] )
    ad <- ad.test(sample,"pllogis", shape = fit.llogis$estimate[1] )
    if(nSims) {
      fit <- logspline(stats)
      D <- 1- plogspline(ks$statistic, fit)
      Ds["llogis"]<-D
    }
    #boot <- bootdist(fit.llogis, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.llogis$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    fits["llogis"]<-list(fit.llogis)
    kstests["llogis"]<-list(ks)
    adtests["llogis"]<-list(ad)
    
  }
  
  
  #POWERLAW FOR CONTINUOUS ATTRIBUTE
  if ("pareto" %in% distributionList)
  {
    print ("fitting Pareto...")
    fit.pareto <- fitdist(sample,"pareto",lower = c(0, 0), start = list(scale = min(sample), shape = 1))
    #fit.pareto <- mle2(LL3(myData = sample), start=list(m=0.001, s=1.001), method = "L-BFGS-B", lower = c(m=0.001, s=1.001))
    fit.pareto$estimate
    fit.pareto$aic
    
    ks.test(sample,"ppareto", scale=fit.pareto$estimate[1] , shape = fit.pareto$estimate[2])
    
    stats <- replicate(nSims, {   
      r <- rpareto(n = length(sample), shape = fit.pareto$estimate[1]  )
      as.numeric(ks.test(r, "ppareto", scale=fit.pareto$estimate[1] , shape = fit.pareto$estimate[2])$statistic
      )      
    })
    
    ks <- ks.test(sample,"ppareto", scale=fit.pareto$estimate[1] , shape = fit.pareto$estimate[2])
    ad <- ad.test(sample,"ppareto", scale=fit.pareto$estimate[1] , shape = fit.pareto$estimate[2])
    if(nSims) {
      fit <- logspline(stats)
      D <- 1- plogspline(ks$statistic, fit)
      Ds["pareto"]<-D
    }
    #boot <- bootdist(fit.pareto, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.pareto$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    fits["pareto"]<-list(fit.pareto)
    kstests["pareto"]<-list(ks)
    adtests["pareto"]<-list(ad)
    
    "if (all(sample==floor(sample)))
      pl<-displ$new(sample)
    else
      pl<-conpl$new(sample)
    pl$setXmin(estimate_xmin(pl))
    "
  }

  # LAPLACE
  if ("laplace" %in% distributionList) {
    print ("fitting laplace...")
    fit.laplace <-fitdist(sample, "laplace",method = c("mle"), start=list(location=1, scale=0.001))
    #plot(fit.norm)
    
    stats <- replicate(nSims, {
      r <- rlaplace(n = length(sample), location = fit.laplace$estimate["location"], scale = fit.laplace$estimate["scale"])
      as.numeric(ks.test(r, "plaplace", location = fit.laplace$estimate["location"], scale = fit.laplace$estimate["scale"])$statistic
      )      
    })
    
    ks<-ks.test(sample, "plaplace", location = fit.laplace$estimate["location"], scale = fit.laplace$estimate["scale"])
    ad<-ad.test(sample, "plaplace", location = fit.laplace$estimate["location"], scale = fit.laplace$estimate["scale"])
    
    if(nSims) {
      fit <- logspline(stats)
      D<-1 - plogspline(ks$statistic, fit)
      Ds["laplace"]<-D
    }
    
    #boot <- bootdist(fit.norm, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.norm$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    
    fits["laplace"]<-list(fit.laplace)
    kstests["laplace"]<-list(ks)
    adtests["laplace"]<-list(ad)
    
  }
  
  #CAUCHY 
  if ("cauchy" %in% distributionList) {
    print ("fitting cauchy...")
    fit.cauchy <-fitdist(sample, "cauchy",method = c("mle"))
    #plot(fit.norm)
    
    stats <- replicate(nSims, {
      r <- rcauchy(n = length(sample), fit.cauchy$estimate[1], fit.cauchy$estimate[2])
      as.numeric(ks.test(r, "pcauchy", fit.cauchy$estimate[1], fit.cauchy$estimate[2])$statistic
      )      
    })
    
    ks<-ks.test(sample, "pcauchy", fit.cauchy$estimate[1], fit.cauchy$estimate[2])
    ad<-ad.test(sample, "pcauchy", fit.cauchy$estimate[1], fit.cauchy$estimate[2])
    
    if(nSims) {
      fit <- logspline(stats)
      D<-1 - plogspline(ks$statistic, fit)
      Ds["cauchy"]<-D
    }
    
    #boot <- bootdist(fit.norm, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.norm$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    
    fits["cauchy"]<-list(fit.cauchy)
    kstests["cauchy"]<-list(ks)
    adtests["cauchy"]<-list(ad)
    
  }
  
  #LOGIS 
  if ("logis" %in% distributionList) {
    print ("fitting logis...")
    fit.logis <-  fitdist(sample, distr="logis", method="mle")

    #plot(fit.norm)
    
    stats <- replicate(nSims, {
      r <- rcauchy(n = length(sample), fit.logis$estimate[1], fit.logis$estimate[2])
      as.numeric(ks.test(r, "plogis", fit.logis$estimate[1], fit.logis$estimate[2])$statistic
      )      
    })
    
    ks<-ks.test(sample, "plogis", fit.logis$estimate[1], fit.logis$estimate[2])
    ad<-ad.test(sample, "plogis", fit.logis$estimate[1], fit.logis$estimate[2])
    
    if(nSims) {
      fit <- logspline(stats)
      D<-1 - plogspline(ks$statistic, fit)
      Ds["logis"]<-D
    }
    
    #boot <- bootdist(fit.norm, bootmethod = "param", niter = 1000, ncpus=6) #uses parametric bootsrap to generate 
    # 1000 samples and compute their parameters according to the given distribution
    #fit.norm$CI<-boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters
    
    fits["logis"]<-list(fit.logis)
    kstests["logis"]<-list(ks)
    adtests["logis"]<-list(ad)
    
  }
  
  if ("beta" %in% distributionList)
    gof<-list(gofstat(fits[- which(names(fits)=="beta")]))
  else
    gof<-list(gofstat(fits))
  return (list("sample"=sample, "fits"=fits,"ks"= kstests,"ad" = adtests, "D" = Ds, "gof"=gof))
  
}

getSortedPValue<-function(resultSet, testColumn='ks') {
  pValues<-c()
  for (distribution in resultSet[[testColumn]]) {
    pValues<-append(pValues,distribution$p.value)
  }
  names(pValues)<-names(resultSet[[testColumn]])
  return (pValues[-4])
}


getSortedD<-function(resultSet, testColumn='ks') {
  pValues<-c()
  for (distribution in resultSet[[testColumn]]) {
    pValues<-append(pValues,distribution$statistic[1])
  }
  names(pValues)<-names(resultSet[[testColumn]])
  return (pValues[-4])
}

getSortedGof<-function(resultSet, measureColumn='aic') {
  gof<-sort(resultSet$gof[[1]][[measureColumn]])
  #names(pValues)<-names(resultSet[[testColumn]])
  return (gof)
  
}

sampleAndTest <-function(data, ER=T, growth=F) {
  #data is a data structure(eg: list, dataframe) containing R and E columns
  samples <- results <- list()
  if(growth)
    if (! "Growth" %in% colnames(data))
      print ("Growth is not actually present in data. Calculating it.") + data<-getGrowth(data)
  for(i in c(100, 200, 300, 500, 750, 1000, 1500, 2000, 3000, 5000, 10000, 20000, 40000, 80000, 150000, 350000, 700000) ) {
    if (ER && i<=min(nrow(subset(data, data$R>=0)), nrow(subset(data, data$E>=0)))) {
      print (paste("testing R and E distribution on", toString(i), "elements."))
      sample<-sample_n(subset(data, R>=0, E>=0), i)
      sampleR<-sample$R+.1
      sampleE<-sample$E+1
      #results[paste(toString(i), "R")]<-list(list("sample"=list(sampleR), "results"=list(fitDistributions(sampleR, nSims = 200))))
      #results[paste(toString(i), "E")]<-list(list("sample"=list(sampleE), "results"=list(fitDistributions(sampleE, nSims = 200))))
      results[paste(toString(i), "R")]<-list(fitDistributions(sampleR))
      results[paste(toString(i), "E")]<-list(fitDistributions(sampleE))
    }
    
    
    if (growth && i<=nrow(subset(data, !is.na(Growth)))) {
      sampleG<-sample(subset(data$Growth, !is.na(data$Growth)), i)
      results[paste(toString(i), "G")]<-list(fitDistributions(sampleG, nSims = 200, distributionList = c("cauchy", "laplace", "logis")))
    }
    
  }
  return (results)
}


getMinSampleSize<-function(samples, pValue=.05, colName='R') {
  
  if (colName=='R')
    l<-seq(1, length(samples),2)
  else
    l<-seq(2, length(samples),2)
  for (i in l) {
    pV<-getSortedPValue(samples[[i]])
    if (max(pV)>=pValue)
      s<-i
  }
  if(exists("s"))
    return (names(samples)[s])
  return (NULL)
}



'
{
aidaInactive<-subset(aida, aida$Status %in% 
  c("Bankruptcy","Dissolved (liquidation)", "Dissolved", "Dissolved (merger)", "Dissolved (demerger)", "Dissolved (bankruptcy)", "In liquidation"))

aidaActive<-subset(aida, aida$Status %in% 
  c("Active", "Active (default of payments)", "Active (receivership)"))

aidaInact<-sampleAndTest(aidaInactive)
save(aidaInact, file=paste(wdir, "files/aidaInact.RData", sep=""))
aidaAct<-sampleAndTest(aidaActive)
save(aidaAct, file=paste(wdir, "files/aidaAct.RData", sep=""))
aidaS<-sampleAndTest(aida)
save(aidaS, file=paste(wdir, "files/aidaS.RData", sep=""))
aida7<-sampleAndTest(subset(aida, Year==2007))
save(aida7, file=paste(wdir, "files/aida7.RData", sep=""))
aida8<-sampleAndTest(subset(aida, Year==2008))
save(aida8, file=paste(wdir, "files/aida8.RData", sep=""))
aida9<-sampleAndTest(subset(aida, Year==2009))
save(aida9, file=paste(wdir, "files/aida9.RData", sep=""))
aida10<-sampleAndTest(subset(aida, Year==2010))
save(aida10, file=paste(wdir, "files/aida10.RData", sep=""))
aida11<-sampleAndTest(subset(aida, Year==2011))
save(aida11, file=paste(wdir, "files/aida11.RData", sep=""))
aida12<-sampleAndTest(subset(aida, Year==2012))
save(aida12, file=paste(wdir, "files/aida12.RData", sep=""))
aida13<-sampleAndTest(subset(aida, Year==2013))
save(aida13, file=paste(wdir, "files/aida13.RData", sep=""))
aida14<-sampleAndTest(subset(aida, Year==2014))
save(aida14, file=paste(wdir, "files/aida14.RData", sep=""))
aida15<-sampleAndTest(subset(aida, Year==2015))
save(aida15, file=paste(wdir, "files/aida15.RData", sep=""))

manS<-sampleAndTest(manufacturing)
save(manS, file=paste(wdir, "files/manS.RData", sep=""))
man7<-sampleAndTest(subset(manufacturing, manufacturing$Year==2007))
save(man7, file=paste(wdir, "files/man7.RData", sep=""))
man8<-sampleAndTest(subset(manufacturing, manufacturing$Year==2008))
save(man8, file=paste(wdir, "files/man8.RData", sep=""))
man9<-sampleAndTest(subset(manufacturing, manufacturing$Year==2009))
save(man9, file=paste(wdir, "files/man9.RData", sep=""))
man10<-sampleAndTest(subset(manufacturing, manufacturing$Year==2010))
save(man10, file=paste(wdir, "files/man10.RData", sep=""))
man11<-sampleAndTest(subset(manufacturing, manufacturing$Year==2011))
save(man11, file=paste(wdir, "files/man11.RData", sep=""))
man12<-sampleAndTest(subset(manufacturing, manufacturing$Year==2012))
save(man12, file=paste(wdir, "files/man12.RData", sep=""))
man13<-sampleAndTest(subset(manufacturing, manufacturing$Year==2013))
save(man13, file=paste(wdir, "files/man13.RData", sep=""))
man14<-sampleAndTest(subset(manufacturing, manufacturing$Year==2014))
save(man14, file=paste(wdir, "files/man14.RData", sep=""))
man15<-sampleAndTest(subset(manufacturing, manufacturing$Year==2015))
save(man15, file=paste(wdir, "files/man15.RData", sep=""))

alim <- sampleAndTest(subset(manufacturing, manufacturing$Subsector=="alimentari"))
save(alim, file=paste(wdir, "files/alim.RData", sep=""))
auto <- sampleAndTest(subset(manufacturing, manufacturing$Subsector=="autoveicoli"))
save(auto, file=paste(wdir, "files/auto.RData", sep=""))
bev <- sampleAndTest(subset(manufacturing, manufacturing$Subsector=="bevande"))
save(bev, file=paste(wdir, "files/bev.RData", sep=""))
legno <- sampleAndTest(subset(manufacturing, manufacturing$Subsector=="legno"))
save(legno, file=paste(wdir, "files/legno.RData", sep=""))
miner <- sampleAndTest(subset(manufacturing, manufacturing$Subsector=="minerali"))
save(miner, file=paste(wdir, "files/miner.RData", sep=""))
pc <- sampleAndTest(subset(manufacturing, manufacturing$Subsector=="computer"))
save(pc, file=paste(wdir, "files/pc.RData", sep=""))
pelle <- sampleAndTest(subset(manufacturing, manufacturing$Subsector=="pelle"))
save(pelle, file=paste(wdir, "files/pelle.RData", sep=""))
tess <- sampleAndTest(subset(manufacturing, manufacturing$Subsector=="tessile"))
save(tess, file=paste(wdir, "files/tess.RData", sep=""))
}

mediaS<-sampleAndTest(media,growth = F)
save(mediaS, file=paste(wdir, "files/mediaS.RData", sep=""))
restaurS<-sampleAndTest(restaurants)
save(restaurS, file=paste(wdir, "files/restaurS.RData", sep=""))
aidaSmall<-sampleAndTest(subset(aida, Size=="Small"))
save(aidaSmall, file=paste(wdir, "files/aidaSmall.RData", sep=""))
aidaMedium<-sampleAndTest(subset(aida, Size=="Medium"))
save(aidaMedium, file=paste(wdir, "files/aidaMedium.RData", sep=""))
aidaLarge<-sampleAndTest(subset(aida, Size=="Large"))
save(aidaLarge, file=paste(wdir, "files/aidaLarge.RData", sep=""))
'
### P VALUES
### AIDA SMALL: R is gamma(then weibull and lnorm, same as "usually" happens). 
### E is weibull! (then gamma)
### AIDA MEDIUM: R is weibull(then gamma, and lnorm), E is PARETO!
### AIDA LARGE: R is lnorm(then weibull)

### EDIT varName TO INSPECT OTHER RESULTS: EG aidaS, aida7, etc. ###
varName="aidaS"
tryCatch(
  samples<-eval(parse(text=varName)),
error = function(e) {
  load(paste(wdir, "files/", varName, ".RData", sep=""))
  samples<-eval(parse(text=varName))
  }
)

pValues<-aicS<-bicS<-list()
for (sample in samples) {
  pv<-getSortedPValue(sample)
  pValues<-append(pValues,list(pv)) 
  aic<-getSortedGof(sample,measureColumn = 'aic')
  aicS<-append(aicS,list(aic))
  bic<-getSortedGof(sample,measureColumn = 'bic')
  bicS<-append(bicS,list(bic)) }
names(pValues)<-names(aicS)<-names(bicS)<-names(samples) 

## EDIT SAMPLE VAR TO HAVE STATISTICS OF ANOTHER SAMPLE ##
distributionColors<-list("My sample"="red", "norm"="brown","llogis"="blue", "lnorm"="yellow","pareto"="aquamarine4","exp"="black","weibull"="blueviolet","gamma"="green", "laplace"="blue", "cauchy"="yellow", "logis"="green")
sample<-samples$`1000 R`
xlab = 'Revenue'
x<-sample$sample
fits<-sample$fits
hist(x, prob=T, breaks="fd", xlim = c(min(x)-.1*abs(min(x)), max(x)+.1*abs(max(x))), col="red", main="Empirical small growth Rate Distribution")
#plot(density(x),lwd=2, col="red")
makeCurves<-function(distribList, curveType="d",estimatedDistr=fits, mySample=NULL, colors=distributionColors) {
  #distribList is the list of distributions you want to plot
  #curveType is "d" if you want to plot a density function, "p" if you want to plot an ecdf(probability)
  #estimatedDistr is a list of fitted distributions (by fitdist function)
  
  legends<-vector(mode = "character")
  distribList<-rev(distribList)
  if(!is.null(mySample)) {
    if(curveType=="d")
      plot(density(mySample), cex=0.5, lwd=2, col="red", xlab=xlab, main = paste(xlab, "Fits"), xlim = c(min(mySample), max(mySample)-(abs(max(mySample))/3)))
    else if(curveType=="p")
      plot(ecdf(mySample),lwd=2, col="red", xlab=xlab, main = paste(xlab, "Fits"))
    legends<-append(legends,"My sample")
  }
  for (i in (1:length(distribList))) {
    currDistr<-distribList[i]
    if(endsWith(names(distribList)[i], "mle-norm") | "norm" == names(distribList)[i]) {
      curve(do.call(paste(curveType, "norm",sep=""), list(x, estimatedDistr$norm$estimate[1], estimatedDistr$norm$estimate[2])), add=T,col = colors[["norm"]], lwd=2)
      legends<-append(legends,"norm")
    }
    else if(endsWith(names(distribList)[i], "lnorm") | "lnorm" %in% names(distribList)[i]) {
      curve(do.call(paste(curveType, "lnorm",sep=""), list(x, estimatedDistr$lnorm$estimate[1], estimatedDistr$lnorm$estimate[2])), add=T,col = distributionColors$lnorm, lwd=2)
      legends<-append(legends,"lnorm")
    }
    else if(endsWith(names(distribList)[i], "gamma") | "gamma" %in% names(distribList)[i]) {
      curve(do.call(paste(curveType, "gamma",sep=""), list(x, estimatedDistr$gamma$estimate[1], estimatedDistr$gamma$estimate[2])), add=T,col = distributionColors$gamma, lwd=2)
      legends<-append(legends,"gamma")
    }
    else if(endsWith(names(distribList)[i], "weibull") | "weibull" %in% names(distribList[i])) {
      curve(do.call(paste(curveType, "weibull",sep=""), list(x, estimatedDistr$weibull$estimate[1], estimatedDistr$weibull$estimate[2])), add=T,col = distributionColors$weibull, lwd=2)
      legends<-append(legends,"weibull")
    }
    else if(endsWith(names(distribList)[i], "exp") | "exp" %in% names(distribList)[i]) {
      curve(do.call(paste(curveType, "exp",sep=""), list(x, estimatedDistr$exp$estimate[1])), add=T,col = distributionColors$exp, lwd=2)
      legends<-append(legends,"exp")
    }
    else if(endsWith(names(distribList)[i], "llogis") | "llogis" %in% names(distribList)[i]) {
      curve(do.call(paste(curveType, "llogis",sep=""), list(x, estimatedDistr$llogis$estimate[1])), add=T,col = distributionColors$llogis, lwd=2)
      legends<-append(legends,"llogis")
    }
    else if(endsWith(names(distribList)[i], "pareto") | "pareto" %in% names(distribList)[i]) {
      curve(do.call(paste(curveType, "pareto",sep=""), list(x,  estimatedDistr$pareto$estimate[1] , estimatedDistr$pareto$estimate[2])), add=T,col = distributionColors$pareto , lwd=2)
      legends<-append(legends,"pareto")
    }
    else if(endsWith(names(distribList)[i], "laplace") | "laplace" %in% names(distribList)[i]) {
       curve(do.call(paste(curveType, "laplace",sep=""), list(x, estimatedDistr$laplace$estimate[1], estimatedDistr$laplace$estimate[2])), add=T,col = distributionColors$laplace, lwd=2)
       legends<-append(legends,"laplace")
    }
    else if(endsWith(names(distribList)[i], "cauchy") | "cauchy" %in% names(distribList)[i]) {
      curve(do.call(paste(curveType, "cauchy",sep=""), list(x, estimatedDistr$cauchy$estimate[1], estimatedDistr$cauchy$estimate[2])), add=T,col = distributionColors$cauchy, lwd=2)
      legends<-append(legends,"cauchy")
    }
    else if(endsWith(names(distribList)[i], "mle-logis") | "logis" %in% names(distribList)[i]) {
      curve(do.call(paste(curveType, "logis",sep=""), list(x, estimatedDistr$logis$estimate[1], estimatedDistr$logis$estimate[2])), add=T,col = distributionColors$logis, lwd=2)
      legends<-append(legends,"logis")
    }
  }
  legend("topright", legend=legends,
         col=as.character(distributionColors[legends]), lty=1, cex=.8)
}
makeCurves(getSortedGof(sample)[1:3], mySample = x)
makeCurves(getSortedGof(sample)[1:3], mySample = subset(x,x>500))
#makeCurves(getSortedPValue(sample)[1:3])
#BETA SAMPLE (VALUES BETWEEN 0 AND 1)
x2<-x/(max(x)+.1)
hist(x2, prob=T, breaks="fd", xlab="growth_random rate", col="grey", main="Empirical small growth Rate Distribution")
curve(dbeta(x, fits$beta$estimate[1], fits$beta$estimate[2]), add=T,col = "red", lwd=2)

### QQPLOTS
y <- rlnorm(length(x), fits$lnorm$estimate[1], fits$lnorm$estimate[2])
qqplot(y, x, xlab="Theoretical Quantiles", ylab = "Empirical Quantiles")
qqline(rlnorm(length(x), fits$lnorm$estimate[1], fits$lnorm$estimate[2]), col = 2,lwd=2,lty=2, distribution = qlnorm)

#CDF
makeCurves(getSortedGof(sample)[1:3], mySample = x , curveType = "p")

# For fitdistr objects:
boot <- bootdist(fits$norm, bootmethod = "nonparam", niter = 1000, ncpus = 6) #uses parametric bootsrap to generate 
# 1000 samples and compute their parameters according to the given distribution
boot$CI[,-1] # returns the 95% bootstrap CIs for all parameters





#CONFIDENCE INTERVALS FOR PARETO PARAMETERS 
myStat <- function(mySample, column) {
  print("boot...")
  fitdist(mySample[column], "pareto", lower = c(0, 0), 
          start = list(scale = min(mySample[column]), shape = 1))$estimate}
samples1<-boots<-list()
for (i in 
     c(100, 200, 500, 1000, 5000, 10000, 20000, 50000)) {
  sample<-samples1[paste(toString(i), 'E', sep="")]<-c(sample(aida$E+1, i))
  print(paste("Bootstrapping over sample size of: ", i, sep=""))
  boots[paste(toString(i), 'E', sep="")]<-list(boot(sample,statistic = myStat, R = 1000))
}

plotConfidInterv(boots$`10000`$t[,2], myValue = boots$`10000`$t0[2], xtitle = 'Vaffanculo')


# For fitdistr objects:
aidaSample <- sample_n(aida, 10000)
fitpar2<-fitdist(aidaSample$E+1,"pareto", method=c("mle"), lower = c(0, 0), start = list(scale = min(aidaSample$E+1), shape = 1))
boot2 <- bootdist(fitpar2, bootmethod = "param", niter = 1000) #uses parametric bootsrap to generate
