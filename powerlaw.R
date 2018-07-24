wdir <- ""
dataDir <- "data/"
packagesFile <- "packages.txt"
source(paste(wdir, "functions.R", sep="")) ### this also loads every needed package
loadDatasets(paste(wdir,dataDir,sep="")) ###USE THIS IF YOU CURRENTLY HAVEN'T DATASETS IN WORKSPACE


expNMappingOld <- function(data, n=3) {
  #bins a dataset by exponentially increasing size of subsequent intervals
  tmp<-table(data)
  t <- c()
  t["[0, 1)"]<-tmp[1]
  binSize <- n
  lastBinElement<-2
  while(lastBinElement+binSize<=length(tmp)) {
    t[paste("[",lastBinElement-1,",",lastBinElement+binSize-1,")",sep="")]<-sum(tmp[lastBinElement:(lastBinElement+binSize-1)])
    lastBinElement<-lastBinElement+binSize
    binSize<-binSize*n
  }
  t[length(t)]<- t[length(t)] + sum(tmp[lastBinElement:length(tmp)])
  names(t)<-append(names(t)[1:length(t)-1], paste("[>",lastBinElement-binSize/n -1,"]",sep=""))
  print(names(t[length(t)]))
  return(t)
}

hypothesisTestingPL<-function(xmin, alpha1, alpha2, sampleSize=50000) {
  #performs a two-sample ks test over two random power law having same xmin
  #and different alphas(alpha1 and alpha2 respectively)
  s2<-rpareto(sampleSize, scale = xmin, shape = alpha2)
  ks.test(s2, "ppareto",scale=xmin,shape = alpha1)
}

### BINNING AIDA EMPLOYEES OVER INTERVALS WITH SIZES THAT INCREASE BY EXP(3)
{
tmp<-expNBinning(aida$E, n = 3)
tmpDfZipf<-data.frame('Interval'=factor(replicate(as.integer(tmp[1]), 1), levels = 1:length(tmp)))
if( length(tmp)>1)
  for (i in 2:length(tmp)) 
    tmpDfZipf<-rbind(tmpDfZipf,list('Interval'=replicate(as.integer(tmp[i]),i)))
myLabels = names(tmp)

ggplot(tmpDfZipf, aes(x=Interval, fill=Interval)) + 
  geom_bar(aes(y=prop.table((..count..)/sum(..count..))), position = "dodge") + 
  ggtitle("Number of employees per firm") + 
  xlab("N. of Employees") + ylab ("Percentage by employees") + 
  geom_text(aes(y = prop.table((..count..)/sum(..count..)) / 2, 
                label = paste0(round(prop.table(..count..) * 100, 1), '%')), 
            stat = 'count', position = position_dodge(.9), size = 3) +
  scale_x_discrete(labels = myLabels) + guides(fill=F) + theme(plot.title = element_text(hjust = .5), axis.text.x = element_text(size=7))
}
remove(tmp,tmpDfZipf,i, myLabels)                                                                             



#GETTING XMIN VALUE TO BE FIXED AS THE MOST LIKELY ONE
### BY INCREASING SAMPLE SIZE, WE GET BROADER AND BROADER TAILS. HENCE WE HAVE XMINS HIGHER FOR BIGGER SAMPLES!
samplesTotalR<-samplesTotalE<-c()
# HOW DOES THE DISTRIBUTION OF XMIN AND ALPHA CHANGES BY INCREASING SAMPLE SIZE
{
distinctSmplSizes<-c(500,2000, 10000, 50000)
for(smplSize in distinctSmplSizes) {
  samplesTotalR[[toString(smplSize)]]<-samplesTotalE[[toString(smplSize)]]<-list()
  for (i in 1:2000) {
    print(paste("Iteration", i, "over 2000 with size: ", smplSize))
    sampleR<-sample(aida$R, smplSize)
    sampleE<-sample(aida$E, smplSize)
    #plR<-conpl$new(sampleR+.1)
    #plR$setXmin(estimate_xmin(plR))
    plE<-displ$new(sampleE+1)
    plE$setXmin(estimate_xmin(plE))
    #samplesTotalR[[toString(smplSize)]]<-append(samplesTotalR[[toString(smplSize)]],list(plR))
    samplesTotalE[[toString(smplSize)]]<-append(samplesTotalE[[toString(smplSize)]],list(plE))
  }
}
  
### retrieving xmins and alphas of the obtained pl for each sample size
allXMinsE<-allAlphasE<-allXMinsR<-allAlphasR<-list()
for (smplSize in as.character(distinctSmplSizes))  {
  sampleE<-samplesTotalE[[smplSize]]
  #sampleR<-samplesTotalR[[smplSize]]
  xminsE<-xminsR<-alphasE<-alphasR<-c()
  for (sample in sampleE) {
    xminsE<-c(xminsE, sample$xmin)
    alphasE<-c(alphasE,sample$pars)
  }
  allXMinsE[[smplSize]]<-c(xminsE)
  allAlphasE[[smplSize]]<-c(alphasE)
  "for (sample in samplesE) {
    xminsR<-c(xminsR, sample$xmin)
    alphasR<-c(alphasR,sample$pars)
  }"
}
remove(xminsE,xminsR,alphasE,alphasR,smplSize, sample,sampleE,sampleR, plE, i)

#plotting xmins for each sample size
par(mfrow=c(2,2))
for (smplSize in as.character(distinctSmplSizes))  {
  xminsE<-allXMinsE[[smplSize]]
  xmin<-getMode(xminsE) ###xmin=11 for sample size 50k
  d<-density(xminsE)
  x1 <- min(which(d$x >= min(d$x)))  
  x2 <- max(which(d$x <  max(d$x)))
  plot(d, xlim=c(quantile(xminsE,.05), quantile(xminsE,.95)),yaxt='n', main = paste("Xmins distribution -", smplSize, " data"), xlab = "bootstrapped xmins")
  with(d, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="grey69"))
  segments(x0 = xmin, x1 = xmin, y0 = 0, y1 = getY(density(xminsE), xmin), col = 'blue', lwd=2)
}

par(mfrow=c(2,2))
#plotting alphas for each sample size
for (smplSize in as.character(distinctSmplSizes))  {
  alphasE<-allAlphasE[[smplSize]]
  alpha<-getMode(alphasE) ###xmin=11 for sample size 50k
  d<-density(alphasE)
  x1 <- min(which(d$x >= min(d$x)))  
  x2 <- max(which(d$x <  max(d$x)))
  plot(d, xlim=c(quantile(alphasE,.05), quantile(alphasE,.95)),yaxt='n', main = paste("alphas distribution -", smplSize, " data"), xlab = "bootstrapped alphas")
  with(d, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="grey69"))
  segments(x0 = alpha, x1 = alpha, y0 = 0, y1 = getY(density(alphasE), alpha), col = 'blue', lwd=2)
}
remove(xminsE,alphasE,x1,x2,d,xmin,alpha,smplSize)

'
for (name in as.character(c(500, 2000, 10000)))  {
  result <- resultsTotalR[[name]]
  allAlphas<-c()
  for (r in result)
    allAlphas<-c(allAlphas, r$bootstraps$pars)
  
  alpha<-getMode(allAlphas) ###xmin=11 for sample size 50k
  d<-density(allAlphas)
  x1 <- min(which(d$x >= min(d$x)))  
  x2 <- max(which(d$x <= max(d$x)))
  plot(d, xlim=c(quantile(allAlphas,.05), quantile(allAlphas,.95)), yaxt="n", main = paste("Alphas distribution -", name, "data"), xlab = "bootstrapped alphas")
  with(d, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="grey69"))
  segments(x0 = alpha, x1 = alpha, y0 = 0, y1 = getY(density(allAlphas), alpha), col = "blue", lwd=2)
}'
}


# HOW DOES THE DISTRIBUTION OF ALPHA CHANGES BY INCREASING XMIN
{
alphasByXMin<-list()
distinctXMins<-c(1, 5, 50, 1000)
for(xmin in distinctXMins) {
  smplSize<-50000
  alphasByXMin[[toString(xmin)]]<-c()
  for (i in 1:2000) {
    print(paste("Iteration" , i, "over 2000, with xmin:", xmin))
    s<-sample(aida$E, smplSize)
    pl<-displ$new(s+1)
    pl$setXmin(xmin)
    pl$setPars(estimate_pars(pl))
    #bt<-(bootstrap_p(pl, xmins=xmin, threads = 6, no_of_sims = 100))
    alphasByXMin[[toString(xmin)]]<-c(alphasByXMin[[toString(xmin)]],pl$pars)
  }
}

for (name in names(alphasByXMin))  {
  alphas <- alphasByXMin[[name]]
  mode<-getMode(alphas)
  plotDensity(alphas,mode = mode, mainTitle = paste("Alphas distribution with xmin:", name), xtitle = "bootstrapped alphas")
}
remove(s, pl, smplSize, alphas,mode, i, xmin, name, distinctXMins)
}


### A POWER LAW EXAMPLE WITH SAMPLE SIZE 2000 FROM THE ENTIRE AIDA DATASET
{
par(mfrow=c(1,2))
smplSize<-2000
sample<-samplesTotalE[[as.character(smplSize)]][[1]]
x<-sample$dat
xmin<-sample$xmin
alpha<-sample$pars
plotDensity(x, xtitle = "Employees", reduceX = .6)
curve(dpareto(x,  xmin , alpha), add=T,col ='forestgreen' , lwd=2)
legend("topright", legend=c("Power Law fitting"),
       col=c("forestgreen"), lty=1, cex=.45)

plotDensity(subset(x, x>=xmin), xtitle = "Employees with E>=xmin", reduceX = .6)
curve(dpareto(x,  xmin , alpha), add=T,col ='forestgreen' , lwd=2)
legend("topright", legend=c("Power Law fitting"),
       col=c("forestgreen"), lty=1, cex=.45)
### CONFIDENCE INTERVALS FOR XMIN AND ALPHA(SAMPLE SIZE 2000)
bt<-bootstrap_p(sample, threads = 5)
xmins<-c(bt$bootstraps$xmin)
alphas<-c(bt$bootstraps$pars)
tmp1<-plotConfidInterv(xmins,xmin, xtitle = "Bootstrapped xmins for sample size 2k")
tmp2<-plotConfidInterv(alphas,alpha, xtitle = "Bootstrapped alphas for sample size 2k")
plot_grid(tmp1,tmp2)
}
remove(smplSize,sample,x,xmin,alpha,bt,xmins,alphas,tmp1,tmp2)

### POWER LAW FOR E ###

### BY FIRMS SIZE
smallFirms<-subset(aida$E,aida$Size=='Small')
mediumFirms<-subset(aida$E,aida$Size=='Medium')
largeFirms<-subset(aida$E,aida$Size=='Large')

## HISTOGRAMS
par(mfrow=c(1,3))
hist(sample(smallFirms,10000),breaks="fd", xlab="Employees")
#lines(density(smallFirms), col='red', add=T)
hist(sample(mediumFirms,10000),breaks="fd", xlab="Employees")
#lines(density(mediumFirms), col='red', add=T)
hist(sample(largeFirms,10000),breaks="fd", xlab="Employees")
#lines(density(largeFirms), col='red', add=T)

##PL
plSmall<-displ$new(smallFirms+1)
plMedium<-displ$new(mediumFirms+1)
plLarge<-displ$new(largeFirms+1)

plot(plSmall, main="Small firms", xlab="Employees")
plSmall$setPars(estimate_pars(plSmall))
lines(plSmall,col='red',lwd=2)
plot(plMedium, main="Medium firms", xlab="Employees")
plMedium$setPars(estimate_pars(plMedium))
lines(plMedium,col='red',lwd=2)
plot(plLarge, main="Large firms", xlab="Employees")
plLarge$setPars(estimate_pars(plLarge))
lines(plLarge,col='red',lwd=2)

#BY SIZE (SMALL, MEDIUM, LARGE)
smplSize<-10000
samplesSize<-list()
#GENERATING 1000 SAMPLES OF SAMPLE SIZE 10000 AND THEN ESTIMATING POWERLAW PARAMETERS OVER THEM
#IN ORDER TO CHECK OUR SAMPLE IS "GOOD" TO REPRESENT ENTIRE SAMPLES OF SMALL, MEDIUM AND LARGE
for (s in unique(aida$Size)) {
  samplesSize[[s]]<-list()
  for (i in 1:1000) {
    print(paste(s, "...", i))
    smp<-subset(aida$E, aida$Size==s)
    if (length(smp)>smplSize)
      smp<-sample(smp,smplSize)

    pl<-displ$new(smp+1)
    pl$setXmin(estimate_xmin(pl))
    samplesSize[[s]]<-append(samplesSize[[s]],list(pl))
    }
}

#RETRIEVING ESTIMATED XMINS AND ALPHAS FOR SMALL, MEDIUM AND LARGE FIRMS
for(size in names(samplesSize)) {
  str1<-paste("xmins",size,sep="")
  str2<-paste("alphas",size,sep="")
  assign(str1,c())
  assign(str2,c())
  for(s in samplesSize[[size]]) {
    assign(str1,c(eval(parse(text=str1)),s$xmin))
    assign(str2,c(eval(parse(text=str2)),s$pars))
  }
}

par(mfrow=c(1,2))
#PLOT CONFIDENCE INTERVALS FOR XMINS AND ALPHAS FOR EACH FIRMS SIZE
for(size in c("Small","Medium","Large")) {
  str1<-paste("xmins",size,sep="")
  str2<-paste("alphas",size,sep="")
  CIx<-plotConfidInterv(data = eval(parse(text=str1)), myValue = eval(parse(text=str1))[[1]])
  CIalpha<-plotConfidInterv(data = eval(parse(text=str2)), myValue = eval(parse(text=str2))[[1]])
  plot_grid(CIx,CIalpha)
}

par(mfrow=c(1,3))
#PLOT TO SEE HOW ALPHAS AND XMINS ARE DISTRIBUTED OVER SMALL, MEDIUM AND LARGE
for(size in c("Small","Medium","Large")) {
  str1<-paste("xmins",size,sep="")
  str2<-paste("alphas",size,sep="")
  plot(eval(parse(text=str1)), eval(parse(text=str2)), main=size, xlab="xmins", ylab="alphas")
}

#CHECK FOR SOME GOOD SAMPLE IN TERMS OF P-VALUE BY KS-TEST...
p=0
i<-1
size<-'Medium'
while(p<.01) {
  mySample<-samplesSize[[size]][[i]]
  x<-mySample$dat
  xmin<-mySample$xmin
  alpha<-mySample$pars
  ks<-ks.test(x,"ppareto", scale=xmin, shape = alpha)
  p<-ks$p.value
  ks2<-ks.test(subset(x,x>=xmin),"ppareto", scale=xmin, shape = alpha)
  p2<-ks2$p.value
  print(paste("p:",p, ", xmin:", xmin, "p2:", p2))
  i<-i+1
  plot(density(x), main=toString(i))
  curve(dpareto(x,scale = xmin, shape = alpha), add = T, col='blue', main=toString(i))
}
bt<-bootstrap_p(mySample, threads = 6)

largeSample<-samplesSize[['Large']][[1]]
a<-plotConfidInterv(xminsLarge, largeSample$xmin, xtitle = "bootstrapped xmins")
b<-plotConfidInterv(alphasLarge, largeSample$pars, xtitle = "bootstrapped alphas")
plot_grid(a,b)

### BY SETTING XMAX 1-2-3, WE GET 100 BOOTSTRAPPED SAMPLES HAVING XMIN=1 AND ALPHA RANGING BETWEEN 1.6 AND 1.9(approximately). 
### THEN, FOR SOME YEARS WE GET  P-VALUES SUFFICIENTLY HIGH(0.4-0.6) (2011-2015). FOR OTHER YEARS(2007, 2009 e.g.) WE GET P-VALUES ALWAYS 0!
### BY PLOTTING, WE CAN SEE THAT FOR SOME YEARS(2013-2015) IT SEEMS REASONABLE, BUT SOMETIMES WE GET P-VALUE 0 EVEN IF WE HAVE SHAPES THAT ARE LIKELY TO BE PARETIAN!
smp<-samplesSize$Small[[1]]
x<-smp$dat
kurtosis(x) ###VERY HIGH, THAT'S WHY WE HAVE VERY "HEAVY" TAILS IN COMPARISON OF NORMAL DISTRIBUTION (kurtosis=3)
xmin<-smp$xmin
alpha<-smp$pars
plotDensity(x, xtitle = "Employees", mainTitle = "Small firms")
curve(dpareto(x, scale = xmin, shape = alpha), add = T, col="red", lwd=2)
x<-subset(x,x>=xmin)
plot(density(x))
curve(dpareto(x, scale = xmin, shape = alpha), add = T, col="red", lwd=2)



### FURTHER ANALYSIS FOR MEDIUM FIRMS
medium1<-subset(mediumFirms,mediumFirms<=170)
medium2<-subset(mediumFirms,mediumFirms>170)
plMedium1<-displ$new(medium1)
plMedium1$setPars(estimate_pars(plMedium1))
bt1<-bootstrap_p(plMedium1,threads = 7)
xminsM1<-c(bt1$bootstraps$xmin)
alphasM1<-c(bt1$bootstraps$pars)
a<-plotConfidInterv(xminsM1, myValue = plMedium1$xmin, xtitle="xmins")
b<-plotConfidInterv(alphasM1, myValue = plMedium1$pars, xtitle='alphas')
plot_grid(a,b)

plMedium2<-displ$new(medium2)
plMedium2$setPars(estimate_pars(plMedium2))
bt2<-bootstrap_p(plMedium2,threads = 5)
xminsM2<-c(bt2$bootstraps$xmin)
alphasM2<-c(bt2$bootstraps$pars)
a<-plotConfidInterv(xminsM2, myValue = plMedium2$xmin, xtitle="xmins")
b<-plotConfidInterv(alphasM2, myValue = plMedium2$pars, xtitle='alphas')
plot_grid(a,b)



###BY YEAR
#WHOLE DATASET
samplesEYear<-resultsEYear<-c()
for (y in (2007:2015)) {
  print(paste("Fitting power law for Employees over Year:", y))
  s<-subset(aida$E, aida$Year==y)
  pl<-displ$new(s+1)
  pl$setXmin(estimate_xmin(pl))
  samplesEYear[y-2006]<-list(pl)
  #resultsEYear[y-2006]<-list(bootstrap_p(pl,threads = 6))
}

for(i in (1:length(samplesEYear))) {
  par(mfrow=c(1,2))
  sample<-samplesEYear[[i]]
  x<-sample$dat
  xmin<-sample$xmin
  alpha<-sample$pars
  plotDensity(x, reduceX = .4, xtitle="Employees", mainTitle = paste("Pareto fit for year", i+2006))
  curve(dpareto(x,xmin,alpha),add = T, col="blue")
  p<-ks.test(x,"ppareto",scale=xmin,shape=alpha)$p.value
  x<-subset(x,x>=xmin)
  plotDensity(x, reduceX = .4, xtitle="Employees>=xmin", mainTitle = paste("Pareto fit for year", i+2006))
  curve(dpareto(x,xmin,alpha),add = T, col="blue")
  p2<-ks.test(x,"ppareto",scale=xmin,shape=alpha)$p.value
  print(paste("Year:",i+2006, "xmin:", xmin, "alpha:", alpha, ", p:", p, ", p2:", p2))
}





#SUBSET: SAMPLE SIZE=10K
smplSize=10000
xmin<-round(getMode(allXMinsE[[as.character(smplSize)]]))
#ESTIMATE PARAMETERS BY BOOTSTRAP(1000 ITERATIONS) FOR EACH YEAR 
#IN ORDER TO ENSURE WE'RE USING AN XMIN THAT IS "GOOD" FOR EACH OF THEM
for (y in (2007:2015)) {
  allXMinsE[[toString(y)]]<-allAlphasE[[toString(y)]]<-c()
  for (i in(1:1000)) {
    print(paste("Iteration", i, "over 1000 for year ",y))
    s<-sample(subset(aida$E, aida$Year==y), smplSize)
    pl<-displ$new(s+1)
    pl$setXmin(estimate_xmin(pl))
    allXMinsE[[toString(y)]]<-c(allXMinsE[[toString(y)]],pl$xmin)
    allAlphasE[[toString(y)]]<-c(allAlphasE[[toString(y)]], pl$pars)
  }
  plotConfidInterv(allXMinsE[[toString(y)]],myValue = xmin, xtitle = paste("bootstrapped xmins for year ", y))
  print(paste("Year,", y, "xmin:", xmin, ", 95% CI: [", quantile(allXMinsE[[toString(y)]],.025), ", ", quantile(allXMinsE[[toString(y)]],.975), "]"))
}



##ESTIMATING ALPHAS FOR EACH DISTINCT YEAR AND TESTING POWER LAW HYPOTHESIS(FIXED XMIN)
samplesYears<-resultsYears<-list()
for (y in (2007:2015)) {
  s<-sample(subset(aida$E, aida$Year==y), smplSize)
  pl<-displ$new(s+1)
  pl$setXmin(xmin)
  pl$setPars(estimate_pars(pl))
  samplesYears[toString(y)]<-list(pl)
  print(paste("Testing PL for year", y))
  resultsYears[toString(y)]<-list(bootstrap_p(pl, threads = 6, xmins = xmin))
}
### CHECKING ALPHA CIs OF THE SAMPLE USED 
for (y in (2007:2015)) {
  alphas<-resultsYears[[toString(y)]]$bootstraps$pars
  alpha<-samplesYears[[toString(y)]]$pars
  plotConfidInterv(alphas,myValue = xmin, xtitle = paste("bootstrapped xmins for year ", y))
  print(paste("Year", y, ", alpha:", alpha,  ", 95% CI: [", quantile(alphas,.025), ", ", quantile(alphas,.975), "]"))
}

### HYPOTHESIS TESTING OVER THE CHANGE OF ALPHA(H0: ALPHA IS THE SAME BETWEEN TWO YEARS)
for (y in (2007:2014)) {
  print(paste("Years:", y, "-",y+1))
  print(hypothesisTestingPL(xmin = xmin, alpha1 = samplesYears[[toString(y)]]$pars, alpha2 = samplesYears[[toString(y+1)]]$pars))
}
par(mfrow=c(1,2))
for (y in (2007:2015)) {
  sample<-samplesE[[toString(y)]]
  x<-sample$dat
  alpha<-sample$pars
  plotDensity(x, reduceX = .4, xtitle="Employees", mainTitle = paste("Power Law fit for year", y))
  curve(dpareto(x,xmin,alpha),add = T, col="blue")
  legend("topright", legend=c("Power Law"),
         col=c("blue"), lty=1, cex=.4, lwd=1.5)
  p<-ks.test(x,"ppareto",scale=xmin,shape=alpha)$p.value
  x<-subset(x,x>=xmin)
  plotDensity(x, reduceX = .4, xtitle="Employees >= xmin", mainTitle = paste("Power Law fit for year", y))
  curve(dpareto(x,xmin,alpha),add = T, col="blue")
  legend("topright", legend=c("Power Law"),
         col=c("blue"), lty=1, cex=.4, lwd=1.5)
  p2<-ks.test(x,"ppareto",scale=xmin,shape=alpha)$p.value
  print(paste("Year:", y, ", alpha:", alpha, "p:", p, ", p2:", p2))
}
"
##UNFIXED XMIN
samplesE2<-resultsE2<-c()
for (y in (2007:2015)) {
  print(y)
  s<-subset(aida$E, aida$Year==y)
  pl<-displ$new(s+1)
  pl$setXmin(estimate_xmin(pl))
  samplesE2[y-2006]<-list(pl)
}

### POWER LAW FOR R
samplesR<-resultR<-list()
for (y in (2007:2015)) {
  s<-sample(subset(aida$R, aida$Year==y), 1000)
  pl<-conpl$new(s+.1)
  pl$setXmin(estimate_xmin(pl))
  samplesR[toString(y)]<-list(pl)
  resultsR[toString(y)]<-list(bootstrap_p(pl, xmins = min(pl$dat)+.2, xmax = max(pl$dat)*1.1, threads = 6))
}
"
