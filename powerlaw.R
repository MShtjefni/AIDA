expNMapping <- function(colData, n=3) {
  tmp<-table(colData)
  t <- c()
  t["[0, 1)"]<-tmp[1]
  mult <- n
  curr<-2
  while(curr+mult<=length(tmp)) {
    t[paste("[",curr-1,",",curr+mult-1,")",sep="")]<-sum(tmp[curr:(curr+mult-1)])
    curr<-curr+mult
    mult<-mult*n
  }
  t[length(t)]<- t[length(t)] + sum(tmp[curr:length(tmp)])
  names(t)<-append(names(t)[1:length(t)-1], paste("[>",curr-mult/n -1,"]",sep=""))
  print(names(t[length(t)]))
  return(t)
}

expNMapping2 <- function(colData, n=3) {
  colData<-as.vector(subset(colData, !is.na(colData)))
  mult <- n
  curr<-1
  t<-list()
  t["[0, 1)"]<-as.integer(length(subset(colData, colData>=0 & colData<1)))
  while(curr+mult<=max(colData)) {
    t[paste("[",curr,",",curr+mult,")",sep="")]<-as.integer(length(subset(colData, colData>=curr & colData<curr+mult)))
    curr<-curr+mult
    mult<-mult*n
  }
  t[length(t)]<- as.integer(t[length(t)]) + length(subset(colData, colData>=curr+mult))
  names(t)<-append(names(t)[1:length(t)-1], paste("[>",curr-mult/n -1,"]",sep=""))
  
  return (t)
}

getMode <- function(x) {
  den <- density(x, kernel=c("gaussian"))
  ( den$x[den$y==max(den$y)] )   
}

getY <- function(dens, x) {
  dens$y[match(T, dens$x>=x)] 
}

tmp<-expNMapping2(aidat$E, n = 3)
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


                                                                                   

### POWER LAW FOR E

#GETTING XMIN VALUE TO BE FIXED AS THE MOST LIKELY ONE
### BY INCREASING SAMPLE SIZE, WE GET BROADER AND BROADER TAILS. HENCE WE HAVE XMINS HIGHER FOR BIGGER SAMPLES!
samplesTotal2<-resultsTotal2<-c()
for(i in c(100, 200, 500, 1000, 2000, 5000, 10000, 20000)) {
  print(paste("Size:", toString(i)))
  smplSize<-i
  samplesTotal2[[toString(i)]]<-resultsTotal2[[toString(i)]]<-list()
  for (j in 1:20) {
    s<-sample(aida$E, smplSize)
    pl<-displ$new(s+1)
    pl$setXmin(estimate_xmin(pl))
    samplesTotal2[[toString(i)]]<-append(samplesTotal2[[toString(i)]],list(pl))
    resultsTotal2[[toString(i)]]<-append(resultsTotal2[[toString(i)]],list(bootstrap_p(pl, threads = 6, no_of_sims = 100)))
  }
}

for (name in names(resultsTotal2))  {
  result <- resultsTotal2[[name]]
  allXMins2<-allAlphas2<-c()
  for (r in result) {
    allXMins2<-c(allXMins2, r$bootstraps$xmin)
    allAlphas2<-c(allAlphas2, r$bootstraps$pars)
  }
  xmin<-round(getMode(allXMins2)) ###xmin=11 for sample size 50k
  alpha<-getMode(allAlphas2) ###xmin=11 for sample size 50k
  plot(density(allXMins2), xaxp=c(0,150, 30), main = paste("Density of Pareto xmins with", name, " data"), xlab = "bootstrapped xmins")
  segments(x0 = xmin, x1 = xmin, y0 = 0, y1 = getY(density(allXMins2), xmin), col = 'blue', lwd=2)
  plot(density(allAlphas2), main = paste("Density of Pareto alphas with", name, "data"), xlab = "bootstrapped alphas")
  segments(x0 = alpha, x1 = alpha, y0 = 0, y1 = getY(density(allAlphas2), alpha), col = 'blue', lwd=2)
  
}
#BY YEAR
samplesE<-resultsE<-list()
for (y in (2007:2015)) {
  s<-sample(subset(aida$E, aida$Year==y), smplSize)
  pl<-displ$new(s+1)
  pl$setXmin(xmin)
  pl$setPars(estimate_pars(pl))
  samplesE[toString(y)]<-list(pl)
  resultsE[toString(y)]<-list(bootstrap_p(pl, threads = 6, xmins = xmin))
}


#BY SIZE (SMALL, MEDIUM, LARGE)
smplSize<-10000
for (s in unique(aida$Size)) {
  smp<-subset(aida$E, aida$Size==s)
  if (length(smp)>smplSize)
    smp<-sample(smp,smplSize)

  pl<-displ$new(smp+1)
  pl$setXmin(estimate_xmin(pl))
  samplesE[s]<-list(pl)
  resultsE[s]<-list(bootstrap_p(pl, threads = 6))
}

### BY SETTING XMAX 1-2-3, WE GET 100 BOOTSTRAPPED SAMPLES HAVING XMIN=1 AND ALPHA RANGING BETWEEN 1.6 AND 1.9(approximately). 
### THEN, FOR SOME YEARS WE GET  P-VALUES SUFFICIENTLY HIGH(0.4-0.6) (2011-2015). FOR OTHER YEARS(2007, 2009 e.g.) WE GET P-VALUES ALWAYS 0!
### BY PLOTTING, WE CAN SEE THAT FOR SOME YEARS(2013-2015) IT SEEMS REASONABLE, BUT SOMETIMES WE GET P-VALUE 0 EVEN IF WE HAVE SHAPES THAT ARE LIKELY TO BE PARETIAN!
smp<-samplesE$Small
x<-smp$dat
kurtosis(x) ###VERY HIGH, THAT'S WHY WE HAVE VERY "HEAVY" TAILS IN COMPARISON OF NORMAL DISTRIBUTION (kurtosis=3)
xmin<-smp$xmin
alpha<-smp$pars
plot(density(x))
curve(dpareto(x, scale = xmin, shape = alpha), add = T, col="red", lwd=2)
x<-subset(x,x>=xmin)
plot(density(x))
curve(dpareto(x, scale = xmin, shape = alpha), add = T, col="red", lwd=2)

### POWER LAW FOR R
samplesR<-resultR<-list()
for (y in (2007:2015)) {
  s<-sample(subset(aida$R, aida$Year==y), 1000)
  pl<-conpl$new(s+.1)
  pl$setXmin(estimate_xmin(pl))
  samplesR[toString(y)]<-list(pl)
  resultsR[toString(y)]<-list(bootstrap_p(pl, xmins = min(pl$dat)+.2, xmax = max(pl$dat)*1.1, threads = 6))
}
