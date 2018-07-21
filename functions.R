#utilsFile <- "utils.R"
source(paste(wdir, "utils.R", sep=""))
ppareto<-actuar::ppareto
rpareto<-actuar::rpareto
dpareto<-actuar::dpareto
qpareto<-actuar::qpareto

"Apply inflaction function"
applyInflation <- function(data, inflations=c(1, .032, .007, .016, .027, .03, .011, .002, -0.001, -0.001, .011)) {
  if ("Infl" %in% colnames(data)) 
    return (data)
  
  data$Infl <- NA
  for (i in seq(2,9)) {
    inflations[i]<-(inflations[i-1]*(1+inflations[i]))
    print(inflations[i])
    data$Infl[data$Year==2006+i]<-inflations[i]
  }
  data$Infl[is.na(data$Infl)]<-1
  data$P <- data$P/data$Infl
  data$R <- data$R/data$Infl
  data$B <- data$B/data$Infl
  return(data)
}

addGeoArea<-function(data) {
  if ("GeoArea" %in% colnames(data)) 
    return (data)
  #VARIABILE AREA GEOGRAFICA NORD, CENTRO, SUD
  #SUD--> Abruzzo, Basilicata, Calabria, Campania, Molise, Puglia, Sardegna, Sicilia
  data$GeoArea=factor("Sud", levels = c("Sud", "Centro", "Nord"))
  #NORD-->Liguria, Lombardia, Piemonte, Valle d'Aosta, Emilia-Romagna, Friuli-Venezia Giulia, Trentino-Alto Adige, Veneto
  data$GeoArea[data$Region=="Liguria" | data$Region=="Lombardia" |data$Region=="Piemonte" |data$Region=="Valle d'Aosta/Vall?e d'Aoste" |data$Region=="Emilia-Romagna"|data$Region=="Friuli-Venezia Giulia"|data$Region=="Trentino-Alto Adige"|data$Region=="Veneto" ]= "Nord"
  #CENTRO--> Lazio, Marche, Toscana ed Umbria
  data$GeoArea[data$Region=="Lazio" |data$Region=="Marche" |data$Region=="Toscana" |data$Region=="Umbria"  ]= "Centro"
  
  data$GeoArea = as.factor(data$GeoArea)
  return(data)
}

addSize <- function(data) {
  if ("Size" %in% colnames(data)) 
    return (data)
  data$Size <- factor("Medium", levels = c("Medium", "Small", "Large"))
  data$Size[data$E <= 49] <- "Small"
  data$Size[data$E >= 250] <- "Large"
  
  return(data)
}

"Subsectors for manufacturing firms."
addSubsectorColumn <- function(data) {
  subsectors<-list(subsector=c('alimentari','bevande','tabacco','tessile','confezAbbigl','pelle','legno','carta','stampa', 'fabbrCoke','prodChimici','prodFarm','gomma','minerali','metallurgia','prodMetallo','computer','appElettriche','macchinari','autoveicoli', 'mezziTrasp', 'mobili', 'altreManuf', 'riparaz'), min=c(100000,110000,120000,130000,140000,150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000,270000,280000,290000,300000,310000,320000,330000), max=c(110000,120000,130000,140000,150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000,270000,280000,290000,300000,310000,320000,330000,340000))
  data$SubSector=factor(x = replicate(n = nrow(data), NA),levels = subsectors$subsector)
  for(i in seq(length(subsectors$subsector))) #mapping subsector to each row based on Ateco
    data$SubSector[data$Ateco>=subsectors$min[i] & data$Ateco<subsectors$max[i]] <- subsectors$subsector[i]
  return (data) 
}

"...Plotting functions..."

### PLOTTING DENSITY BY GGPLOT
plotDensityGGPlot<-function(data, myValue=F, xtitle='My Data') {
  "plot(density(data))
  abline(v=quantile(data, conf/2), col='red')
  abline(v=quantile(data, 1-(conf/2)), col='red')"
  # subset region and plot
  plt<-ggplot(as.data.frame(data), aes(x=data)) + geom_density(colour = "black", fill = "grey69", size=1) 
  
  if(myValue) {
    d <- ggplot_build(plt)$data[[1]]
    myColor='blue'
    ypos <- d$y[match(x=T, d$x>=myValue)]
    plt <- plt + geom_segment(aes(x=myValue, xend=myValue, y=0, yend=ypos), color=myColor, size=.9) +
      labs(x=xtitle)
  }
  
  plt + geom_density(colour = "black", size=1)
  #return(d)
}

#GGPLOT CI
plotConfidInterv<-function(data, myValue=F, conf=.05, xtitle='data') {
  "plot(density(data))
  abline(v=quantile(data, conf/2), col='red')
  abline(v=quantile(data, 1-(conf/2)), col='red')"
  # subset region and plot
  plt<-ggplot(as.data.frame(data), aes(x=data)) + geom_density(colour = "black", size=1.2) 
  d <- ggplot_build(plt)$data[[1]]
  ypos1 <- d$y[match(x=T, d$x>=quantile(data,conf/2))]
  ypos2 <- d$y[match(x=T, d$x>=quantile(data,1-conf/2))]
  
  plt <- plt + 
    geom_area(data = subset(d, x >= quantile(data,.025) & x <= quantile(data,.975)), aes(x=x, y=y), fill="grey69") +
    geom_segment(aes(x=quantile(data, .025), xend=quantile(data, .025), y=0, yend=ypos1), color="navyblue", size=1.2) + 
    geom_segment(aes(x=quantile(data, .975), xend=quantile(data, .975), y=0, yend=ypos2), color="navyblue", size=1.2)
  if(myValue) {
    myColor='red'
    ypos <- d$y[match(x=T, d$x>=myValue)]
    plt <- plt + geom_segment(aes(x=myValue, xend=myValue, y=0, yend=ypos), color=myColor, size=1.2) +
      labs(x=xtitle)
  }
  plt + geom_density(colour = "black", size=1.2)
}

#DENSITY THROUGH BASE PLOT FUNCTION
plotDensity<-function(data, mode=F, reduceX=0, xtitle='', mainTitle='') {
  d<-density(data)
  x1 <- min(which(d$x >= min(d$x)))  
  x2 <- max(which(d$x <= max(d$x)))
  plot(d, yaxt='n', main = mainTitle, xlab = xtitle, xlim=c(min(d$x), max(d$x)-abs(max(d$x)*reduceX)))
  with(d, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="grey69"))
  if(mode) {
    mode<-getMode(data)
    segments(x0 = mode, x1 = mode, y0 = 0, y1 = getY(d, mode), col = 'blue', lwd=2)
  }
}

expNBinning <- function(data, n=3) {
  #bins a dataset by exponentially increasing size of subsequent intervals
  data<-as.vector(subset(data, !is.na(data)))
  binSize <- n
  lastBinElement<-1
  nrowLstInterval<-as.integer(length(subset(data, data>=0 & data<lastBinElement)))
  t<-list()
  t["[0, 1)"]<-nrowLstInterval
  while(lastBinElement+binSize<=max(data) & round(nrowLstInterval/length(data),1)>0) {
    nrowLstInterval<-as.integer(length(subset(data, data>=lastBinElement & data<lastBinElement+binSize)))
    t[paste0("[",lastBinElement,",",lastBinElement+binSize,")")]<-nrowLstInterval
    lastBinElement<-lastBinElement+binSize
    binSize<-binSize*n
    print(paste0("nrow:", nrowLstInterval, ", lastBinElement: ", lastBinElement, ", binSize: ", binSize))
  }
  nrowLstInterval<-as.integer(length(subset(data, data>=lastBinElement)))
  t[paste0("[>=",lastBinElement,"]")]<-nrowLstInterval
  #t[length(t)]<- as.integer(t[length(t)]) + length(subset(data, data>=lastBinElement+binSize))
  #names(t)<-append(names(t)[1:length(t)-1], paste("[>",lastBinElement-binSize/n -1,"]",sep=""))
  
  return (t)
}

"... densities functions utilities"
getMode <- function(data) { 
  #function to get the most likely value from a dataset
  den <- density(data, kernel=c("gaussian"))
  ( den$x[den$y==max(den$y)] )   
}

getY <- function(dens, x) {
  #function to get the y axis from an x point in a density
  dens$y[match(T, dens$x>=x)] 
}


###