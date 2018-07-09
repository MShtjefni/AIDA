loadPackages <- function(filePath) {

  conn <- file(filePath,open="r")
  listOfPack <-trimws(readLines(conn))
  close(conn)
  remove(conn)
  for (i in which(! listOfPack %in% .packages())){
    tryCatch(library(listOfPack[i],character.only = T), #loads all the libraries needed
    error = function(e) {
      install.packages(toString(listOfPack[i])); #if can't load a library, it'll be downloaded
      library(listOfPack[i],character.only = T)
      } 
    )
  }
}



### PLOTTING BY GGPLOT

plotDensity<-function(data, myValue=F, xtitle='My Data') {
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
  #return(d)
}
