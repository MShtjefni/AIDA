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
                                                                                   