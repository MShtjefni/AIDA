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
  names(t)<-append(names(t)[1:length(t)-1], paste("[",curr-mult/n,", ", max(as.numeric(names(tmp))),"]",sep=""))
  print(names(t[length(t)]))
  return(t)
}

tmp<-expNMapping(aidat$E)
tmpDfZipf<-data.frame('Interval'=factor(replicate(tmp[1], 1), levels = 1:length(tmp)))
if( length(tmp)>1)
  for (i in 2:length(tmp)) 
    tmpDfZipf<-rbind(tmpDfZipf,list('Interval'=replicate(tmp[i],i)))

myLabels = names(tmp)

ggplot(tmpDfZipf, aes(x=Interval, fill=Interval)) + 
  geom_bar(aes(y=prop.table((..count..)/sum(..count..))), position = "dodge") + 
  ggtitle("Number of employees per firm") + 
  xlab("N. of Employees") + ylab ("Percentage by employees") + 
  geom_text(aes(y = prop.table((..count..)/sum(..count..)) / 2, 
                label = paste0(round(prop.table(..count..) * 100, 1), '%')), 
            stat = 'count', position = position_dodge(.9), size = 3) +
  scale_x_discrete(labels = myLabels) + guides(fill=F) + theme(plot.title = element_text(hjust = .5), axis.text.x = element_text(size=7.5))
                                                                                   