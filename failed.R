aidaFailed<-subset(aidat,aidat$Status %in% c("Bankruptcy", "Dissolved", "Dissolved (bankruptcy)"))

mean(subset(aidaFailed$R, aidaFailed$R>=0)) #4849.32
mean(subset(aidat$R, aidat$R>=0)) # 3072.71

mean(subset(aidaFailed$P, !is.na(aidaFailed$P))) #--55.15 ->NEGATIVE AVG PROFIT!
mean(subset(aidat$P, !is.na(aidat$P))) #37.53
length(subset(aidaFailed$P,aidaFailed$P>=0)) #406225 rows have positive Profit (49% of total rows of aidaFailed)
length(subset(aidat$P,aidat$P>=0)) #4962601 rows (59.2% that is much more than 49% of aidaFailed)

mean(subset(aidaFailed$E, aidaFailed$E>=0)) #13.26
mean(subset(aidat$E, aidat$E>=0)) #10.86

aidat<-addSize(aidat)
aidaFailed<-addSize(aidaFailed)
nrow(subset(aidaFailed,aidaFailed$Size=='Small')) #632218 -> 76.15% (THAT'S WHY THE AVERAGE REVENUE IS HIGHER THAN THE AIDAT ONE!)
nrow(subset(aidat,aidat$Size=='Small')) #7302797 -> 86.96%

nrow(subset(aidaFailed,aidaFailed$Size=='Medium')) #195230 -> 23.51% (THAT'S WHY THE AVERAGE REVENUE IS HIGHER THAN THE AIDAT ONE!)
nrow(subset(aidat,aidat$Size=='Medium')) #632218 -> 12.64%

nrow(subset(aidaFailed,aidaFailed$Size=='Large')) #2720 -> 0.33% 
nrow(subset(aidat,aidat$Size=='Large')) #33681 -> 0.4%

#ADDING GROWTH
failedGrowth<-getGrowth(aidaFailed)
### ANALYZING THE LAST YEAR(S) OF LIFE OF A FIRM.
aidaFailed<-group_by(aidaFailed,TaxID)
summarize(aidaFailed, lastYear = max(Year))
lastYears<-summarize(aidaFailed, lastYear = max(Year))
table(lastYears$lastYear)
# THE MAJORITY OF FIRMS FAILED BETWEEN 2012-2014. FURTHER ANALYSIS OF THE DISTRIBUTIONS OF SUCH YEARS
hist(lastYears$lastYear, breaks= seq(min(lastYears$lastYear),max(lastYears$lastYear),1))

#2012
aidaFailed12<-subset(aidaFailed,aidaFailed$Year==2012)
mean(subset(aidaFailed12$R, aidaFailed12$R>=0)) #816.54 -> VERY LOW!
mean(subset(aidaFailed12$P, !is.na(aidaFailed12$P))) #-172.42 -> VERY LOW!
failedThisYear<-aidaFailed[aidaFailed$TaxID %in% lastYears$TaxID[lastYears$lastYear==2012] ,]



aidaFailed13<-subset(aidaFailed,aidaFailed$Year==2013)

failedThisYear<-aidaFailed[aidaFailed$TaxID %in% lastYears$TaxID[lastYears$lastYear==2013] ,]




aidaFailed14<-subset(aidaFailed,aidaFailed$Year==2014)