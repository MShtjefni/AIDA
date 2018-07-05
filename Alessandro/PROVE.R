library(MASS)
smoke <- as.numeric(factor(survey$Smoke,levels=c("Never","Occas","Regul","Heavy"))) 
exer <- as.numeric(factor(survey$Exer,levels=c("None","Some","Freq"))) 


m <- cbind(exer, smoke)



require(MASS)
data(birthwt)

birthwt
cor.boot <- function(data, k) cor(data[k,])[1,2]
cor.res <- boot(data=with(birthwt, cbind(lwt, bwt)),statistic=cor.boot, R=500)
bt = boot.ci(cor.res, type="bca")
bt$call

