library(MASS)
smoke <- as.numeric(factor(survey$Smoke,levels=c("Never","Occas","Regul","Heavy"))) 
exer <- as.numeric(factor(survey$Exer,levels=c("None","Some","Freq"))) 


m <- cbind(exer, smoke)


