head(cars)
linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)
summary(linearMod)


# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Build the model on training data
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

summary (lmMod)  # model summary


actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
correlation_accuracy
head(actuals_preds)

#### vecchio ####
#trasformo year in numeric per la selezione
manufacturing$Year =  as.numeric(as.character(manufacturing$Year))

#trasformo year in numeric per la selezione
manufacturing$Year =  as.numeric(as.character(manufacturing$Year))

manufacturing = sample.rows(aida,35000,replace = FALSE)
x = aida[c("E","R","B","P")]
y = aida[c("Year","E","R","B","P")]
y$E=y$E+1

View(aida)
plot(x$E,x$R)
abline(lm(x$R[x$Year==2007]~x$E[x$Year==2007]),col='red',lwd=2)


r<- by(x, x$Year, FUN = function(x) cor(x, use = "pairwise",method = "pearson"))

c = cor(x, use = "pairwise",method = "pearson")
corrplot(c,method = "number", type="upper", order="hclust")

#matrice delle correlazioni per year per E, R, P, B
par(mfrow= c(3,3))
plotCorr = function(r){
  year=c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
  k=1
  for(i in r){
    i = i[,-1]
    i = i[-1,]
    
    corrplot(i,method = "number", type="upper", order="hclust")
    
    title(main = year[k],line = +3)
    k=k+1
    
  }
}
plotCorr(r)
par(mfrow= c(1,1))

# plot per E-R per ogni Year
par(mfrow= c(3,3))
plotByYear=function(x){
  a=split(x,x$Year)
  year=c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
  k=1
  for(i in a){
    plot(i$E,i$R,xlab = "Employee",ylab = "Revenue")
    title(main = year[k],line = +3)
    k=k+1
  }
}
plotByYear(x)
par(mfrow= c(1,1))