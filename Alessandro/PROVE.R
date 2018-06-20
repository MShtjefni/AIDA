#PRIMO TIPO DI TEST
#1-Suppose you have a sample which you suspect is from t-distribution, and has size = n
#2-Estimate the t-distribution parameters from the sample.
#3-Generate M samples of size n from the estimated distribution.
#4-For each sample obtain KS statistics using the estimated distribution as theoretical
#5-Build empirical nonparametric distribution from obtained statistics, e.g. using kernel density estimators
#6-Obtain KS statistics for the original sample and the estimated distribution
#7-Obtain p-value for the KS-stat using the empirical distribution of statistics
#8-Make decision based on confidence level
{
x <- rnorm(1000) #abbiamo una certa distribuzione di cui non conosciamo la sua vera distribuzione
x
#vogliamo vedere se si adatta ad una normale

descdist(x, discrete = FALSE,obs.col="red", boot=1000,obs.pch = 15, boot.col="blue") 
#attraverso descdist individuiamo che la distribuzione a noi sconosciuta è individuata intorno ad una normale

#fittiamo i parametri della nostra ipotetica distribuzione
fit.norm <- fitdist(x,"norm")
fit.norm$estimate #parametri stimati

#DA QUESTO OTTENIAMO UN 0.78522225>0.05 QUINDI POSSIAMO DIRE CHE LA NOSTRA DISTRIBUZIONE E' NORMALE
n.sims <- 5e4
stats <- replicate(n.sims, {
  r <- rnorm(n = length(x), mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])
  as.numeric(ks.test(r, "pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic
  )      
})
stats
fit <- logspline(stats)
1 - plogspline(ks.test(x,"pnorm", mean = fit.norm$estimate["mean"], sd = fit.norm$estimate["sd"])$statistic, fit)

#ADESSO VEDIAMO SE LA NOSTRA DISTRIBUZIONE PUÒ ADATTARSI AD ALTRE. Per lognorm il valore è 0


fit.lnorm <- fitdist(x,"lnorm")
fit.lnorm$estimate #parametri stimati
stats <- replicate(n.sims, {
  r <- rlnorm(n = length(x), meanlog  = fit.lnorm$estimate["meanlog"], sdlog = fit.lnorm$estimate["sdlog"])
  as.numeric(ks.test(r, "plnorm", meanlog  = fit.lnorm$estimate["meanlog"], sdlog = fit.lnorm$estimate["sdlog"])$statistic
  )      
})

fit <- logspline(stats)
1 - plogspline(ks.test(x,"plnorm", meanlog  = fit.lnorm$estimate["meanlog"], sdlog = fit.lnorm$estimate["sdlog"])$statistic, fit)
}

set.seed(3)
x <- rgamma(1e5, 2, .2)
plot(density(x))
x
