#POWER LAW
ls() #controllo le variabili di ambiente
rm(list = ls()) #rimuoviamo tutte le varaibili d'ambiente
ls() #controllo se sono state eliminate le variabili d'ambiente
library(qcc)
library("poweRlaw")
#For the power law analysis I use poweRlaw library
#Firts step create an object powerLaw with our dataset
#Second step I estimate the xmin for the alpha comupation.
#Third step generate bootstrap from the data and analize if the bootstraping are generated from powerlae distribution

#ESEMPIO LIBRO
{
#MOBY DICK
  {
data("moby", package="poweRlaw")
m_pl = displ$new(moby)
plot(m_pl)
lines(m_pl, col=2)
#stimiamo i parametri della power law
est = estimate_xmin(m_pl)
est
#e aggiorniamo l'oggetto powerlaw
m_pl$setXmin(est)
#il valore di alpha viene calcolato dopo che viene individuato il valore xmin. (esiste una fomula logaritmica)
bs = bootstrap(m_pl, no_of_sims=100, threads=4)
plot(bs, trim=0.1)
bs$p


#per vedere se un certo insieme di dataset ha powerlaw, possiamo usare
bs_p = bootstrap_p(m_pl, no_of_sims=100, threads=4)
bs_p$p
plot(bs_p)
  }
#BLACK OUT
  {
    
    
    blackouts = read.table("blackouts.txt")
  }





}

#For the sample I take the manufacturing sector and the food subsector. 
#I analyze the alpha change in the year 2007-2015
#Some proprierties: if xmin decrease-->aplha decrease

#If xmin is the same and alpha change then the curve is smaller
{
m = displ$new() #create new poweRlaw object
m$setXmin(1)#setxmin equal1
x = 1:20
m$setPars(1.5)
plot(x, dist_pdf(m, x), type="b",col="red" ,ylab="PDF")#pdf
m$setPars(2.0)
lines(x, dist_pdf(m, x), type="b",col="blue")#pdf
m$setPars(2.5)
lines(x, dist_pdf(m, x), type="b",col="green")#pdf
plot(x, dist_cdf(m, x), type="b")
}

#FOOD SUBSECTOR FOR YEARS
manufacturing = get(load("manufacturing.RData"))
manufacturing = manufacturing[manufacturing$SubSector=="Alimentare",]
#View(manufacturing)
nrow(manufacturing)#27468 records

##### POWERLAW FOR YEARS#####
#for 2007
#Continuous power law
#E p.value=0.6 aplha=2.205376 xmin= 78
#R p.value=1 alpha=2.332321 xmin=99240 
#Continuous log-normal
#Continuous exponential
{
E.2007 = manufacturing$E[manufacturing$Year==2007]
R.2007 = manufacturing$R[manufacturing$Year==2007]

#Employee 2007 p.value=0.6 aplha=2.205376 xmin= 78
tb.E.2007 = table(E.2007)
plot(names(tb.E.2007),tb.E.2007,log="xy",xlab="Employee",ylab="Freq")
m_bl_E.2007 = conpl$new(E.2007)#create ppwerlaw object
plot(m_bl_E.2007)
est = estimate_xmin(m_bl_E.2007)#estimate the xmin for the alpha calculation
m_bl_E.2007$setXmin(est)#add xmin to power law object
m_bl_E.2007$xmin #min
m_bl_E.2007$pars #alpha
plot(m_bl_E.2007,xlab="Employee",ylab="CDF") #log-log plot 
lines(m_bl_E.2007)


plot(m_bl_E.2007$dat,dist_cdf(m_bl_E.2007,m_bl_E.2007$dat), type="b")#PDF

bs_p_E.2007 = bootstrap_p(m_bl_E.2007,no_of_sims=100, threads=4)
plot(bs_p_E.2007, trim=0.1)
bs_p_E.2007$p


#Revenue 2007 p.value=1 alpha=2.332321 xmin=99240 it's not power law because
tb.R.2007= table(round(R.2007))
plot(names(tb.R.2007),tb.R.2007,log="xy",xlab="Revenue",ylab="Freq")
m_bl_R.2007 = conpl$new(round(R.2007))#create ppwerlaw object
est = estimate_xmin(m_bl_R.2007)#estimate the xmin for the alpha calculation
m_bl_R.2007$setXmin(est)#add xmin to power law object
m_bl_R.2007$xmin #min
m_bl_R.2007$pars #alpha
plot(m_bl_R.2007,xlab="Revenue",ylab="CDF")
lines(m_bl_R.2007)

bs_p_R.2007 = bootstrap_p(m_bl_R.2007,no_of_sims=100, threads=4)
plot(bs_p_R.2007, trim=0.1)
bs_p_R.2007$p

}

#for 2008
#E p.value=0.66 aplha=2.244039 xmin=74 
#R p.value=1 alpha=2.247032 xmin=98234
{
E.2008 = manufacturing$E[manufacturing$Year==2008]
R.2008 = manufacturing$R[manufacturing$Year==2008]

#Employee 2008 p.value=0.66 aplha=2.244039 xmin=74 
tb.E.2008 = table(E.2008)

plot(names(tb.E.2008),tb.E.2008,log="xy",xlab="Employee",ylab="Freq")
m_bl_E.2008 = conpl$new(E.2008)#create ppwerlaw object
est = estimate_xmin(m_bl_E.2008)#estimate the xmin for the alpha calculation
m_bl_E.2008$setXmin(est)#add xmin to power law object
m_bl_E.2008$xmin #min
m_bl_E.2008$pars #alpha
plot(m_bl_E.2008,xlab="Employee",ylab="CDF") #log-log plot 
lines(m_bl_E.2008)

bs_p_E.2008 = bootstrap_p(m_bl_E.2008,no_of_sims=100, threads=4)
plot(bs_p_E.2008, trim=0.1)
bs_p_E.2008$p


#Revenue 2008 p.value=1 alpha=2.247032 xmin=98234 it's not power law because
tb.R.2008= table(round(R.2008))
plot(names(tb.R.2008),tb.R.2008,log="xy",xlab="Revenue",ylab="Freq")
m_bl_R.2008 = conpl$new(round(R.2008))#create ppwerlaw object
est = estimate_xmin(m_bl_R.2008)#estimate the xmin for the alpha calculation
m_bl_R.2008$setXmin(est)#add xmin to power law object
m_bl_R.2008$xmin #min
m_bl_R.2008$pars #alpha
plot(m_bl_R.2008,xlab="Revenue",ylab="CDF")
lines(m_bl_R.2008)

bs_p_R.2008 = bootstrap_p(m_bl_R.2008,no_of_sims=100, threads=4)
plot(bs_p_R.2008, trim=0.1)
bs_p_R.2008$p
}

#for2009
#E p.value=0.96 aplha=2.272558 xmin=127
#R p.value=1 alpha=2.330397 xmin=99406
{
E.2009 = manufacturing$E[manufacturing$Year==2009]
R.2009 = manufacturing$R[manufacturing$Year==2009]

#Employee 2008 p.value=0.96 aplha=2.272558 xmin=127
tb.E.2009 = table(E.2009)
plot(names(tb.E.2009),tb.E.2009,log="xy",xlab="Employee",ylab="Freq")
m_bl_E.2009 = conpl$new(E.2009)#create ppwerlaw object
est = estimate_xmin(m_bl_E.2009)#estimate the xmin for the alpha calculation
m_bl_E.2009$setXmin(est)#add xmin to power law object
m_bl_E.2009$xmin #min
m_bl_E.2009$pars #alpha
plot(m_bl_E.2009,xlab="Employee",ylab="CDF") #log-log plot 
lines(m_bl_E.2009)

bs_p_E.2009 = bootstrap_p(m_bl_E.2009,no_of_sims=100, threads=4)
plot(bs_p_E.2009, trim=0.1)
bs_p_E.2009$p


#Revenue 2008 p.value=1 alpha=2.330397 xmin=99406 it's not power law because
tb.R.2009= table(round(R.2009))
plot(names(tb.R.2009),tb.R.2009,log="xy",xlab="Revenue",ylab="Freq")
m_bl_R.2009 = conpl$new(round(R.2009))#create ppwerlaw object
est = estimate_xmin(m_bl_R.2009)#estimate the xmin for the alpha calculation
m_bl_R.2009$setXmin(est)#add xmin to power law object
m_bl_R.2009$xmin #min
m_bl_R.2009$pars #alpha
plot(m_bl_R.2009,xlab="Revenue",ylab="CDF")
lines(m_bl_R.2009)

bs_p_R.2009 = bootstrap_p(m_bl_R.2009,no_of_sims=100, threads=4)
plot(bs_p_R.2009, trim=0.1)
bs_p_R.2009$p

}

#for2010
#E p.value=0.82 aplha=2.294162 xmin=105
#R p.value=1 alpha=2.282562 xmin= 97742
{
  E.2010 = manufacturing$E[manufacturing$Year==2010]
  R.2010 = manufacturing$R[manufacturing$Year==2010]
  
  #Employee 2010 p.value=0.82 aplha=2.294162 xmin=105
  tb.E.2010 = table(E.2010)
  plot(names(tb.E.2010),tb.E.2010,log="xy",xlab="Employee",ylab="Freq")
  m_bl_E.2010 = conpl$new(E.2010)#create ppwerlaw object
  est = estimate_xmin(m_bl_E.2010)#estimate the xmin for the alpha calculation
  m_bl_E.2010$setXmin(est)#add xmin to power law object
  m_bl_E.2010$xmin #min
  m_bl_E.2010$pars #alpha
  plot(m_bl_E.2010,xlab="Employee",ylab="CDF") #log-log plot 
  lines(m_bl_E.2010)
  
  bs_p_E.2010 = bootstrap_p(m_bl_E.2010,no_of_sims=100, threads=4)
  plot(bs_p_E.2010, trim=0.1)
  bs_p_E.2010$p
  
  
  #Revenue 2010 p.value=1 alpha=2.282562 xmin= 97742 it's not power law because
  tb.R.2010= table(round(R.2010))
  plot(names(tb.R.2010),tb.R.2010,log="xy",xlab="Revenue",ylab="Freq")
  m_bl_R.2010 = conpl$new(round(R.2010))#create ppwerlaw object
  est = estimate_xmin(m_bl_R.2010)#estimate the xmin for the alpha calculation
  m_bl_R.2010$setXmin(est)#add xmin to power law object
  m_bl_R.2010$xmin #min
  m_bl_R.2010$pars #alpha
  plot(m_bl_R.2010,xlab="Revenue",ylab="CDF")
  lines(m_bl_R.2010)
  
  bs_p_R.2010 = bootstrap_p(m_bl_R.2010,no_of_sims=100, threads=4)
  plot(bs_p_R.2010, trim=0.1)
  bs_p_R.2010$p
  
}

#for2011
#E p.value=0.6 aplha=2.239692 xmin=124
#R p.value=1 alpha=2.257669 xmin=98520
{
  E.2011 = manufacturing$E[manufacturing$Year==2011]
  R.2011 = manufacturing$R[manufacturing$Year==2011]
  
  #Employee 2011 p.value=0.6 aplha=2.239692 xmin=124
  tb.E.2011 = table(E.2011)
  plot(names(tb.E.2011),tb.E.2011,log="xy",xlab="Employee",ylab="Freq")
  m_bl_E.2011 = conpl$new(E.2011)#create ppwerlaw object
  est = estimate_xmin(m_bl_E.2011)#estimate the xmin for the alpha calculation
  m_bl_E.2011$setXmin(est)#add xmin to power law object
  m_bl_E.2011$xmin #min
  m_bl_E.2011$pars #alpha
  plot(m_bl_E.2011,xlab="Employee",ylab="CDF") #log-log plot 
  lines(m_bl_E.2011)
  
  bs_p_E.2011 = bootstrap_p(m_bl_E.2011,no_of_sims=100, threads=4)
  plot(bs_p_E.2011, trim=0.1)
  bs_p_E.2011$p
  
  
  #Revenue 2011 p.value=1 alpha=2.257669 xmin=98520
  tb.R.2011= table(round(R.2011))
  plot(names(tb.R.2011),tb.R.2011,log="xy",xlab="Revenue",ylab="Freq")
  m_bl_R.2011 = conpl$new(round(R.2011))#create ppwerlaw object
  est = estimate_xmin(m_bl_R.2011)#estimate the xmin for the alpha calculation
  m_bl_R.2011$setXmin(est)#add xmin to power law object
  m_bl_R.2011$xmin #min
  m_bl_R.2011$pars #alpha
  plot(m_bl_R.2011,xlab="Revenue",ylab="CDF")
  lines(m_bl_R.2011)
  
  bs_p_R.2011 = bootstrap_p(m_bl_R.2011,no_of_sims=100, threads=4)
  plot(bs_p_R.2011, trim=0.1)
  bs_p_R.2011$p
  
}

#for2012
#E p.value=0.89 aplha=2.275677 xmin=98
#R p.value=1 alpha=2.294387 xmin=99343
{
  E.2012 = manufacturing$E[manufacturing$Year==2012]
  R.2012 = manufacturing$R[manufacturing$Year==2012]
  
  #Employee 2012 p.value=0.89 aplha=2.275677 xmin=98
  tb.E.2012 = table(E.2012)
  plot(names(tb.E.2012),tb.E.2012,log="xy",xlab="Employee",ylab="Freq")
  m_bl_E.2012 = conpl$new(E.2012)#create ppwerlaw object
  est = estimate_xmin(m_bl_E.2012)#estimate the xmin for the alpha calculation
  m_bl_E.2012$setXmin(est)#add xmin to power law object
  m_bl_E.2012$xmin #min
  m_bl_E.2012$pars #alpha
  plot(m_bl_E.2012,xlab="Employee",ylab="CDF") #log-log plot 
  lines(m_bl_E.2012,col="red")
  
  bs_p_E.2012 = bootstrap_p(m_bl_E.2012,no_of_sims=100, threads=4)
  plot(bs_p_E.2012, trim=0.1)
  bs_p_E.2012$p
  
  
  #Revenue 2012 p.value=1 alpha=2.294387 xmin=99343
  tb.R.2012= table(round(R.2012))
  plot(names(tb.R.2012),tb.R.2012,log="xy",xlab="Revenue",ylab="Freq")
  m_bl_R.2012 = conpl$new(round(R.2012))#create ppwerlaw object
  est = estimate_xmin(m_bl_R.2012)#estimate the xmin for the alpha calculation
  m_bl_R.2012$setXmin(est)#add xmin to power law object
  m_bl_R.2012$xmin #min
  m_bl_R.2012$pars #alpha
  plot(m_bl_R.2012,xlab="Revenue",ylab="CDF")
  lines(m_bl_R.2012)
  
  bs_p_R.2012 = bootstrap_p(m_bl_R.2012,no_of_sims=100, threads=4)
  plot(bs_p_R.2012, trim=0.1)
  bs_p_R.2012$p
  
}

#for2013
#E  p.value=0.48 aplha=2.265985 xmin=99
#R  p.value=1 alpha=2.309056 xmin=99433
{
  E.2013 = manufacturing$E[manufacturing$Year==2013]
  R.2013 = manufacturing$R[manufacturing$Year==2013]
  
  #Employee 2013 p.value=0.48 aplha=2.265985 xmin=99
  tb.E.2013 = table(E.2013)
  plot(names(tb.E.2013),tb.E.2013,log="xy",xlab="Employee",ylab="Freq")
  m_bl_E.2013 = conpl$new(E.2013)#create ppwerlaw object
  est = estimate_xmin(m_bl_E.2013)#estimate the xmin for the alpha calculation
  m_bl_E.2013$setXmin(est)#add xmin to power law object
  m_bl_E.2013$xmin #min
  m_bl_E.2013$pars #alpha
  plot(m_bl_E.2013,xlab="Employee",ylab="CDF") #log-log plot 
  lines(m_bl_E.2013,col="red")
  
  bs_p_E.2013 = bootstrap_p(m_bl_E.2013,no_of_sims=100, threads=4)
  plot(bs_p_E.2013, trim=0.1)
  bs_p_E.2013$p
  
  
  #Revenue 2013 p.value=1 alpha=2.309056 xmin=99433
  tb.R.2013= table(round(R.2013))
  plot(names(tb.R.2013),tb.R.2013,log="xy",xlab="Revenue",ylab="Freq")
  m_bl_R.2013 = conpl$new(round(R.2013))#create ppwerlaw object
  est = estimate_xmin(m_bl_R.2013)#estimate the xmin for the alpha calculation
  m_bl_R.2013$setXmin(est)#add xmin to power law object
  m_bl_R.2013$xmin #min
  m_bl_R.2013$pars #alpha
  plot(m_bl_R.2013,xlab="Revenue",ylab="CDF")
  lines(m_bl_R.2013)
  
  bs_p_R.2013 = bootstrap_p(m_bl_R.2013,no_of_sims=100, threads=4)
  plot(bs_p_R.2013, trim=0.1)
  bs_p_R.2013$p
  
}

#for2014
#E p.value=0.62 aplha=2.27115 xmin=101
#R p.value= alpha=2.297056 xmin=98521
{
  E.2014 = manufacturing$E[manufacturing$Year==2014]
  R.2014 = manufacturing$R[manufacturing$Year==2014]
  
  #Employee 2014 p.value= aplha=2.27115 xmin=101
  tb.E.2014 = table(E.2014)
  plot(names(tb.E.2014),tb.E.2014,log="xy",xlab="Employee",ylab="Freq")
  m_bl_E.2014 = conpl$new(E.2014)#create ppwerlaw object
  est = estimate_xmin(m_bl_E.2014)#estimate the xmin for the alpha calculation
  m_bl_E.2014$setXmin(est)#add xmin to power law object
  m_bl_E.2014$xmin #min
  m_bl_E.2014$pars #alpha
  plot(m_bl_E.2014,xlab="Employee",ylab="CDF") #log-log plot 
  lines(m_bl_E.2014,col="red")
  
  bs_p_E.2014 = bootstrap_p(m_bl_E.2014,no_of_sims=100, threads=4)
  plot(bs_p_E.2014, trim=0.1)
  bs_p_E.2014$p
  
  
  #Revenue 2014 p.value= alpha=2.297056 xmin=98521
  tb.R.2014= table(round(R.2014))
  plot(names(tb.R.2014),tb.R.2014,log="xy",xlab="Revenue",ylab="Freq")
  m_bl_R.2014 = conpl$new(round(R.2014))#create ppwerlaw object
  est = estimate_xmin(m_bl_R.2014)#estimate the xmin for the alpha calculation
  m_bl_R.2014$setXmin(est)#add xmin to power law object
  m_bl_R.2014$xmin #min
  m_bl_R.2014$pars #alpha
  plot(m_bl_R.2014,xlab="Revenue",ylab="CDF")
  lines(m_bl_R.2014)
  
  bs_p_R.2014 = bootstrap_p(m_bl_R.2014,no_of_sims=100, threads=4)
  plot(bs_p_R.2014, trim=0.1)
  bs_p_R.2014$p
  
}

#for2015
#E p.value=1 aplha=2.295916 xmin=102
#R
{
  E.2015 = manufacturing$E[manufacturing$Year==2015]
  R.2015 = manufacturing$R[manufacturing$Year==2015]
  
  #Employee 2015 p.value=1 aplha=2.295916 xmin=102
  tb.E.2015 = table(E.2015)
  plot(names(tb.E.2015),tb.E.2015,log="xy",xlab="Employee",ylab="Freq")
  m_bl_E.2015 = conpl$new(E.2015)#create ppwerlaw object
  est = estimate_xmin(m_bl_E.2015)#estimate the xmin for the alpha calculation
  m_bl_E.2015$setXmin(est)#add xmin to power law object
  m_bl_E.2015$xmin #min
  m_bl_E.2015$pars #alpha
  plot(m_bl_E.2015,xlab="Employee",ylab="CDF") #log-log plot 
  lines(m_bl_E.2015,col="red")
  
  bs_p_E.2015 = bootstrap_p(m_bl_E.2015,no_of_sims=100, threads=4)
  plot(bs_p_E.2015, trim=0.1)
  bs_p_E.2015$p
  
  
  #Revenue 2015 p.value=1 alpha=2.36435 xmin=97915
  tb.R.2015 = table(round(R.2015))
  plot(names(tb.R.2015),tb.R.2015,log="xy",xlab="Revenue",ylab="Freq")
  m_bl_R.2015 = conpl$new(round(R.2015))#create ppwerlaw object
  est = estimate_xmin(m_bl_R.2015)#estimate the xmin for the alpha calculation
  m_bl_R.2015$setXmin(est)#add xmin to power law object
  m_bl_R.2015$xmin #min
  m_bl_R.2015$pars #alpha
  plot(m_bl_R.2015,xlab="Revenue",ylab="CDF")
  lines(m_bl_R.2015)
  
  bs_p_R.2015 = bootstrap_p(m_bl_R.2015,no_of_sims=100, threads=4)
  plot(bs_p_R.2015, trim=0.1)
  bs_p_R.2015$p
  
}



###### RECAP FOR E - BY YEARS#####
#E 2007 p.value=0.6 aplha=2.205376 xmin= 78
#E 2008 p.value=0.66 aplha=2.244039 xmin=74
#E 2009 p.value=0.96 aplha=2.272558 xmin=127
#E 2010 p.value=0.82 aplha=2.294162 xmin=105
#E 2011 p.value=0.6 aplha=2.239692 xmin=124
#E 2012 p.value=0.89 aplha=2.275677 xmin=98
#E 2013 p.value=0.48 aplha=2.265985 xmin=99
#E 2014 p.value=0.62 aplha=2.27115 xmin=101
#E 2015 p.value=1 aplha=2.295916 xmin=102
par(mfrow=c(2,1))
plot(c(2007,2008,2009,2010,2011,2012,2013,2014,2015),c(78,74,127,105,124,98,99,101,102),xlab="Year",ylab="xmin",main="Xmin for Year")
lines(c(2007,2008,2009,2010,2011,2012,2013,2014,2015),c(78,74,127,105,124,98,99,101,102),xlab="Year",ylab="xmin")
plot(c(2007,2008,2009,2010,2011,2012,2013,2014,2015),c(2.205376,2.244039,2.272558,2.294162,2.239692,2.275677,2.265985,2.27115,2.295916 ),xlab="Year",ylab="alpha",main="Alpha for Year")
lines(c(2007,2008,2009,2010,2011,2012,2013,2014,2015),c(2.205376,2.244039,2.272558,2.294162,2.239692,2.275677,2.265985,2.27115,2.295916 ),xlab="Year",ylab="alpha")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
#PDF FOR 2007-2015x = 1:20
{
   plot(m_bl_E.2007$dat, dist_pdf(m_bl_E.2007, m_bl_E.2007$dat),lwd=2, type="l",col="1",xlab="Employee",ylab="PDF") #PDF 2007
  lines(m_bl_E.2008$dat, dist_pdf(m_bl_E.2008, m_bl_E.2008$dat),lwd=2, type="l",col="2") #PDF 2008
  lines(m_bl_E.2009$dat, dist_pdf(m_bl_E.2009, m_bl_E.2009$dat),lwd=2, type="l",col="3") #PDF 2009
  lines(m_bl_E.2010$dat, dist_pdf(m_bl_E.2010, m_bl_E.2010$dat),lwd=2, type="l",col="4") #PDF 2010
  lines(m_bl_E.2011$dat, dist_pdf(m_bl_E.2011, m_bl_E.2011$dat),lwd=2, type="l",col="5") #PDF 2011
  lines(m_bl_E.2012$dat, dist_pdf(m_bl_E.2012, m_bl_E.2012$dat),lwd=2, type="l",col="6") #PDF 2012
  lines(m_bl_E.2013$dat, dist_pdf(m_bl_E.2013, m_bl_E.2013$dat),lwd=2, type="l",col="7") #PDF 2013
  lines(m_bl_E.2014$dat, dist_pdf(m_bl_E.2014, m_bl_E.2014$dat),lwd=2, type="l",col="10") #PDF 2014
  lines(m_bl_E.2015$dat, dist_pdf(m_bl_E.2015, m_bl_E.2015$dat),lwd=2, type="l",col="9") #PDF 2015
  legend("bottomright",legend=c("2007","2008","2009","2010","2011","2012","2013","2014","2015"),
         col=c("1", "2","3","4","5","6","7","8","9"), lty=1:2, cex=0.8)

}
#CDF FOR 2007-2015
{
plot(m_bl_E.2007$dat, dist_cdf(m_bl_E.2007, m_bl_E.2007$dat), lwd=2,type="l",col="1",xlab="Employee",ylab="CDF") #CDF 2007
lines(m_bl_E.2008$dat, dist_cdf(m_bl_E.2008, m_bl_E.2008$dat),lwd=2, type="l",col="2") #CDF 2008
lines(m_bl_E.2009$dat, dist_cdf(m_bl_E.2009, m_bl_E.2009$dat),lwd=2, type="l",col="3") #CDF 2009
lines(m_bl_E.2010$dat, dist_cdf(m_bl_E.2010, m_bl_E.2010$dat),lwd=2, type="l",col="4") #CDF 2010
lines(m_bl_E.2011$dat, dist_cdf(m_bl_E.2011, m_bl_E.2011$dat),lwd=2, type="l",col="5") #CDF 2011
lines(m_bl_E.2012$dat, dist_cdf(m_bl_E.2012, m_bl_E.2012$dat),lwd=2, type="l",col="6") #CDF 2012
lines(m_bl_E.2013$dat, dist_cdf(m_bl_E.2013, m_bl_E.2013$dat),lwd=2, type="l",col="7") #CDF 2013
lines(m_bl_E.2014$dat, dist_cdf(m_bl_E.2014, m_bl_E.2014$dat),lwd=2, type="l",col="10") #CDF 2014
lines(m_bl_E.2015$dat, dist_cdf(m_bl_E.2015, m_bl_E.2015$dat),lwd=2, type="l",col="9") #CDF 2015
legend("bottomright",legend=c("2007","2008","2009","2010","2011","2012","2013","2014","2015"),
       col=c("1", "2","3","4","5","6","7","8","9"), lty=1:2, cex=0.8)

}
par(mfrow=c(1,1))

#N.employee 122726 aplha=2.205376
 #N.Employee 122860 aplha=2.244039
#N.Employee 124596 aplha=2.272558
#N.Employee 123940 alpha=2.294162
#N.Employee 129417 alpha=2.239692
#N.Employee 132242 aplha=2.275677
#N.Employee 134181 aplha=2.265985
#N.Employee 133911 aplha=2.27115
#N.Employee 136228 aplha=2.295916

   








###### RECAP FOR R - BY YEARS#####
#R 2007 p.value=1 alpha=2.332321 xmin=99240 
#R 2008 p.value=1 alpha=2.247032 xmin=98234
#R 2009 p.value=1 alpha=2.330397 xmin=99406
#R 2010 p.value=1 alpha=2.282562 xmin= 97742
#R 2011 p.value=1 alpha=2.257669 xmin=98520
#R 2012 p.value=1 alpha=2.294387 xmin=99343
#R 2013 p.value=1 alpha=2.257669 xmin=98520
#R 2014 p.value= alpha=2.297056 xmin=98521
#R 2015 p.value=1 alpha=2.36435 xmin=97915


  
{
#
m = displ$new()
m$setXmin(5)
m$xmin
x = 1:20
m$setPars(1.5)
plot(x, dist_pdf(m, x), type="b",col="red")
m$setPars(2.0)
lines(x, dist_pdf(m, x), type="b",col="blue")
m$setPars(2.5)
lines(x, dist_pdf(m, x), type="b",col="green")
plot(x, dist_cdf(m, x), type="b")
}
