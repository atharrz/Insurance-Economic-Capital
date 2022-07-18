#Economic Capital
# The code below was created with the purpose of leveraging automation and reduce distribution fitting for each line ob business separately

x<-c("fitdistrplus", "readxl","actuar","dplyr", "tidyverse", "tibble", "ggplot2","MASS","magicfor")
lapply(x, require, character.only = TRUE)


#________________________LOAD DATA______________________________
# Here we load a sample Excel file consisting of monthly claims data of an imaginary insurance company,
# we assume that the insurance company underwriting portfolio has only 2 Lines (Fire and Automobile)

File <- read_excel("~/Desktop/Quering/R/Insurance-Economic-Capital/Loss.xlsx")

#we have a 5-year period record for each Line of Business (LoB)
View(File)


File <- within(File, {
  Line <- factor(Line)
  subline <- factor(subline)
})

#converting Null to zero
File <- File%>%
  replace(is.na(.), 0)


#considering the effect of inflation, 
# It means that if a loss occurs 5 years ago, it can now have much more negative impact
#so we assume a desired inflation rate (completely depends on the portfolio and economic conditions) and compute the present value of each loss
# here we choose an inflation rate of 5%.

a <- (1 + 0.05)

File <- File %>% 
  mutate(totalloss_inflated=
           case_when(year == 2017 ~ totalloss*(a^4), 
                     year == 2018 ~ totalloss*(a^3),
                     year == 2019 ~ totalloss*(a^2),
                     year == 2020 ~ totalloss*a,
                     year == 2021 ~ totalloss),
         
         Severity = if_else(countloss == 0, totalloss_inflated, #to avoid undefined (0 divide by 0), where we have no loss
                            totalloss_inflated/countloss),
         
         severity = Severity/1000000 #re-scaling the numbers to "Million"
  )         

#___________________Histograms and Boxplots_________________________
par(ask=F)
for (i in unique(File$subline)) {
  hist(File[File$subline == i, ]$countloss,
       xlab=" Count  ", ylab=" Frequency   ",
      main = paste0("Loss Count for"," ", i));

  boxplot(File[File$subline == i, ]$countloss,
          main = paste0("Count of Loss"," ", i)
  )
}


#_______Distribution Fitting for both severity and frequency______________

library(magicfor)
magic_for(silent = TRUE)

for (i in unique(File$subline)) {
  
  c1 <- File[File$subline == i, ]$countloss
  
  
  # OUTLIERS detection
# there are so many ways to treat outliers, here based on histograms and boxplots, 
# replacing outliers with upper and lower limits (97.5 and 1 percentile) was chosen.
# outliers can be replaced with mean, mode or even totally omitted and deleted. It completely relies on data. 
  
  upper_bound <- quantile(c1, 0.975)
  lower_bound <- quantile(c1, 0.01)
  
  #note that the frequency refers to discrete numbers and cannot have decimal numbers.
  
  c1[which(c1>upper_bound)] <- round(upper_bound, digits = 0)
  c1[which(c1<lower_bound)] <- round(lower_bound, digits = 0)
  
  
  #see the effects of replacing outliers
  
  par(mfrow=c(1,2)) 
  boxplot(File[File$subline == i, ]$countloss, main=paste("Countloss -",i))
  boxplot(c1, main=paste("Adjusted Countloss -",i))
  
  
  #FREQUENCY distribution fitting
  
  poisson<-fitdistrplus::fitdist(c1, "pois", method = "mle", discrete = T)
  binom<-fitdistrplus::fitdist(c1, "nbinom", method = "mle", discrete = T)

  # The selected distribution was picked using AIC metric
  
  fit_freq <-if_else((which(c(binom$aic,poisson$aic)==min(c(poisson$aic,binom$aic),na.rm = TRUE)))==1,"Negative.Binomial","Poisson")
  
  
  #Computing the mean and variance of frequency distribution
  

  p <- switch (fit_freq,
               Negative.Binomial = (binom$estimate["size"])/(binom$estimate["mu"]+binom$estimate["size"]))
  
  k <- switch (fit_freq, Negative.Binomial = (binom$estimate["size"]))
  
  q=1-p  
  
  lambda <- switch (fit_freq, Poisson = poisson$estimate["lambda"])
 
  Mean_fre <- switch (fit_freq,
                      Poisson = poisson$estimate["lambda"],
                      Negative.Binomial = binom$estimate["mu"])
  Variance_fre <- switch (fit_freq,
                Poisson = poisson$estimate["lambda"],
                Negative.Binomial= (k*1/(p*p)))
  
  print(paste("Fitted Distribution for",i ,"Frequency is",fit_freq))
  
  
  plot_fre <- switch (fit_freq,
                      Poisson =  plot(poisson, histo =TRUE, demp = TRUE),
                      Negative.Binomial = plot(binom, histo =TRUE, demp = TRUE))
  
  
  
  #__________Severity Distribution______________
  s1 <- File[File$subline == i, ]$severity
  
  upper_bound_s <- min(quantile(s1, 0.95),sort(s1)[58])
  lower_bound_s <- quantile(s1, 0.01)
  
  s1[which(s1>upper_bound_s)] <- upper_bound_s
  s1[which(s1<lower_bound_s)] <- lower_bound_s
  
  #Hampel filter
  #upper_bound_s = median(severity) + 3 * mad(severity, constant = 1) 
  #lower_bound_s = median(severity) - 3 * mad(severity, constant = 1)
  
  #IQR
  #quartiles <- quantile(s1, probs=c(.25, .75), na.rm = FALSE)
  #upper_bound_s <- quartiles[2] + 1.5*IQR (s1)
  #lower_bound_s <- quartiles[1] - 1.5*IQR (s1)
 
  
  par(mfrow=c(1,2))
  boxplot(File[File$subline == i, ]$severity, main=paste("Severity -",i))
  boxplot(s1, main=paste("Adjusted Severity -",i))
  
  
  #Distributions
  Lognormal <-fitdistrplus::fitdist(s1[s1>0], "lnorm", method = "mle")
  Gamma <-fitdistrplus::fitdist(s1[s1>0], "gamma", method = "mle", lower=c(0,0))
  Weibull <-fitdistrplus::fitdist(s1[s1>0], "weibull", method = "mle",start = list(shape = 1, scale = 50))
  Exponenial <-fitdistrplus::fitdist(s1[s1>0], "exp", method = "mle")
  Burr <-fitdistrplus::fitdist(s1[s1>0], "burr",start = list(shape1=0.2, shape2=1, rate=1), lower=c(0,0),method = "mge",gof="CvM")#method ="qme",probs = c(1/4,2/4, 3/4))
  Loglogistic <- fitdistrplus::fitdist(s1[s1>0], "llogis", start = list(shape =1, scale = 500))
  Pareto <- fitdistrplus::fitdist(s1[s1>0], "pareto", start = list(shape = 1, scale = 500))
  Inverse.Gamma<-  fitdistrplus::fitdist(s1[s1>0], "invgamma", method = "mle")
  
  # gofstat_s <- fitdistrplus::gofstat(list
  #                                    (Lognormal,Gamma,Weibull,Exponenial,Burr,Loglogistic,Pareto),
  #                                    fitnames = c("Lognormal","Gamma","Weibull","Exponenial","Burr","Loglogistic","Pareto"))

  gofstat_s <- as.data.frame(cbind(aic=c(Lognormal$aic,Gamma$aic,Weibull$aic,Exponenial$aic,Burr$aic,Loglogistic$aic,Pareto$aic,Inverse.Gamma$aic),
                     names=c("Lognormal","Gamma","Weibull","Exponenial","Burr","Loglogistic","Pareto","Inverse.Gamma")))
  
  #fit_s <- names(which(gofstat_s$aic == (min(gofstat_s$aic[which(min(gofstat_s$aic!=min(goftstat_s$aic)))],na.rm = TRUE))))
  
  
  #Sometimes the second and third raw moments of distributions is Inf,
  # that's the purpose behind the loop below, to find the distribution with minimum AIC yet definite raw moments values
  
  for (a in 1:7) {
    
  fit_s <- gofstat_s$names[which(gofstat_s$aic == sort(gofstat_s$aic)[a])]
  
  m1 <- switch (fit_s,
                Lognormal = actuar::mlnorm(1,meanlog=Lognormal$estimate["meanlog"], sdlog=Lognormal$estimate["sdlog"]),
                Gamma = actuar::mgamma(1,shape=Gamma$estimate["shape"], rate=Gamma$estimate["rate"]),
                Weibull=actuar::mweibull(1,shape=Weibull$estimate["shape"], scale=Weibull$estimate["scale"]),
                Exponenial=actuar::mexp(1,rate=Exponenial$estimate["rate"]),
                Loglogistic=actuar::mllogis(1,shape=Loglogistic$estimate["shape"],scale=Loglogistic$estimate["scale"]),
                Pareto=actuar::mpareto(1,shape=Pareto$estimate["shape"],scale=Pareto$estimate["scale"]),
                Inverse.Gamma=actuar::minvgamma(1,shape=Inverse.Gamma$estimate["shape"],scale=Inverse.Gamma$estimate["scale"]),
                Burr=actuar::mburr(1,shape1=Burr$estimate["shape1"],shape2=Burr$estimate["shape2"], rate=Burr$estimate["rate"]))
  
  m2 <- switch (fit_s,
                Lognormal = (actuar::mlnorm(2,meanlog=Lognormal$estimate["meanlog"], sdlog=Lognormal$estimate["sdlog"])),
                Gamma = (actuar::mgamma(2,shape=Gamma$estimate["shape"], rate=Gamma$estimate["rate"])),
                Weibull=(actuar::mweibull(2,shape=Weibull$estimate["shape"], scale=Weibull$estimate["scale"])),
                Exponenial=(actuar::mexp(2,rate=Exponenial$estimate["rate"])),
                Loglogistic=actuar::mllogis(2,shape=Loglogistic$estimate["shape"],scale=Loglogistic$estimate["scale"]),
                Pareto=actuar::mpareto(2,shape=Pareto$estimate["shape"],scale=Pareto$estimate["scale"]),
                Inverse.Gamma=actuar::minvgamma(2,shape=Inverse.Gamma$estimate["shape"],scale=Inverse.Gamma$estimate["scale"]),
                Burr=actuar::mburr(2,shape1=Burr$estimate["shape1"],shape2=Burr$estimate["shape2"], rate=Burr$estimate["rate"]))
  
  m3 <- switch (fit_s,
                Lognormal = (actuar::mlnorm(3,meanlog=Lognormal$estimate["meanlog"], sdlog=Lognormal$estimate["sdlog"])),
                Gamma = (actuar::mgamma(3,shape=Gamma$estimate["shape"], rate=Gamma$estimate["rate"])),
                Weibull=(actuar::mweibull(3,shape=Weibull$estimate["shape"], scale=Weibull$estimate["scale"])),
                Exponenial=(actuar::mexp(3,rate=Exponenial$estimate["rate"])),
                Loglogistic=actuar::mllogis(3,shape=Loglogistic$estimate["shape"],scale=Loglogistic$estimate["scale"]),
                Pareto=actuar::mpareto(3,shape=Pareto$estimate["shape"],scale=Pareto$estimate["scale"]),
                Inverse.Gamma=actuar::minvgamma(3,shape=Inverse.Gamma$estimate["shape"],scale=Inverse.Gamma$estimate["scale"]),
                Burr=actuar::mburr(3,shape1=Burr$estimate["shape1"],shape2=Burr$estimate["shape2"], rate=Burr$estimate["rate"]))
   
   a <- a+1
   if(m1!=Inf && m2!=Inf && m3!=Inf)
   {break}
  }
  
  plot_s <- switch (fit_s,
                    Lognormal =  plot(Lognormal, histo =TRUE, demp = TRUE, sub=paste(i)),
                    Gamma =      plot(Gamma, histo =TRUE, demp = TRUE, sub=paste(i)),
                    Weibull =    plot(Weibull, histo =TRUE, demp = TRUE, sub=paste(i)),
                    Exponenial = plot(Exponenial, histo =TRUE, demp = TRUE, sub=paste(i)),
                    Loglogistic = plot(Loglogistic, histo =TRUE, demp = TRUE, sub=paste(i)),
                    Pareto = plot(Pareto, histo =TRUE, demp = TRUE, sub=paste(i)),
                    Burr =       plot(Burr, histo =TRUE, demp = TRUE, sub=paste(i)))
  
  
  print(paste("Fitted Distribution for",i , "Severity is",fit_s))
  
 
#__________Compound Distribution Function___________
  # now that we have known distribution for both severity and frequency, 
  # we want to find the mean, variance an skewness of compound distribution function.
  # we need this characteristics to put into the NP approximation and compute the VaR.


# Mean of Compound distribution function AND the EXPECTED LOSS

E <- switch (fit_freq,
                    Poisson = lambda*m1,
                    Negative.Binomial = (m1*k*q)/p)



Variance <- switch (fit_freq,
             Poisson = lambda*m2,
             Negative.Binomial = ((k*q)*((p*m2)+(q*m1*m1)))/(p^2))



Skew <- switch (fit_freq,
                    Poisson = (lambda*m3)/((lambda*m2)^1.5),
                    Negative.Binomial = ((p*p*q*m3)+(3*p*q*q*m1*m2)+(2*(q^3)*(m1^3))) /(sqrt(k)*((p*q*m2)+(q*q*m1*m1))^1.5)
                  ) 


#_______VaR_____________

#from the normal standard distribution, z(alpha):

a.90 = round(1.2818,digits=1)
a.95 = round(1.6449,digits=1)
a.99 = round(2.3265,digits=1)
a.995= round(2.5758,digits=1)

VaR.90 <-  (E + (sqrt(Variance) * (a.90+ ((Skew/6)*((a.90*a.90)-1)))))
VaR.95 <-  (E + (sqrt(Variance) * (a.95+ ((Skew/6)*((a.95*a.95)-1)))))
VaR.99 <-  (E + (sqrt(Variance) * (a.99+ ((Skew/6)*((a.99*a.99)-1)))))
VaR.995<-  (E + (sqrt(Variance) * (a.995+((Skew/6)*((a.995*a.995)-1)))))



#__________The Economic Capital_____________

#for example with the confidence level of 1%, the economic capital is equal to:

EC <- VaR.99 - E

put(E,VaR.90,VaR.95,VaR.99,VaR.995,EC )
}

magic_result_as_dataframe()