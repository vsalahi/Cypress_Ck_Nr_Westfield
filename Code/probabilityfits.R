# Load libraries and set working directory
library(MASS)
library(extRemes)
library(fitdistrplus)
setwd('C:\\Users\\vahid\\Google Drive\\GraduateStudies\\Semester 6\\SurfaceWaterHydrology\\Module1\\RCode')  # Change as needed

# Read the CSV file 
a <- read.csv('Peakflowdata.csv') # This file should be in the same directory
sf <- a[,4]

# Obtain empirical CDF using Gringorten
# Perform Exploratory Data Analysis
summary(sf) # get summary statistics
# Plot empirical CDF using Gringorten Plotting Position
ecdf.grt <- (rank(sf)-0.44)/(length(sf) + 0.12)
plot(sf,ecdf.grt,xlab='Peakflow (cfs)',ylab='Cumulative Probability')
grid()
#Plot the boxplot
boxplot(sf,ylab='Peakflow (cfs)')
grid()

# use fitdistr function from library (MASS)
# fit normal and lognormal distribution
sf.nd <- fitdistr(sf,"normal") # No need to supply start values for normal
sf.ln <- fitdistr(sf,"Log-normal")

#fit gamma distribution, Use MOM estimates as starting point
sf.gm <- fitdistr(sf,dgamma,list(shape = 0.5264, rate = 5.15e-05))

#fit gumbel and gev functions using extReme package

sf.gum <- fevd(sf,location.fun=~1,scale.fun=~1,shape.fun=0,type='Gumbel')
sf.evd <- fevd(sf,location.fun=~1,scale.fun=~1,shape.fun=0,type='GEV')


# Obtain theoretical quantiles corresponding to the observed data
q.nd <- qnorm(ecdf.grt,sf.nd$estimate[1],sf.nd$estimate[2]) 
q.ln <-qlnorm(ecdf.grt,sf.ln$estimate[1],sf.ln$estimate[2])
q.gm <- qgamma(ecdf.grt,0.5264,5.15e-05)
q.gum <-qevd(ecdf.grt,sf.gum$results$par[1],sf.gum$results$par[2],type='Gumbel')
q.evd <-qevd(ecdf.grt,sf.evd$results$par[1],sf.evd$results$par[2],sf.evd$results$par[3],type='GEV')

#Bind the data and make plot
all.dat <- cbind(sf,q.nd,q.ln,q.gm,q.gum,q.evd)
colnames(all.dat) <- c("Observed","Normal","Log-Normal","Gamma","Gumbel","EVD")
par(mfrow=c(3,2))
plot(sf,ecdf.grt,xlab='Peakflow (cfs)',ylab='Cumulative Probability')
title('Empirical CDF')
plot(sf,q.nd,xlab='Observed',ylab='Predicted')
title('Normal Distribution')
plot(sf,q.ln,xlab='Observed',ylab='Predicted')
title('Lognormal Distribution')
plot(sf,q.gm,xlab='Observed',ylab='Predicted')
title('Gamma Distribution')
plot(sf,q.gum,xlab='Observed',ylab='Predicted')
title('Gumbel Distribution')
plot(sf,q.evd,xlab='Observed',ylab='Predicted')
title('GEV Distribution')