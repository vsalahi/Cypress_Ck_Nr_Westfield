gringorten <- function(data)
{
grig <- (rank(data)-0.44)/(length(data)+0.12)
return(grig)
}

# Load library and read the data
library(fitdistrplus)  # this library needs to be installed

a <- read.csv('modelfit.csv') # this file should be in the same subdirectory as this R file
data <- a[,1]
# Fit Gamma and Normal Distribitions
norm.mle <- fitdist(data,"norm",method="mle")
gamma.mle <- fitdist(data,"gamma",method="mle")
# Calculate empirical CDF adn model probabiluties
data.sort <- sort(data)
gring.p <- (rank(data.sort)-0.44)/(length(data)+0.12)
normal.p <- pnorm(data.sort,mean=norm.mle$estimate[1],sd=norm.mle$estimate[2])
gamma.p <- pgamma(data.sort,shape=gamma.mle$estimate[1],rate=gamma.mle$estimate[2])

#plot the empirical CDF and model derived CDFs on the same chart
plot(data.sort,gring.p,pch=3,xlab='Value',ylab='Probability')
lines(data.sort,normal.p,col='red',lty=2)
lines(data.sort,gamma.p,col='blue',lty=3)
legend(7,0.8,legend=c("Empirical","Normal","Gamma"),lty=c(NA,2,3),col=c('black','red','blue'),
pch=c(3,NA,NA))
grid()

# Get AIC values
norm.aic <- norm.mle$aic
gamma.aic <- gamma.mle$aic

# Perform KS test
norm.ks <- ks.test(data,"pnorm",mean=norm.mle$estimate[1],sd=norm.mle$estimate[2])
gamma.ks <- ks.test(data,"pgamma",shape=gamma.mle$estimate[1],rate=gamma.mle$estimate[2])
norm.ks
gamma.ks
