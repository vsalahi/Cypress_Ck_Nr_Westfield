# Exploratory data analysis of Addis Data

# Set working directory
setwd('C:\\Users\\vahid\\Google Drive\\GraduateStudies\\Semester 6\\SurfaceWaterHydrology\\Module1\\RCode')

# read the data
a <- read.csv('addisdata.csv')

summary(a)
boxplot(a[,3]~a[,2],outline=F,xlab='Month',las=2,ylab='Precip(mm)')
grid()
title('Monthly Variability in Precipitation in Addis Ababa')

# Function to calculate empirical CDF using Gringorten Formula
gringorten <- function(x)
{
n <- length(x)
r <- rank(x)
F <- (r-0.44)/(n+0.12)
return(F)
}

precip.prob <- gringorten(a[,3])
plot(a[,3],precip.prob,xlab='Precipitation (mm)',ylab='Probability')
grid()
title('Prob. Dist. of Mon. Precip. at Addis Ababa')

# Subset data for August = 7 month
precip.aug <- subset(a[,3],a[,2]==7)
# Use the Gringorten formula to obtain august probabilities
prob.aug <- gringorten(precip.aug)
# make the plot
plot(precip.aug,prob.aug,xlab='Precipitation (mm)',ylab='Probability')
grid()
title('Probability of August Precipitation')

# Plot histogram of annual rainfall data
hist(a[,3],xlab='Precipitation (mm)',ylab='Frequency',
main='Monthly Precipitation',xlim=c(0,500),freq=T,col='blue')
grid()
# draw a line for the mean value
abline(v=mean(a[,3]),col='red',lty=2,lwd=2)


# Create density plots for rainfall data
par(mfrow=c(2,1))
hist(a[,3],xlab='Precipitation (mm)',ylab='Frequency',
main='Monthly Precipitation',xlim=c(0,500),freq=F,col='blue')
grid()

# create a sequence of normal distribution
x <- seq(0,max(a[,3]),0.1)
dnormal <- dnorm(x, mean=mean(a[,3]), sd=sd(a[,3]))
lines(x,dnormal,col='red')

# Create a density plot for August Precipitation
hist(precip.aug,xlab='Precipitation (mm)',ylab='Frequency',
main='August Precipitation',xlim=c(0,500),freq=F,col='blue')
grid()
# Create a sequence of normal distributions and add to the plot
x <- seq(0,max(precip.aug),0.1)
dnormal <- dnorm(x, mean=mean(precip.aug), sd=sd(precip.aug))
lines(x,dnormal,col='red')

precip <- a[,3] # define a variable and store precip data
precip.std <- scale(precip,center=TRUE) # center and standardize
precip.std.prob <- gringorten(precip.std)
plot(precip.std, precip.std.prob,xlab='Standardized Precipitation', 
     ylab='Probability',xlim=c(-4,4))
grid()
# create probabilities for standard normal distribution
x <- seq(-4,4,0.1)
std.norm <- pnorm(x,0,1)
# Plot standard normal distribution as lines
lines(x,std.norm,col='red',lty=2,lwd=2)
#Plot the legend
legend(-3.5,0.9,legend=c('Std. Precip','Std. Normal'),col=c('black','red'),
lty=c(NA,2),lwd=c(NA,2),pch=c(1,NA))
title('Standardized Precipitation')

