################################################################
## Exploratory data analysis for threshold detection
## For LTER Working group on threshold detection
## Initial coding by Aaron Ellison (Harvard) & Sapna Sharma
## (Wisconsin/Chicago)
## Final coding and commenting by Aaron Ellison (AME)
## Initial version: 23 May 2011
## Revised:  3 October 2011 (AME)
##11 October 2011 (AME)
## Current version: 11 October 2011
## Questions? TANSTAAFL
###############################################################

###############################################################
## Required libraries
###############################################################

require(plotrix)
require(strucchange)

###############################################################
## Data should be: csv, txt, or equivalent file
## Columns: time, driver, response
## LTER data: time in years
## SET WORKING DIRECTORY if needed
## REPLACE "mydata.csv" below with your data file
###############################################################

setwd( )

mydata <- read.csv("mydata.csv", header=TRUE)
## OR if .txt
## mydata <- read.table("mydata.txt", header=TRUE, sep="\t")

mytime <- mydata$time
mydriver <- mydata$driver
myresponse <- mydata$response

###############################################################
## Standardize data (sd units), and interpolate over NAs if necessary
## Fit a loess curve to visualize trend if any
## Create time-series object for later use
###############################################################

myresponse <- (myresponse-mean(myresponse, na.rm=TRUE))/sd(myresponse, na.rm=TRUE)

###############################################################
#User must define value1, value2, value3 for interpolation
###############################################################

mystart <- value1 #beginning of useful data (time point, indexes response)
myend <- value2 #end of useful data (time point, indexes response)
myfrequency <- value3 #frequency of time (1 = annual, etc.)

###############################################################
#interpolation for missing data
#may not be required for all datasets
#uncomment if needed
#
  interp.data <- myresponse
# for (i in 1:length(interp.data))
# {if (is.na(interp.data[i]))
                                        #interp.data[i] <- rnorm(1,mean(interp.data[mystart:myend],na.rm=T),
                                        #sd(interp.data[mystart:myend],na.rm=T))}
###############################################################

###############################################################
# replace "interp.data" with "myresponse" here and in plot routines
# if no interpolation was needed

myresponse.loess <- loess(interp.data ~ mytime)
interp.data <- ts(interp.data, start=mystart, frequency = myfrequency)


###############################################################
## Change-point analysis (strucchange)
###############################################################

y <- interp.data #place-holder

## difference or detrend, you decide

# difference the time series to remove trend
y.diff <- diff(y)

# OR detrend relative to loess to remove trend
y.detrend <- y-predict(myresponse.loess)

# first use basic model that fits constant to the ts data
# then plot the result
ocus.y.detrend <- efp(y.detrend ~1, type="OLS-CUSUM")
plot(ocus.y.detrend)

# Fstats gives a sequence of F statistics
# compute and plot
fs.y.detrend <- Fstats(y.detrend~1)
plot(fs.y.detrend)

# breakpoint estimate for the F-statistic
# this will be slow for very long (thousands of points) time series
breakpoints(fs.y.detrend)

# this computes all possible breakpoints
# this will be slow for very long (thousands of points) time series

bp.y.detrend <- breakpoints(y.detrend~1)

# model selection (n breakpoints)using minimum of BIC or RSS
plot(bp.y.detrend)

# this identifies one breakpoint
bp1.y.detrend <- breakpoints(bp.y.detrend, breaks=1)

# fit a null model with no breakpoint
fm0.y.detrend <- lm(y.detrend ~ 1)

# use the breakpoint for coefficients on either side
y.detrend.fac <- breakfactor(bp1.y.detrend, breaks=1)

# fits alternative model with breakpoints in y.detrend.fac
fm1.y.detrend <- lm(y.detrend ~ y.detrend.fac -1, )

## this process could be repeated for multiple breakpoints...

# return coefficents
coef(fm0.y.detrend)
coef(fm1.y.detrend)

# plot the detrended series then add the predicted values from strucchange models
plot(y.detrend)
lines(ts(predict(fm0.y.detrend), start=mystart, frequency=myfrequency), col="red", lty=3)
lines(ts(predict(fm1.y.detrend), start=mystart, frequency=myfrequency), col="blue", lty=1)



###############################################################
## Finished plots
## NEED TO SET BEGINNING (value 4) AND END (value 5) OF TIME SERIES
###############################################################

mytime1 <- value4
mytime2 <- value5
mytimes <- c(mytime1, mytime2)

par(mfrow=c(2,2))

###############################################################

# 1. Driver alone (time series)

plot(mytime, mydriver,col="black", font.axis=2, font.lab=2,cex=1.25,
     cex.lab=1.75, cex.axis=1.5, xlim=mytimes), type="b", pch=19,xlab="", ylab="Driver", xlab="Time", cex=2, font=2)
