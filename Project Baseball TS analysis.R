load("C:/Users/wonha/Desktop/SMU Class Material/6363 Time Series Analysis/Project/Teams.RData")
#Data set span: 1871 - 2018
Hitting <- aggregate(cbind(Teams$HR, Teams$G), by=list(Category=Teams$yearID), FUN=sum)
Attendance <- aggregate(cbind(Teams$attendance, Teams$G), by=list(Category=Teams$yearID), FUN = sum)

names(Hitting)[names(Hitting) == "V1"] <- "Home_Runs"
names(Hitting)[names(Hitting) == "V2"] <- "Games"

names(Attendance)[names(Attendance) == "V1"] <- "Attendance"
names(Attendance)[names(Attendance) == "V2"] <- "Games"

Attendance$AttendperG <- Attendance$Attendance/(Attendance$Games/2)

HR.ts <- as.ts(Hitting$Home_Runs, start=1871) 
Attend.ts <- as.ts(Attendance$Attendance, start=1871)
AttendperG.ts <- as.ts(Attendance$AttendperG, start=1871)

par(mfcol = c(3,1))
plot(HR.ts, type = "l", main = "Total # of Home Runs Hit in MLB", ylab = "Home Runs", sub = "1871 - 2018") #Looks like an upward trend
plot(Attend.ts, type = "l", main = "Total Attendance in MLB", ylab = "Total Attendance", sub = "1871 - 2018") #Looks like an upward trend
plot(AttendperG.ts, type = "l", main = "Attendance per Game in MLB", ylab = "Attendance per Game", sub = "1871 - 2018") #Looks like an upward trend

#Creating a training series and test series for forcasting later
TrainHR.ts <- HR.ts[1:140]
TestHR.ts <- HR.ts[141:148]

#Try decomposing trend and seasonality like in chapter 1
##Can't do that because I don't know any recognized periods.  For these annual values, I don't know if there are any natural cycles or patterns

#Correlogram of each series
#par(mfcol = c(1,1))
acf(HR.ts, lag.max = 50, main = "Correlogram: Home Runs") #very slow decay

#par(mfcol = c(2,1))
#acf(Attend.ts) #need to account for NAs
#acf(AttendperG.ts)#need to account for NAs

## In the Ks and Attendance time series, I get an error because of a missing value.
## Missing Values for t = 44 and 45 for attendance (1914,1915)
## Consider shortening those time series
acf(Attend.ts[46:148], lag.max = 50, main = "Correlogram: Attendance") #Very slow decay
acf(AttendperG.ts[46:148], lag.max = 50, main = "Correlogram: Attendance per Game") #Very slow decay


########################################################
#Plotting and correlogram of differenced of time series#
########################################################
par(mfrow = c(2,1))
plot(diff(HR.ts), type = "l", main = "Differenced Time Series of Home Runs", sub = "1871 - 2018", ylab = "Home Runs") # There is increasing variance in the differenced ts
acf(diff(HR.ts), main = "Correlogram: Differenced Time Series of Home Runs") # There seems to be some significant lags, enough to further investigate

plot(diff(Attend.ts[46:148]), type = "l") #Spikes in variance of differenced attendance
acf(diff(Attend.ts[46:148])) #Differenced attendance ts correlogram does not have many signifcant lags. It had slow decay in regular correlogram.

plot(diff(AttendperG.ts[46:148]), type = "l", main = "Differenced Time Series of Attendance per Game", sub = "1871 - 2018", ylab = "Attendance per Game") #Spikes in variance of differenced attendance
acf(diff(AttendperG.ts[46:148]), main = "Correlogram: Differenced Time Series of Attendance per Game") #Differenced attendance ts correlogram does not have many signifcant lags. It had slow decay in regular correlogram.
#This indicates that we might be able to more easily model attendance per game


########################################################################
######################        HRs             ##########################
########################################################################
####################
#Holt-Winters Model#
####################
par(mfcol = c(1,1))
HR.hw <- HoltWinters(HR.ts, alpha=1, gamma=FALSE, seasonal = "mult")  ### Not gamma=0.  Setting "Gamma=False" means we don't suspect seasonality in a random walk.
HR.hw #HR.hw summary is same regardless of if I choose seasonal = multiplicative or additive
acf(resid(HR.hw)) #Looks like we can improve on this

#See how well Holt-Winters predicts on the training and test HR time series
HW.Train <- HoltWinters(TrainHR.ts, alpha = 1, gamma = F)
HW.Train

#Predict and plot prediction
HW.predict <- predict(HW.Train, n.ahead = 8)   
HW.predict

ts.plot(HR.ts, HW.predict, lty=1:3, col = 1:2, main = "Forecasting Home Run Totals for 2011-2018 Seasons", sub = "Method: Holt-Winters", ylab = "Total Home Runs") 
#We see that it recognizes the upward trend we see throughout, but it does so in a slow smooth line.  Not accurate.
#Move on to more methods


#####################
#Fitting an AR model#
#####################
#Home Runs
HR.ar <- ar(HR.ts, method = "mle")
HR.ar$order
HR.ar$ar
acf(HR.ar$res[-(1:HR.ar$order)])
#An AR(3) model doesn't seem like a great fit.  There are about 5 lags that are slightly significant
acf(HR.ar$res[-(1:HR.ar$order)], lag=50) #First half looks like above, last half has insignificant lags
acf(HR.ar$resid[-(1:HR.ar$order)])
AIC(HR.ar) #Does not work for ar fxn



###################
#Regression Models#
###################
#Home Runs
HR.lm <- lm(HR.ts ~ time(HR.ts))
coef(HR.lm)
confint(HR.lm)
acf(HR.lm$residuals, lag = 50)
#there is slow decay in the residuals and it cycles to negative magnitude
#Need to look into seasonal effects
pacf(HR.lm$residuals) #Looks very strange.  This simple model might not be good enough.
HR.lm.aic <- AIC(HR.lm)
HR.lm.aic


############
#ARMA Model#
############
#replicate AR model above with arima fxn so I can get the AIC number
HR.ar3 <- arima(resid(HR.lm), order = c(3,0,0))
HR.ar.aic <- HR.ar3$aic

#Home Runs
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:4) for (j in 0:2) {
  fit.aic <- AIC(arima(resid(HR.lm), order = c(i, 0, j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(HR.lm), order = best.order)
    best.aic <- fit.aic
  }
}

HR.bestARMA <- best.order 
HR.bestARMA#(1, 0, 1)

HR.ARMA.res <- resid(best.arma)
acf(HR.ARMA.res) #Significant lags later on

HR.ARMA.aic <- best.aic

model_compare <- cbind(HR.lm.aic, HR.ar.aic, HR.ARMA.aic) 
colnames(model_compare) <- c("LinearModel", "AR(3)","ARMA")
rownames(model_compare) <- "AIC"
model_compare #ARMA is best model so far.

#Trying frequency/spectral model#
#Using ARMA model
plotts.sample.wge(HR.ts)#Time Series of HRs
aic5.wge(HR.ts,p=0:6,q=0:2) #Spits out ARMA(4,2) on the time series

plotts.sample.wge(resid(HR.lm))#Time Series of residuals from linear model
aic5.wge(resid(HR.lm), p=0:6, q=0:2) #Spits out ARMA(1,1) exactly as we saw before
## AIC selects an ARMA(3,1) stationary model ##
HR.spec.est<-est.arma.wge(resid(HR.lm),p=1,q=1)

###Check residuals ###
plotts.sample.wge(HR.spec.est$res,arlimits=TRUE)
ljung.wge(HR.spec.est$res,p=1,q=1) #pvalue under .05 so there is still some variance that needs to be accounted for, just like above.
#This verifies what I concluded above, so we continue on with a more complex model.


##########################
#ARIMA Model/SARIMA Model#
##########################
plot(HR.ts)
plot(diff(HR.ts))
plot(diff(log(HR.ts))) #variance switches to high at beginning and small at end

get.best.arima <- function(x.ts, maxord=c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for(p in 0:maxord[1]) for (d in 0:maxord[2]) for (q in 0:maxord[3])
    for(P in 0:maxord[4]) for (D in 0:maxord[5]) for (Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, best.fit, best.model)
}

best.arima.HR <- get.best.arima(HR.ts, maxord= c(2,2,2,2,2,2))

best.model.arima.HR <- best.arima.HR[[3]]
best.model.arima.HR #(1, 2, 1, 1, 2, 2)

best.fit.HR <- best.arima.HR[[2]]
best.fit.HR

best.arima.HR.aic <- best.arima.HR[[1]]
model_compare <- cbind(HR.lm.aic, HR.ar.aic, HR.ARMA.aic, best.arima.HR.aic) 
colnames(model_compare) <- c("LinearModel", "AR(3)","ARMA", "SARIMA")
rownames(model_compare) <- "AIC"
model_compare #SARIMA is best model so far.

acf(resid(best.fit.HR)) #Still doesn't look great.  Check for conditional heteroskedacity
acf(resid(best.fit.HR)^2) #Definitely some patterned cycling going on here. Move on to GARCH

#############
#GARCH Model#
#############
library(tseries)
HR.SARIMA.res <- resid(best.fit.HR)
HR.garch <- garch(HR.SARIMA.res, trace = F)
HR.garch.res <- resid(HR.garch)[-1]

acf(HR.garch.res)
acf(HR.garch.res^2)

garch.HR.aic <- AIC(HR.garch)
model_compare <- cbind(HR.lm.aic, HR.ar.aic, HR.ARMA.aic, best.arima.HR.aic, garch.HR.aic) 
colnames(model_compare) <- c("LinearModel", "AR(3)","ARMA", "SARIMA", "GARCH")
rownames(model_compare) <- "AIC"
model_compare #GARCH of SARIMA model is best model so far.

#Looks like I need to move on to taking fractional difference before I move on to other models

#Try frequency/spectral model#
#ARIMA, because we assume not stationary, so we difference
diff.HR <- artrans.wge(resid(HR.lm),phi.tr=1)  #this command is a little cryptic, but means "transform the AR (phi) side of equation by differencing once" 
plotts.sample.wge(d1.temp,arlimits=TRUE) #Similar to what we saw in the ARIMA above, doesn't have great correlogram
aic5.wge(d1.temp,p=0:6,q=0:2)# AIC selects an ARMA(3,2)
diff.HR.est<-est.arma.wge(d1.temp,p=3,q=2)
###Check residuals ###
plotts.sample.wge(diff.HR.est$res,arlimits=TRUE)#acf looks good, though you still see the growing oscillation
ljung.wge(diff.HR.est$res,p=3,q=2) #We see that all the variance has been accounted for now with the spectral modeling
#Final Spectral Model
mean(resid(HR.lm))
diff.HR.est$phi
diff.HR.est$theta


#######################
#Fractional difference#
#######################
library(fracdiff)
fds.HR <- fracdiff(HR.ts, nar=25)
#I suspect that I will end up needing ARMA, so I'll go straight to that.
n <- length(HR.ts)
L <- 30
d <- fds.HR$d
fdc <- d
fdc[1] <- fdc
for (k in 2:L) fdc[k] <- fdc[k-1] * (d+1-k) /k
y <- rep (0, L)
for (i in (L+1):n) {
  csm <- HR.ts[i]
  for (j in 1:L) csm <- csm + ((-1)^j) * fdc[j] * HR.ts[i-j]
  y[i] <- csm
}
ts.fracdiff <- y[(L+1):n]

best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) for (j in 0:2) {
  fit.aic <- AIC(arima(ts.fracdiff, order = c(i, 0, j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(ts.fracdiff, order = best.order)
    best.aic <- fit.aic
  }
}

#Find best ARMA model using the fractionally differenced time series
bestARMA <- best.order 
bestARMA#(1, 0, 1)

FARMA.resid <- resid(best.arma)
acf(FARMA.resid, lag.max = 50) #few significant lags, and magnitude is of those are still low.
acf(FARMA.resid^2, lag.max = 50) #Still needs some work as there are a couple significant lags and there is a clear pattern in the correlogram.

#How does the model compare to others I have tried?
arfima.aic <- best.arma$aic
model_compare <- cbind(HR.lm.aic, HR.ar.aic, HR.ARMA.aic, best.arima.HR.aic, garch.HR.aic, arfima.aic) 
colnames(model_compare) <- c("LinearModel", "AR(3)","ARMA", "SARIMA", "GARCH", "ARFIMA")
rownames(model_compare) <- "AIC"
model_compare #ARFIMA model is best model so far, by a lot.


################################
#GARCH:fractionally differenced#
################################
HR.garch_F <- garch(FARMA.resid, trace = F)
HR.garch_F.res <- resid(HR.garch_F)[-1]

acf(HR.garch_F.res, lag.max = 50)
acf(HR.garch_F.res^2, lag.max = 50)

#How does the model compare to others I have tried?
garch_f.HR.aic <- AIC(HR.garch_F)
model_compare <- cbind(HR.lm.aic, HR.ar.aic, HR.ARMA.aic, best.arima.HR.aic, garch.HR.aic, arfima.aic, garch_f.HR.aic) 
colnames(model_compare) <- c("LinearModel", "AR(3)","ARMA", "SARIMA", "GARCH", "ARFIMA", "GARCH:Frac. Diff")
rownames(model_compare) <- "AIC"
model_compare #GARCH on ARFIMA model provides an improvement on the ARFIMA model.

#To find the model equation, I have to pull the coefficients from the models
d
summary(HR.garch_F)








####################################################################################
######################        Attendance per game         ##########################
####################################################################################

###What I need to do first is use imputation to impute the 3 missing years
#install.packages("imputeTS")
library(imputeTS)
#Plot missing values
par(mfcol = c(1,1))
plotNA.distribution(AttendperG.ts)
#Based on the fact that I know that home runs required ARIMA type modeling, I'll have it do an imputation based on ARIMA for attendnace per game
imp_AttendperG <- na_kalman(AttendperG.ts, model = "auto.arima")
#Now look at time series with imputations
plotNA.imputations(AttendperG.ts, imp_AttendperG)
statsNA(AttendperG.ts)
#Not comfortable with this as the first 20ish values are imputed and could effect analysis 
which(is.na(AttendperG.ts))
new.AttendperG.ts <- AttendperG.ts[22:length(AttendperG.ts)]

#new imputed time series of attendance per game
new.imp_AttendperG <- na_kalman(new.AttendperG.ts, model = "auto.arima")
plotNA.imputations(AttendperG.ts[22:length(AttendperG.ts)], new.imp_AttendperG)


#####################
#Fitting an AR model#
#####################
par(mfcol = c(1,1))
AttendperG.ar <- ar(new.imp_AttendperG, method = "mle")
AttendperG.ar$order
AttendperG.ar$ar
acf(AttendperG.ar$res[-(1:AttendperG.ar$order)])
#An AR(3) model Looks like a pretty good fit already.
acf(AttendperG.ar$res[-(1:AttendperG.ar$order)], lag=50)
acf((AttendperG.ar$res[-(1:AttendperG.ar$order)])^2, lag=50) #Still looks good.
pacf(AttendperG.ar$res[-(1:AttendperG.ar$order)])



###################################################
#Check if the two variables are lagging each other#
###################################################
##############
#VAR analysis#
##############
ccf(HR.ts[22:length(HR.ts)], new.imp_AttendperG) #Looks like a lot of correlation with each other



