load("C:/Users/wonha/Desktop/Teams.RData")
Hitting <- aggregate(cbind(Teams$HR, Teams$SO, Teams$G), by=list(Category=Teams$yearID), FUN=sum)
ERAs <- aggregate(cbind(Teams$ERA, Teams$G), by=list(Category=Teams$yearID), FUN=mean)
Attendance <- aggregate(cbind(Teams$attendance, Teams$G), by=list(Category=Teams$yearID), FUN = sum)

names(Hitting)[names(Hitting) == "V1"] <- "Home_Runs"
names(Hitting)[names(Hitting) == "V2"] <- "Strike_Outs"
names(Hitting)[names(Hitting) == "V3"] <- "Games"

names(ERAs)[names(ERAs) == "V1"] <- "ERA"
names(ERAs)[names(ERAs) == "V2"] <- "Games" #Don't really need this since ERA is already a rate statistic

names(Attendance)[names(Attendance) == "V1"] <- "Attendance"
names(Attendance)[names(Attendance) == "V2"] <- "Games"

Hitting$HRperG <- Hitting$Home_Runs/(Hitting$Games/2)
Hitting$SOperG <- Hitting$Strike_Outs/(Hitting$Games/2)

Attendance$AttendperG <- Attendance$Attendance/(Attendance$Games/2)

HR.ts <- as.ts(Hitting$Home_Runs, start=1871)
Ks.ts <- as.ts(Hitting$Strike_Outs, start=1871)
HRperG.ts <- as.ts(Hitting$HRperG, start=1871)
KsperG.ts <- as.ts(Hitting$SOperG, start=1871)

ERA.ts <- as.ts(ERAs$ERA, start=1871)

Attend.ts <- as.ts(Attendance$Attendance, start=1871)
AttendperG.ts <- as.ts(Attendance$AttendperG, start=1871)

par(mfcol = c(2,1))
plot(HR.ts, type = "l") #Looks like an upward trend
plot(HRperG.ts, type = "l") #Looks like an upward trend

plot(Ks.ts, type= "l") #Looks like an upward trend
plot(KsperG.ts, type="l") #Looks like an upward trend

par(mfcol = c(1,1))
plot(ERA.ts, type = "l") #Looks like it has a stable mean, variance looks unstable

par(mfcol = c(2,1))
plot(Attend.ts, type = "l") #Looks like an upward trend
plot(AttendperG.ts, type = "l") #Looks like an upward trend

#Try decomposing trend and seasonality like in chapter 1
##Can't do that because I don't know any recognized periods.  For these annual values, I don't know if there are any natural cycles or patterns

#Correlogram of each series
acf(HR.ts, lag.max = 50) #very slow decay
acf(HRperG.ts, lag.max = 50) #Very slow decay

acf(Ks.ts) #need to account for NA's
acf(KsperG.ts) #need to account for NAs

par(mfcol = c(1,1))
acf(ERA.ts) #slow decay, and looks like there is a cycle

par(mfcol = c(2,1))
acf(Attend.ts) #need to account for NAs
acf(AttendperG.ts)#need to account for NAs

## In the Ks and Attendance time series, I get an error because of a missing value.
## Missing Values for Ks at year 41 and 42 (1911 and 1912), and 44 and 45 for attendance (1914,1915)
## Consider shortening those time series
par(mfcol = c(2,1))
acf(Ks.ts[43:148], lag.max = 50) #Very slow decay
acf((KsperG.ts[43:148]), lag.max = 50) #Very slow decay

acf(Attend.ts[46:148], lag.max = 50) #Very slow decay
acf(AttendperG.ts[46:148], lag.max = 50) #Very slow decay


########################################################
#Plotting and correlogram of differenced of time series#
########################################################
plot(diff(HR.ts), type = "l") # There is increasing variance in the differenced ts
acf(diff(HR.ts)) # There seems to be some significant lags, enough to further investigate

plot(diff(HRperG.ts), type = "l") # There is increasing variance in the differenced ts
acf(diff(HRperG.ts)) #The differenced HRperG correlogram looks good, but the slow decay from before is worrying.

plot(diff(Ks.ts[43:148]), type = "l") #sudden spikes in variance of differenced Ks ts
acf(diff(Ks.ts[43:148])) #Looks like there are less significant lags, but the ones that are there are large enough to be concerned with, on top of the cyclical acf we saw before

plot(diff(KsperG.ts[43:148]), type = "l") #Not sure what to make, of this, looks like white noise
acf(diff(KsperG.ts[43:148])) #The differenced HRperG correlogram looks good, but the slow decay from before is worrying.

plot(diff(ERA.ts), type = "l") #Variance of differenced ERA ts has large variation at beginning, Settles later on
acf(diff(ERA.ts)) #Difference ERA correlogram looks good, however, we saw earlier in the slow decay in regular correlogram.

plot(diff(Attend.ts[46:148]), type = "l") #Spikes in variance of differenced attendance
acf(diff(Attend.ts[46:148])) #Differenced attendance ts correlogram does not have many signifcant lags. It had slow decay in regular correlogram.

plot(diff(AttendperG.ts[46:148]), type = "l") #Spikes in variance of differenced attendance
acf(diff(AttendperG.ts[46:148])) #Differenced attendance ts correlogram does not have many signifcant lags. It had slow decay in regular correlogram.

########################################################################
######################        HRs             ##########################
########################################################################
#####################
#Fitting an AR model#
#####################
par(mfcol = c(1,1))
#Home Runs
HR.ar <- ar(HR.ts, method = "mle")
HR.ar$order
HR.ar$ar
acf(HR.ar$res[-(1:HR.ar$order)])
#An AR(3) model doesn't seem like a great fit.  There are about 5 lags that are slightly significant
acf(HR.ar$res[-(1:HR.ar$order)], lag=50) #First half looks like above, last half has insignificant lags
acf(HR.ar$resid[-(1:HR.ar$order)])
AIC(HR.ar) #Does not work for ar fxn

#Home Runs per Game
# #ar modeling does not seem to work for this, maybe try arima fxn
# HRperG.ar <- ar(HRperG.ts, method = "mle")
# HRperG.ar$order
# HRperG.ar$ar
# acf(HRperG.ar$res[-(1:HRperG.ar$order)])
# #An AR(3) model doesn't seem like a great fit.  There are about 5 lags that are slightly significant
# acf(HRperG.ar$res[-(1:HRperG.ar$order)], lag=50) #First half looks like above, last half has insignificant lags
# acf(HRperG.ar$resid[-(1:HRperG.ar$order)])
# #Error in solve.default(res$hessian * length(x)) : 
# #Lapack routine dgesv: system is exactly singular: U[2,2] = 0


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

#Home Runs per Game
HRperG.lm <- lm(HRperG.ts ~ time(HRperG.ts))
coef(HRperG.lm)
confint(HRperG.lm)
acf(HRperG.lm$residuals, lag = 50)
#there is slow decay in the residuals and it cycles to negative magnitude
#Need to look into seasonal effects
pacf(HRperG.lm$residuals) #This one looks good.
HRperG.lm.aic <- AIC(HRperG.lm)
HRperG.lm.aic #I shouldn't compare AIC for raw totals vs. rate though...


############
#ARMA Model#
############
#replicate AR model above with arima fxn so I can get the AIC
HR.ar3 <- arima(resid(HR.lm), order = c(3,0,0))
HR.ar.aic <- HR.ar3$aic

#Home Runs
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) for (j in 0:2) {
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

#Home Runs per Game
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:3) for (j in 0:3) {
  fit.aic <- AIC(arima(resid(HRperG.lm), order = c(i, 0, j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(HRperG.lm), order = best.order)
    best.aic <- fit.aic
  }
}

HRperG.bestARMA <- best.order 
HRperG.bestARMA#(1, 0, 1)

HRperG.ARMA.res <- resid(best.arma)
acf(HRperG.ARMA.res) #No significant lags, but they seem to be growing
acf(HRperG.ARMA.res, lag.max = 50) #No significant lags

HRperG.ARMA.aic <- best.aic

model_compare_perG <- cbind(HRperG.lm.aic, HRperG.ARMA.aic) 
colnames(model_compare_perG) <- c("LinearModelperG", "ARMAperG")
rownames(model_compare_perG) <- "AIC"
model_compare_perG #Remember I couldn't do an AR on the per games ts.  ARMA is the best model.


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


#####################
#Fitting an AR model#
#####################
par(mfcol = c(1,1))
AttendperG.ar <- ar(imp_AttendperG, method = "mle")
AttendperG.ar$order
AttendperG.ar$ar
acf(AttendperG.ar$res[-(1:AttendperG.ar$order)])
#An AR(3) model Looks like a pretty good fit already.
acf(AttendperG.ar$res[-(1:AttendperG.ar$order)], lag=50)
acf((AttendperG.ar$res[-(1:AttendperG.ar$order)])^2, lag=50) #Still looks good.

#Stop here and talk to Dr. Robertson.


# ###################
# #Regression Models#
# ###################
# #Home Runs
# AttendperG.lm <- lm(AttendperG.ts ~ time(AttendperG.ts))
# coef(AttendperG.lm)
# confint(AttendperG.lm)
# acf(AttendperG.lm$residuals, lag = 50)
# #there is slow decay in the residuals and it cycles to negative magnitude
# #Need to look into seasonal effects
# pacf(AttendperG.lm$residuals) #Looks very strange.  This simple model might not be good enough.
# AttendperG.lm.aic <- AIC(AttendperG.lm)
# AttendperG.lm.aic
# 
# #Home Runs per Game
# AttendperGperG.lm <- lm(AttendperGperG.ts ~ time(AttendperGperG.ts))
# coef(AttendperGperG.lm)
# confint(AttendperGperG.lm)
# acf(AttendperGperG.lm$residuals, lag = 50)
# #there is slow decay in the residuals and it cycles to negative magnitude
# #Need to look into seasonal effects
# pacf(AttendperGperG.lm$residuals) #This one looks good.
# AttendperGperG.lm.aic <- AIC(AttendperGperG.lm)
# AttendperGperG.lm.aic #I shouldn't compare AIC for raw totals vs. rate though...
# 
# 
# ############
# #ARMA Model#
# ############
# #replicate AR model above with arima fxn so I can get the AIC
# AttendperG.ar3 <- arima(resid(AttendperG.lm), order = c(3,0,0))
# AttendperG.ar.aic <- AttendperG.ar3$aic
# 
# #Home Runs
# best.order <- c(0, 0, 0)
# best.aic <- Inf
# for (i in 0:2) for (j in 0:2) {
#   fit.aic <- AIC(arima(resid(AttendperG.lm), order = c(i, 0, j)))
#   if (fit.aic < best.aic) {
#     best.order <- c(i, 0, j)
#     best.arma <- arima(resid(AttendperG.lm), order = best.order)
#     best.aic <- fit.aic
#   }
# }
# 
# AttendperG.bestARMA <- best.order 
# AttendperG.bestARMA#(1, 0, 1)
# 
# AttendperG.ARMA.res <- resid(best.arma)
# acf(AttendperG.ARMA.res) #Significant lags later on
# 
# AttendperG.ARMA.aic <- best.aic
# 
# model_compare <- cbind(AttendperG.lm.aic, AttendperG.ar.aic, AttendperG.ARMA.aic) 
# colnames(model_compare) <- c("LinearModel", "AR(3)","ARMA")
# rownames(model_compare) <- "AIC"
# model_compare #ARMA is best model so far.
# 
# #Home Runs per Game
# best.order <- c(0, 0, 0)
# best.aic <- Inf
# for (i in 0:3) for (j in 0:3) {
#   fit.aic <- AIC(arima(resid(AttendperGperG.lm), order = c(i, 0, j)))
#   if (fit.aic < best.aic) {
#     best.order <- c(i, 0, j)
#     best.arma <- arima(resid(AttendperGperG.lm), order = best.order)
#     best.aic <- fit.aic
#   }
# }
# 
# AttendperGperG.bestARMA <- best.order 
# AttendperGperG.bestARMA#(1, 0, 1)
# 
# AttendperGperG.ARMA.res <- resid(best.arma)
# acf(AttendperGperG.ARMA.res) #No significant lags, but they seem to be growing
# acf(AttendperGperG.ARMA.res, lag.max = 50) #No significant lags
# 
# AttendperGperG.ARMA.aic <- best.aic
# 
# model_compare_perG <- cbind(AttendperGperG.lm.aic, AttendperGperG.ARMA.aic) 
# colnames(model_compare_perG) <- c("LinearModelperG", "ARMAperG")
# rownames(model_compare_perG) <- "AIC"
# model_compare_perG #Remember I couldn't do an AR on the per games ts.  ARMA is the best model.
# 
# 
# ##########################
# #ARIMA Model/SARIMA Model#
# ##########################
# plot(AttendperG.ts)
# plot(diff(AttendperG.ts))
# plot(diff(log(AttendperG.ts))) #variance switches to high at beginning and small at end
# 
# get.best.arima <- function(x.ts, maxord=c(1,1,1,1,1,1))
# {
#   best.aic <- 1e8
#   n <- length(x.ts)
#   for(p in 0:maxord[1]) for (d in 0:maxord[2]) for (q in 0:maxord[3])
#     for(P in 0:maxord[4]) for (D in 0:maxord[5]) for (Q in 0:maxord[6])
#     {
#       fit <- arima(x.ts, order = c(p,d,q),
#                    seas = list(order = c(P,D,Q),
#                                frequency(x.ts)), method = "CSS")
#       fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
#       if (fit.aic < best.aic)
#       {
#         best.aic <- fit.aic
#         best.fit <- fit
#         best.model <- c(p,d,q,P,D,Q)
#       }
#     }
#   list(best.aic, best.fit, best.model)
# }
# 
# best.arima.AttendperG <- get.best.arima(AttendperG.ts, maxord= c(2,2,2,2,2,2))
# 
# best.model.arima.AttendperG <- best.arima.AttendperG[[3]]
# best.model.arima.AttendperG #(1, 2, 1, 1, 2, 2)
# 
# best.fit.AttendperG <- best.arima.AttendperG[[2]]
# best.fit.AttendperG
# 
# best.arima.AttendperG.aic <- best.arima.AttendperG[[1]]
# model_compare <- cbind(AttendperG.lm.aic, AttendperG.ar.aic, AttendperG.ARMA.aic, best.arima.AttendperG.aic) 
# colnames(model_compare) <- c("LinearModel", "AR(3)","ARMA", "SARIMA")
# rownames(model_compare) <- "AIC"
# model_compare #SARIMA is best model so far.
# 
# acf(resid(best.fit.AttendperG)) #Still doesn't look great.  Check for conditional heteroskedacity
# acf(resid(best.fit.AttendperG)^2) #Definitely some patterned cycling going on here. Move on to GARCH
# 
# 
# #############
# #GARCH Model#
# #############
# library(tseries)
# AttendperG.SARIMA.res <- resid(best.fit.AttendperG)
# AttendperG.garch <- garch(AttendperG.SARIMA.res, trace = F)
# AttendperG.garch.res <- resid(AttendperG.garch)[-1]
# 
# acf(AttendperG.garch.res)
# acf(AttendperG.garch.res^2)
# 
# garch.AttendperG.aic <- AIC(AttendperG.garch)
# model_compare <- cbind(AttendperG.lm.aic, AttendperG.ar.aic, AttendperG.ARMA.aic, best.arima.AttendperG.aic, garch.AttendperG.aic) 
# colnames(model_compare) <- c("LinearModel", "AR(3)","ARMA", "SARIMA", "GARCH")
# rownames(model_compare) <- "AIC"
# model_compare #GARCH of SARIMA model is best model so far.
# 
# #Looks like I need to move on to taking fractional difference before I move on to other models
# 
# 
# #######################
# #Fractional difference#
# #######################
# library(fracdiff)
# fds.AttendperG <- fracdiff(AttendperG.ts, nar=25)
# #I suspect that I will end up needing ARMA, so I'll go straight to that.
# n <- length(AttendperG.ts)
# L <- 30
# d <- fds.AttendperG$d
# fdc <- d
# fdc[1] <- fdc
# for (k in 2:L) fdc[k] <- fdc[k-1] * (d+1-k) /k
# y <- rep (0, L)
# for (i in (L+1):n) {
#   csm <- AttendperG.ts[i]
#   for (j in 1:L) csm <- csm + ((-1)^j) * fdc[j] * AttendperG.ts[i-j]
#   y[i] <- csm
# }
# ts.fracdiff <- y[(L+1):n]
# 
# best.order <- c(0, 0, 0)
# best.aic <- Inf
# for (i in 0:2) for (j in 0:2) {
#   fit.aic <- AIC(arima(ts.fracdiff, order = c(i, 0, j)))
#   if (fit.aic < best.aic) {
#     best.order <- c(i, 0, j)
#     best.arma <- arima(ts.fracdiff, order = best.order)
#     best.aic <- fit.aic
#   }
# }
# 
# #Find best ARMA model using the fractionally differenced time series
# bestARMA <- best.order 
# bestARMA#(1, 0, 1)
# 
# FARMA.resid <- resid(best.arma)
# acf(FARMA.resid, lag.max = 50) #Significant lags
# 
# #To find the model equation, I have to put the coefficients from the models
# d
# best.arma
# 
# #How does the model compare to others I have tried?
# arfima.aic <- best.arma$aic
# model_compare <- cbind(AttendperG.lm.aic, AttendperG.ar.aic, AttendperG.ARMA.aic, best.arima.AttendperG.aic, garch.AttendperG.aic, arfima.aic) 
# colnames(model_compare) <- c("LinearModel", "AR(3)","ARMA", "SARIMA", "GARCH", "ARFIMA")
# rownames(model_compare) <- "AIC"
# model_compare #ARFIMA model is best model so far, by a lot.
# 





