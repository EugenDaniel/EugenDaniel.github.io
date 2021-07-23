# FORECATING AND TIME SERIES - ASSIGNMENT 2
# TEAM D

###################################### ENVIRONMENT SETUP ###################################### 

# Load the necessary R packages
#install.packages("normwhn.test")
#install.packages("astsa")
library(normwhn.test)
library(fBasics)
library(forecast)
library(tseries)
library(astsa)
library(stats)

# Set the folder path for the user and read the file
folder_path <- "C:/Users/Dennis/Documents/Dennis Documents/MBA/IE/MBD/Term 2/Forecasting Time Series/Group Assignments/Homework 2"
dataset <- read.csv(file.path(folder_path,"coca_cola_earnings.csv"),header = T,sep=";",dec=",")

# Cast the anntime column to date
dataset$anntime <- as.Date(strptime(dataset$anntime,format = "%Y%m%d"))

# Get the quarterly earnings per share of Coca-Cola Company in a time series

    # Check the starting year and month of the data
      start_year = as.numeric(format(min(dataset$anntime),"%Y"))
      start_month = as.numeric(format(min(dataset$anntime),"%m"))
    # Define the earnings as a quarterly time series
    # (frequency is set to 4 since it is a quarterly data)
      CC_TS <- ts(dataset$value,start = c(start_year,start_month),frequency = 4)

###################################### ANALYSIS OF DATA ###################################### 

#### Data Visualization ####
      
      
# Plot the time series, acf and pacf
windows()
par(mfrow=c(3,1))
plot.ts(CC_TS, main="Coca Cola Quarterly Earnings Time Series") 
acf(dataset$value, main="acf of Coca Cola Quarterly Earnings")
pacf(dataset$value, main="pacf of Coca Cola Quarterly Earnings")
# Time series does not look stationary in the mean (has an increasing trend) 
# and heteroscedastic(variance is incresing in time).

# Check for the stationarity in the mean by checking the number of regular and seasonal differences 
# needed by utilizing ADF and OSCB tests, respectively.

# ADF: Augmented Dickey-Füller Test 
    # Ho: Data is not stationary in the mean (unit root exists)
    # Ha: Data is stationary in the mean 

ndiffs(CC_TS, alpha=0.05, test=c("adf"))

# Number of regular differences required for stationarity = 1.

# OSCB: Osborn-Chui-Smith-Birchenhall Test
  # Ho: A seasonal unit root exists
  # Ha: ASK THE PROFESSOR ???

nsdiffs(CC_TS,m = 4,test=c("ocsb")) # Since we have quarterly data, frequency was set to 4.
# Number of seasonal differences required for stationarity = 1.


# Taking logs after differencing the data produces NaNs, so we are taking logs first.

# Since the data is not stationary in the variance, taking logs would be a good option.
log_z <- log(dataset$value)

# Plot the time series, acf and pacf
windows()
par(mfrow=c(3,1)) 
plot.ts(log(CC_TS), main="Quarterly Log Returns of Coca Cola")
acf(log_z, main = "acf of Quarterly Log Returns of Coca Cola")
pacf(log_z, main = "pacf of Quarterly Log Returns of Coca Cola")


# Data seems stationary in the variance but not in the mean. 

# Plot the decomposition of additive time series
plot(log(CC_TS),type="l")
title("Quarterly Log Returns of Coca Cola")
components <- decompose(log(CC_TS))
components
plot(components)


# Check the number of regular differences needed again: 

# ADF: Augmented Dickey-Füller Test 
  # Ho: Data is not stationary in the mean (unit root exists)
  # Ha: Data is stationary in the mean 

ndiffs(log_z, alpha=0.05, test=c("adf"))
# Number of regular differences required for stationarity = 1.


# Perform 1 regular differencing on the logged data (z_t)
diff_log_z <- diff(log_z)

# Plot the time series, acf and pacf
windows()
par(mfrow=c(3,1)) 
plot.ts(diff_log_z, main="Log Returns of Coca Cola After d = 1") # Data seems stationary in the mean and variance.
acf(diff_log_z, main="acf of Log Returns of Coca Cola After d = 1")
pacf(diff_log_z, main="pacf Log Returns of Coca Cola After d = 1") 


# Since we oberve strong seasonality, we need to check for the seasnonal differencing option.
# OSCB: Osborn-Chui-Smith-Birchenhall Test
  # Ho: A seasonal unit root is present in a seasonal autoregressive model
  # Ha: No seasonal unit root required 

nsdiffs(diff_log_z,m = 4,test=c("ocsb"))
# Number of seasonal differences required for stationarity = 1.


# Perform seasonal differencing.
s_diff_log_z <- diff(diff_log_z,4)

# Plot the time series, acf and pacf
windows()
par(mfrow=c(3,1)) 
plot.ts(s_diff_log_z, main="Log Returns of Coca Cola After d = 1 & D = 1 (lag-4)") # Data seems stationary in the mean and variance.
acf(s_diff_log_z, main="acf of Log Returns of Coca Cola After d = 1 & D = 1 (lag-4)") # Lag 1-4-5-9 is significant
pacf(s_diff_log_z, main="pacf Log Returns of Coca Cola After d = 1 & D = 1 (lag-4)") # Lag 1 seems significant for non-seasonal term and there are spikes 
# in s = 4 and multiplicatives.


#### MODEL 1 ####
# We can fit a SARIMA model to the logged data. 

fit_1 <- sarima(xdata=log_z,p=1,d=1,q=5,P=2,D=1,Q=0,S = 4,details = FALSE)
fit_1
# When fitted using arima(...) calculate CIs for coefficients by Coeff +/- 1.96 *s.e. It can be seen 
# that 0 is in the confidence interval for MA 1,2,3 and 4. The others seem significant.

# In the sarima(...) model, check the p-values, says the same about the insigficant features.
# Check again by forcing the insignificant parameters' coefficients to 0.

# Force insignificant parameters to 0
fit_1 <- sarima(xdata=log_z,p=1,d=1,q=5,P=2,D=1,Q=0,S = 4,fixed = c(NA,0,0,0,0,NA,NA,NA),details = FALSE)
fit_1 # We can see from the p-values that all variables are significant.

# Fit the model found with arima(...) format to be able to access $residuals.
fit_1 <- arima(log_z,order=c(1,1,5),seasonal = list(order=c(2,1,0),period = 4),fixed = c(NA,0,0,0,0,NA,NA,NA))
fit_1

# We can keep investigating the residuals further if they are WN or not.

windows()
par(mfrow=c(3,1)) 
ts.plot(fit_1$residuals)
acf(fit_1$residuals,20)
pacf(fit_1$residuals,20) # No out of limits in acf and pacf - data is not correlated

# ADF: Augmented Dickey-Füller Test 
  # Ho: Data is not stationary in the mean (unit root exists)
  # Ha: Data is stationary in the mean 
ndiffs(fit_1$residuals, alpha=0.05, test=c("adf")) # Regular differences required is 0.

# T-TEST
# Check for mean = 0
  # Ho: Mean of the data is 0
  # H1: Mean of the data is different than 0

t.test(fit_1$residuals) 
#p-value = 0.8132 > 0.05, fail to reject Ho and conclude that mean is 0.


# BOX TEST
# Check for correlation in data
  # Ho: Data is uncorrelated
  # Ha: Data is correlated

Box.test(fit_1$residuals,lag=12,type="Ljung") # Lag-12 seems the one closest to the limit.
#p-value = 0.3604 > 0.05, we fail to reject Ho and conclude that data is uncorrelated.

# Since we have stationary and uncorrelated data with 0 mean, we can conclude that our
# residuals are White Noise.


# WHITE NOISE CROSS CHECK
windows()
whitenoise.test(fit_1$residuals)
#Test value (p-value) 0.1464764 > 0.05 thus fail to reject Ho --> Residuals are White Noise.

# SHAPIRO-WILK TEST 
# Check for normal distribution
  # Ho: Data is normally distributed
  # Ha: Data is not normally distributed

shapiro.test(fit_1$residuals)
#p-value = 1.451e-07 < 0.05. 
#Thus, we reject Ho and say that data is not normally distributed.





#### MODEL 2 ####
fit_2 <- sarima(xdata=log_z,p=4,d=1,q=0,P=2,D=1,Q=0,S = 4,details=FALSE)
fit_2 # From the p-values displayed, one can see that AR3 and SAR1 are insignificant.

# Force AR3 and SAR1 to 0.
fit_2 <- sarima(xdata=log_z,p=4,d=1,q=0,P=2,D=1,Q=0,S = 4,details=FALSE,fixed=c(NA,NA,0,NA,0,NA))
fit_2 # All parameters seem significant.

# Fit the model using arima(...) format to be able to access $residuals.
fit_2 <- arima(log_z,order=c(4,1,0),seasonal = list(order=c(2,1,0),period = 4),fixed=c(NA,NA,0,NA,0,NA))
fit_2

# We can keep investigating the residuals further if they are WN or not.

windows()
par(mfrow=c(3,1)) 
ts.plot(fit_2$residuals)
acf(fit_2$residuals,20)
pacf(fit_2$residuals,20) # lag 16 is out of limits in pacf, going to be checked with Box-Test.

# ADF: Augmented Dickey-Füller Test 
  # Ho: Data is not stationary in the mean (unit root exists)
  # Ha: Data is stationary in the mean 
ndiffs(fit_2$residuals, alpha=0.05, test=c("adf")) # Regular differences required is 0.

# T-TEST
# Check for mean = 0
  # Ho: Mean of the data is 0
  # H1: Mean of the data is different than 0

t.test(fit_2$residuals) 
#p-value = 0.7651 > 0.05, fail to reject Ho and conclude that mean is 0.


# BOX TEST
# Check for correlation in data
  # Ho: Data is uncorrelated
  # Ha: Data is correlated

Box.test(fit_2$residuals,lag=16,type="Ljung") # We need to check for lag-16.
#p-value = 0.7784 > 0.05, we fail to reject Ho and conclude that data is uncorrelated.

# Since we have stationary and uncorrelated data with 0 mean, we can conclude that our
# residuals are White Noise.


# WHITE NOISE CROSS CHECK
windows()
whitenoise.test(fit_2$residuals) 
#Test value (p-value) 0.1464764 > 0.05 thus fail to reject Ho --> White Noise.

# SHAPIRO-WILK TEST 
# Check for normal distribution
  # Ho: Data is normally distributed
  # Ha: Data is not normally distributed

shapiro.test(fit_2$residuals) 
#p-value = 4.486e-05 < 0.05. 
#Thus, we reject Ho and say that data is not normally distributed.





#### MODEL 3 ####
fit_3 <- sarima(xdata=log_z,p=8,d=1,q=5,P=0,D=1,Q=0,S = 4,details=FALSE)
fit_3 
# Looking at the p-values, one can see that only AR5,MA1,MA4 and MA5 are significant.

# Force insignificant variables coefficients to 0. 
fit_3 <- sarima(xdata=log_z,p=8,d=1,q=5,P=0,D=1,Q=0,S = 4,details=FALSE,fixed=c(0,0,0,0,NA,0,0,0,NA,0,0,NA,NA))
fit_3
# All seem significant.

# Fit the model using arima(...) format to be able to access $residuals.
fit_3 <- arima(log_z,order=c(8,1,5),seasonal = list(order=c(0,1,0),period = 4),fixed=c(0,0,0,0,NA,0,0,0,NA,0,0,NA,NA))
fit_3


# We can keep investigating the residuals further if they are WN or not.

windows()
par(mfrow=c(3,1)) 
ts.plot(fit_3$residuals)
acf(fit_3$residuals,20)
pacf(fit_3$residuals,20) # Lag 18 seem close to limits in pacf - to be checked with Box-Test.

# ADF: Augmented Dickey-Füller Test 
  # Ho: Data is not stationary in the mean (unit root exists)
  # Ha: Data is stationary in the mean 
ndiffs(fit_3$residuals, alpha=0.05, test=c("adf")) # Regular differences required is 0.

# T-TEST
# Check for mean = 0
  # Ho: Mean of the data is 0
  # H1: Mean of the data is different than 0

t.test(fit_3$residuals) 
#p-value = 0.4504 > 0.05, fail to reject Ho and conclude that mean is 0.


# BOX TEST
# Check for correlation in data
  # Ho: Data is uncorrelated
  # Ha: Data is correlated

Box.test(fit_3$residuals,lag=18,type="Ljung") #p-value = 0.6691 > 0.05, we fail to reject Ho. 
# After checking for all lags close to the limits, we can conclude that data is uncorrelated.

# Since we have stationary and uncorrelated data with 0 mean, we can conclude that our
# residuals are White Noise.


# WHITE NOISE CROSS CHECK
windows()
whitenoise.test(fit_3$residuals) 
#Test value (p-value) 0.09647798 > 0.05 thus fail to reject Ho --> White Noise.

# SHAPIRO-WILK TEST 
# Check for normal distribution
  # Ho: Data is normally distributed
  # Ha: Data is not normally distributed

shapiro.test(fit_3$residuals) 
# p-value = 8.469e-06 < 0.05. 
# Thus, we reject Ho and say that data is not normally distributed.




#### MODEL 4 : AUTO-ARIMA ####

fit_4 <- auto.arima(y=log(CC_TS),seasonal = TRUE)
fit_4 # ARIMA(0,1,1)(0,1,1)[4] is the model R founds as better.

# Fit the model using arima(...) format to be able to access $residuals.
fit_4 <- arima(log_z,order=c(0,1,1),seasonal = list(order=c(0,1,1),period = 4))
fit_4


# Investigate if the residuals further if they are WN or not.

windows()
par(mfrow=c(3,1)) 
ts.plot(fit_4$residuals)
acf(fit_4$residuals,20)
pacf(fit_4$residuals,20) # Lags 5, 9 and 18 seem close to limits in pacf - to be checked with Box-Test.

# ADF: Augmented Dickey-Füller Test 
  # Ho: Data is not stationary in the mean (unit root exists)
  # Ha: Data is stationary in the mean 
ndiffs(fit_4$residuals, alpha=0.05, test=c("adf")) # Regular differences required is 0.

# T-TEST
# Check for mean = 0
  # Ho: Mean of the data is 0
  # H1: Mean of the data is different than 0

t.test(fit_4$residuals) 
#p-value = 0.5664 > 0.05, fail to reject Ho and conclude that mean is 0.


# BOX TEST
# Check for correlation in data
  # Ho: Data is uncorrelated
  # Ha: Data is correlated

Box.test(fit_4$residuals,lag=5,type="Ljung") #p-value = 0.3103 > 0.05, we fail to reject Ho.
Box.test(fit_4$residuals,lag=9,type="Ljung") #p-value = 0.1824 > 0.05, we fail to reject Ho.
Box.test(fit_4$residuals,lag=18,type="Ljung") #p-value = 0.2788 > 0.05, we fail to reject Ho. 
# After checking for all lags close to the limits, we can conclude that data is uncorrelated.

# Since we have stationary and uncorrelated data with 0 mean, we can conclude that our
# residuals are White Noise.


# WHITE NOISE CROSS CHECK
windows()
whitenoise.test(fit_4$residuals) 
#Test value (p-value) 0.3019962 > 0.05 thus fail to reject Ho --> White Noise.

# SHAPIRO-WILK TEST 
# Check for normal distribution
  # Ho: Data is normally distributed
  # Ha: Data is not normally distributed

shapiro.test(fit_4$residuals) 
#p-value = 8.49e-07 < 0.05. 
#Thus, we reject Ho and say that data is not normally distributed.





#### COMPARING POTENTIAL MODELS ####


#### RECURSIVE SCHEME FOR POTENTIAL MODELS ####

# To compare more models, expand the potential_models list by adding the model with the same label as the name

potential_models = list("fit_1" = fit_1, "fit_2" = fit_2,"fit_3" = fit_3,"fit_4"=fit_4)

n <- length(dataset$value) # 107 observations
n
n_estimation <- 83
n_forecast <- n - n_estimation # 24 observations
horizon <- 4 # number of periods ahead
n_models <- length(potential_models)

predictions <- matrix(0, nrow = n_forecast, ncol = horizon)
real <- matrix(0, nrow = n_forecast, ncol=1)
real <- dataset$value[(n_estimation + 1) : n] 
MSFE <- matrix(0, nrow = horizon, ncol= n_models)
colnames(MSFE) <- names(potential_models)
MAPE <- matrix(0, nrow = horizon, ncol= n_models)
colnames(MAPE) <- names(potential_models)

# Preparing an empty matrix for plots, to be filled with predcitions in the loop
graph_preds <- matrix(0, nrow = 24, ncol= n_models*horizon)
colnames(graph_preds) <- paste(sort(rep(names(potential_models),horizon)),rep(1:horizon,n_models),sep="_")

counter <- 1
for (fit in potential_models){
  for (p_ahead in 1:horizon) {
    for (i in 1:n_forecast) {
      y <- dataset$value[1:(n_estimation - p_ahead + i)];
          # Get the initial fitted model's order
          pdq <- as.vector(arimaorder(fit)[1:3])
          PDQ <- as.vector(arimaorder(fit)[4:6])
          per <- as.vector(arimaorder(fit)[7])
          # Get the fixed order of the initial model
          x <- as.character(fit$call[5])
          u <- strsplit(as.character(gsub("[[:punct:][:blank:]]+", " ", x)), "(?<=[[:space:]])",perl=TRUE)[[1]]
          fixed <- as.numeric(as.vector(u[2:length(u)]))
          # Check if there is a fixed component or not and fit accordingly
          if(x[1]!="NULL"){
            fit_new <- arima(log(y),order=pdq,seasonal = list(order=PDQ,period = per),fixed=fixed)
          }
          if(x[1]=="NULL"){
            fit_new <- arima(log(y),order=pdq,seasonal = list(order=PDQ,period = per))
          }
      predictt <-stats::predict(fit_new, n.ahead = p_ahead);
      predictions[i, p_ahead]<- exp(predictt$pred[p_ahead]);
  }
  graph_preds[,(counter-1)* 4 + p_ahead] <- predictions[,p_ahead] # Fill the matrix with predictions for further plotting
  error <- real - predictions[, p_ahead];
  MSFE[p_ahead,counter] <- mean(error^2);
  MAPE[p_ahead,counter] <- mean(abs(error/real)) *100;
  }
  counter <- counter + 1;
}


#### PLOT THE FORECASTS vs. REALIZATIONS FROM RECURSIVE SCHEME ####

# Plot the predictions of the models for 24 point predictions they did:
df <- as.data.frame(graph_preds)

par(mfrow=c(2,2)) 
plot(dataset$value[(length(dataset$value)-23):(length(dataset$value))],x=c((length(dataset$value)-23):(length(dataset$value))),col="black",type="b", pch=20, lwd=1, ylim=c(0,1.2),xlab = "x", ylab = "y", main = "Original vs Fit 1")
lines(df$fit_1_4,x=c((length(dataset$value)-23):(length(dataset$value))),col="blue3",type="b", pch=1, lwd=1)
#legend("topleft", legend =c("dataset","fit_1"), col=c("black", "blue3"), lty = 1, cex=0.8)

plot(dataset$value[(length(dataset$value)-23):(length(dataset$value))],x=c((length(dataset$value)-23):(length(dataset$value))),col="black",type="b", lwd=1, pch=20, ylim=c(0,1.2),xlab = "x", ylab = "y", main = "Original vs Fit 2")
lines(df$fit_2_4,x=c((length(dataset$value)-23):(length(dataset$value))),col="darkgoldenrod1",type="b", pch=2, lwd=1)
#legend("topleft", legend =c("dataset","fit_2"), col=c("black", "darkgoldenrod1"), lty = 1, cex=0.8)

plot(dataset$value[(length(dataset$value)-23):(length(dataset$value))],x=c((length(dataset$value)-23):(length(dataset$value))),col="black",type="b", lwd=1, pch=20,ylim=c(0,1.2),xlab = "x", ylab = "y", main = "Original vs Fit 3")
lines(df$fit_3_4,x=c((length(dataset$value)-23):(length(dataset$value))),col="red1",type="b", pch=3, lwd=1)
#legend("topleft", legend =c("dataset","fit_3"), col=c("black", "aquamarine1"), lty = 1, cex=0.8)

plot(dataset$value[(length(dataset$value)-23):(length(dataset$value))],x=c((length(dataset$value)-23):(length(dataset$value))),col="black",type="b", lwd=1, pch=20,ylim=c(0,1.2),xlab = "x", ylab = "y", main = "Original vs Fit 4")
lines(df$fit_4_4,x=c((length(dataset$value)-23):(length(dataset$value))),col="deeppink",type="b", pch=4, lwd=1)
#legend("topleft", legend= c("dataset", "fit_4"), col=c("black", "deeppink"), lty = 1, cex=0.8)


#### ROLLING SCHEME FOR POTENTIAL MODELS ####

# To compare more models, expand the potential_models list by adding the model with the same label as the name

potential_models = list("fit_1" = fit_1,"fit_2" = fit_2,"fit_3" = fit_3,"fit_4"=fit_4)

n <- length(dataset$value) # 107 observations
n_estimation <- 83
n_forecast <- n - n_estimation # 24 observations
horizon <- 4 # number of periods ahead
n_models <- length(potential_models)

predictions <- matrix(0, nrow = n_forecast, ncol = horizon)
real <- matrix(0, nrow = n_forecast, ncol=1)
real <- dataset$value[(n_estimation + 1) : n] 
MSFE <- matrix(0, nrow = horizon, ncol= n_models)
colnames(MSFE) <- names(potential_models)
MAPE <- matrix(0, nrow = horizon, ncol= n_models)
colnames(MAPE) <- names(potential_models)

# Preparing an empty matrix for plots, to be filled with predcitions in the loop
graph_preds <- matrix(0, nrow = 24, ncol= n_models*horizon)
colnames(graph_preds) <- paste(sort(rep(names(potential_models),horizon)),rep(1:horizon,n_models),sep="_")


counter <- 1
for (fit in potential_models){
  for (p_ahead in 1:horizon) {
    for (i in 1:n_forecast) {
      y <- dataset$value[i:(n_estimation - p_ahead + i)];
          # Get the initial fitted model's order
          pdq <- as.vector(arimaorder(fit)[1:3])
          PDQ <- as.vector(arimaorder(fit)[4:6])
          per <- as.vector(arimaorder(fit)[7])
          # Get the fixed order of the initial model
          x <- as.character(fit$call[5])
          u <- strsplit(as.character(gsub("[[:punct:][:blank:]]+", " ", x)), "(?<=[[:space:]])",perl=TRUE)[[1]]
          fixed <- as.numeric(as.vector(u[2:length(u)]))
          # Check if there is a fixed component or not and fit accordingly
          if(x[1]!="NULL"){
            fit_new <- arima(log(y),order=pdq,seasonal = list(order=PDQ,period = per),fixed=fixed)
          }
          if(x[1]=="NULL"){
            fit_new <- arima(log(y),order=pdq,seasonal = list(order=PDQ,period = per))
          }
      predictt <-stats::predict(fit_new, n.ahead = p_ahead);
      predictions[i, p_ahead]<- exp(predictt$pred[p_ahead]);
    }
    graph_preds[,(counter-1)* 4 + p_ahead] <- predictions[,p_ahead] # Fill the matrix with predictions for further plotting
    error <- real - predictions[, p_ahead];
    MSFE[p_ahead,counter] <- mean(error^2);
    MAPE[p_ahead,counter] <- mean(abs(error/real)) *100;
  }
  counter <- counter + 1;
}



#### PLOT THE FORECASTS vs. REALIZATIONS FROM ROLLING SCHEME ####

# Warning: graph_preds (and all other matrices) gets over-written in the loop above with the rolling forecasts. 
# Thus, to obtain the recursive graphs, the loop for the recursive scheme should be run again.

# Plot the predictions of the models for 24 point predictions they did:
df <- as.data.frame(graph_preds)

par(mfrow=c(2,2)) 
plot(dataset$value[(length(dataset$value)-23):(length(dataset$value))],x=c((length(dataset$value)-23):(length(dataset$value))),col="black",type="b", pch=20, lwd=1, ylim=c(0,1.2),xlab = "x", ylab = "y", main = "Original vs Fit 1")
lines(df$fit_1_4,x=c((length(dataset$value)-23):(length(dataset$value))),col="blue3",type="b", pch=1, lwd=1)
#legend("topleft", legend =c("dataset","fit_1"), col=c("black", "blue3"), lty = 1, cex=0.8)

plot(dataset$value[(length(dataset$value)-23):(length(dataset$value))],x=c((length(dataset$value)-23):(length(dataset$value))),col="black",type="b", lwd=1, pch=20, ylim=c(0,1.2),xlab = "x", ylab = "y", main = "Original vs Fit 2")
lines(df$fit_2_4,x=c((length(dataset$value)-23):(length(dataset$value))),col="darkgoldenrod1",type="b", pch=2, lwd=1)
#legend("topleft", legend =c("dataset","fit_2"), col=c("black", "darkgoldenrod1"), lty = 1, cex=0.8)

plot(dataset$value[(length(dataset$value)-23):(length(dataset$value))],x=c((length(dataset$value)-23):(length(dataset$value))),col="black",type="b", lwd=1, pch=20,ylim=c(0,1.2),xlab = "x", ylab = "y", main = "Original vs Fit 3")
lines(df$fit_3_4,x=c((length(dataset$value)-23):(length(dataset$value))),col="red1",type="b", pch=3, lwd=1)
#legend("topleft", legend =c("dataset","fit_3"), col=c("black", "aquamarine1"), lty = 1, cex=0.8)

plot(dataset$value[(length(dataset$value)-23):(length(dataset$value))],x=c((length(dataset$value)-23):(length(dataset$value))),col="black",type="b", lwd=1, pch=20,ylim=c(0,1.2),xlab = "x", ylab = "y", main = "Original vs Fit 4")
lines(df$fit_4_4,x=c((length(dataset$value)-23):(length(dataset$value))),col="deeppink",type="b", pch=4, lwd=1)
#legend("topleft", legend= c("dataset", "fit_4"), col=c("black", "deeppink"), lty = 1, cex=0.8)


# Plot the logged predictions for the winning model (Model 4) for next 4 quarters
sarima.for(log_z,n.ahead = 4,0,1,1,0,1,1,S = 4)

