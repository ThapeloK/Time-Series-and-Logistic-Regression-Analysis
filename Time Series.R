
library(class)
library(gmodels)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(Metrics)
library(psych)
library(tseries)
library(forecast)



# Load the Dataset into a Dataframe
weather_data <- read.csv("weather_revised1.csv",header = TRUE, na.strings=c(""), stringsAsFactors = TRUE)
summary(weather_data)

# Convert the 'date' column to Date type
weather_data$date <- dmy(weather_data$date)

# Check whether date is in the correct format
print(min(weather_data$date))
print(max(weather_data$date))

# Check the structure of the data
str(weather_data)

#Summary Statistics
num_features <- weather_data[, sapply(weather_data, is.numeric)]
describe(num_features)
#Identify and count missing values
sapply(weather_data, function(x) sum(is.na(x)))

# Calculate percentage of missing values for each column
missing_percentage <- sapply(weather_data, function(x) {
  missing_count <- sum(is.na(x))
  total_count <- length(x)
  percentage <- (missing_count / total_count) * 100
  return(percentage)
})

# Print the results
cat("Percentage of missing values for each column:\n")
print(missing_percentage)

# Remove rows with missing values
weather_data <- na.omit(weather_data)

# Check if missing values are removed
sapply(weather_data, function(x) sum(is.na(x)))

# Data Preprocessing
#plot(weather_data , col = 'black', main = "Air Passengers")

# Create a line plot using ggplot
ggplot(weather_data, aes(x = date, y = gmin.Grass.Minimum.Temperature...degrees.C., color = "black")) +
  geom_line() +
  labs(title = "Grass Minimum Temperature (C)") +
  theme_minimal()

# Create a boxplot using ggplot
ggplot(weather_data, aes(y = gmin.Grass.Minimum.Temperature...degrees.C.)) +
  geom_boxplot(fill = 'pink') +
  labs(title = "Grass Minimum Temperature (C)") +
  theme_minimal()


# Calculate the lower and upper bounds for outliers
lower_bound <- quantile(weather_data$gmin.Grass.Minimum.Temperature...degrees.C., 0.25) - 1.5 * IQR(weather_data$gmin.Grass.Minimum.Temperature...degrees.C.)
upper_bound <- quantile(weather_data$gmin.Grass.Minimum.Temperature...degrees.C., 0.75) + 1.5 * IQR(weather_data$gmin.Grass.Minimum.Temperature...degrees.C.)

# Impute outliers with the median
weather_data <- weather_data %>%
  mutate(gmin.Grass.Minimum.Temperature...degrees.C. = ifelse(gmin.Grass.Minimum.Temperature...degrees.C. < lower_bound | gmin.Grass.Minimum.Temperature...degrees.C. > upper_bound, median(gmin.Grass.Minimum.Temperature...degrees.C., na.rm = TRUE), gmin.Grass.Minimum.Temperature...degrees.C.))


#Boxplot after Removing Outliers
# Create a boxplot using ggplot
ggplot(weather_data, aes(y = gmin.Grass.Minimum.Temperature...degrees.C.)) +
  geom_boxplot(fill = 'tan3') +
  labs(title = "Grass Minimum Temperature (C)") +
  theme_minimal()


# Create a density plot using ggplot
ggplot(weather_data, aes(x = gmin.Grass.Minimum.Temperature...degrees.C.)) +
  geom_density(fill = 'steelblue3', color = 'black') +
  labs(title = "Density Plot of Grass Minimum Temperature (C)") +
  theme_minimal()

# Create a quantile-quantile (Q-Q) plot
qqnorm(weather_data$gmin.Grass.Minimum.Temperature...degrees.C.)
qqline(weather_data$gmin.Grass.Minimum.Temperature...degrees.C., col = 2)

########################### Simple Time Series Models ###################################

# Preliminary Assessment on  the nature and coponents of the raw time series. 
gmin_Grass_Min_Temp <- c(weather_data$gmin.Grass.Minimum.Temperature...degrees.C.)
ts_gmin_Grass_Min_Temp <- ts(gmin_Grass_Min_Temp, start = c(1942),end = c(2023,10), frequency = 12)
plot(ts_gmin_Grass_Min_Temp)
autoplot(ts_gmin_Grass_Min_Temp)
start(ts_gmin_Grass_Min_Temp)
end(ts_gmin_Grass_Min_Temp)
frequency(ts_gmin_Grass_Min_Temp)
length(gmin_Grass_Min_Temp)

#Subsetting the series with the window function for test
train_weather <- window(ts_gmin_Grass_Min_Temp, start=c(2019), end=c(2022,12))
train_weather
# Plot the time series
autoplot(train_weather)
start(train_weather)
end(train_weather)
frequency(train_weather)

# check the first few values in train weather dataset
head(train_weather)

length(train_weather)

# Calculate the autocorrelation function
acf(train_weather)


#Plot a simple moving average
plot(train_weather, main="Raw Time Series")
plot(ma(train_weather,5))  
plot(ma(train_weather,7))  

autoplot(train_weather)+
  autolayer(ma(train_weather,5))+
  autolayer(ma(train_weather,7))

ggtsdisplay(train_weather)


#Estimation and Discussion of suitable time series model
#Fitting & Comparing with 'Basic' models

# Model 1: Mean model (Average Method)
fcast.mean<-meanf(train_weather, h=5)
summary(fcast.mean)
plot(fcast.mean)

# Model 2: Naive model (Random Walk Method)
fcast.naive<-naive(train_weather,h=5)
summary(fcast.naive)
plot(fcast.naive)

# Model 3: Linear Model with drift
model_drift <- tslm(train_weather ~ trend)
# Forecast using the drift method
fcast_drift <- forecast(model_drift, h = 5)
# Print summary
summary(fcast_drift)
# Plot the forecast
plot(fcast_drift)

# Model 4: Seasonal naive model
fcast.seasonalnaive<-snaive(train_weather,h=5)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)

# Model 5: Drift Method
fcast.driftm<-rwf(train_weather,h=5)
summary(fcast.driftm)
plot(fcast.driftm)

# Simple Time Series Models Performance
# accuracy(fcast.mean)
# accuracy(fcast.naive)
# accuracy(fcast_drift)
# accuracy(fcast.seasonalnaive)
# accuracy(fcast.driftm)

# Model Evaluation Using 2023 data
test_weather <- window(ts_gmin_Grass_Min_Temp, start=c(2023))
time(test_weather)
length(time(test_weather))


# Evaluate the Model Using the method that performed the best (Average Method)
ts_weather_forecast <- meanf(test_weather, h=5)
summary(ts_weather_forecast)
plot(ts_weather_forecast)
length(ts_weather_forecast)
# Forecasting Accuracy
accuracy(ts_weather_forecast)


train_weather %>% tsCV (forecastfunction = rwf, drift = TRUE, h = 5) -> e
e^2 %>% mean(na.rm = TRUE) %>% sort()

# RMSE 29.86831
########################### Seasonal Decomposition ###################################


# Additive Decomposition
decomposition_additive <- decompose(train_weather, type = "additive")
plot(decomposition_additive)

# Calculate and plot moving averages
weather_data$ma_5 <- zoo::rollmean(weather_data$gmin.Grass.Minimum.Temperature...degrees.C., k = 5, fill = NA)
ggplot(weather_data, aes(x = date)) +
  geom_line(aes(y = gmin.Grass.Minimum.Temperature...degrees.C., color = "Original")) +
  geom_line(aes(y = ma_5, color = "5-Day Moving Average")) +
  labs(title = "Original vs. 5-Day Moving Average",
       x = "Date",
       y = "Grass Minimum Temperature (°C)",
       color = "Legend")

########################### Exponential Smoothing ###################################

plot(train_weather)
autoplot(train_weather)
adf.test(train_weather) # p-value = 0.01

ses(train_weather)
holt(train_weather)
hw(train_weather)

# Use the `ses` function to forecast the series, and plot the forecasts.
#Simple Exponential Smoothing
model1 <- ses(train_weather, h=5)
model1$model
model1
summary(model1)
autoplot(model1)
autoplot(model1) + autolayer(fitted(model1), series = "Fitted")
forecast(model1)

#AIC: 298.9745
# Compute the RMSE values for the training data
accuracy(model1)
#                     ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set -0.0009765971 3.053282 2.461475 -11.67029 29.63036 0.6657633 0.2391989


# Now apply Holts linear method to gmin.Grass.Minimum.Temperature...degrees.C series and compute five-day forecasts.
model2 <- holt(train_weather, h=5)
model2$model
summary(model2)
plot(model2)
autoplot(model2)
autoplot(model2) + autolayer(fitted(model2), series = "Fitted")
forecast(model2)
# AIC: 301.3424
# Compute the RMSE values for the training data 
accuracy(model2)
#                    ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
# Training set -0.05585018 3.001811 2.372595 -11.73877 28.72999 0.6417236 0.2281692

# Fit exponential smoothing model
# Double Exponential model (Holt- exponential smoothing)
model3 <- ets(train_weather, model = "AAN")
#model3$model
summary(model3)
plot(model3)# Compute the RMSE values
autoplot(model3)
autoplot(model3) + autolayer(fitted(model3), series = "Fitted")
forecast(model3)
# AIC: 298.9745
accuracy(model3)
#                    ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set -0.0009765971 3.053282 2.461475 -11.67029 29.63036 0.6657633 0.2391989

# Fit exponential smoothing model
# Triple Exponential model (Holt-Winters exponential smoothing)
model4 <- ets(train_weather, model = "AAA")
#model4$model
summary(model4)
plot(model4)# Compute the RMSE values
autoplot(model4)
autoplot(model4) + autolayer(fitted(model4), series = "Fitted")
forecast(model4)
# AIC: 314.3311
accuracy(model4)
#                    ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set -0.006546976 2.676513 2.195246 -8.341091 25.40056 0.5937555 0.2438328


# Fit exponential smoothing model
model5 <- ets(train_weather, model = "ZZZ")
#model5$model
summary(model5)
plot(model5)# Compute the RMSE values
# AIC: 298.9744
# Compute the RMSE values
accuracy(model5)
#                    ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set 0.0005991618 3.053282 2.461213 -11.65223 29.62288 0.6656924 0.239199


# Make predictions on the training set
exp_smooth_predictions <- forecast(model5, h = length(train_weather))

# Plot the original series and predictions
autoplot(exp_smooth_predictions) +
  labs(title = "Exponential Smoothing Forecast",
       x = "Date",
       y = "Grass Minimum Temperature (°C)")

# Evaluate the model
exp_weather_forecast <- ets(test_weather, model= "ZZZ")
summary(exp_weather_forecast)
plot(exp_weather_forecast)
# AIC: 47.19722
# Compute the RMSE values
accuracy(exp_weather_forecast)
#                    ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set 1.217879e-05 2.480769 2.080072 -13.27012 33.70556  NaN -0.1601051

exp_weather_forecast2 <- ets(test_weather, model= "ANN")
summary(exp_weather_forecast2)
plot(exp_weather_forecast2)
# AIC: 47.19722
# Compute the RMSE values
accuracy(exp_weather_forecast2)
#                    ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set 1.217879e-05 2.480769 2.080072 -13.27012 33.70556  NaN -0.1601051




###########################  ARIMA/SARIMA ###################################
#Plot the Weather Time Series
autoplot(train_weather)

#Check the order of differencing required
ndiffs(train_weather)   # No differencing is needed


#Plot the differenced grass min Time Series
# diff_train_weather <- diff(train_weather)
# plot(diff_train_weather)

#Assess stationarity of the time series
adf.test(train_weather)
ndiffs(train_weather)

# Display the time series along with the ACF and PACF
ggtsdisplay(train_weather, main =" Train Weather")

#ACF/PACF plots. Choosing p and q
Acf(train_weather)
Pacf(train_weather)
# The PACF Plot indicates that the best / most suitable model is AR(1)



# Fitting an ARIMA model
#ARIMA(0,0,1)
fit001<- arima(train_weather, order=c(0,0,1))
summary(fit001)
# aic = 246.19

# Fit Close Variations
# ARIMA(1,0,0)
# ARIMA(1,0,1)
# ARIMA(0,0,1)
# ARIMA(0,0,2)
# ARIMA(2,0,0)

fit100 <- arima(train_weather, order=c(1,0,0))
summary(fit100)
# aic = 246.44

fit101 <- arima(train_weather, order=c(1,0,1))
summary(fit101)
# aic = 248.15

fit001 <- arima(train_weather, order=c(0,0,1))
summary(fit001)
# aic = 246.19

fit002 <- arima(train_weather, order=c(0,0,2))
summary(fit002)
# aic = 247.98

fit200 <- arima(train_weather, order=c(2,0,0))
summary(fit200)
# aic = 247.55



# Fit ARIMA Model
arima_model <- auto.arima(train_weather)
summary(arima_model)
#ARIMA(0,0,1) with non-zero mean 
# AIC=246.19


#Evaluating Model Fit
qqnorm(fit001$residuals)
qqline(fit001$residuals)
Box.test(fit001$residuals, type="Ljung-Box")
checkresiduals(fit001)
accuracy(fit001)

#Forecasting with the fitted model
forecast(fit001, 5)
plot(forecast(fit001, 5), xlab="Date", ylab="Grass_Min_Temp")


# Make predictions on the training set
arima_predictions <- forecast(arima_model, h = length(test_weather))
autoplot(arima_predictions)
print(arima_predictions$mean)



arima_predictions1 <- forecast::forecast(fit001, h = length(test_weather))
autoplot(arima_predictions1)
print(arima_predictions1$mean)
accuracy(arima_predictions1)


# Plot the original series and predictions
autoplot(arima_predictions) +
  labs(title = "ARIMA Forecast",
       x = "Date",
       y = "Grass Minimum Temperature (°C)")

#Evaluate Model
accuracy(arima_predictions)

# Now you can use the MAE function
mae001 <- round(mae(arima_predictions$mean, test_weather), 3)
rmse001 <- round(rmse(arima_predictions$mean, test_weather),3)
mape001 <- round(mape(arima_predictions$mean, test_weather),3)

cat(paste("ARIMA(0,0,1) -", "MAE:" , mae001, " RMSE:", rmse001, " MAPE:", mape001))








