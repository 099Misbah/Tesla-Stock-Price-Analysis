# Install and load required libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("forecast")) install.packages("forecast")
if (!require("tseries")) install.packages("tseries")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(forecast)
library(tseries)
library(lubridate)

library(googledrive)
"Documents/Misbah Docs/College Work_Data/Sem -04/5306 Time series/AEP_hourly.xlsx"
# Step 1: Upload the file
file_path <- file.choose()  # This will open a file picker to upload your file

# Step 2: Read the CSV file
data <- read.csv(file_path)

# View the first few rows of the dataset
head(data)

# Inspect the dataset
head(data)
# Check for missing values in the entire dataset
sum(is.na(data))
#-----------------------------------------------------------------------------------
#Aggregate Weekly Closing Prices (2015–2023 Only)
# Step 2: Convert the Date column to Date format and filter for 2015 to 2023
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data_filtered <- data %>% filter(Date >= "2015-01-01" & Date <= "2023-12-31")

# Step 3: Resample the data to weekly frequency
# Calculate weekly averages for 'Open', 'High', 'Low', 'Close', and 'Volume'
data_weekly <- data_filtered %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarize(
    Open = mean(Open, na.rm = TRUE),
    High = mean(High, na.rm = TRUE),
    Low = mean(Low, na.rm = TRUE),
    Close = mean(Close, na.rm = TRUE),
    Adj.Close = mean(Adj.Close, na.rm = TRUE),
    Volume = sum(Volume, na.rm = TRUE)
  )
data_weekly

# Step 4: Verify the number of data points
nrow(data_weekly)  # Should be around 468

# Step 5: Exploratory Data Analysis (EDA)
# Plot the weekly closing price
ggplot(data_weekly, aes(x = Week, y = Close)) +
  geom_line(color = "blue") +
  labs(title = "Weekly Closing Price (2015-2023)", x = "Week", y = "Closing Price") +
  theme_minimal()

# Step 6: Convert data to a time-series object
# Use the 'Close' price as the primary variable
ts_data <- ts(data_weekly$Close, start = c(2015, 1), frequency = 52)

# Step 7: Decompose the time series
ts_decomposed <- decompose(ts_data, type = "multiplicative")
plot(ts_decomposed)

# Step 8: Stationarity Test
library(tseries)
adf_test <- adf.test(ts_data)
print(adf_test)

# ACF and PACF plots before differencing
acf(ts_data, main = "ACF of Original Series")
pacf(ts_data, main = "PACF of Original Series")

# Step 9: Differencing the series to make it stationary
ts_diff <- diff(ts_data)
adf_test_diff <- adf.test(ts_diff)
print(adf_test_diff)

# ACF and PACF plots after differencing
acf(ts_diff, main = "ACF of Differenced Series")
pacf(ts_diff, main = "PACF of Differenced Series")



#----------------------------------------------------------------------------------------
#MODEL -1 ARIMA
# Step 10: Model selection and fitting
# Fit an ARIMA model
ts_model <- auto.arima(ts_data, seasonal = TRUE)
summary(ts_model)

# Step 11: Forecasting with ARIMA
forecast_values <- forecast(ts_model, h = 52)  # Forecast next 52 weeks
plot(forecast_values)

# Residual analysis for ARIMA
checkresiduals(ts_model)


#-----------------------------------------------------------------------------------
#MODEL -2 ETS
# Step 12: Fit and compare another model (Exponential Smoothing)
es_model <- ets(ts_data)
summary(es_model)

# Forecast with Exponential Smoothing
forecast_es <- forecast(es_model, h = 52)
plot(forecast_es, main = "Exponential Smoothing Forecast")

# Residual analysis for ETS
checkresiduals(es_model)

# Comparison between ARIMA and Exponential Smoothing
accuracy_arima <- accuracy(forecast_values)
accuracy_es <- accuracy(forecast_es)
print("ARIMA Accuracy:")
print(accuracy_arima)
print("Exponential Smoothing Accuracy:")
print(accuracy_es)

# Step 13: Model evaluation
# Split the dataset into training and testing sets
train_data <- window(ts_data, end = c(2022, 52))
test_data <- window(ts_data, start = c(2023, 1))

# Refit the ARIMA model on training data
train_model_arima <- auto.arima(train_data, seasonal = TRUE)
train_forecast_arima <- forecast(train_model_arima, h = length(test_data))

# Refit the Exponential Smoothing model on training data
train_model_es <- ets(train_data)
train_forecast_es <- forecast(train_model_es, h = length(test_data))

# Plot actual vs forecast for ARIMA
autoplot(train_forecast_arima) +
  autolayer(test_data, series = "Actual", color = "red") +
  labs(title = "Actual vs Forecast (ARIMA)", x = "Week", y = "Closing Price") +
  theme_minimal()

# Plot actual vs forecast for Exponential Smoothing
autoplot(train_forecast_es) +
  autolayer(test_data, series = "Actual", color = "red") +
  labs(title = "Actual vs Forecast (Exponential Smoothing)", x = "Week", y = "Closing Price") +
  theme_minimal()

# Calculate accuracy metrics for both models
accuracy_arima_train <- accuracy(train_forecast_arima, test_data)
accuracy_es_train <- accuracy(train_forecast_es, test_data)

print("Training Accuracy (ARIMA):")
print(accuracy_arima_train)
print("Training Accuracy (Exponential Smoothing):")
print(accuracy_es_train)

#----------------------------------------------------------------------------------------------
#MODEL -3 SARIMA
# Step 15: Fit a Seasonal ARIMA (SARIMA) model

# Seasonal ARIMA model fitting using the ts_data
sarima_model <- auto.arima(ts_data, seasonal = TRUE, 
                           stepwise = FALSE, 
                           approximation = FALSE, 
                           trace = TRUE)

# Summary of the SARIMA model
summary(sarima_model)

# Step 16: Forecast using the SARIMA model
sarima_forecast <- forecast(sarima_model, h = 52)  # Forecast next 52 weeks
plot(sarima_forecast, main = "SARIMA Forecast for Weekly Closing Prices")

# Step 17: Residual diagnostics for SARIMA model
checkresiduals(sarima_model)

# Step 18: Model Evaluation - Training and Testing Split
# Split the dataset into training (2015-2022) and testing (2023) sets
train_data_sarima <- window(ts_data, end = c(2022, 52))
test_data_sarima <- window(ts_data, start = c(2023, 1))

# Refit the SARIMA model on training data
sarima_train_model <- auto.arima(train_data_sarima, seasonal = TRUE, 
                                 stepwise = FALSE, 
                                 approximation = FALSE, 
                                 trace = TRUE)

# Forecast with SARIMA model for testing period
sarima_train_forecast <- forecast(sarima_train_model, h = length(test_data_sarima))

# Plot actual vs forecast for SARIMA
autoplot(sarima_train_forecast) +
  autolayer(test_data_sarima, series = "Actual", color = "red") +
  labs(title = "Actual vs SARIMA Forecast", x = "Week", y = "Closing Price") +
  theme_minimal()

# Step 19: Calculate accuracy metrics for SARIMA model
sarima_accuracy <- accuracy(sarima_train_forecast, test_data_sarima)
print("SARIMA Model Accuracy:")
print(sarima_accuracy)

# Step 20: Compare SARIMA model with other models
# Add SARIMA accuracy to your comparison table
comparison_table <- data.frame(
  Model = c("ARIMA", "Exponential Smoothing", "SARIMA"),
  RMSE = c(accuracy_arima_train[2, "RMSE"], 
           accuracy_es_train[2, "RMSE"], 
           sarima_accuracy[2, "RMSE"]),
  MAE = c(accuracy_arima_train[2, "MAE"], 
          accuracy_es_train[2, "MAE"], 
          sarima_accuracy[2, "MAE"])
)

print("Model Comparison Table:")
print(comparison_table)

# Discussion: Based on RMSE and MAE, evaluate which model performs better.



