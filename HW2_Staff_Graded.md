# Homewrok 2 STAFF Graded

###  Problem A

**The appropriate seasonal lag term S = 12**

### Problem B
```
library(forecast)
library(lubridate)
library(dplyr)

# Load data
df <- read.csv("MeanDallasTemps.csv")

# Format date and prepare data
df$Month <- mdy(df$Month)
amdt_ts <- ts(df$AvgTemp, start = c(2000, 1), frequency = 12)

# Fit SARIMA Models
params <- expand.grid(p = 0:1, d = 0:1, q = 0:1, P = 0:1, D = 0:1, Q = 0:1)

# Exclude cases where d=0 and (P,D,Q) = (1,0,1)
params <- params[!(params$d == 0 & params$P == 1 & params$D == 0 & params$Q == 1), ]

results <- params %>%
  rowwise() %>%
  mutate(
    AICc = tryCatch({
      fit <- Arima(amdt_ts,
                   order = c(p, d, q),
                   seasonal = list(order = c(P, D, Q), period = 12),
                   include.mean = FALSE, include.drift = FALSE)
      fit$aicc
    }, error = function(e) NA_real_)
  ) %>%
  ungroup()

# Show Results 
results_sorted <- arrange(results, AICc)
print(results_sorted)

best <- results_sorted %>% filter(!is.na(AICc)) %>% slice(1)
cat(sprintf(
  "Best SARIMA model: (%d,%d,%d)x(%d,%d,%d)[12] with AICc = %.4f\n",
  best$p, best$d, best$q, best$P, best$D, best$Q, best$AICc
))

```
**Best SARIMA model: (1,0,1)x(0,1,1)[12] with AICc = 1282.2633**


## Problem C
```
library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)

# Load and prepare data
df <- read.csv("MeanDallasTemps.csv", stringsAsFactors = FALSE) %>%
  mutate(Month = mdy(Month)) %>%
  arrange(Month)

ts_data <- ts(df$AvgTemp, start = c(2000, 1), frequency = 12)

# Model parameters
order <- c(1, 0, 1)
seasonal <- c(0, 1, 1)
period <- 12

# Rolling forecast loop
forecast_dates <- seq(ymd("2016-01-01"), ymd("2020-12-01"), by = "month")

pred_df <- bind_rows(lapply(forecast_dates, function(date) {
  train_end <- date %m-% months(12)
  train_ts <- window(ts_data, end = c(year(train_end), month(train_end)))
  
  model <- Arima(train_ts,
                 order = order,
                 seasonal = list(order = seasonal, period = period),
                 include.mean = FALSE,
                 include.drift = FALSE)
  
  fc <- forecast(model, h = 12, level = 95)
  data.frame(
    Date = date,
    Forecast = fc$mean[12],
    Lower95 = fc$lower[12],
    Upper95 = fc$upper[12]
  )
}))

# Prepare observed data for plotting
obs_df <- data.frame(
  Date = as.Date(time(window(ts_data, start = c(2010, 1), end = c(2020, 12)))),
  Observed = as.numeric(window(ts_data, start = c(2010, 1), end = c(2020, 12)))
)

# Combine and plot
plot_df <- full_join(obs_df, pred_df, by = "Date")

ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = Observed, color = "Observed"), size = 1) +
  geom_point(aes(y = Observed, color = "Observed"), alpha = 0.6) +
  geom_line(aes(y = Forecast, color = "Forecast"), linetype = "dashed", size = 1) +
  geom_point(aes(y = Forecast, color = "Forecast"), shape = 4, size = 2) +
  geom_ribbon(aes(ymin = Lower95, ymax = Upper95, fill = "95% CI"), alpha = 0.2) +
  scale_color_manual(values = c("Observed" = "steelblue", "Forecast" = "firebrick")) +
  scale_fill_manual(values = c("95% CI" = "grey")) +
  labs(
    title = "1-Year-Ahead SARIMA Forecasts (2016–2020)",
    subtitle = "Observed vs. Rolling Forecast with 95% Confidence Intervals",
    x = "Date", y = "Avg Monthly Max Temp (°F)",
    color = "", fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

```
**Forecast Plot:**
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/SarimaForecast.png)

**January 2018 Forecast with 95% Confidence Interval**

```
# January 2018 Forecast
jan2018 <- filter(pred_df, Date == ymd("2018-01-01"))

with(jan2018, {
  cat(sprintf("Forecast Jan 2018: %.2f °F\n", Forecast))
  cat(sprintf("95%% Prediction Interval: [%.2f, %.2f] °F", Lower95, Upper95))
})
```
**Forecast Jan 2018: 58.06 °F**

**95% Prediction Interval: [51.06, 65.05] °F**


## Problem D
```
library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)

#Get data
df <- read.csv("MeanDallasTemps.csv", stringsAsFactors = FALSE) %>%
  mutate(Month = mdy(Month)) %>%
  arrange(Month)
# Set start date
amdt_ts <- ts(df$AvgTemp, start = c(2000, 1), frequency = 12)

#ARIMA(3,1,1) without seasonal component
order_params <- c(3, 1, 1)

# Forecast Jan 2016 – Dec 2020 
forecast_dates <- seq(ymd("2016-01-01"), ymd("2020-12-01"), by = "month")

# Prediction code
predictions <- lapply(forecast_dates, function(date) {
  train_end <- date %m-% months(12)
  train_ts <- window(amdt_ts, end = c(year(train_end), month(train_end)))
  
  fit <- Arima(train_ts, order = order_params, include.mean = FALSE, include.drift = FALSE)
  fc <- forecast(fit, h = 12, level = 95)
  
  i <- 12
  data.frame(
    Date = date,
    Predicted = fc$mean[i],
    Lower95 = fc$lower[i],
    Upper95 = fc$upper[i]
  )
})

pred_df <- bind_rows(predictions)

# Lock data between 2010–2020
obs_ts <- window(amdt_ts, start = c(2010, 1), end = c(2020, 12))
obs_df <- data.frame(Date = as.Date(time(obs_ts)), Observed = as.numeric(obs_ts))

# Plot
plot_df <- full_join(obs_df, pred_df, by = "Date")

ggplot(plot_df, aes(Date)) +
  geom_line(aes(y = Observed, color = "Observed"), size = 1) +
  geom_point(aes(y = Observed, color = "Observed"), alpha = 0.6) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", size = 1) +
  geom_point(aes(y = Predicted, color = "Predicted"), shape = 4, size = 2) +
  geom_ribbon(aes(ymin = Lower95, ymax = Upper95, fill = "95% CI"), alpha = 0.2) +
  scale_color_manual(values = c("Observed" = "steelblue", "Predicted" = "firebrick")) +
  scale_fill_manual(name = "Interval", values = c("95% CI" = "grey")) +
  labs(
    title = "Dallas aMDT Forecasts: ARIMA(3,1,1) One-Year-Ahead",
    subtitle = "Rolling origin predictions (2016–2020)",
    x = "Date", y = "Average Monthly Daily Temperature (°F)",
    color = "", fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/NoSeasonalPlot.png)

**January 2018 Forecast with 95% Confidence Interval**
```
jan2018 <- filter(pred_df, Date == ymd("2018-01-01"))

with(jan2018, {
  cat(sprintf("Forecast Jan 2018: %.2f °F\n", Predicted))
  cat(sprintf("95%% Prediction Interval: [%.2f, %.2f] °F\n", Lower95, Upper95))
})
```

**Jan 2018 Forecast: 67.93 °F**

**95% Prediction Interval: [44.74, 91.12] °F**

**Does the fitted model produce predictions that capture seasonal behavior?**

Answer: No, the ARIMA(3,1,1) model, by its very definition as a non-seasonal ARIMA model, does not explicitly include components to capture seasonal behavior. 

**How do the predictions from the ARIMA(3, 1, 1) model that does not include a specific seasonal component compare to the predictions from the model fitted in part c.?**

Answer: The predictions from the ARIMA(3,1,1) model is inferior in capturing the overall pattern of the Dallas aMDT data compared to the SARIMA(1,0,1)x(0,1,1)_12 model. Due to its inability to capture seasonality, the ARIMA(3,1,1) model has higher AICc values indicating a poorer fit for data with seasonality and has weaker predictability. 

## Problem  E
```
library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)

# Load and prepare data
amdt_ts <- read.csv("MeanDallasTemps.csv", stringsAsFactors = FALSE) %>%
  mutate(Month = mdy(Month)) %>%
  arrange(Month) %>%
  pull(AvgTemp) %>%
  ts(start = c(2000, 1), frequency = 12)

# ARIMA model parameters
sarima_order <- c(12, 1, 0)

# Generate rolling forecasts
forecast_dates <- seq(ymd("2016-01-01"), ymd("2020-12-01"), by = "month")
predictions_df <- data.frame(Date = forecast_dates, Predicted_aMDT = NA, Lower_95 = NA, Upper_95 = NA)

for (i in seq_along(forecast_dates)) {
  train_end <- forecast_dates[i] %m-% months(12)
  train_ts <- window(amdt_ts, end = c(year(train_end), month(train_end)))
  fc <- forecast(Arima(train_ts, order = sarima_order, include.mean = FALSE, include.drift = FALSE), h = 12)
  predictions_df[i, 2:4] <- c(fc$mean[12], fc$lower[12, "95%"], fc$upper[12, "95%"])
}

# Prepare plot data
obs_df <- data.frame(
  Date = as.Date(time(window(amdt_ts, start = c(2010, 1), end = c(2020, 12)))),
  Observed_aMDT = as.numeric(window(amdt_ts, start = c(2010, 1), end = c(2020, 12)))
)
full_plot_data <- full_join(obs_df, predictions_df, by = "Date")

# Plot
ggplot(full_plot_data, aes(x = Date)) +
  geom_line(aes(y = Observed_aMDT, color = "Observed"), size = 0.8) +
  geom_point(aes(y = Observed_aMDT, color = "Observed"), size = 1.2) +
  geom_line(aes(y = Predicted_aMDT, color = "Predicted"), linetype = "dashed", size = 0.9) +
  geom_point(aes(y = Predicted_aMDT, color = "Predicted"), shape = 4, stroke = 1.1, size = 1.8) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95, fill = "95% CI"), alpha = 0.2) +
  scale_color_manual(name = "Series", values = c("Observed" = "darkblue", "Predicted" = "darkred")) +
  scale_fill_manual(name = "Interval", values = c("95% CI" = "grey")) +
  labs(
    title = "Dallas aMDT: Observed vs. ARIMA(12,1,0) Forecasts",
    subtitle = "Rolling one-year-ahead forecasts (2016–2020)",
    x = "Date", y = "Avg Monthly Daily Temp (°F)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
```

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/partD.png)

**Jan 2018 Forecast**
```
with(filter(predictions_df, Date == ymd("2018-01-01")), {
  cat(sprintf("Forecast Jan 2018: %.2f °F\n", Predicted_aMDT))
  cat(sprintf("95%% Prediction Interval: [%.2f, %.2f] °F\n", Lower_95, Upper_95))
})
```
**Jan 2018 Forecast: 62.06 °F**

**95% Confidence Interval: [53.16, 70.97] °F**

**Does the fitted model produce predictions that capture seasonal behavior? **
Answer: The ARIMA(12,1,0) model, despite having a high AR order (p=12), does not explicitly capture seasonal behavior in the same way a that earlier SARIMA model did. However, ARIMA(12, 1, 0) predicts better than earler version of ARIMA model. 

**How do the predictions from the ARIMA(12,1,0) model compare to the predictions from the models fitted earlier?***
Asnwer: The ARIMA(12,1,0) model improves on basic non-seasonal ARIMA models by capturing yearly patterns since it includes lag of 12 for monthly data. However, it is proved that it is not as good as SARIMA model, which worked better than both ARIMA model we tried fitting. 
