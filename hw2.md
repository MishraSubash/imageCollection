# Homewrok 2

###  Problem 1 Part A
```
library(forecast)

# Read Data 
data <- read.csv("ARMAData1.csv", header = TRUE)
x = data$x

# Store AICc values
results <- data.frame(p = integer(), q = integer(), AICc = numeric())

# Iterate through combinations of p and q
for (p in 1:3) {
  for (q in 1:3) {
    if (p == 2 && q == 2) next  # Skip ARMA(2,2)
    
    model <- Arima(x, order = c(p, 0, q), include.mean = TRUE)
    # Calculate aicc at various p, q combinations 
    aicc <- model$aicc
    
    results <- rbind(results, data.frame(p = p, q = q, AICc = aicc))
  }
}

# Print all AICc values
print(results)

# Identify best model which would have lowest AIC value
best_model <- results[which.min(results$AICc), ]
cat("Best model: ARMA(", best_model$p, ",", best_model$q, ") with AICc = ", best_model$AICc)
```
**Best model: ARMA( 3 , 1 ) with AICc =  446.6945**


## Problem 1 Part B
```
# Equation for the fitted model
# Fit the model from part with ARMA (3, 1) which yielded best model
final_model <- Arima(x, order = c(3, 0, 1), include.mean = TRUE)

# Grab coefficinets
coefs <- coef(final_model)
sigma2 <- final_model$sigma2

# Print out the model equation
cat("Fitted ARMA(3,1) model equation:\n")
cat("x_t = ", round(coefs["intercept"], 4), 
    if ("ar1" %in% names(coefs)) paste0(" + ", round(coefs["ar1"], 4), " x_{t-1}") else "",
    if ("ar2" %in% names(coefs)) paste0(" + ", round(coefs["ar2"], 4), " x_{t-2}") else "",
    if ("ar3" %in% names(coefs)) paste0(" + ", round(coefs["ar3"], 4), " x_{t-3}") else "", " + w_t",
    if ("ma1" %in% names(coefs)) paste0(" + ", round(coefs["ma1"], 4), " w_{t-1}") else "",
    if ("ma2" %in% names(coefs)) paste0(" + ", round(coefs["ma2"], 4), " w_{t-2}") else "")

cat("Estimated white noise variance σ² =", round(sigma2, 4))
```
**Equation:**
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Screenshot%202025-06-19%20121915.jpg)

**Estimated white noise variance σ² = 1.0621**

## Problem 1 Part C
```
library(forecast)

# Read Data 
data <- read.csv("ARMAData2.csv", header = TRUE)
x = data$x

# Split into training and test sets 
train <- x[1:150] # first 150 into train
test <- x[151:length(x)] 

# Fit ARMA(2,2) model
arma_model <- Arima(train, order = c(2, 0, 2), include.mean = TRUE)

# get AIC value
aicc_value <- arma_model$aicc
cat("AIcc Value for model ARMA(2, 2) model on traning data:", aicc_value)
```

**AIC Value for model ARMA(2, 2) model on traning data: 457.3228**

## Problem 1 Part D
```
library(ggplot2)
# Forecast over the length of test set with 95% interval
h <- length(test)
forecast_result <- forecast(arma_model, h = h, level = 95)

# Combine actual and predicted into a single data frame
# for presenting dat into a graph
full_time <- 1:length(x)
train_time <- 1:150
test_time <- 151:length(x)

df_plot <- data.frame(
  time = full_time,
  actual = as.numeric(x),
  type = c(rep("Train", 150), rep("Test", h))
)

# Add forecasts and intervals to test dataset
forecast_df <- data.frame(
  time = test_time,
  forecast = as.numeric(forecast_result$mean),
  lower = forecast_result$lower[, 1],
  upper = forecast_result$upper[, 1]
)

# Plot graph 
ggplot() +
  geom_line(data = df_plot, aes(x = time, y = actual, color = type), size = 1) +
  geom_line(data = forecast_df, aes(x = time, y = forecast, color = "Prediction"), linetype = "dashed", size = 1) +
  geom_ribbon(data = forecast_df, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(
    title = paste("ARMA(2,2) Model: Actual vs. Predicted with 95% PI (AICc:", round(aicc_value, 2), ")"),
    x = "Time",
    y = "Value",
    color = "Data Type"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Train" = "black", "Test" = "darkred", "Prediction" = "blue")) +
  theme(plot.title = element_text(hjust = 0.5))
```

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Rplot_most_updated.png)

## Question 2 Part A
**How well do the predictions match the test set? How does the quality of the predictions change over time?**

Answer: The predictions match the test set okay at the very beginning, with most actual values falling within the 95% predictive intervals. However, as time progresses, the predictive intervals widen, indicating increasing uncertainty. Consequently, the quality of the predictions degrades pretty quick and continues degrading over time, becoming less precise in capturing the true values further into the forecast.

## Question 2 Part B

```
library(dplyr)
library(ggplot2)
library(lubridate) 
library(forecast) 


# Read data
car_crashes_df <- read.csv("TexasAccidents.csv", header = TRUE)

# Convert Date to Date object and ensure DayOfWeek is an ordered factor
car_crashes_df <- car_crashes_df %>%mutate(Date = mdy(Date)) 

# Ensure DayOfWeek is an ordered factor for consistent plotting and color mapping
car_crashes_df$DayOfWeek <- factor(car_crashes_df$DayOfWeek,
                                   levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# Filter a_4: Moving Average Filter (1/7, 0, ..., 0, 1/7, ..., 0, 1/7) over length 43
car_crashes_df <- car_crashes_df %>%
  group_by(DayOfWeek) %>%
  mutate(
    # align = "center" is default for `centre = TRUE` and handles NAs at boundaries
    Smoothed_a4 = ma(Freq, order = 7, centre = TRUE)
  ) %>%
  ungroup()

custom_colors_distinct <- c(
  "Monday" = "#F8766D",
  "Tuesday" = "#B79F00",
  "Wednesday" = "#00BA38",
  "Thursday" = "#00BFC4",
  "Friday" = "#619CFF",
  "Saturday" = "#F564E3",
  "Sunday" = "#FF6666"
)

# Plot with raw points, raw lines, and weekday-wise smoothed lines
ggplot(car_crashes_df, aes(x = Date, y = Freq, color = DayOfWeek, group = DayOfWeek)) +
  # Raw data
  geom_point(alpha = 0.3, size = 0.8) +
  geom_line(alpha = 0.3, size = 0.4) +
  
  # Weekday-specific smoothing using Smoothed_a4
  geom_line(aes(y = Smoothed_a4), size = 1.2, alpha = 1) +
  
  # Color and formatting
  scale_color_manual(values = custom_colors_distinct) +
  labs(
    title = "Car Accidents in Texas: Weekday-Specific Smoothing a4)",
    subtitle = "Smoothed weekly curve",
    x = "Date",
    y = "Number of Accidents",
    color = "Day of Week"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Rplot_smoothing_curve.png)

**Explain why the two filtered time series (applying filters a1 and a2) differ in terms of their smoothness**

Answer: Filter a₂ uses a narrower window and focuses only on three points (one per week). So, it retains more short-term fluctuations. As seen in given plot (Figure 3), this creates less smoothing, resulting in a jagged appearance while still separating days of the week.

Filter a₄, being of length 43, averages more widely across same weekdays over six weeks.
This increases smoothness by reducing noise and random fluctuations, giving smoother curves for each day-of-week series.


## Question 2 Part C
Asnwer: Visual trends in a smoothed time series can be suggestive, but without a careful design (like a control group or pre/post statistical comparison), they can mislead. An observed drop in crashes after September 2017, for instance, could be mistakenly attributed to the law when it may be due to unrelated seasonal or external factors.
