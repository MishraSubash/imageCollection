# Time Series State-Space and Hidden Markov Models

### Part A
```
#Load Libraries 
library(dlm)
library(ggplot2)
library(tidyverse)

# Load Data 
data_df <- read.csv("DLM_Data.csv")
y <- data_df$yt
n <- length(y)
time <- 1:n

# Define Model Parameters 
F_val <- 1.2
G_val <- 0.8
sigma_v_sq <- 9
sigma_w_sq <- 4
m_prev <- 0
C_prev <- 25

# Initialize Result Vectors
alpha <- R <- f <- Q <- m <- C <- numeric(n)

#  Kalman Filter Loop 
for (t in 1:n) {
  alpha[t] <- G_val * m_prev
  R[t]     <- G_val^2 * C_prev + sigma_w_sq
  f[t]     <- F_val * alpha[t]
  Q[t]     <- F_val^2 * R[t] + sigma_v_sq

  current_y <- y[t]

  if (!is.na(current_y)) {
    K_t   <- (R[t] * F_val) / Q[t]
    m[t]  <- alpha[t] + K_t * (current_y - f[t])
    C[t]  <- R[t] * (1 - K_t * F_val)
  } else {
    m[t] <- alpha[t]
    C[t] <- R[t]
  }

  m_prev <- m[t]
  C_prev <- C[t]
}

# Plotting 
results_df <- tibble(
  time = time,
  yt = y,
  predicted_y_t = f,
  sd_y_t = sqrt(Q),
  lower_ci = f - 1.96 * sqrt(Q),
  upper_ci = f + 1.96 * sqrt(Q)
)

ggplot(results_df, aes(x = time)) +
  geom_point(aes(y = yt, color = "Observed yₜ"), size = 2, alpha = 0.8) +
  geom_line(aes(y = yt, color = "Observed yₜ"), alpha = 0.5) +
  geom_line(aes(y = predicted_y_t, color = "Predicted yₜ (One-Step Ahead)"), size = 1) +
  geom_point(aes(y = predicted_y_t, color = "Predicted yₜ (One-Step Ahead)"), shape = 4, size = 2, stroke = 1.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "95% CI"), alpha = 0.2) +
  scale_color_manual(name = "Series", values = c("Observed yₜ" = "darkblue", "Predicted yₜ (One-Step Ahead)" = "darkred")) +
  scale_fill_manual(name = "Interval", values = c("95% CI" = "grey")) +
  labs(
    title = "Kalman Filter: Observed yₜ vs. One-Step-Ahead Predicted yₜ",
    x = "Time (t)",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Report alpha_40 and R_40 
cat("alpha_40", alpha[40])
cat("R_40", R[40])

```
**Alpha_40: = 3.53**

**R_40: = 5.95**
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/FIGURE_PART_A.png)

## Part B
```
# --- Load Libraries ---
library(dlm)
library(ggplot2)
library(tidyverse)

# Load Data 
data_df <- read.csv("DLM_Data.csv")
y <- data_df$yt
n <- length(y)
time <- 1:n

# Define Model Parameters 
F_val <- 1.2
G_val <- 0.8
sigma_v_sq <- 9  
sigma_w_sq <- 4  
m_prev <- 0     
C_prev <- 25    

# Initialize Vectors to Store Kalman Filter 
alpha <- R <- f <- Q <- m <- C <- numeric(n)

# Kalman Filter Loop
for (t in 1:n) {
  # Prediction Step
  alpha[t] <- G_val * m_prev
  R[t]     <- G_val^2 * C_prev + sigma_w_sq
  f[t]     <- F_val * alpha[t]
  Q[t]     <- F_val^2 * R[t] + sigma_v_sq
  
  # Update Step
  if (!is.na(y[t])) {
    K_t   <- R[t] * F_val / Q[t]
    m[t]  <- alpha[t] + K_t * (y[t] - f[t])
    C[t]  <- R[t] * (1 - K_t * F_val)
  } else {
    m[t] <- alpha[t]
    C[t] <- R[t]
  }
  
  # Update priors
  m_prev <- m[t]
  C_prev <- C[t]
}

# Prepare Data for Plotting
results_df <- tibble(
  time = time,
  yt = y,
  predicted_y_t = f,
  sd_y_t = sqrt(Q),
  lower_ci = predicted_y_t - 1.96 * sd_y_t,
  upper_ci = predicted_y_t + 1.96 * sd_y_t
)

# Plot Observed vs. Predicted y
ggplot(results_df, aes(x = time)) +
  geom_point(aes(y = yt, color = "Observed yₜ"), alpha = 0.8, size = 2) +
  geom_line(aes(y = yt, color = "Observed yₜ"), alpha = 0.5, size = 0.8) +
  
  geom_line(aes(y = predicted_y_t, color = "Predicted yₜ (One-Step Ahead)"), size = 1) +
  geom_point(aes(y = predicted_y_t, color = "Predicted yₜ (One-Step Ahead)"), shape = 4, size = 2, stroke = 1.2) +
  
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "95% CI"), alpha = 0.2) +
  
  scale_color_manual(name = "Series",
                     values = c("Observed yₜ" = "darkblue", "Predicted yₜ (One-Step Ahead)" = "darkred")) +
  scale_fill_manual(name = "Interval",
                    values = c("95% CI" = "grey")) +
  
  labs(
    title = "Kalman Filter: Observed yₜ vs. One-Step-Ahead Predicted yₜ",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Report Numerical Values for t = 40
f_40 <- f[40]
Q_40 <- Q[40]


cat("Value for f_40", f_40)
cat("Value for Q_40", Q_40)

```
**Value for F_40: 4.23**

**Value for Q_40: 17.57**
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/plot_figure_B.png)

## Part C
```
# Load Libraries 
library(dlm)
library(ggplot2)
library(tidyverse)

# Load Data
data_df <- read.csv("DLM_Data.csv")
y <- data_df$yt
n <- length(y)
time <- 1:n

# Define Model Parameters 
F_val <- 1.2
G_val <- 0.8
sigma_v_sq <- 9  
sigma_w_sq <- 4   
m_prev <- 0      
C_prev <- 25      

# Initialize Vectors to Store Kalman Filter Results
alpha <- R <- f <- Q <- m <- C <- numeric(n)

for (t in 1:n) {
  # Prediction Step
  alpha[t] <- G_val * m_prev                     
  R[t]     <- G_val^2 * C_prev + sigma_w_sq       
  
  f[t]     <- F_val * alpha[t]                    
  Q[t]     <- F_val^2 * R[t] + sigma_v_sq         
  
  # Observation available?
  if (!is.na(y[t])) {
    K_t   <- R[t] * F_val / Q[t]                  
    m[t]  <- alpha[t] + K_t * (y[t] - f[t])      
    C[t]  <- R[t] * (1 - K_t * F_val)             
  } else {
    m[t] <- alpha[t]
    C[t] <- R[t]
  }
  
  # Update priors
  m_prev <- m[t]
  C_prev <- C[t]
}

# Prepare Data for Plotting 
results_df <- tibble(
  time = time,
  yt = y,
  filtered_theta = m,
  sd_theta = sqrt(C),
  lower_ci = m - 1.96 * sd_theta,
  upper_ci = m + 1.96 * sd_theta
)

# Plot Filtered vs Observed 
ggplot(results_df, aes(x = time)) +
  geom_point(aes(y = yt, color = "Observed yₜ"), size = 2, alpha = 0.8) +
  geom_line(aes(y = yt, color = "Observed yₜ"), size = 0.8, alpha = 0.5) +
  geom_line(aes(y = filtered_theta, color = "Filtered θₜ"), size = 1) +
  geom_point(aes(y = filtered_theta, color = "Filtered θₜ"), shape = 4, size = 2, stroke = 1.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "95% CI"), alpha = 0.2) +
  scale_color_manual(name = "Series",
                     values = c("Observed yₜ" = "steelblue", "Filtered θₜ" = "darkred")) +
  scale_fill_manual(name = "Interval", values = c("95% CI" = "grey")) +
  labs(
    title = "Kalman Filter: Observed yₜ vs Filtered θₜ",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

#  Report Values for t = 40 
cat("\n--- Reported Numerical Values ---\n")
cat(sprintf("m₄₀ (filtered mean of θ₄₀): %.2f\n", m[40]))
cat(sprintf("C₄₀ (filtered variance of θ₄₀): %.2f\n", C[40]))

```
**M40 = 0.66**

**C40 = 3.05**
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/plot_for_C.png)

## Part D

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/unnamed.jpg)

## Part E
```
# Load necessary libraries
library(dlm) 
library(ggplot2)
library(tidyverse) 

# Load the Dataset from CSV
data_df <- read.csv("DLM_Data.csv")

# Extract the observed time series 
y <- data_df$yt
n <- length(y)
time_points <- 1:n

# Create a data frame for plotting observed data
observed_data_df <- data.frame(time = time_points, yt = y )

#  Define Model Parameters
F_val <- 1.2
G_val <- 0.8
sigma_v_sq <- 9 
sigma_w_sq <- 4 

# Initial prior for theta
m_prev <- 0  
C_prev <- 25 

# Initialize lists to store Kalman Filter 
alpha_values <- numeric(n) 
R_values <- numeric(n)     
m_values <- numeric(n)    
C_values <- numeric(n)    


# Kalman Filter (Forward Pass) 
cat("Starting Kalman Filter (Forward Pass)...\n")

for (t in time_points) {
  # Prediction Step (Time Update) 
  alpha_curr <- G_val * m_prev
  R_curr <- G_val^2 * C_prev + sigma_w_sq
  
  alpha_values[t] <- alpha_curr
  R_values[t] <- R_curr
  
  current_yt_obs <- y[t]
  
  # Update Step (Measurement Update) 
  if (!is.na(current_yt_obs)) { # If observation is available
    f_curr <- F_val * alpha_curr
    Q_curr <- F_val^2 * R_curr + sigma_v_sq
    K_t <- (R_curr * F_val) / Q_curr
    
    m_curr <- alpha_curr + K_t * (current_yt_obs - f_curr)
    C_curr <- R_curr * (1 - K_t * F_val)
    
  } else { 
    m_curr <- alpha_curr
    C_curr <- R_curr
  }
  
  m_values[t] <- m_curr
  C_values[t] <- C_curr
  
  # Prepare for next iteration
  m_prev <- m_curr
  C_prev <- C_curr
}

# Initialize smoothed values with the last filtered values
m_smoothed <- numeric(n)
C_smoothed <- numeric(n)

m_smoothed[n] <- m_values[n]
C_smoothed[n] <- C_values[n]

# Loop backwards from T-1 down to 1
for (t in (n - 1):1) { 
  # Smoother Gain J_t
  J_t <- (C_values[t] * G_val) / R_values[t+1]
  
  # Smoothed Mean (m_t^S)
  m_smoothed[t] <- m_values[t] + J_t * (m_smoothed[t+1] - alpha_values[t+1])
  
  # Smoothed Variance (C_t^S)
  C_smoothed[t] <- C_values[t] + J_t^2 * (C_smoothed[t+1] - R_values[t+1])
}

# Data for Plotting
kalman_smoothed_df <- data.frame(
  time = time_points,
  smoothed_theta_t = m_smoothed,
  sd_smoothed_theta_t = sqrt(C_smoothed)
) %>%
  mutate(
    lower_ci = smoothed_theta_t - 1.96 * sd_smoothed_theta_t,
    upper_ci = smoothed_theta_t + 1.96 * sd_smoothed_theta_t
  )

# Combine observed data with Kalman smoothed results for plotting
plot_data <- left_join(observed_data_df, kalman_smoothed_df, by = "time")

# Create Time Series Plot
ggplot(plot_data, aes(x = time)) +
  # Plot observed yt values (points for clarity with NAs)
  geom_point(aes(y = yt, color = "Observed y_t"), alpha = 0.8, size = 2) +
  geom_line(aes(y = yt, color = "Observed y_t"), alpha = 0.5, size = 0.8) +
  
  # Plot smoothed estimates 
  geom_line(aes(y = smoothed_theta_t, color = "Smoothed θ_t"), linetype = "solid", size = 1) +
  geom_point(aes(y = smoothed_theta_t, color = "Smoothed θ_t"), shape = 4, size = 2, stroke = 1.2) +
  
  # Plot 95% Confidence Band 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "95% CI for Smoothed θ_t"), alpha = 0.2) +
  
  # Custom colors for lines and fill
  scale_color_manual(name = "Series",
                     values = c("Observed y_t" = "darkblue", "Smoothed θ_t" = "darkgreen")) +
  scale_fill_manual(name = "Interval",
                    values = c("95% CI for Smoothed θ_t" = "grey")) +
  
  # Labels and Title
  labs(
    title = "Kalman Smoother: Observed y_t vs. Smoothed θ_t",
    x = "Time (t)",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Report Smoothed Theta_t values for missing 
missing_y_indices <- which(is.na(y)) 
if (length(missing_y_indices) > 0) {
  for (idx in missing_y_indices) {
    cat(sprintf("Smoothed θ_%d: %.2f (95%% CI: %.2f - %.2f)\n",
                idx,
                kalman_smoothed_df$smoothed_theta_t[idx],
                kalman_smoothed_df$lower_ci[idx],
                kalman_smoothed_df$upper_ci[idx]))
  }
} else {
  cat("No missing y_t observations in the dataset.\n")
}

```
**Missing Observations Position: 11, 23, 24, 25, 26, 27, 27, 64, 80**

**Smoothed θ_t values for missing y_t observations**
- Smoothed θ_11: 0.76 (95% CI: -3.19 - 4.71)
- Smoothed θ_23: 2.53 (95% CI: -2.15 - 7.20)
- Smoothed θ_24: 1.77 (95% CI: -3.46 - 7.00)
- Smoothed θ_25: 1.10 (95% CI: -4.29 - 6.48)
- Smoothed θ_26: 0.48 (95% CI: -4.75 - 5.71)
- Smoothed θ_27: -0.12 (95% CI: -4.79 - 4.56)
- Smoothed θ_64: -0.68 (95% CI: -4.63 - 3.27)
- Smoothed θ_80: 0.09 (95% CI: -3.86 - 4.04)

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Figure_E.png)


## Problem  F
```
# Load Libraries
library(dlm)
library(ggplot2)
library(tidyverse)

# Load Data
data_df <- read.csv("DLM_Data.csv")
y_t_series <- data_df$yt
n_observed <- length(y_t_series)
n_forecast <- 10
total_plot_length <- n_observed + n_forecast

# Create Observed Data Frame
observed_data_df <- data.frame(
  time = 1:n_observed,
  yt = y_t_series
)

# Define Model Parameters
F_val <- 1.2
G_val <- 0.8
sigma_v_sq <- 9  # Measurement noise variance
sigma_w_sq <- 4  # Process noise variance
m_prev <- 0
C_prev <- 25

# Initialize Kalman Filter Storage
alpha_values <- numeric(n_observed)
R_values     <- numeric(n_observed)
f_values     <- numeric(n_observed)
Q_values     <- numeric(n_observed)
m_values     <- numeric(n_observed)
C_values     <- numeric(n_observed)

# Kalman Filter: Forward Pass 
for (t in 1:n_observed) {
  alpha_curr <- G_val * m_prev
  R_curr     <- G_val^2 * C_prev + sigma_w_sq
  
  alpha_values[t] <- alpha_curr
  R_values[t]     <- R_curr
  
  f_curr <- F_val * alpha_curr
  Q_curr <- F_val^2 * R_curr + sigma_v_sq
  f_values[t] <- f_curr
  Q_values[t] <- Q_curr
  
  current_y <- y_t_series[t]
  
  if (!is.na(current_y)) {
    K_t <- (R_curr * F_val) / Q_curr
    m_curr <- alpha_curr + K_t * (current_y - f_curr)
    C_curr <- R_curr * (1 - K_t * F_val)
  } else {
    m_curr <- alpha_curr
    C_curr <- R_curr
  }
  
  m_values[t] <- m_curr
  C_values[t] <- C_curr
  m_prev <- m_curr
  C_prev <- C_curr
}

# Forecasting θ_t and y_t from t=101 to 110 
m_forecast_prev <- m_values[n_observed]
C_forecast_prev <- C_values[n_observed]

forecast_f_values <- numeric(n_forecast)
forecast_Q_values <- numeric(n_forecast)

for (k in 1:n_forecast) {
  alpha_forecast <- G_val * m_forecast_prev
  R_forecast     <- G_val^2 * C_forecast_prev + sigma_w_sq
  
  f_forecast <- F_val * alpha_forecast
  Q_forecast <- F_val^2 * R_forecast + sigma_v_sq
  
  forecast_f_values[k] <- f_forecast
  forecast_Q_values[k] <- Q_forecast
  
  m_forecast_prev <- alpha_forecast
  C_forecast_prev <- R_forecast
}

# Prepare Data for Plotting
kalman_prediction_original_df <- data.frame(
  time = 1:n_observed,
  predicted_y_t = f_values,
  sd_predicted_y_t = sqrt(Q_values)
) %>%
  mutate(
    lower_ci = predicted_y_t - 1.96 * sd_predicted_y_t,
    upper_ci = predicted_y_t + 1.96 * sd_predicted_y_t,
    type = "Original Prediction"
  )

kalman_forecast_df <- data.frame(
  time = (n_observed + 1):total_plot_length,
  predicted_y_t = forecast_f_values,
  sd_predicted_y_t = sqrt(forecast_Q_values)
) %>%
  mutate(
    lower_ci = predicted_y_t - 1.96 * sd_predicted_y_t,
    upper_ci = predicted_y_t + 1.96 * sd_predicted_y_t,
    type = "Forecast"
  )

all_predictions_df <- bind_rows(kalman_prediction_original_df, kalman_forecast_df)
plot_data_full <- full_join(observed_data_df, all_predictions_df, by = "time")

# Plot
ggplot(plot_data_full, aes(x = time)) +
  geom_point(aes(y = yt, color = "Observed y_t"), size = 2, alpha = 0.8) +
  geom_line(aes(y = yt, color = "Observed y_t"), alpha = 0.5, size = 0.8) +
  geom_line(aes(y = predicted_y_t, color = type), size = 1, linetype = "dashed") +
  geom_point(aes(y = predicted_y_t, color = type), shape = 4, size = 2, stroke = 1.2) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = type), alpha = 0.2) +
  scale_color_manual(
    name = "Series",
    values = c("Observed y_t" = "darkblue", "Original Prediction" = "darkred", "Forecast" = "purple")
  ) +
  scale_fill_manual(
    name = "Interval",
    values = c("Original Prediction" = "grey", "Forecast" = "lightgreen")
  ) +
  labs(
    title = "Kalman Filter: Observed y_t vs. One-Step-Ahead Predictions & Forecasts",
    x = "Time (t)",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Report Forecast Variances
Q_101 <- forecast_Q_values[1]
Q_110 <- forecast_Q_values[n_forecast]

cat("Q_101", Q_101)
cat("Q_110", Q_110)
```
**Q101 = 17.57**

**Q110 = 24.87**
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/FIGURE_PART_A.png)

**Why is Q_101 < Q_110?**

Predictive variance (Q_t) increases over time due to the accumulation of uncertainty in the latent state θ_t. Since each step adds process noise (w_t), the variance of θ_t (R_t) grows as we forecast further ahead. Thus, Q_t = F^2 * R_t + σ_v^2 becomes larger for later t. So, predictions like y_110 are more uncertain than y_101.
