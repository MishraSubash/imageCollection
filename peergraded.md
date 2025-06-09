## Part C
```
# Load a library
library(ggplot2)

# Time vector from 0 to 120
t <- 0:120

# Signal component: 10 * sin(t/5)
signal <- 10 * sin(t / 5)

# Noise component: wt ~ N(0, 2^2)
noise <- rnorm(length(t), mean = 0, sd = 2)

# Realization of the process: x_t = signal + noise
xt <- signal + noise

# Create data frame for plotting
df <- data.frame(time = t, x_t = xt)

# Plot the time series
ggplot(df, aes(x = time, y = x_t)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 1.2) +
  labs(title = "Realization of Signal + Noise Process",
       x = "Time (t)",
       y = expression(x[t])) +
  theme_minimal()
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/hw1_peerGraded.png)

## Part D
```
# Load libraries
library(ggplot2)
library(gridExtra)

# Set seed for reproducibility
set.seed(123)

# Time points
t <- 0:120

# Signal (same for both): 10 * sin(t/5)
signal <- 10 * sin(t / 5)

# Noise 1: N(0, 2^2)
noise1 <- rnorm(length(t), mean = 0, sd = 2)
x1 <- signal + noise1
df1 <- data.frame(time = t, value = x1)

# Noise 2: N(0, 4^2)
noise2 <- rnorm(length(t), mean = 0, sd = 4)
x2 <- signal + noise2
df2 <- data.frame(time = t, value = x2)

# Plot 1: Original noise
p1 <- ggplot(df1, aes(x = time, y = value)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 1.2) +
  ylim(-15, 15) +
  labs(title = "Signal with σ² = 4",
       x = "Time (t)", y = expression(x[t])) +
  theme_minimal()

# Plot 2: Noisier signal
p2 <- ggplot(df2, aes(x = time, y = value)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 1.2) +
  ylim(-15, 15) +
  labs(title = "Signal with σ² = 16",
       x = "Time (t)", y = expression(x[t])) +
  theme_minimal()

# Display side-by-side
grid.arrange(p1, p2, ncol = 2)

# Signal-to-noise ratio calculation
# Signal variance is var(10 * sin(t/5))
signal_var <- var(signal)
noise_var2 <- 16  # For second case

SNR <- signal_var / noise_var2

# Print SNR
cat("Signal-to-noise ratio (σ² = 16):", round(SNR, 4), "\n")
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/peergraded_image2.png)

**The new Signal-to-noise ratio (σ² = 16): 3.1932**


## Part D
```
# For reproducibility
set.seed(123)  

# Time vector
t <- 0:120

# True signal
signal <- 10 * sin(t / 5)

# Variance of the signal
signal_var <- var(signal)

# Process 1: noise with variance 4
noise1 <- rnorm(length(t), mean = 0, sd = 2)
x1 <- signal + noise1
snr1 <- signal_var / var(noise1)

# Process 2: noise with variance 16
noise2 <- rnorm(length(t), mean = 0, sd = 4)
x2 <- signal + noise2
snr2 <- signal_var / var(noise2)

# Expected value at t = 45
t_val <- 45
expected_val <- 10 * sin(t_val / 5)

# Observed values at t = 45 
obs1 <- x1[t_val + 1] 
obs2 <- x2[t_val + 1]

# Print summary
cat("Signal Variance:", round(signal_var, 4), "\n")
cat("SNR (Process 1):", round(snr1, 4), "\n")
cat("SNR (Process 2):", round(snr2, 4), "\n")
cat("Expected Value at t=45:", round(expected_val, 4), "\n")
cat("Observed Value at t=45 (Process 1):", round(obs1, 4), "\n")
cat("Observed Value at t=45 (Process 2):", round(obs2, 4), "\n")
```

| Feature  | Process 1 (σ² = 4) | Process 2 (σ² = 16)  | Reason for Similarity/Difference |
| ------------- |:-------------:|------------- |:-------------:|
| Period      | 31.42    |31.42      | Same value as both are determined solely by given function     |
| Signal-to-Noise Ratio     | 16.0983     |3.3066      |Higher Noise equates to lower SNR     |
| Expected Value at t=45| 4.1212     |4.1212      | Mean is unaffected by noise variance     |
| Observed Value at t=45      | 1.875     |6.6675      | Higher Noise increase variance     |
