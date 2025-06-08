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
