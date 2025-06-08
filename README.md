# HOMEWORK: Time Series Introduction

### Generate Tennis Court using the given code
```
library(ggplot2)
library(MASS)  # generating bivariate normal samples

# Set the seed for generating same random samples
set.seed(123)

# Creates a data.frame object, the easy structure to use for ggploting
tennisCourt <- data.frame(x1 = c(0,4.5,18,31.5,36,0,4.5,4.5,0,-2),
                          x2 = c(0,4.5,18,31.5,36,36,31.5,31.5,36,38),
                          y1 = c(-39,-39,-21,-39,-39,39,21,-21,-39,0), 
                          y2 = c(39,39,21,39,39,39,21,-21,-39,0),
                          width = c(rep(1,9),3))

ggTennis <- ggplot(tennisCourt) + 
  geom_segment(aes(x = x1,y = y1,xend = x2,yend = y2),size = tennisCourt$width) + 
  labs(x = "Lateral Location (X1)", y = "Depth Location (X2)", title = "Tennis Serve Locations") 
# Running the next line will show your tennis court as a plot
ggTennis
```
## Tennis Court
![Tennis Court](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/updated_ten.png)

## Problem A: Solution 
```
# Set parameters
mu <- c(29, 16)
# covariance matrix
Sigma <- matrix(c(4, 4,
                  4, 16), nrow = 2)
sample_size = 5000
# Get Samples
samples <- as.data.frame(mvrnorm(n = sample_size, mu = mu, Sigma = Sigma))
colnames(samples) <- c("x", "y")

# Add the 5,000 sample points to the plot
ggTennisWithSamples <- ggTennis + 
  geom_point(data = samples, aes(x = x, y = y), color = 'blue', alpha = 0.3, size = 0.7)

# Show the plot
ggTennisWithSamples
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/problem2_image.png)


## Problem B: Solution
```
# Define the legal serve based on given boundries
is_legal <- with(samples, x >= 18 & x <= 31.5 & y >= 0 & y <= 21)

# Approximate probability
approx_prob <- mean(is_legal)

# Print the result
cat("Approximate probability of a legal serve:", round(approx_prob, 4), "\n")
```

**Approximate probability of a legal serve: 0.8254 or 82.54%**

## Problem C: Solution
```
# Given parameters
mu1 <- 29
mu2 <- 16
sigma1 <- sqrt(4)  # 2
sigma2 <- sqrt(16) # 4
rho <- 4 / (sigma1 * sigma2)  # 4 / (2 * 4) = 0.5

# Conditioning on X1 = 30.5
x1_cond <- 30.5

# Conditional mean and variance
mu_cond <- mu2 + rho * (sigma2 / sigma1) * (x1_cond - mu1)
var_cond <- sigma2^2 * (1 - rho^2)
sd_cond <- sqrt(var_cond)

# Probability that X2 is in [0, 21] given X1 = 30.5
p_legal_depth <- pnorm(21, mean = mu_cond, sd = sd_cond) - pnorm(0, mean = mu_cond, sd = sd_cond)

# Print result
cat("Probability that X2 is between 0 and 21 (legal depth):", round(p_legal_depth, 4), "\n")
```
**Probability that X2 is between 0 and 21 (legal depth): 0.8438 or 84.38%**

## Problem D: Solution
```
# 500 realizations of X2
x2_cond <- rnorm(500, mean = mu_cond, sd = sd_cond)
x1_jittered <- jitter(rep(x1_cond, 500), amount = 0.15)

# Create data frame for conditional samples
conditional_samples <- data.frame(x = x1_jittered, y = x2_cond)

# Plot original + conditional samples
ggplot() +
  # Original samples
  geom_point(data = samples, aes(x = x, y = y), color = 'blue', alpha = 0.3, size = 0.7) +
  # Conditional samples
  geom_point(data = conditional_samples, aes(x = x, y = y), color = 'red', alpha = 0.8, size = 1.2) +
  labs(title = "Scatterplot of Tennis Serve Locations",
       subtitle = "Original bivariate normal samples (blue) and conditional X2 given X1 = 30.5 (red)",
       x = "Lateral Location (X1)", y = "Depth Location (X2)") 
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Hw1_Final_image.png)

Result Explanation: 
  - The original points (blue dots) are widely spread out.
  - The conditional points (red dots) appear as a vertical band centered around 𝑋1 = 30.5 with some horizontal spread due to jittering.
  - The distribution of X2 in the red points is tighter, concentrated around a higher mean (around 17), and with a smaller spread compared to the full dataset probably becasue with higher variance due to correlation.

