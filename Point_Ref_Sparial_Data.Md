# Point-Referenced Spatial Data and Gaussian Processes

### Part A
```library(leaflet)
# Load required libraries
library(ggplot2)
library(mapproj)
library(geoR)
library(leaflet)
library(gridExtra)

df = read.csv("rain.txt", sep = "\t")
# plot: Rainfall = color, Altitude = size
combined_plot <- ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(color = rainfall, size = altitude_miles), alpha = 0.8) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  scale_size(range = c(2, 8)) +
  labs(
    title = "Spatial Variation of Rainfall (color) and Altitude (size)",
    x = "x",
    y = "y",
    color = "Rainfall (mm)",
    size = "Altitude (miles)"
  ) +
  theme_minimal()

# Display
combined_plot
  ```
**Exploratory plots showing the spatial variation in rainfall and altitude**
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/RplotPart1.png)
**Conclusion:** 

There appears to be spatial dependence in both average rainfall and altitude: nearby locations tend to have similar colors and sizes, indicating that rainfall and altitude values are not randomly scattered but vary smoothly across space. Specifically, regions with higher rainfall (darker blue) often cluster together, and larger point sizes (higher altitude in miles) also show some clustering, suggesting a possible relationship between terrain elevation and rainfall patterns in this region.

So, the map suggests that both variables display clear spatial autocorrelation, a key sign of spatial structure.

# Part B
```
# Fit linear model: sqrt(rainfall) in altitude_miles
df$sqrt_rainfall <- sqrt(df$rainfall)
fit <- lm(sqrt_rainfall ~ altitude_miles, data = df)

# Model summary
summary_fit <- summary(fit)

# Estimated regression equation
intercept <- coef(fit)[1]
slope <- coef(fit)[2]
cat("Estimated regression equation:\n")
cat("sqrt(rainfall) =", round(intercept, 3), "+", round(slope, 3), "* altitude_miles\n\n")

# Estimated error variance (residual variance)
sigma2_hat <- summary_fit$sigma^2
cat("Estimated error variance:", round(sigma2_hat, 3), "\n\n")

# Proportion of variation explained (R-squared)
r2 <- summary_fit$r.squared
cat("Proportion of variation explained (R^2):", round(r2, 3), "\n")
```
- Estimated regression equation: = 14.127 - 8.368 * altitude_miles
- Estimated error variance: 18.28
- Proportion of variation explained (R^2): 0.031


# Part C
```
# Compute pairwise Euclidean distances
dist_matrix <- as.matrix(dist(df[, c("x", "y")], method = "euclidean"))

# Extract upper triangle distances (excluding diagonal zeros)
distances <- dist_matrix[upper.tri(dist_matrix)]

# Create data frame for ggplot
dist_df <- data.frame(distance = distances)

# Plot relative frequency histogram with binwidth = 20 miles
hist_plot <- ggplot(dist_df, aes(x = distance)) +
  geom_histogram(aes(y = ..density..), binwidth = 20, fill = "steelblue", color = "black") +
  labs(
    title = "Relative Frequency Histogram of Euclidean Distances",
    x = "Distance (miles)",
    y = "Relative Frequency"
  ) +
  theme_minimal()

# Display plot
hist_plot
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Rplot_partc.png)

## Part D

```
# Compute residuals
df$altitude_miles <- df$altitude / 5280
df$sqrt_rainfall <- sqrt(df$rainfall)

fit <- lm(sqrt_rainfall ~ altitude_miles, data = df)
df$residuals <- resid(fit)

# Compute pairwise distances and residual products
n <- nrow(df)
dist_matrix <- as.matrix(dist(df[, c("x", "y")], method = "euclidean"))

# Make empty data frame to store pairwise info
pairs <- data.frame(
  i = integer(),
  j = integer(),
  distance = numeric(),
  resid_i = numeric(),
  resid_j = numeric()
)

# Extract upper triangle only (i < j)
for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    pairs <- rbind(pairs, data.frame(
      i = i,
      j = j,
      distance = dist_matrix[i, j],
      resid_i = df$residuals[i],
      resid_j = df$residuals[j]
    ))
  }
}

# Define distance bins and bin centers
bin_edges <- seq(0, 300, by = 20)
bin_centers <- bin_edges[-length(bin_edges)] + 10  # center of each bin

# Assign bins to pairs
pairs$bin <- cut(pairs$distance,
                 breaks = bin_edges,
                 right = FALSE,
                 labels = bin_centers)

# Compute correlation per bin
bin_summary <- pairs %>%
  group_by(bin) %>%
  summarise(
    cor = ifelse(length(resid_i) > 1,
                 cor(resid_i, resid_j),
                 NA_real_),
    n_pairs = n()
  ) %>%
  filter(!is.na(bin))

# Make sure bin is numeric for plotting
bin_summary$bin_center <- as.numeric(as.character(bin_summary$bin))

# Plot: bin center vs correlation, size by number of pairs
ggplot(bin_summary, aes(x = bin_center, y = cor, size = n_pairs)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  labs(
    title = "Correlation of Residuals vs Distance Bin Center",
    x = "Distance Bin Center (miles)",
    y = "Correlation of Residuals",
    size = "Number of Pairs"
  ) +
  theme_minimal()

```

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Rplot_PartD.png)

## Part E
There are fewer pairs of locations in the longer-distance bins because, in any fixed study area, there are naturally more location pairs that are close together than far apart. For example, when locations are clustered or spread out over a limited region, it is more likely to find pairs separated by moderate distances (like 80–100 miles) than by very large distances (like 280–300 miles). The maximum distance is limited by the geographic extent of the region, so only a few pairs can span that full range, resulting in fewer pairs in the largest distance bins.

## Part F
```
# Load geoR
library(geoR)

# Coordinates:
coords <- as.matrix(df[, c("x", "y")])

# Data: sqrt(rainfall)
data_response <- df$sqrt_rainfall

# Covariate: altitude in miles
covariates <- data.frame(altitude_miles = df$altitude_miles)

# Create geodata object
geo_data <- as.geodata(cbind(df$x, df$y, data_response), covariate = covariates)

# Fit normal spatial linear model with exponential covariance and no nugget
fit_geo <- likfit(
  geodata = geo_data,
  trend = ~ df$altitude_miles,
  ini.cov.pars = c(10, 100),  # initial guesses: (sigma2, phi)
  cov.model = "exponential",
  nugget = 0,
  lik.method = "ML"
)

# Extract key estimates
beta0 <- fit_geo$beta[1]
beta1 <- fit_geo$beta[2]
sigma2 <- fit_geo$sigmasq
phi <- fit_geo$phi

cat("\nEstimated intercept (beta0):", round(beta0, 3), "\n")
cat("Estimated slope (beta1):", round(beta1, 3), "\n")
cat("Estimated sigma^2:", round(sigma2, 3), "\n")
cat("Estimated phi:", round(phi, 3), "\n")

```
- Estimated intercept (β0) : 11.597
- Estimated slope (β1),  : 0.029
- Covariance parameters (σ^2, ϕ): (20.973, 42.407)

## Part G
```
Using previously defined bin_summary$bin_center and bin_summary$cor from part D

# Drop any NA or missing correlations
empirical <- na.omit(bin_summary)

# Fit exponential model: cor ≈ exp(-h/phi)
exp_model <- nls(cor ~ exp(-bin_center / phi),
                 data = empirical,
                 start = list(phi = 100))  # starting guess

# Extract fitted phi
phi_hat <- coef(exp_model)[["phi"]]
cat("Estimated phi:", round(phi_hat, 2), "\n")

# Add fitted line to the scatter plot
ggplot(empirical, aes(x = bin_center, y = cor, size = n_pairs)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  stat_function(fun = function(h) exp(-h / phi_hat),
                color = "red",
                linetype = "dashed",
                size = 1) +
  labs(
    title = "Correlation of Residuals with Fitted Exponential Model",
    x = "Distance Bin Center (miles)",
    y = "Correlation of Residuals",
    size = "Number of Pairs"
  ) +
  theme_minimal()
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Rplot_Part_G.png)

## Part H
```
# Non-spatial Model
fit <- lm(sqrt_rainfall ~ df$altitude_miles, data = df)
AIC_non_spatial <- AIC(fit)
cat("Non-spatial model AIC:", round(AIC_non_spatial, 2), "\n")

# Spatial model (fitted using geoR)
loglik_spatial <- fit_geo$loglik
num_params_spatial <- 4  # β₀, β₁, σ², ϕ

AIC_spatial <- -2 * loglik_spatial + 2 * num_params_spatial
cat("Spatial model AIC:", round(AIC_spatial, 2), "\n")
```
- Non-spatial model AIC: 578.35
- Spatial model AIC: 503.49


## Part I 
In addition to the coordinates, I would need relevant predictor variables that influence rainfall. For instance, altitude, slope, aspect, distance to mountains, or proximity to moisture sources. These covariates help explain local variation in rainfall and improve the accuracy of predictions at unmonitored locations when using spatial regression or kriging models.
