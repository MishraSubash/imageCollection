# STAFF-GRADED HOMEWORK: Spatial Point Patterns and Processes

### Part A
```library(leaflet)
# Load data
data <- read.csv("chi_markets.csv")
lon <- data$lon
lat <- data$lat

# Create dataframe
supermarkets <- data.frame(
  lat = lat,
  lon = lon
)

# Calculate bounding box
min_lon <- min(supermarkets$lon)
max_lon <- max(supermarkets$lon)
min_lat <- min(supermarkets$lat)
max_lat <- max(supermarkets$lat)

# Create leaflet map with rectangle
leaflet(data = supermarkets) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat) %>%
  addRectangles(
    lng1 = min_lon, lat1 = min_lat,
    lng2 = max_lon, lat2 = max_lat,
    color = "red",
    weight = 2,
    fillOpacity = 0.1
  )
  ```
**Leaflet Plot of Chicago Supermarket Locations**
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Rplot_LeafLet%20Plot.png)

**Why no supermarkets in the top right corner?**
The bounding rectangle is defined by the most extreme latitude and longitude values in the dataset creating a box that fully contains all supermarket locations. However, this does not mean that supermarkets must be evenly distributed throughout that entire rectangle. In this case, the top right corner of the rectangle extends into Lake Michigan, which is an unbuildable area. It is obvious that there won’t be any supermarkets in the water.

# Part B
```
# PPP object 
load("chi_supermarkets_ppp.Rdata")
plot(markets_ppp)
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/market_ppp_plot.png)

- It looks the clustering of markets along the shoreline in Chicago. This suggests that the locations of markets are not independently and uniformly distributed across the city, but instead show a spatial dependence related to their proximity to the shoreline.
- The intensity of points varies across the region. So, it is unlikely to follow a homogeneous spatial Poisson process.

# Part C
```
inhomo <- ppm(markets_ppp ~ polynom(x,y,2))

plot(inhomo, main = "Inhomogeneous Poisson Point Process",how = 'image', se = FALSE, col = grey(seq(1,0,length =300)))
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Rplot_inhomo_plot.png)


```
intercept <- round(coef(inhomo)[["(Intercept)"]], digits = 4)
intercept

predict.ppm(object = inhomo,locations = data.frame(x = 443674,y = 4636999))
```

**Intercepts: -31395.97**

**numerical value of the intensity function at the point (x = 443674, y = 4636999): 7.705657e-07 or 7.706 X 10^(-7)**
