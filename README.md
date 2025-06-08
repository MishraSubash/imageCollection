# STAFF-GRADED HOMEWORK: Time Series Introduction

## Homework Questions 
### Generate Tennis Court using the given code
```
library(ggplot2)
library(MASS)  # generating bivariate normal samples

# Set the seed for generating same random samples
set.seed(123)

#Creates a data.frame object, the easy structure to use for ggploting
tennisCourt <- data.frame(x1 = c(0,4.5,18,31.5,36,0,4.5,4.5,0,-2),
                          x2 = c(0,4.5,18,31.5,36,36,31.5,31.5,36,38),
                          y1 = c(-39,-39,-21,-39,-39,39,21,-21,-39,0), 
                          y2 = c(39,39,21,39,39,39,21,-21,-39,0),
                          width = c(rep(1,9),3))

ggTennis <- ggplot(tennisCourt) + 
  geom_segment(aes(x = x1,y = y1,xend = x2,yend = y2),size = tennisCourt$width) + 
  labs(x = "Lateral Location (X1)", y = "Depth Location (X2)", title = "Tennis Serve Locations") 
#Running the next line will show your tennis court as a plot
ggTennis
```
## Tennis Court
![Tennis Court](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/updated_ten.png)
