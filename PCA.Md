## PEER GRADED-HOMEWORK: SVD and PCA

## Part A
```
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd 

data = pd.read_csv("/content/PCAData.csv")
data = data[['0', '1']]

# Separate the columns for plotting
x_coords = data['0']
y_coords = data['1']

# Create the scatterplot
plt.figure(figsize=(8, 6))
plt.scatter(x_coords, y_coords, alpha=0.7, color='blue', s=50)
plt.title('Scatterplot of Data')
plt.grid(True, linestyle='--', alpha=0.6)
plt.show()
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Fig1.png)

**Description**

    The scatterplot shows a distribution of points that appears to be elongated. 
    The points seem to form a roughly linear cluster, extending from the bottom-left
    to the top-right. The strongest variation (or spread) of the points is observed
    along a diagonal direction, approximately from the lower-left quadrant to the upper-right 
    quadrant of the plot. This suggests a positive correlation between 'Column 0' and 'Column 1'.

## Part B

```
# Center the data (subtract the mean from each column)
mean_data = np.mean(data, axis=0)
centered_data = data - mean_data

# Calculate the covariance matrix.
covariance_matrix = np.cov(centered_data, rowvar=False)

# Compute eigenvalues and eigenvectors of the covariance matrix
eigenvalues, eigenvectors = np.linalg.eig(covariance_matrix)

print(f"Eigenvalues:{np.round(eigenvalues, 2)}")
print(f"Eigenvectors {np.round(eigenvectors, 2)}")
```
- Eigenvalues:  8.17, 1.73
- Eigenvectors  [ 0.81 -0.58],  [ 0.58  0.81]


## Part C
```
# Plot eigenvectors on the scatterplot
plt.figure(figsize=(8, 6))
plt.scatter(x_coords, y_coords, alpha=0.7, color='blue', s=50, label='Data Points')
plt.title('Scatterplot with Principal Components')
plt.grid(True, linestyle='--', alpha=0.6)
plt.axhline(0, color='grey', linewidth=0.8)
plt.axvline(0, color='grey', linewidth=0.8)

# Plot the eigenvectors
for i in range(len(eigenvalues)):
    scale_factor = np.sqrt(eigenvalues[i]) * 3
    start_point = mean_data
    end_point = mean_data + eigenvectors[:, i] * scale_factor
    plt.plot([start_point[0], end_point[0]], [start_point[1], end_point[1]],
             color='red' if i == 0 else 'green',
             linewidth=2,
             label=f'PC {i+1} (Eigenvalue: {np.round(eigenvalues[i], 2)})')
plt.legend()
plt.show()
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Fig2.png)

**Interpretation**

**How do the directions relate to each other?**

The eigenvectors are perpendicular (orthogonal) to each other.
This is always true in PCA, since principal components form an orthogonal basis.

**Which direction does the first (and longer) eigenvector point?**

The first eigenvector (PC1) points in the direction where the data varies most strongly (diagonal from bottom-left to top-right). This is the line of maximum spread.

**What does this imply about PCA?**

PC1 captures the major trend or dominant pattern in the data.

PC2 is orthogonal and captures remaining variation.

This shows that PCA rotates the coordinate system to align with directions of maximum variance, enabling dimensionality reduction and clearer structure.
