# STAFF-GRADED HOMEWORK: Recap Matrices

**A. What is the maximum value in this matrix? What does this imply about the missing values of the matrix?**

    Answer: Maximum distance in the matrix: 99.99971353

    Missing values usually indicate that the distance is unknown, 
    not that the cities are infinitely far apart. 
    In distance matrices, missing ≠ 0 or missing ≠ infinity. 

**B. What is the closest location to "Katy city"? How many locations are within 25 miles of "Flower Mound town" (excluding the town itself)?**

    Answer: 
    Closest distance from Katy city: 5.337001509
    Closest city ID to Katy city: 277
    Closest Location to Katy city: Cinco Ranch CDP

    Number of locations within 25 miles of Flower Mound town: 75


**C. Find the time it takes to sum all of the elements in the matrix.**

    Answer: Time taken to sum all elements: 0.028404 seconds

**D. Compute the time it takes to sum all the elements in this new matrix. How does this compare to your previous results?**

    Answer: Time taken to sum cleaned elements: 0.009349 seconds

      After cleaning, distances > 50 and missing values are set to zero,
      effectively creating a sparser matrix. Cleaning reduces the number
      of non-zero elements, speeding up sum operation.

**E. Compute the time it takes to sum all the elements in this new sparse matrix. How does this compare to your previous results?**

    Time taken to compute sum on newly sparsed matrix: 0.001005 seconds

    Storing the distance matrix as a sparse matrix where missing values are not stored
    but real zeros are kept. It significantly reduced the time needed to compute 
    the sum compared to the dense format.

    This shows that using a sparse representation is highly efficient when the matrix 
    has many missing (or naturally zero) entries. By avoiding storage and computation 
    of missing values, the sparse format achieves faster operations and uses less memory 
    and less storage space. 


**F. Knowing that the matrix is symmetric and has zeros along the diagonal, how else could you save storage space?**

    If matrix is symmetric, it only need to store one half of the matrix.
    If the matrix diagonal is zero, they don’t need to store the diagonal elements either.

    We could cut the storage by roughly half (minus the diagonal). It also reduces 
    the computation for operations like sums and distances which  implies significant 
    storage and performance gains. 


## Complete Python Code.
```
# Import libraries
import pandas as pd
import numpy as np
import time
from scipy.sparse import csr_matrix

# Read csv files 
tx_city_dist_df = pd.read_csv('TexasCityDistanceMatrix.csv', header=0)
tx_city_key_df = pd.read_csv('TexasCityKey.csv')
```

### Part A 
```
# Compute the maximum value in the distance matrix
# Drop the 'Place' column to create a nXn square matrix
max_distance_df = tx_city_dist_df.drop(columns=['Place'])
max_distance = max_distance_df.max().max()
print(f"Maximum distance in the matrix: {max_distance}")
```

### Part B
```
# Loc the dataframe for "Katy city" and get the 'Place' id to look up 
# for City Name from another dataframe.
katy_idx = tx_city_key_df[tx_city_key_df['Name'] == "Katy city"]['Place'].iloc[0]
v_katy_idx = f"V{katy_idx}" # Matching columns names so adding 'V' suffix

# Create a datframe with only necessary fields.
katy_distances = tx_city_dist_df[['Place', v_katy_idx]]

# Exclude the distance to itself
katy_distances_no_self = katy_distances[katy_distances['Place'] != katy_idx]

# Find the closest city ID
# Get closest distance
closest_distance = katy_distances_no_self[v_katy_idx].min()
print(f"Closest distance from Katy city: {closest_distance}")

# Get the 'Place' id for the closest city associated with 'closest_distance'
closest_city = katy_distances_no_self[katy_distances_no_self[v_katy_idx] == closest_distance]['Place'].iloc[0]
print(f"Closest city ID to Katy city: {closest_city}")

# Now look up the name of the closest city name using the 'closest_city' place id 
closest_city_name = tx_city_key_df[tx_city_key_df['Place'] == closest_city]['Name'].iloc[0]
print(f"Closest Location to Katy city: {closest_city_name}")
# Get the place Id for "Flower Mound town" 
flower_idx = tx_city_key_df[tx_city_key_df['Name'] == "Flower Mound town"]['Place'].iloc[0]

# Grab the columns for "Flower Mound town" in the distance matrix
flower_distances = tx_city_dist_df[tx_city_dist_df['Place'] == flower_idx]

flower_distances_no_self = flower_distances.drop(columns= f"V{flower_idx}").iloc[0]
# Count how many cities are within 25 miles
within_25_miles = (flower_distances_no_self < 25).sum()
print(f"Number of locations within 25 miles of Flower Mound town: {within_25_miles}")

```

### Part C 
```
# Time the sum operation
start_time = time.time()
total_sum = max_distance_df.sum().sum()
end_time = time.time()
elapsed_time = end_time - start_time

print(f"Time taken to sum all elements: {elapsed_time:.6f} seconds")
```

### Part D

```
# Clean the distance matrix by replacing values greater than 50 with 0
# and filling NaN values with 0
cleaned_dist_df = max_distance_df.mask(max_distance_df > 50, 0).fillna(0)

# Capture time to perform sum operation on cleaned data
start_time_cleaned = time.time()
cleaned_total_sum = cleaned_dist_df.sum().sum()
end_time_cleaned = time.time()
cleaned_elapsed_time = end_time_cleaned - start_time_cleaned
print(f"Time taken to sum cleaned elements: {cleaned_elapsed_time:.6f} seconds")
```
### Part E
```
# Build sparse matrix: skip NaNs, keep zeros
# Flatten and build row, col, data arrays:
values = []
rows = []
cols = []

matrix_values = max_distance_df.values

for i in range(matrix_values.shape[0]):
    for j in range(matrix_values.shape[1]):
        val = matrix_values[i, j]
        if not np.isnan(val):
            rows.append(i)
            cols.append(j)
            values.append(val)

# Create CSR sparse matrix
sparse_matrix = csr_matrix((values, (rows, cols)), shape=matrix_values.shape)

# Sum sparse matrix and time it
start_sparse = time.time()
sparse_sum = sparse_matrix.sum()
end_sparse = time.time()
sparse_time = end_sparse - start_sparse

print(f"Time for sparse sum: {sparse_time:.6f} seconds")
```
