# Matrix Norms and Eigenvalues

**(a) Is this matrix dense or sparse? Is it symmetric?**
        
        Sparsity: 6x6 = 36 elements, 30 zeros => 83% zeros => Matrix is SPARSE.
        Symmetry: Diagonal matrix => A = A^T => Matrix is SYMMETRIC.


**(b) Eigenvalues and Eigenvectors**

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/medium%20Sized.jpg)

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/medium_Sized.jpg)

**Explanation for not showing work for all:** 

    For a diagonal matrix, the properties of eigenvalues and eigenvectors are straightforward.
    The eigenvalues are precisely the diagonal entries, and the corresponding eigenvectors 
    are the standard basis vectors. This pattern holds for all diagonal entries, 
    making redundant calculations unnecessary.

**(c) Computation of Frobenius, Operator, and Nuclear norms of the matrix**
- **Frobenius Norm:** The Frobenius norm is the square root of the sum of the squares of the diagonal elements. 

      Frobenius Norm = √[(2^2+ (1.)^2+(-3)^2+(-1)^2+ (0.5)^2+(-0.5)^2 )] = 4.1

- **Operator Norm:** The operator norm is the largest singular value of the matrix.

      Max singular value = max |eigenvalue| 

      Operator Norm (||A||_2)  = 3

- **Nuclear Norm:** The sum of all singular values of the matrix

      Sum of singular values    = sum of absolute eigenvalues
                                =  2 + 1.5 + 3 + 1 + 0.5 + 0.5 
                                = 8.5

**(d) Compare the largest eigenvalue in part b to the operator norm in part c. Are they equal? Why or why not?**

      The largest eigenvalue is max (λ_i) = max ({2, 1.5, −3, −1 ,0.5, −0.5 })   = 2

      Operator norm: 3

They are not equal. 

**Why or why not?**

    For a general matrix, the operator norm is defined as the largest singular value, 
    not necessarily the largest eigenvalue. Singular values are always non-negative. 
    While the eigenvalues can be negative, the singular values are the absolute 
    values of the eigenvalues for symmetric matrices. In this case, the largest 
    singular value (3) comes from the eigenvalue -3 (i.e., ∣−3∣=3), 
    which is larger than the largest positive eigenvalue (2). 
    Therefore, the largest eigenvalue (2) is not equal to the operator norm (3) 
    because the operator norm considers the magnitude of the eigenvalues.


## Python Code: 
```
import numpy as np

# Given matrix A
A = np.diag([2, 1.5, -3, -1, 0.5, -0.5])

print("Matrix A:")
print(A)

# a. Dense or Sparse, Symmetric?
# Count zeros to check sparsity
total_elements = A.size
zero_elements = np.count_nonzero(A == 0)
sparsity = zero_elements / total_elements

print(f"\nSparsity: {sparsity*100:.2f}%")
print(f"Is symmetric? {np.allclose(A, A.T)}")

# b. Eigenvalues & eigenvectors
eigvals, eigvecs = np.linalg.eig(A)

print("\nEigenvalues:")
print(eigvals)

print("\nEigenvectors (columns):")
print(eigvecs)

# c. Norms

# Frobenius Norm
fro_norm = np.linalg.norm(A, 'fro')

# Operator Norm (2-norm)
operator_norm = np.linalg.norm(A, 2)

# Nuclear Norm 
# For diagonal symmetric: sum of absolute eigenvalues
nuclear_norm = np.sum(np.abs(eigvals))

print(f"\nFrobenius Norm: {fro_norm:.4f}")
print(f"Operator Norm: {operator_norm:.4f}")
print(f"Nuclear Norm: {nuclear_norm:.4f}")

# d. Compare largest eigenvalue and operator norm
largest_eigenvalue = np.max(eigvals)
print(f"\nLargest  Eigenvalue: {largest_eigenvalue}")
print(f"Operator Norm: {operator_norm}")

if np.isclose(largest_eigenvalue, operator_norm):
    print("They are equal. For symmetric matrices, the operator norm equals the largest eigenvalue.")
else:
    print("They are NOT equal!")
```
