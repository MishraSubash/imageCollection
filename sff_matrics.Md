## Staff Graded HOMEWORK: SVD and PCA

## Part A
```
import numpy as np
import matplotlib.pyplot as plt

# Load data from CSV
data = np.loadtxt('MNIST_Subset.csv', delimiter=',')

# Infer image dimensions
num_images, num_pixels = data.shape
img_size = int(np.sqrt(num_pixels))

# Plot first 5 images
plt.figure(figsize=(12, 3))
for i in range(5):
    plt.subplot(1, 5, i + 1)
    plt.imshow(data[i].reshape(img_size, img_size), cmap='gray')
    plt.title(f"Image {i+1}")
    plt.axis('off')
plt.tight_layout()
plt.show()
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/fig_1.png)
**What numbers displayed in these images? Are the images clear or noisy?**

Image 1 looks black and white blank page.
Image 2 though 4 looks like they are number 4 and image 5 looks like number 4 or 9 can't really tell. 

Images are noisy. Pixel-level distortions reduces clarity.

## Part B

```
# Truncate matrices
U_k = U[:, :k]
S_k = np.diag(s[:k])
Vh_k = Vh[:k, :]

image_height = int(np.sqrt(num_pixels))
image_width = int(np.sqrt(num_pixels))

# Denoise 
denoised_data = np.clip(U_k @ S_k @ Vh_k, 0, 255).astype(np.uint8)

# Plot original vs denoised images
plt.figure(figsize=(15, 6))
for i in range(min(5, num_images)):
    # Original
    plt.subplot(2, 5, i + 1)
    plt.imshow(data[i].reshape(image_height, image_width), cmap='gray')
    plt.title(f"Original {i+1}")
    plt.axis('off')

    # Denoised
    plt.subplot(2, 5, i + 6)
    plt.imshow(denoised_data[i].reshape(image_height, image_width), cmap='gray')
    plt.title(f"Denoised {i+1}")
    plt.axis('off')

plt.tight_layout()
plt.show()
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/fig_2.png)

**How do the shapes of the digits in the denoised images compare to the original noisy images? How do the edges of the image compare to the original noisy images?**

    The cleaned-up (denoised) digit images look smoother and simpler than
    the original noisy ones. Most of the random dots and noise have been 
    removed, so the basic shape of each digit is easier to see. 

    The edges in the denoised images appear more blurred and less sharp compared
    to the original noisy images. Although much of the edge noise has been removed, 
    using only a few key components smooths out sharp transitions. As a result, 
    the boundaries of the digits become softer and less clearly defined. 

## Part C
```
# Perform Singular Value Decomposition (SVD)
U, s, Vh = np.linalg.svd(data)

# Number of singular values/vectors to keep for denoising
k_denoise = 15

# Extract the top-k components
U_k = U[:, :k_denoise]
S_k = np.diag(s[:k_denoise])
Vh_k = Vh[:k_denoise, :]

# Reconstruct the denoised data
denoised_data_k15 = U_k @ S_k @ Vh_k

# Clip pixel values to valid range and convert to uint8
denoised_data_k15 = np.clip(denoised_data_k15, 0, 255).astype(np.uint8)

# Plot original and denoised images side by side
plt.figure(figsize=(15, 6))
for i in range(min(5, num_images)):
    # Original image
    plt.subplot(2, 5, i + 1)
    plt.imshow(data[i].reshape(image_height, image_width), cmap='gray')
    plt.title(f"Original {i + 1}")
    plt.axis('off')

    # Denoised image
    plt.subplot(2, 5, i + 6)
    plt.imshow(denoised_data_k15[i].reshape(image_height, image_width), cmap='gray')
    plt.title(f"Denoised {i + 1}")
    plt.axis('off')

plt.tight_layout()
plt.show()
```
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/fig_3.png)
**How do the shapes of the digits in the denoised images compare to the original noisy images?** 

    When we look at the cleaned (denoised) images using k = 15, the shapes of the 
    digits become much easier to see and understand compared to the noisy ones. 
    Most of the random spots and pixel noise are removed, so the digits look 
    cleaner in areas except the first image. 

**How do the edges of the image compare to the original noisy images?** 

    With k = 15, the edges of the cleaned images look sharper and clearer than 
    those in the noisy ones. Although some tiny details might still be a bit 
    smoothed out, using more components (higher k) helps keep the edges more 
    intact.

**Compare denoised images using k=15 and k=2**

    The images cleaned using k = 15 look a lot clearer and show more detail than 
    those cleaned with k = 2. When we use a higher k, we keep more of the original 
    image’s important features, so the digits look more like how they did before 
    any noise was added. The shapes are sharper, and the outlines are easier to see. 
    On the other hand, using only k = 2 makes the digits look very basic and 
    sometimes hard to recognize because important details get lost. 


## Part D
```
# Perform Singular Value Decomposition (SVD)
U, s, Vh = np.linalg.svd(data)
k_denoise = 250 

# Select first k singular values/vectors
U_k = U[:, :k_denoise]
S_k = np.diag(s[:k_denoise])
Vh_k = Vh[:k_denoise, :]

# Reconstruct denoised data
denoised_data_k250 = U_k @ S_k @ Vh_k

# Clip values to valid pixel range
denoised_data_k250 = np.clip(denoised_data_k250, 0, 255).astype(np.uint8)

plt.figure(figsize=(15, 6))
for i in range(min(5, num_images)):
    # Plot original noisy image
    original_noisy_image = data[i].reshape(image_height, image_width)
    plt.subplot(2, 5, i + 1)
    plt.imshow(original_noisy_image, cmap='gray')
    plt.title(f"Original {i+1}")
    plt.axis('off')

    # Plot denoised image
    denoised_image_k250 = denoised_data_k250[i].reshape(image_height, image_width)
    plt.subplot(2, 5, i + 6)
    plt.imshow(denoised_image_k250, cmap='gray')
    plt.title(f"Denoised (k={k_denoise}) {i+1}")
    plt.axis('off')

plt.tight_layout()
plt.show()
```

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Fig_4.png)

**How do the shapes of the digits in the denoised images compare to the original noisy images?**

    The denoised images with k = 250 look pretty much identical to original image esp from 
    image 2 thorugh 5. Image 1 is distorted further with k = 250. 

**How do the edges of the image compare to the original noisy images?**

    Edges is also similar to the original image. 

**Compare your denoised images using k = 250 to your results under k = 2 and k = 15.**

    Images denoised with k=250 are identical to original but denoised with k=2 or k=15 
    looks cleaner except image 1. 
    At k=2, images look blurry and generalized.
    At k=15, images improve but still lack the fine details.
