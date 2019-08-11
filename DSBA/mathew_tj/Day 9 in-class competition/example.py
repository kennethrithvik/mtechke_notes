# -*- coding: utf-8 -*-
"""
The example code for in-class image processing competition
Prepared by Tian Jing
KE Unit7, Developing Intelligent Systems for Performing Business Analytics
Year 2018


Key reference
1. Conventional image interpolation provided by OPENCV, http://tanbakuchi.com/posts/comparison-of-openv-interpolation-algorithms/
2. Advanced edge-adaptive image interpolation, see various algorithms available in Section “Image interpolation and super-resolution”, available at https://xinli.faculty.wvu.edu/reproducible-research/reproducible-research-in-image-processing
3. Learning-based image interpolation (also called single image super-resolution), https://github.com/IvoryCandy/super-resolution
4. CVPR challenge, http://www.vision.ee.ethz.ch/ntire18/

"""

import cv2
import numpy as np
import math

def psnr(img1, img2):
    mse = np.mean( (img1 - img2) ** 2 )
    if mse == 0:
        return 100
    PIXEL_MAX = 255.0
    return 20 * math.log10(PIXEL_MAX / math.sqrt(mse))
	
    
# A list to save PSNR result for each image
psnr_result = [0] * 7

# Conduct test for all 7 test images
for i in range(7):
    # Load the ground truth image
    img_truth = cv2.imread('dataset\image_' + str(i) + '_truth.png')
    
    # Load the input low-resolution image
    img_input = cv2.imread('dataset\image_' + str(i) + '_input.png')
    
    # Use your own code here to perform 2*2 interpolation on the input image
    img_output = cv2.resize(img_input, None, fx=2, fy=2, interpolation = cv2.INTER_CUBIC)
    
    # Calculate the PSNR by comparing your output image and the ground truth image
    psnr_result[i] = psnr(img_truth, img_output)
    

# Submit the average PSNR for all 7 test images
print('Your need to submit average PSNR {:.4f} in IVLE forum'.format(np.mean(psnr_result)))