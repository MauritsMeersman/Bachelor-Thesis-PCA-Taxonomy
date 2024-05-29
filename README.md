# Species Differentiation Analysis

## Background

This project involves the statistical analysis of two sympatric sister species, *S. altus* and *S. angusticeps*, which have undergone a recent speciation event. Our aim is to identify distinguishing characteristics using Principal Component Analysis (PCA) and hypothesis testing to differentiate between these species.

## Code Overview

The repository contains scripts for the following tasks:

- **Data Preparation**: Loading and preparing datasets for analysis.
- **PCA Analysis**: Performing PCA on different subsets of the data and visualizing the results.
- **Normality Testing**: Assessing the normality of variables using the Shapiro-Wilk test.
- **Hypothesis Testing**: Conducting ANOVA and Kruskal-Wallis tests based on the normality of the data.
- **Visualization**: Creating various plots to visualize the data and the PCA results.
- **Results Interpretation**: Adding PCA scores to the data frames and plotting factor loadings.

## Installation

To run the scripts in this repository, you need R installed on your system along with several packages. You can install the necessary packages by running the following command in your R console:

```R
install.packages(c("ggplot2", "Factoshiny", "FactoMineR", "factoextra", "ggbiplot", "ggpmisc", "readxl"))
