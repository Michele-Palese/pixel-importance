# Identifying discriminative pixels in hand-drawn spiral images to detect Parkinson’s disease.

# Overview
This project performs an image-based analysis using R to identify which pixels are most relevant for distinguishing spirals drawn by Parkinson’s disease patients from those drawn by healthy individuals.

- Dataset: ~50 subjects

- Image size: 64×64 grayscale

- Goal: Highlight pixels most informative for classification

Unlike deep learning approaches, I focus on interpretable models.

## Methods
Two supervised learning models were applied:

- LASSO-regularized logistic regression – for sparse, interpretable feature selection

- Random Forest – to evaluate non-linear pixel importance

Achieved strong classification performance, even with limited data

🛠️ Requirements
R ≥ 4.0

Packages: glmnet, randomForest, dplyr, ranger
