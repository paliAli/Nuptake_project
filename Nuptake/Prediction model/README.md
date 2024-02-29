This repository contains the code and implementation of a prediction model designed to estimate nitrogen uptake based on vegetation indices. The model employs a Leave One Out Cross Validation (LOOCV) approach.

# Creation of prediction model
I took the vegetation indices and nitrogen uptake data frames and created a model to predict nitrogen uptakes from vegetation indices values
There were only 10 nitrogen uptake values associated with each vegetation index -> separating the dataset into training and testing sets would be impractical
Instead, I used Leave One Out Cross Validation which is suitable for such a small dataset

## Leave One Out Cross Validation (LOOCV)
LOOCV involves training the model on all data points except one and then validating the model's performance on the excluded point. This process is repeated for each data point, allowing the model to be trained and validated on the entire dataset. 

A linear relationship between vegetation indices and nitrogen uptake was used in the model.

## Evaluation
I plotted the predicted nitrogen uptake against the actual measure values and calculated the R^2 and RMSE.

**R^2: Coefficient of determination indicating the proportion of the variance in the dependent variable that is predictable from the independent variable.
RMSE: Root Mean Square Error representing the square root of the average squared differences between predicted and actual values.**

The Caret package for R was used.
