# Regression Analysis of House Prices

## Description
This project is to predict house prices in a Midwestern city as a function of various features of home and surrounding property.  The data includes 522 observations and 11 variables. By data transformation, model selection and variable selection, cross validation and model adequacy check, I developed a regression model to predict the house price in R.

## Data 
* Price: the property's sale price in dollars. This is the target variable to predict.
* Area: house size in square feet
* Lot: Lot size in square feet
* Bedroom: Number of bedrooms
* Bathroom: Number of bathrooms
* Year: Year property was originally constructed
* Garage: Number of cars that garage will hold
* Aircon: Presence or absence of air conditioning: 1 if yes; 0 otherwise
* Pool: Presence or absence of swimming pool: 1 if yes; 0 otherwise
* Highway: Presence or absence of adjacency to highway: 1 if yes; 0 otherwise
* Quality: Index for quality of construction: 1 = high quality; 2 = medium quality; 3 = low quality

## Process
### Data Manipulation
* Missing Value check
* Add dummy variable for categorical variable: Quality
* Visulize data by generating scatterplot for quantitative variables and boxplot for categorical variables
* Tansform data: do log transformation to Price
* Seperate data into train and test set

### Model Building
* use stepwise method to find preliminary model
* check correlation and VIF between quantitative predictors and their interactions to remove highly correlated items
* use exhaustive method on the preliminary model to find several alternative models
* do cross validation for each alternative models
* predict house prices of test data using each alternative models
* choose the final model by comparing adjust R-square, MSE of train data, PRESS, MSE of cross-validation, MSE of test data
* analyze the final model by checking the estimated coefficients, residual plots, qq plot and outlier and influential points. 

## Technologies
* R version: 3.5.2
* ggplot2 version: 3.1.0
* MASS version: 7.3-51.1
* boot version: 1.3-20
* leaps version: 3.0
* caret version: 6.0-81
* car version: 3.0-2



