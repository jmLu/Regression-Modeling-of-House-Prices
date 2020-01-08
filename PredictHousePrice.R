# title: "Predict House Price using Regression Model"
# date: "May, 2014"

# load data
housing <- read.table("D:\\project\\housing.txt", header=FALSE)

# remove style column
housing <- housing[,-11]
housing <- housing[,-1]

## add column name
colnames(housing) <- c("Price", "Area", "Bedroom", "Bathroom", "Aircon",
                       "Garage", "Pool", "Year", "Quality", "Lot","Highway")
head(housing)

# check missing value
summary(housing)
mv <- apply(housing, 2, function(x){sum(is.na(x))})
mv[mv > 0]
# no missing value

#transform built year to age of the house
housing$Year <- 2014 - housing$Year
#rename Year to Age
colnames(housing)[colnames(housing) == 'Year'] <- 'Age'
head(housing)

# encode Quality which has three level: 1, 2,3
housing$Quality <- factor(housing$Quality)
contrasts(housing$Quality)<-contr.treatment(3, base=1)
housing$Quality

# visualize data
# create scatterplot matrix for quantitative variables
pairs(~Price+Area+Bedroom+Bathroom+Age+Lot+Garage, data=housing,
      main="ScatterPlot Matrix of Quantitative Variables")

# create boxplot for Quality
library(ggplot2)
ggplot(data=housing, aes(x=Quality, y=Price)) +
  geom_boxplot() +
  #geom_point(position='jitter', color='blue',alpha=0.5) +
  geom_rug(sides='l',color='black')

# create boxplot for with or without pool
ggplot(data=housing, aes(x=factor(Pool), y=Price)) +
  geom_boxplot() +
  geom_point(position='jitter', color='blue', alpha=0.5) +
  geom_rug(sides='l', color='black') + 
  labs(title="House Price by Pool", x = "Pool", y="Price")

# create boxplot for with or without airconditoner
ggplot(data=housing, aes(x=factor(Aircon), y=Price)) +
  geom_boxplot() +
  geom_point(position='jitter', color='blue', alpha=0.5) +
  geom_rug(sides='l', color='black') +
  labs(title="House Price by Aircon", x = "Aircon", y="Price")

# create boxplot for Highway whether adjunt to highway
ggplot(data=housing, aes(x=factor(Highway), y=Price)) +
  geom_boxplot() +
  geom_point(position='jitter', color='blue', alpha=0.5) +
  geom_rug(sides='l', color='black')

# fit preliminary model
fit.preliminary <- lm(Price ~ Area + Bedroom + Bathroom + Aircon + Garage
                      + Pool + Age + Quality + Lot + Highway, 
                      data = housing)
summary(fit.preliminary)

# check residual plot
plot(fitted(fit.preliminary), rstudent(fit.preliminary),
     main='residual plot', xlab='fitted value', ylab='studentized residual')
abline(0,0)

# check qq plot to check normal assumption
qqnorm(rstudent(fit.preliminary),pch=19)
abline(0,1)

#Residual plot shows that variance fan out, 
#From qq plot, I can find that there is violation of normal assumption
#So transform response variable firstly.

# use box-cox method to find the transformation for response variable
library(MASS)
BC <- boxcox(fit.preliminary)
BC$x[BC$y==max(BC$y)]
S <- max(BC$y) - 0.5*qchisq(0.95,1)
S
BC$x[BC$y>S]
## 95% CI for lamda: [-0.3434343, -0.1010101]
## so try lamda = 0 and -0.5
## when lamda = 0, use log transformation
## when lamda = -0.5, use 1/sqrt(y) transformation

# do log tranform to Price
fit.pre.log <- lm(I(log(Price)) ~ Area + Bedroom + Bathroom + Aircon + Garage
                  + Pool + Age + Quality + Lot + Highway, data = housing)
summary(fit.pre.log)

# check residual plot
plot(fitted(fit.pre.log), rstudent(fit.pre.log),
     main='residual plot', xlab='fitted value', ylab='studentized residual')
abline(0,0)

# check qq plot to check normal assumption
qqnorm(rstudent(fit.pre.log),pch=19)
abline(0,1)

# do 1/sqrt(y) transformation
fit.pre.sqrt <- lm(I(1/sqrt(Price)) ~ Area + Bedroom + Bathroom + Aircon + Garage
                   + Pool + Year + Quality + Lot + Highway, data=housing)
summary(fit.pre.sqrt)

# check residual plot
plot(fitted(fit.pre.sqrt), rstudent(fit.pre.sqrt),
     main='residual plot', xlab='fitted value', ylab='studentized residual')
abline(0,0)

# check qq plot to check normal assumption
qqnorm(rstudent(fit.pre.sqrt),pch=19)
abline(0,1)

#By comparing residual plot, qq plot and adjusted R-squared, log transformation is
#better than 1/sqrt.

# indicate outliers
res.stud <- rstudent(fit.pre.log)
res_abs <- abs(res.stud)
sort(res_abs)

# find leverage points
hii <- hatvalues(fit.pre.log)
sort(hii)

# find cook's distance
cd <- cooks.distance(fit.pre.log)
sort(cd)

#By checking Hii, Cook's Distiance and Standard Residual of all the points, 
#I decide to choose 3 possible influential points, which are 11, 103, 104. 
#And I will pay attention to these 3 points after find the final model.

# separate data into train and test set
library(caret)
set.seed(12340)
train <- createDataPartition(housing$Price, p = .8, 
list = FALSE, 
times = 1)
housing_train <- housing[train,]
housing_test <- housing[-train,]
price_test <- housing_test$Price

# stepwise model selection method using AIC as criterion
Null <- lm(I(log(Price)) ~ 1, data=housing_train)
fit.full.all <- lm(I(log(Price)) ~ Area*Bedroom*Bathroom*Garage*Age*Lot          
*Aircon*Pool*Quality*Highway, data= housing_train)
fit.step<- step(Null, scope=list(upper=fit.full.all), data=housing_train, 
direction="both")
#The model selected is following:
#Step:  AIC=-1557.04
#I(log(Price)) ~ Area + Quality + Age + Lot + Garage + Bathroom +
#Pool + Bedroom + Area:Quality + Area:Bathroom + Quality:Lot + 
#Age:Bathroom + Lot:Pool + Quality:Bathroom + Garage:Bedroom + 
#Age:Garage + Lot:Bedroom + Area:Lot + Area:Quality:Bathroom

# stepwise using BIC
n <- length(housing_train$Price)
fit.step.2<- step(Null, scope=list(upper=fit.full.all), data=housing_train, 
direction="both", k=log(n))
#The model selected is following:
#Step:  AIC=-1479.6
#I(log(Price)) ~ Area + Quality + Age + Lot + Garage + Bathroom + 
#Area:Quality + Area:Bathroom

# check dependency between quantitative predictors
x.cor <- cbind(Area, Age, Lot, Bathroom, Garage, Bedroom, Area*Bathroom,
               Area*Bedroom, Age*Bathroom, Area*Lot, Lot*Bathroom)
colnames(x.cor)[7:11] <- c('Area*Bathroom', 'Area*Bedroom', 'Age*Bathroom',
                           'Area*Lot', 'Lot*Bathroom')
cor(x.cor)
library(car)
vif.fit.step <- solve(cor(x.cor))
vif.fit.step
#From correlation and vif matrix, the interactions between quantitative
#predictors cause the serious multicollinearity problem.
#so I remove all interaction items and redo the above process.

# update preliminary model to the following
fit.full.update <- lm(I(log(Price)) ~ Area*Aircon*Pool*Quality*Highway
                                    + Bedroom*Aircon*Pool*Quality*Highway 
                                    + Bathroom*Aircon*Pool*Quality*Highway 
                                    + Garage*Aircon*Pool*Quality*Highway
                                    + Age*Aircon*Pool*Quality*Highway 
                                    + Lot*Aircon*Pool*Quality*Highway, 
                      data=housing_train)
fit.step.update<- step(Null, scope=list(upper=fit.full.update), data=housing_train, 
direction="both")
#The model selected is following:
#Step:  AIC=-1524.05
#I(log(Price)) ~ Area + Quality + Age + Lot + Garage + Bathroom + 
#Aircon + Area:Quality + Quality:Bathroom + Quality:Lot + 
#Lot:Aircon + Quality:Aircon + Quality:Lot:Aircon

#add dummy variable to represent Quality
#because when I use exhaustive method we need to construct X matrix
len_Q <- length(housing_train$Quality)
Q2 <- c(rep(0, len_Q))
Q3 <- c(rep(0, len_Q))
for (i in 1:len_Q)
{
  if (housing_train$Quality[i]==1) {
  Q2[i]=0
  Q3[i]=0
  }
  else if (housing_train$Quality[i]==2) {
  Q2[i]=1
  Q3[i]=0
  }
  else if (housing_train$Quality[i]==3) {
  Q2[i]=0
  Q3[i]=1
  }
}

## use exhaustive method to find several best models
area_train <- housing_train$Area
age_train <- housing_train$Age
lot_train <- housing_train$Lot
garage_train <- housing_train$Garage
bathroom_train <- housing_train$Bathroom
aircon_train <- housing_train$Aircon
X <- cbind(area_train, Q2, Q3, age_train, lot_train, garage_train, bathroom_train, 
           aircon_train, Q2*area_train, Q3*area_train, Q2*lot_train, Q3*lot_train,    
           Q2*bathroom_train, Q3*bathroom_train, Q2*aircon_train, Q3*aircon_train,        
           lot_train*aircon_train)
library(leaps)
all <- regsubsets(X, y=log(housing_train$Price),  method = "exhaustive", 
all.best = FALSE, nbest = 3, data = housing_train)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
p <- apply(Matrix,1, sum)
n <- length(housing_train$Price)
MSE <- SSRes/(n-p)
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)
colnames(output)[3:19] <- c("Area", "Q2", "Q3", "Age", "Lot", "Garage", 
                            "Bathroom", "Aircon", "Area*Q2", "Area*Q3",
                            "Lot*Q2", "Lot*Q3","Q2*Bathroom", "Q3*Bathroom",
                            "Q2*Aircon", "Q3*Aircon", "Lot*Aircon") 
output
#By comparing SSres, adjusted R-square, MSE and Cp, I choose the 
#three models as following.
#Model 1:
#I(log(Price)) ~ Area + Quality + Age + Lot + Garage + Area*Quality
#Model 2:
#I(log(Price)) ~ Area + Quality + Age + Lot + Bathroom + Garage + 
#                Area*Quality + Bathroom*Quality
#Model 3:
#I(log(Price)) ~ Area + Quality + Age + Lot + Bathroom + Garage + 
#                Aircon + Area*Quality + Bathroom*Quality + 
#                Lot*Aircon + Aircon*Quality

#Fit above three models separately, do cross-validation of each 
#model and predict house price of the test data, I try to find 
#the best model.
fit.final.1 <- lm(I(log(Price)) ~ Area + Quality + Age + Lot + Garage + 
                                  Area*Quality, data = housing_train)
summary(fit.final.1)
#calculate PRESS
press.final.1 <- sum((fit.final.1$residuals/(1-hatvalues(fit.final.1)))^2)
# k fold cross validatian
# use cv.glm() from boot package
library(boot)
set.seed(1234)
glm.final.1 <- glm(I(log(Price)) ~ Area + Quality + Age + Lot + Garage +                                
                                   Area*Quality, data = housing_train)
cv.error <- cv.glm(data=housing_train, glm.final.1, K=5) # K should be uppercase
cv.error$delta
# predict house price on test data
pred.final.1 <- predict(fit.final.1, housing_test[, -1])
pred.final.1
# MSE of test set
MSE.final.1 <- mean((pred.final.1-log(price_test))^2)
MSE.final.1

# model 2
fit.final.2 <- lm(I(log(Price)) ~ Area + Quality + Age + Lot + Bathroom + Garage +    
                                  Area*Quality + Bathroom*Quality, 
                    data = housing_train)
summary(fit.final.2)
#calculate PRESS
press.final.2 <- sum((fit.final.2$residuals/(1-hatvalues(fit.final.2)))^2)
press.final.2
# k fold cross validatian
# use cv.glm() from boot package
set.seed(1234)
glm.final.2 <- glm(I(log(Price)) ~ Area + Quality + Age + Lot + Bathroom + Garage + 
                                   Area*Quality + Bathroom*Quality, 
                   data = housing_train)
cv.error.2 <- cv.glm(data=housing_train, glm.final.2, K=5) 
cv.error.2$delta
# predict house price on test data
pred.final.2 <- predict(fit.final.2, housing_test[, -1])
pred.final.2
# MSE of test set
MSE.final.2 <- mean((pred.final.2-log(price_test))^2)
MSE.final.2

# model 3
fit.final.3 <- lm(I(log(Price)) ~ Area + Quality + Age + Lot + Bathroom + Garage + 
                                  Aircon + Area*Quality + Bathroom*Quality + 
                                  Lot*Aircon + Aircon*Quality, data = housing_train)
summary(fit.final.3)
#calculate PRESS
press.final.3 <- sum((fit.final.3$residuals/(1-hatvalues(fit.final.3)))^2)
press.final.3
# k fold cross validatian
# use cv.glm() from boot package
set.seed(1234)
glm.final.3 <- glm(I(log(Price)) ~ Area + Quality + Age + Lot + Bathroom + Garage + 
                                   Aircon + Area*Quality + Bathroom*Quality + 
                                   Lot*Aircon + Aircon*Quality, data = housing_train)
cv.error.3 <- cv.glm(data=housing_train, glm.final.3, K=5) 
cv.error.3$delta
# predict house price on test data
pred.final.3 <- predict(fit.final.3, housing_test[, -1])
pred.final.3
# MSE of test set
MSE.final.3 <- mean((pred.final.3-log(price_test))^2)
MSE.final.3

#By Comparing Adjust R-square, MSE, PRESS, MSE of cross-validataion and MSE of 
#test data, I choose model 2 as the final model.

#check model adequancy using full data
# residual plot
fit.final <- lm(I(log(Price)) ~ Area + Quality + Age + Lot + Garage + Bathroom +
                                Area*Quality + Bathroom*Quality, data = housing)
summary(fit.final)
# since the p-vlaue of Quality3:Bathroom is > 0.05, I remove Bathroom*Quality
fit.final <- lm(I(log(Price)) ~ Area + Quality + Age + Lot + Garage + Bathroom +
                  Area*Quality, data = housing)
summary(fit.final)
plot(fitted.values(fit.final), rstudent(fit.final),  xlab='fitted values',
ylab='studentized residuals', main='residual vs fitted value')
abline(-3,0, col='blue')
abline(3,0, col='blue')
abline(0,0, col='blue')

# qqplot
qqnorm(rstudent(fit.final),pch=19)
abline(0,1)

## check leverage point
hii <- hatvalues(fit.final)
#sort(hii)
p <- length(coef(fit.final))
n <- length(fitted(fit.final))
plot(hii, main='Index Plot of Hat values')
abline(h=c(2,3)*p/n, col='red', lty=2)


## check Cook's Distance
cutoff <- 4/(n-p)
cd <- cooks.distance(fit.final)
plot(cd, main="Index plot of Cook's Distance")
abline(h=cutoff, lty=2, col='red')
#sort(cd)

## influential points
library(car)
influencePlot(fit.final, id=list(method="identify"), main='influence plot',
              sub="circle size is proportional to Cook's Distance")
#From the result of studentized residual, cook's distance and hii, 
#I find several possible influential points: 11, 103, 104, 120 and 203. 
#I remove these points and fit the final model again to check the effect.

remove <- c(11, 103, 104, 120, 201)
data_exc <- housing[-remove,]
dim(data_exc)
fit_influence <- lm(I(log(Price)) ~ Area + Quality + Age + Lot + Bathroom + Garage +
                                    Area*Quality, data = data_exc)
summary(fit_influence)
press.fit_influence <- sum((fit_influence$residuals/(1-hatvalues(fit_influence)))^2)
#Although some estimated coefficient values have moderate percentage changes, 
#MSres and PRESS have only small changes. Since our objective is to predict house 
#price, these 5 observations will not cause serious effect. 

# redo cross-validation to full data
glm.fit <- glm(I(log(Price)) ~ Area + Quality + Age + Lot + Bathroom + Garage +
                               Area*Quality, data = housing)
cv.error.final <- cv.glm(data=housing, glm.fit, K=5) 
cv.error.final$delta
# result:MSE of cv
# 0.03009106
# MSE of full data:
# 0.02859481
#Since MSE of cross validataion and MSE of full data is very close, there is no 
#obvious overfitting problem.
