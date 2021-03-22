---
title: "Cars Analysis"
author: "Alejandro Fernandez Benages"
date: "17/03/2021"
---

# Libraries
```{r}
#install.packages('tidyverse')
#install.packages(readr)
library("readr")
library(ggplot2)
```

# Load CSV
```{r}
IrisDataset <- read.csv('cars.csv')
```

# 1. EDA 
## 1.1 Knowing the data
```{r}
# To list the attributes of the data set.
attributes(cars)
# To print the min, max, mean, median, and quartiles of each attribute.
summary(cars)
# To display the structure of your data set.
str(cars) 
# To get th names of the attributes within your data set.
names(cars) 
```

## 1.2 Some Plots
### Variable: SPEED
```{r}
hist(cars$speed, 
     main="Speed distribution", xlab="Speed [mph]") 
```
A Histogram reveals that the most frequent speed of cars is between 10 to 20. 

### Variable: STOPPING DISTANCE
```{r}
hist(cars$dist, 
     main="Stopping distance distribution", xlab="Stopping Distance [feet]")
```
The most frequent distance of most cars is between 20 to 40. 
```{r}
ggplot(data = cars, aes(x = "", y = dist)) + 
  geom_boxplot()+ theme_classic()+ 
  xlab("")+ylab("Stopping Distance [feet]")
```
A histogram doesn't reveal much data. 
```{r}
ggplot(data = cars, aes(x = "", y = dist)) + 
  geom_violin()+ theme_classic()+ 
  xlab("")+ylab("Stopping Distance [feet]")
```
But a Violin plot does!. Now we have a much clear idea of the distribution's shape. 

## Normal Quantile Plot is a way to see if your data is normally distributed.
```{r}
qqnorm(cars$speed) 
qqnorm(cars$dist)
```
We can see that there is a positive correlation btw Speed and Distance

### Scatter plot of Speed vs Distance
```{r}
plot(cars, col='blue', pch=20, cex=2, 
     main="Speed and Stopping Distance for 50 Cars", 
     xlab="Speed [mph]", ylab="Stopping Distance [feet]")
```

## Missing Values?

If there are, the summarye() will count how many NA’s you have.
```{r}
summary(cars)
```
Also you can show your NA’s through logical data. (TRUE if it’s missing, FALSE if it’s not.)
```{r}
is.na(cars) 
```
Replace the missing values with the mean, which is common technique, but **something to use with care with as it can skew the data**.
```{r}
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)]<-mean(DatasetName$ColumnName,na.rm = TRUE)
```


```{r}




#' Creating Testing and Training Sets

set.seed(122)

# These two lines calculate the sizes of each set but do not create the sets:
trainSize<-round(nrow(cars)*0.7) # 70/30%
testSize<-nrow(cars)-trainSize

# If you’d like to see how many instances will be in each set..
trainSize
testSize

#' How do you create the train/test sets?. We also want these sets to be in a 
#' randomized order, which will create the most optimal model.
#'   To perform this, you need to run these three lines of code. 
training_indices<-sample(seq_len(nrow(cars)),size =trainSize)
trainSet<-cars[training_indices,]
testSet<-cars[-training_indices,]



#' Linear Regression
#' 
#' 
#' The next step is to predict the cars distances through the speed of the cars. 
#' To do this, we’ll be using the prediction function – predict() 
#' 
#' Inp Var -> speed of the cars
#' 
#' Out Var -> cars distances

 
# The basic line of code for the linear model function. 
cars_regression <-lm(formula = dist ~ speed, trainSet)
# To see key metrics of your model:
summary(cars_regression)



#' **Predictions**

cars_predictions <- predict(cars_regression, testSet)
cars_predictions

#' Confidence in your predictions: In order to have an idea about the accuracy of 
#' the predictions, you can ask for intervals around your prediction. To get a 
#' matrix with the prediction and a 95 percent confidence interval around the mean 
#' prediction, you set the argument interval to ‘confidence’ like this:
predict(cars_regression,trainSet, interval='confidence')
predict(cars_regression,testSet, interval='prediction')





```

