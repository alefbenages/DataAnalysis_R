# ---
# title: "Iris Analysis"
# author: "Alejandro Fernandez Benages"
# date: "17/03/2021"
# ---
 
 


# **libraries** #
  
#install.packages('tidyverse')
# install.packages(readr)
library("readr")
#library(tidyverse)
library(ggplot2)

#' **Load CSV**
#' 
#setwd('/home/ale/Dropbox/UBIQUM/3. DA with R/1.GetStarted/Iris') #set wd
IrisDataset <- read.csv('iris.csv')

#' **Some info about the Data**
#' 
attributes(IrisDataset)
summary(IrisDataset) 
str(IrisDataset)
names(IrisDataset)

#' specific info about the Petal.Lenght 
summary(IrisDataset$Petal.Length)
#' specific info about the Petal.Width 
summary(IrisDataset$Petal.Width)


#' **Missing Values?** 
#' 
#' Summary will count how many NA’s you have.
summary(IrisDataset)
#' Anyway *is.na(dataset)* show your NA’s through logical data. (TRUE if it’s missing, FALSE if it’s not.)
is.na(IrisDataset) 

#' Replace the missing values with the mean, which is common technique, but   
#' something to use with care with as it can skew the data.
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)]<-mean(DatasetName$ColumnName,na.rm = TRUE)

#' **Some Plots**
#'
#' PETAL LENGTH
hist(IrisDataset$Petal.Length, 
     main="Sepal Length distribution", xlab="Lenght [mm]", 
     col='green')
     
#' boxplot
ggplot(data = IrisDataset, aes(x = "", y = Petal.Length)) + 
  geom_boxplot() + theme_classic()+ 
  xlab("")+ylab("Petal Lenght [mm]")

#' violinplot
ggplot(data = IrisDataset, aes(x = "", y = Petal.Length)) + 
  geom_violin() + theme_classic()+ 
  xlab("")+ylab("Petal Lenght [mm]")+ 
  geom_boxplot(width=0.1)+ 
  stat_summary(fun.y=median, geom="point", size=2, color="red") 


#' PETAL WIDTH
hist(IrisDataset$Petal.Width, 
     main="Sepal Length distribution", xlab="Lenght [mm]", 
     col='violet')

#' boxplot
ggplot(data = IrisDataset, aes(x = "", y = Petal.Width)) + 
  geom_boxplot() + theme_classic()+ 
  xlab("")+ylab("Petal Lenght [mm]")

#' violinplot
ggplot(data = IrisDataset, aes(x = "", y = Petal.Width)) + 
  geom_violin() + theme_classic()+ 
  xlab("")+ylab("Petal Lenght [mm]")+ 
  geom_boxplot(width=0.1)+ 
  stat_summary(fun.y=median, geom="point", size=2, color="red") 

qqnorm(IrisDataset$Sepal.Length)

#' SPECIES distributon
IrisDataset$Species<- as.numeric(IrisDataset$Species) # changes the type to numeric
hist(IrisDataset$Species,
     main="Species distribution", xlab="Species",
     col='darkmagenta')  

#' Scatter plot Petal Length vs Petal Width
ggplot(data = IrisDataset, aes(x = Petal.Length  , y = Petal.Width))  + 
  geom_point(shape = 1) + 
  xlab("Petal Lenght [mm]")+ylab("Petal Width [mm]") +
  theme_bw()



#' **Train/Test Split**
#'
set.seed(123)
#' set dimension of Train/Test vectors
trainSize <- round(nrow(IrisDataset) * 0.2)
testSize <- nrow(IrisDataset) - trainSize
#' just to check, print the dimensions 
trainSize
testSize
#' assign the values to empty vectors  
training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSize)   
trainSet <- IrisDataset[training_indices, ]
testSet <- IrisDataset[-training_indices, ]


#' **Linear Model**
#' 
#' The analysis goal is to predict a petal's length using the petal’s width.  
#' Inp Var -> petal’s width
#' Out Var -> petal's length

LinearModel<- lm(Petal.Width ~ Petal.Length, trainSet )

summary(LinearModel)
     
prediction<-predict(LinearModel)
     
prediction
