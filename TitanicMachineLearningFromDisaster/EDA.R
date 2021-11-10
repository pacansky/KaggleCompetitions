rm(list = ls())
#install.packages('lubridate')
#install.packages("dplyr")
#install.packages("tidyverse")
#intalled.package('broom')
library(tidyverse)
library(dplyr)
library(xlsx)
library(reclin)
library(magrittr)
library(ggplot2)
library(broom)


titanic_train <- read.csv("D:/SGH/Kaggle/train.csv")

print(titanic_train)
head(titanic_train, limit = 5)
str(titanic_train)

summary(titanic_train)

sapply(titanic_train, class)

titanic_train$Sex = as.factor(titanic_train$Sex)
titanic_train$Survived = as.factor(titanic_train$Survived)

print(titanic_train, limit = 3)

sum(is.na(titanic_train))


titanic_train_drop <- titanic_train[rowSums(is.na(titanic_train)) <= 0, ]
Age2 <- mean(titanic_train_drop$Age)

titanic_final <- titanic_train %>%
  mutate(age_without_na = ifelse(is.na(Age), Age2, Age))%>%
  select(!Age)

sum(is.na(titanic_final))

sapply(titanic_final, class)

titanic_final %>%
  ggplot(aes(x = age_without_na))+
  geom_histogram(breaks=seq(0, 80, by=7), 
           col="blue", 
           alpha = .2)+
  labs(title = "Histogram by Age", x = "Age", y = "Count")

titanic_survivor <- titanic_final[titanic_final$Survived == 1, ]
titanic_survivor_NOT <- titanic_final[titanic_final$Survived == 0, ]


titanic_survivor%>%
  ggplot(aes(x = Sex))+
  geom_bar()

titanic_survivor_NOT %>%
  ggplot(aes(x = Sex))+
  geom_bar()


123
