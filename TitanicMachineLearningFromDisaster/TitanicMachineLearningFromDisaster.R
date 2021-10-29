#####
# SETUP
#####

library(dplyr)
library(data.table)
library(ggplot2)

rm(list=ls())

train <- fread("train.csv")

#####
# EDA
#####


# Structure:
train
train[, vapply(.SD, class, character(1))]
train[, vapply(.SD, uniqueN, numeric(1))]   # Name, PassengerId, Ticket(?) are useless vars for prediction


# NA's:
train[, vapply(.SD, anyNA, logical(1))]     # Age is mostly missing; why?
naniar::vis_miss(train)


# Statistics:
summary(train)

train %>% 
  group_by(Pclass) %>% 
  summarize(SurvivalRate = mean(Survived))     # Rich were more willing to survive

train %>% 
  group_by(Survived) %>% 
  summarize(mean(Fare))

train %>% 
  group_by(SibSp) %>% 
  summarize(SurvivalRate = mean(Survived))     # those being with max 2 siblings+spouces were more willing to survive

train %>% 
  group_by(Sex) %>% 
  summarize(SurvivalRate = mean(Survived))     # there were no boats for men as women came out first

