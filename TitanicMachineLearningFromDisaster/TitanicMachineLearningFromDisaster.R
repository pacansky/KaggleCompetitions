#####
# SETUP
#####

library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)

rm(list = ls())

TrainSet <- fread("train.csv")


#####
# EDA
#####

# Structure:
TrainSet[, lapply(.SD, uniqueN)] # Name, PassengerId, Ticket, Cabin(?) are useless for prediction
TrainSet[, PassengerId:= NULL
         ][, Name:= NULL
           ][, Ticket := NULL]


# NA's:
TrainSet[, lapply(.SD, anyNA)] # why Age is missing?
naniar::vis_miss(TrainSet)


# Convert strings to factors
TrainSet[, lapply(.SD, class)]
ColumnsToFactor <- c("Survived", "Pclass", "Sex", "SibSp", "Parch", "Embarked")
TrainSet[, (ColumnsToFactor) := lapply(.SD, as.factor), .SDcols = ColumnsToFactor]


# Bar plots of categorical vars:
CreateBarPlot <- function(TrainSet, AxisX) {
  
  ggplot(TrainSet, aes_string(AxisX, fill = AxisX)) +
    geom_bar() + 
    theme_minimal() +
    scale_color_brewer(palette = "Accent") +
    labs(y = "Number of observations", title = paste("Bar plot of ", AxisX, " variable"))
}

CreateBarPlot(TrainSet, "Survived")
CreateBarPlot(TrainSet, "Pclass")
CreateBarPlot(TrainSet, "Sex")
CreateBarPlot(TrainSet, "SibSp")
CreateBarPlot(TrainSet, "Parch")
CreateBarPlot(TrainSet, "Embarked")


# Statistics and histograms of continuous vars:
CreateSummaryStatisticsTable <- function(VariableContinuous) {
  
  data.table(Min = min(VariableContinuous, na.rm = TRUE),
             Q25 = quantile(VariableContinuous, 0.25, na.rm = TRUE),
             Median = median(VariableContinuous, na.rm = TRUE),
             Q75 = quantile(VariableContinuous, 0.75, na.rm = TRUE),
             Max = max(VariableContinuous, na.rm = TRUE),
             Mean = mean(VariableContinuous, na.rm = TRUE),
             `Standard Deviation` = sd(VariableContinuous, na.rm = TRUE))
}

CreateHistogramPlot <- function(TrainSet, AxisX) {
  ggplot(TrainSet, aes_string(AxisX)) +
    geom_histogram(color = "red", fill = "deepskyblue3") +
    theme_minimal() +
    labs(y = "Number of observations", title = paste("Histogram of ", AxisX, " variable"))
}

CreateSummaryStatisticsTable(TrainSet$Age)
CreateHistogramPlot(TrainSet, "Age")

CreateSummaryStatisticsTable(TrainSet$Fare)
CreateHistogramPlot(TrainSet, "Fare")


# Correlations:


# Rich influence:

TrainSet[, mean(Fare), by = Pclass][] # Rich were more willing to survive
TrainSet[, mean(as.numeric(Survived)-1), by = Pclass]

summary(lm(Fare ~ I(factor(Pclass)), data = train))

TrainSet[, mean(as.numeric(Survived)-1), by = Embarked] # the highest survival rate came from Southampton

TrainSet[, mean(Fare), by = Embarked][] # the richest passengers were from Southampton


# Age:
TrainSet[, mean(Age, na.rm = TRUE), by = Survived] # a small different in age;

TrainSet[, sum(is.na(Age)), by = Survived] # however, there were more NA from those who did not survived


# Siblings & sex:
TrainSet[, mean(as.numeric(Survived)-1), by = SibSp] # those being with max 1-2 were more willing to survive; however see next

TrainSet[, mean(as.numeric(Survived)-1), by = Parch] # those with 2 parents/children more likely to survive

TrainSet[, mean(as.numeric(Survived)-1), by = Sex] # there were no boats for men as women came out first


# => to survive you'd better be a rich young woman with a spouse