rm(list = ls())

set.seed(45273)

require(dplyr)
require(ggplot2)
require(caret)

if(!file.exists("pml-training.csv"))
{
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv");
}

data <- read.csv("pml-training.csv")

data <- tbl_df(data)

dim(data)

# Data Cleaning

# remove time and user info

data <- select(data, -(1:7))

hasNA <- function(x) { sum(is.na(x)) > 0 }

columns.with.na <- lapply(data, hasNA)

names.of.columns.without.na <- names(data)[!as.logical(columns.with.na)]

data <- select(data, names.of.columns.without.na)

rm("names.of.columns.without.na", "columns.with.na", "hasNA")

factor.columns <- lapply(data, class)

names.of.non.factor.columns.with.class <- c("classe", names(data)[factor.columns != "factor"])

data <- select(data, names.of.non.factor.columns.with.class)

rm("factor.columns", "names.of.non.factor.columns.with.class")

# Data analysis

# g <- ggplot(data, aes(color = classe))
# 
# g <- g + geom_boxplot(aes(x = classe, y = roll_belt))
# g

# Model training

inTrain <- createDataPartition(y = data$classe, p = 0.6, list = FALSE)

training <- data[inTrain,]
testing <- data[-inTrain,]

rm("inTrain")

# trc <- trainControl(method = "cv")

require(parallel)
require(doParallel)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE)

model.fit <- train(classe ~ ., data = training, method = "rf", trControl = fitControl)

stopCluster(cluster)
registerDoSEQ()

rm("fitControl", "cluster")

pred.train <- predict(model.fit, training)

cm <- confusionMatrix(pred.train, training$classe)

# Testing

pred.test <- predict(model.fit, testing)

confusionMatrix(pred.test, testing$classe)

# Validation

validate <- read.csv("pml-testing.csv")

validate <- tbl_df(validate)

pred.valid <- predict(model.fit, validate)

