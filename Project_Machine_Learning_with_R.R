library(tidyverse)
library(janitor)
library(caret)

#load data
wine <- read_csv("WineQT.csv")

#review
View(wine)

#clean column name
wine <- wine %>%
  clean_names()

# check missing value
mean(complete.cases(wine)) == 1


#split data
set.seed(42)
n <- nrow(wine)
id <- sample(1:n, size = n*0.8)
train_data <- wine[id, ]
test_data <- wine[-id, ]

#train 
#lm
lm_model <- train(quality ~ .,
                  data = train_data,
                  method = "lm")
summary(lm_model)
lm_model$finalModel

#socre 
lm_pred <- predict(lm_model,
                   newdata = test_data)

#evaluate 
lm_error <- lm_pred - test_data$quality
lm_rmse <- sqrt(mean(lm_error**2))
lm_rmse

#knn
knn_model <- train(quality ~ ., 
                   data = train_data,
                   method = "knn")
knn_model

#socre 
knn_pred <- predict(knn_model,
                    newdata = test_data)

#evaluate 
knn_error <- knn_pred - test_data$quality
knn_rmse <- sqrt(mean(knn_error**2))
knn_rmse

#decision tree
tree_model <- train(quality ~ .,
                    data = train_data,
                    method = "rpart")
tree_model

#socre 
tree_pred <- predict(tree_model,
                     newdata = test_data)

#evaluate 
tree_error <- tree_pred - test_data$quality
tree_rmse <- sqrt(mean(tree_error**2))
tree_rmse

#random forest
rf_model <- train(quality ~ .,
                  data = train_data,
                  method = "rf")
rf_model

#socre 
rf_pred <- predict(rf_model,
                   newdata = test_data)

#evaluate 
rf_error <- rf_pred - test_data$quality
rf_rmse <- sqrt(mean(rf_error**2))
rf_rmse

#model comparison
modelList <- list(
  lm= lm_model,
  knn = knn_model,
  dicidiontree = tree_model,
  randomforest = rf_model)

result <- resamples(modelList)
summary(result)

#####
#CV
set.seed(42)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

knn_model_v2 <- train(quality ~ ., 
                      data = train_data,
                      method = "knn",
                      trControl = ctrl)

#socre 
knn_pred_v2 <- predict(knn_model_v2,
                       newdata = test_data)

#evaluate 
knn_error_v2 <- knn_pred_v2 - test_data$quality
knn_rmse_v2 <- sqrt(mean(knn_error_v2**2))

#decision tree
set.seed(42)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE)

tree_model_v2 <- train(quality ~ .,
                       data = train_data,
                       method = "rpart",
                       trControl = ctrl)

#socre 
tree_pred_v2 <- predict(tree_model_v2,
                        newdata = test_data)

#evaluate 
tree_error_v2 <- tree_pred_v2 - test_data$quality
tree_rmse_v2 <- sqrt(mean(tree_error_v2**2))

#random forest
set.seed(42)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE)

rf_model_v2 <- train(quality ~ .,
                     data = train_data,
                     method = "rf",
                     trControl = ctrl)

#socre 
rf_pred_v2 <- predict(rf_model_v2,
                      newdata = test_data)

#evaluate 
rf_error_v2 <- rf_pred_v2 - test_data$quality
rf_rmse_v2 <- sqrt(mean(rf_error_v2**2))

#model comparison
modelList <- list(
  knn = knn_model_v2,
  dicidiontree = tree_model_v2,
  randomforest = rf_model_v2)

result <- resamples(modelList)
summary(result)

#####
#k-flod repeatedcv
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5, 
  verboseIter = TRUE)

knn_model_v3 <- train(quality ~ ., 
                      data = train_data,
                      method = "knn",
                      trControl = ctrl)

#socre 
knn_pred_v3 <- predict(knn_model_v3,
                       newdata = test_data)

#evaluate 
knn_error_v3 <- knn_pred_v3 - test_data$quality
knn_rmse_v3 <- sqrt(mean(knn_error_v3**2))

#decision tree
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5, 
  verboseIter = TRUE)

tree_model_v3 <- train(quality ~ .,
                       data = train_data,
                       method = "rpart",
                       trControl = ctrl)

#socre 
tree_pred_v3 <- predict(tree_model_v3,
                        newdata = test_data)

#evaluate 
tree_error_v3 <- tree_pred_v3 - test_data$quality
tree_rmse_v3 <- sqrt(mean(tree_error_v3**2))

#random forest
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5, 
  verboseIter = TRUE)

rf_model_v3 <- train(quality ~ .,
                     data = train_data,
                     method = "rf",
                     trControl = ctrl)

#socre 
rf_pred_v3 <- predict(rf_model_v3,
                      newdata = test_data)

#evaluate 
rf_error_v3 <- rf_pred_v3 - test_data$quality
rf_rmse_v3 <- sqrt(mean(rf_error_v3**2))

#model comparison
modelList <- list(
  knn = knn_model_v3,
  dicidiontree = tree_model_v3,
  randomforest = rf_model_v3)

result <- resamples(modelList)
summary(result)

# test model comparison
test_modelList <- list(
  knn = knn_rmse_v3,
  dicidiontree = tree_rmse_v3,
  randomforest = rf_rmse_v3)

#####
#random search 
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5, 
  verboseIter = TRUE)

knn_model_v4 <- train(quality ~ ., 
                      data = train_data,
                      method = "knn",
                      trControl = ctrl,
                      tuneLength = 5)

#socre 
knn_pred_v4 <- predict(knn_model_v4,
                       newdata = test_data)

#evaluate 
knn_error_v4 <- knn_pred_v4 - test_data$quality
knn_rmse_v4 <- sqrt(mean(knn_error_v4**2))

#dicidion tree 
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5, 
  verboseIter = TRUE)

tree_model_v4 <- train(quality ~ ., 
                       data = train_data,
                       method = "rpart",
                       trControl = ctrl,
                       tuneLength = 5)

#socre 
tree_pred_v4 <- predict(tree_model_v4,
                        newdata = test_data)

#evaluate 
tree_error_v4 <- tree_pred_v4 - test_data$quality
tree_rmse_v4 <- sqrt(mean(tree_error_v4**2))

#random forest
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5, 
  verboseIter = TRUE)

rf_model_v4 <- train(quality ~ ., 
                     data = train_data,
                     method = "rf",
                     trControl = ctrl,
                     tuneLength = 5)

#socre 
rf_pred_v4 <- predict(rf_model_v4,
                      newdata = test_data)

#evaluate 
rf_error_v4 <- rf_pred_v4 - test_data$quality
rf_rmse_v4 <- sqrt(mean(rf_error_v4**2))

#model comparison 
modelList_v4 <- list(
  knn = knn_model_v4,
  dicidiontree = tree_model_v4,
  randomforest = rf_model_v4)

result <- resamples(modelList_v4)
summary(result)

test_modelList_v4 <- list(
  knn = knn_rmse_v4,
  dicidiontree = tree_rmse_v4,
  randomforest = rf_rmse_v4)

ggplot(test_data, aes(alcohol)) +
  geom_histogram()

############
#grid search 
#knn
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5, 
  verboseIter = TRUE)

myGrid <- expand.grid(k = 1:10)

knn_model_v5 <- train(quality ~ ., 
                      data = train_data,
                      method = "knn",
                      trControl = ctrl,
                      tuneGrid = myGrid)

#socre 
knn_pred_v5 <- predict(knn_model_v5,
                       newdata = test_data)

#evaluate 
knn_error_v5 <- knn_pred_v5 - test_data$quality
knn_rmse_v5 <- sqrt(mean(knn_error_v5**2))

#dicidiontree 
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5, 
  verboseIter = TRUE)

myGrid <- expand.grid(cp=seq(0, 
                             0.05, 
                             by = 0.005))

tree_model_v5 <- train(quality ~ ., 
                       data = train_data,
                       method = "rpart",
                       trControl = ctrl,
                       tuneGrid = myGrid)

#socre 
tree_pred_v5 <- predict(tree_model_v5,
                        newdata = test_data)

#evaluate 
tree_error_v5 <- tree_pred_v5 - test_data$quality
tree_rmse_v5 <- sqrt(mean(tree_error_v5**2))

#randomforest 
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5, 
  verboseIter = TRUE)

myGrid <- expand.grid(.mtry=c(1:10))

rf_model_v5 <- train(quality ~ ., 
                     data = train_data,
                     method = "rf",
                     trControl = ctrl,
                     tuneGrid = myGrid)

#socre 
rf_pred_v5 <- predict(rf_model_v5,
                      newdata = test_data)

#evaluate 
rf_error_v5 <- rf_pred_v5 - test_data$quality
rf_rmse_v5 <- sqrt(mean(rf_error_v5**2))

#model comparison
modelList_v5 <- list(
  knn = knn_model_v4,
  dicidiontree = tree_model_v5,
  randomforest = rf_model_v5)

result <- resamples(modelList_v5)
summary(result)

test_modelList_v5 <- list(
  knn = knn_rmse_v5,
  dicidiontree = tree_rmse_v5,
  randomforest = rf_rmse_v5)