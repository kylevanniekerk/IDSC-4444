install.packages("GGally")
install.packages("labelVector")
install.packages("tidyverse")
library(GGally)
library(caret)
library(rpart.plot)
library(gridExtra)
library(labelVector)
library(tidyverse)
View(df)
df <- read.csv("bike_day.csv")
summary(df)
ggplot(data=df, aes(x = cnt_bike)) +
  geom_histogram(colour = "grey", fill = "black") +
  xlim(0,9000) +
  ylim(0,75) +
  ggtitle("Distribution of the number of bike rides") +
  labs(x = "bike days")

trainrows <- createDataPartition(y = df$cnt_bike, p = 0.7, list = FALSE) 
train_set <- df[trainrows,]  
test_set <- df[-trainrows,]

train_stand <- train_set
test_stand <- test_set

library(standardize)
train_stand <- apply(train_stand, MARGIN = 2, FUN = scale)
test_stand <- apply(test_stand, MARGIN = 2, FUN = scale)

knn <- train(cnt_bike~., train_stand, method = "knn")
knn

knnpred <- predict(knn, test_stand)

h_pred_knn <- ggplot(data = test_stand, aes(x = knnpred)) + 
  geom_histogram(colour = "lightblue", fill = "darkblue") +
  xlim (0,35000) + 
  ylim (0,200) + 
  ggtitle("KNN, Distribution of predicted bike count") +
  labs(x = "bike count")

bike_dist<- ggplot(data=test_stand, aes(x = cnt_bike)) + 
  geom_histogram(colour = "grey", fill = "black") +
  xlim (0,35000) + 
  ylim (0,200) + 
  ggtitle("Original bike count Distribution") +
  labs(x = "bike count")

grid.arrange(price_dist, h_pred_knn, nrow=1)

knn_error <- knnpred - test_stand

h_error_knn <- ggplot(data= test_stand, aes(x = knn_error)) + 
  geom_histogram(colour = "lightblue", fill = "blue") +
  xlim (-10000, 10000) + 
  ylim (0, 150) + 
  ggtitle("KNN, Distribution of Prediction Error") +
  labs(x = "Prediction Error")

p_error_knn<- ggplot(data = test_stand, aes(x=Price, y=knn_error)) +
  geom_point(size=2, color = "blue") +
  ylim (-5000, 8000) +
  xlim (0, 30000) +
  ggtitle("KNN, Prediction Error vs Actual bike count") +
  labs(x = "Actual Price", y = "KNN Prediction Error")

grid.arrange(h_error_knn, p_error_knn)


knnME <- mean(knn_error)
knnRMSE<- RMSE(pred = knnpred, obs = test_stand)
knnME
knnRMSE


rtree <- train(cnt_bike~., train_set, method = "rpart")
rtree

rpart.plot(rtree$finalModel, digits=-3)
treePred <- predict(rtree, test_set)

h_pred_tree<- ggplot(data= test_set, aes(x = treePred)) + 
  geom_histogram(colour = "red", fill = "darkred") +
  xlim (0,30000) + 
  ylim (0, 300) + 
  ggtitle("Tree, Distribution of Predictions") +
  labs(x = "Predictions")

grid.arrange(bike_dist,h_pred_tree, nrow=1)

tree_error <-treePred - test_set$cnt_bike
h_error_tree<- ggplot(data= test_set, aes(x = tree_error)) + 
  geom_histogram(colour = "darkred", fill = "red") +
  xlim (10000, 10000) + 
  ylim (0, 150) + 
  ggtitle("Tree, Distribution of Prediction Error") +
  labs(x = "Prediction Error")

p_error_tree<- ggplot(data = test_set, aes(x=cnt_bike, y=tree_error)) +
  geom_point(size=2, color = "red") +
  ylim (-5000, 5000) +
  xlim (0, 10000) +
  ggtitle("Tree, Prediction Error vs Actual bike count") +
  labs(x = "Actual bike count", y = "Tree Prediction Error")
grid.arrange(h_error_tree, p_error_tree)

ME_tree <- mean(tree_error)
treeRMSE <- RMSE(pred = treePred, obs = test_set$cnt_bike)
ME_tree
treeRMSE

cor(df)

train_set_lr <- train_set 
test_set_lr <- test_set 

lin_reg <- train(cnt_bike~., train_set_lr, method = "lm")
lin_reg
lin_pred <- predict(lin_reg, newdata = test_set_lr)

h_pred_lm <- ggplot(data= test_set_lr, aes(x = lin_pred)) + 
  geom_histogram(colour = "seagreen", fill = "darkgreen") +
  xlim (0,30000) + 
  ylim (0, 200) + 
  ggtitle("Linear Reg., Distribution of Predictions") +
  labs(x = "Predicted Price")

grid.arrange(bike_dist, h_pred_lm, nrow = 1)

lm_error <- lin_pred - test_set_lr$cnt_bike

h_error_lm <- ggplot(data= test_set_lr, aes(x = lm_error)) + 
  geom_histogram(colour = "darkgreen", fill = "seagreen") +
  xlim (-15000, 15000) + 
  ylim (0, 150) + 
  ggtitle("Linear Reg., Distribution of Prediction Error") +
  labs(x = "Prediction Error")

p_error_lm<- ggplot(data = test_set_lr, aes(x=cnt_bike, y=lm_error)) +
  geom_point(size=2, color = "seagreen") +
  ylim (-5000, 5000) +
  xlim (0, 10000) +
  ggtitle("Linear Reg., Prediction Error vs true bike count") +
  labs(x = "bike count", y = "Linear Reg. Prediction Error")
grid.arrange(h_error_lm, p_error_lm)

ME_lin <- mean(lm_error)
lin_RMSE <- RMSE(pred = lin_pred, obs = test_set_lr$cnt_bike)
ME_lin
lin_RMSE 
