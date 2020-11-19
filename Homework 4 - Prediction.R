install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
library("caret")
library("e1071")
library("rpart")
library("rpart.plot")
View(short)

short <- read.csv("credit_short.csv")
trainshort <- createDataPartition(y = short$DEFAULT, p = 0.8, list = FALSE)
short_train <- short[trainshort,]
short_test <- short[-trainshort,]

library(standardize)
train_stand <- short_train
train_stand[,1:16] <- apply(train_stand[,1:16], MARGIN = 2, FUN = scale)
test_stand <- short_test
test_stand[,1:16] <- apply(test_stand[,1:16], MARGIN = 2, FUN = scale)

grid <- expand.grid(k = c(10,30,50,70,80))

fitKNN <- train(data = train_stand, method = "knn", DEFAULT~.,
                 trControl = trainControl(search = "grid"), tuneGrid=grid)

plot(fitKNN, ylab = "Accuracy")

knn_predictions <- predict(fitKNN, test_stand)
confusionMatrix(knn_predictions, test_stand$DEFAULT, mode = "prec_recall", positive = "YES")

long <- read.csv("credit_long.csv")
summary(long)
View(long)

trainlong <- createDataPartition(y = long$DEFAULT, p = 0.8, list = FALSE)
long_train <- long[trainlong,]
long_test <- long[-trainlong,]

ltrain_stand <- long_train
ltrain_stand[,1:16] <- apply(ltrain_stand[,1:16], MARGIN = 2, FUN = scale)
ltest_stand <- long_test
ltest_stand[,1:16] <- apply(ltest_stand[,1:16], MARGIN = 2, FUN = scale)
fitDT <- train(data = ltrain_stand, method = "rpart", DEFAULT~.)

rpart.plot(fitDT$finalModel)

DT_predict <- predict(fitDT$finalModel, newdata = ltest_stand, type = "class")
confusionMatrix(DT_predict, ltest_stand$DEFAULT, mode = "prec_recall", positive = "NO")

DT_prob <- as.data.frame(predict(fitDT$finalModel, newdata = ltest_stand, type = "prob"))
DT_prob$pred_class <- ifelse(DT_prob$NO > 0.8, "NO.", "YES.")
DT_prob$pred_class <- as.factor(DT_prob$pred_class)
confusionMatrix(DT_prob$pred_class, ltest_stand$DEFAULT, mode = "prec_recall")
