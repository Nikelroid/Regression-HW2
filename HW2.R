
#install.packages('caret')
#install.packages('pROC')
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("leaps")
#install.packages("MASS")
#install.packages("tree")
library("tree")
library(tidyverse)
library(leaps)
library(MASS)
library(ggplot2)
library(pROC)
library(caret)
diabets = read.csv("D:/Terme8/Regression/HW2/Data/d2.csv")


diabets <- diabets[!duplicated(diabets), ]
diabets <- na.omit(diabets)
diabets$output <- as.factor(diabets$Diabetes_binary)


model <- glm(Diabetes_binary ~ BMI, data = diabets)
summary(model)
Diabetstic = as.factor(diabets$Diabetes_binary)
# Density areas without lines
ggplot(diabets, aes(x = BMI, fill = Diabetstic , colours = Diabetstic)) +
  geom_density(alpha = 0.4, color = NA) 


model <- glm(Diabetes_binary ~ HighBP, data = diabets)
summary(model)
Diabetstic = as.factor(diabets$Diabetes_binary)
# Density areas without lines
ggplot(diabets, aes(x = HighBP, fill = Diabetstic , colours = Diabetstic)) +
  geom_histogram(alpha = 0.4, position = "identity",bins = 3) 


model <- glm(Diabetes_binary ~ HighChol, data = diabets)
summary(model)
Diabetstic = as.factor(diabets$Diabetes_binary)
# Density areas without lines
ggplot(diabets, aes(x = HighChol, fill = Diabetstic , colours = Diabetstic)) +
  geom_histogram(alpha = 0.4, position = "identity",bins = 3) 


model <- glm(Diabetes_binary ~ GenHlth, data = diabets)
summary(model)
Diabetstic = as.factor(diabets$Diabetes_binary)
# Density areas without lines
ggplot(diabets, aes(x = GenHlth, fill = Diabetstic , colours = Diabetstic)) +
  geom_histogram(alpha = 1, position = "dodge",bins = 9) 


model <- glm(Diabetes_binary ~ Age, data = diabets)
summary(model)
Diabetstic = as.factor(diabets$Diabetes_binary)
# Density areas without lines
ggplot(diabets, aes(x = Age, fill = Diabetstic , colours = Diabetstic)) +
  geom_boxplot(alpha = 1) 


model <- glm(Diabetes_binary ~ Education, data = diabets)
summary(model)
Diabetstic = as.factor(diabets$Diabetes_binary)
# Density areas without lines
ggplot(diabets, aes(x = Education, fill = Diabetstic , colours = Diabetstic)) +
  geom_histogram(alpha = 0.4, position = "identity",bins = 8) 


model <- glm(Diabetes_binary ~ Income, data = diabets)
summary(model)
Diabetstic = as.factor(diabets$Diabetes_binary)
# Density areas without lines
ggplot(diabets, aes(x = Income, fill = Diabetstic , colours = Diabetstic)) +
  geom_histogram(alpha = 1, position = "dodge",bins = 6) 


model <- glm(Diabetes_binary ~ Income+Education+Age+GenHlth+HighChol+HighBP+BMI, data = diabets)
summary(model)
Diabetstic = as.factor(diabets$Diabetes_binary)


md  <- glm(Diabetes_binary ~., data = diabets)
summary(md)

full.model <- glm(Diabetes_binary ~.-output, data = diabets)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)

train.control <- trainControl(method = "cv", number = 10)
step.model <- train(Diabetes_binary ~.-output, data = diabets,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 5)
mod <- glm(Diabetes_binary ~ HighBP + HighChol + BMI + GenHlth + Age, 
           data = diabets)
summary(mod)


full.model <- glm(Diabetes_binary ~.-output, data = diabets)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)

set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
step.model <- train(Diabetes_binary ~.-output, data = diabets,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 5)
mod <- glm(Diabetes_binary ~ HighBP + HighChol + BMI + GenHlth + Age, 
           data = diabets)
summary(mod)


diabets$Diabetes_binary <- as.factor(diabets$Diabetes_binary)
preProcess <- c("center","scale")
i <- createDataPartition(y = diabets$Diabetes_binary, times = 1, p = 0.7, list = FALSE)
training_set <- diabets[i,]
test_set <- diabets[-i,]
trControl <- trainControl(method = "cv",number = 5)

model <- train(Diabetes_binary ~ Age+GenHlth+HighChol+HighBP+BMI, method='qda', data = training_set, metric='Accuracy',preProcess = preProcess, trControl=trControl)
summary(model)
test_set$pred <- predict(model, test_set)
test_set$factor_pred <- as.factor(test_set$pred)
test_set$factor_truth <- as.factor(test_set$Diabetes_binary)
precision <- posPredValue(test_set$factor_truth, test_set$factor_pred)
recall <- sensitivity(test_set$factor_truth, test_set$factor_pred)
cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table
test_set$pred <- predict(model, test_set,probability=TRUE)
#________________________________________________
print(paste("Precision: ",precision))
print(paste("Accuracy: ",accuracy))
print(paste("Recall: ",recall))
acc_qda <- accuracy
#________________________________________________
Reference <- factor(c(0, 1, 0, 1))
Prediction   <- factor(c(0, 0, 1, 1))
Y  <- array(cmat)
df <- data.frame(Prediction, Reference, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") 

model <- train(Diabetes_binary ~ Age+GenHlth+HighChol+HighBP+BMI, method='rf', data = training_set, metric='Accuracy',preProcess = preProcess, trControl=trControl)
summary(model)
test_set$pred <- predict(model, test_set)
test_set$factor_pred <- as.factor(test_set$pred)
test_set$factor_truth <- as.factor(test_set$Diabetes_binary)
precision <- posPredValue(test_set$factor_truth, test_set$factor_pred)
recall <- sensitivity(test_set$factor_truth, test_set$factor_pred)
cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table
test_set$pred <- predict(model, test_set,probability=TRUE)
#________________________________________________
print(paste("Precision: ",precision))
print(paste("Accuracy: ",accuracy))
print(paste("Recall: ",recall))
acc_rf <- accuracy
#________________________________________________
Reference <- factor(c(0, 1, 0, 1))
Prediction   <- factor(c(0, 0, 1, 1))
Y  <- array(cmat)
df <- data.frame(Prediction, Reference, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") 


model <- train(Diabetes_binary ~ Age+GenHlth+HighChol+HighBP+BMI, method='lda', data = training_set, metric='Accuracy',preProcess = preProcess,trainControl = trainControl(method = "cv"))
summary(model)
test_set$pred <- predict(model, test_set)
test_set$factor_pred <- as.factor(test_set$pred)
test_set$factor_truth <- as.factor(test_set$Diabetes_binary)
precision <- posPredValue(test_set$factor_truth, test_set$factor_pred)
recall <- sensitivity(test_set$factor_truth, test_set$factor_pred)
cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table
test_set$pred <- predict(model, test_set,probability=TRUE)
#________________________________________________
print(paste("Precision: ",precision))
print(paste("Accuracy: ",accuracy))
print(paste("Recall: ",recall))
acc_lda <- accuracy
#________________________________________________
Reference <- factor(c(0, 1, 0, 1))
Prediction   <- factor(c(0, 0, 1, 1))
Y  <- array(cmat)
df <- data.frame(Prediction, Reference, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") 


model <- train(Diabetes_binary ~ Age+GenHlth+HighChol+HighBP+BMI, method='nnet', data = training_set, metric='Accuracy',preProcess = preProcess, trControl=trControl)
summary(model)
test_set$pred <- predict(model, test_set)
test_set$factor_pred <- as.factor(test_set$pred)
test_set$factor_truth <- as.factor(test_set$Diabetes_binary)
precision <- posPredValue(test_set$factor_truth, test_set$factor_pred)
recall <- sensitivity(test_set$factor_truth, test_set$factor_pred)
cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table
test_set$pred <- predict(model, test_set,probability=TRUE)
#________________________________________________
print(paste("Precision: ",precision))
print(paste("Accuracy: ",accuracy))
print(paste("Recall: ",recall))
acc_nn <- accuracy
#________________________________________________
Reference <- factor(c(0, 1, 0, 1))
Prediction   <- factor(c(0, 0, 1, 1))
Y  <- array(cmat)
df <- data.frame(Prediction, Reference, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") 


model <- train(Diabetes_binary ~ Age+GenHlth+HighChol+HighBP+BMI, method='ctree', data = training_set)
summary(model)
test_set$pred <- predict(model, test_set)
test_set$factor_pred <- as.factor(test_set$pred)
test_set$factor_truth <- as.factor(test_set$Diabetes_binary)
precision <- posPredValue(test_set$factor_truth, test_set$factor_pred)
recall <- sensitivity(test_set$factor_truth, test_set$factor_pred)
cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table
test_set$pred <- predict(model, test_set,probability=TRUE)
#________________________________________________
print(paste("Precision: ",precision))
print(paste("Accuracy: ",accuracy))
print(paste("Recall: ",recall))
acc_tree <- accuracy
#________________________________________________
Reference <- factor(c(0, 1, 0, 1))
Prediction   <- factor(c(0, 0, 1, 1))
Y  <- array(cmat)
df <- data.frame(Prediction, Reference, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") 


model <- train(Diabetes_binary ~ Age+GenHlth+HighChol+HighBP+BMI, method='svmLinear', data = training_set, metric='Accuracy',preProcess = preProcess, trControl=trControl)
summary(model)
test_set$pred <- predict(model, test_set)
test_set$factor_pred <- as.factor(test_set$pred)
test_set$factor_truth <- as.factor(test_set$Diabetes_binary)
precision <- posPredValue(test_set$factor_truth, test_set$factor_pred)
recall <- sensitivity(test_set$factor_truth, test_set$factor_pred)
cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table
test_set$pred <- predict(model, test_set,probability=TRUE)
#________________________________________________
print(paste("Precision: ",precision))
print(paste("Accuracy: ",accuracy))
print(paste("Recall: ",recall))
acc_svm <- accuracy
#________________________________________________
Reference <- factor(c(0, 1, 0, 1))
Prediction   <- factor(c(0, 0, 1, 1))
Y  <- array(cmat)
df <- data.frame(Prediction, Reference, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") 


model <- train(Diabetes_binary ~ Age+GenHlth+HighChol+HighBP+BMI, method='LogitBoost', data = training_set, metric='Accuracy',preProcess = preProcess, trControl=trControl)
summary(model)
test_set$pred <- predict(model, test_set)
test_set$factor_pred <- as.factor(test_set$pred)
test_set$factor_truth <- as.factor(test_set$Diabetes_binary)
precision <- posPredValue(test_set$factor_truth, test_set$factor_pred)
recall <- sensitivity(test_set$factor_truth, test_set$factor_pred)
cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table
test_set$pred <- predict(model, test_set,probability=TRUE)
#________________________________________________
print(paste("Precision: ",precision))
print(paste("Accuracy: ",accuracy))
print(paste("Recall: ",recall))
acc_lr <- accuracy
#________________________________________________
Reference <- factor(c(0, 1, 0, 1))
Prediction   <- factor(c(0, 0, 1, 1))
Y  <- array(cmat)
df <- data.frame(Prediction, Reference, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") 


model <- train(Diabetes_binary ~ Age+GenHlth+HighChol+HighBP+BMI, method='knn', data = training_set, metric='Accuracy',preProcess = preProcess, trControl=trControl)
summary(model)
test_set$pred <- predict(model, test_set)
test_set$factor_pred <- as.factor(test_set$pred)
test_set$factor_truth <- as.factor(test_set$Diabetes_binary)
precision <- posPredValue(test_set$factor_truth, test_set$factor_pred)
recall <- sensitivity(test_set$factor_truth, test_set$factor_pred)
cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table
test_set$pred <- predict(model, test_set,probability=TRUE)
#________________________________________________
print(paste("Precision: ",precision))
print(paste("Accuracy: ",accuracy))
print(paste("Recall: ",recall))
acc_knn <- accuracy
#________________________________________________
Reference <- factor(c(0, 1, 0, 1))
Prediction   <- factor(c(0, 0, 1, 1))
Y  <- array(cmat)
df <- data.frame(Prediction, Reference, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") 


print(paste('Accuracy in QDA:',acc_qda*100,'%'))
print(paste('Accuracy in LDA:',acc_lda*100,'%'))
print(paste('Accuracy in Randomforrest:',acc_rf*100,'%'))
print(paste('Accuracy in Neural Network:',acc_nn*100,'%'))
print(paste('Accuracy in CTree:',acc_tree*100,'%'))
print(paste('Accuracy in Linear SVM:',acc_svm*100,'%'))
print(paste('Accuracy in Logreg:',acc_lr*100,'%'))
print(paste('Accuracy in knn:',acc_knn*100,'%'))



tree_model <- tree(Diabetes_binary ~ .-output , training_set , mindev=7e-4, minsize=8)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)

test_set$pred <- predict(tree_model, test_set,type="class")
ErrorPrune<-mean(test_set$pred!=test_set$Diabetes_binary)
cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table

real <- factor(c(0, 0, 1, 1))
pred   <- factor(c(0, 1, 0, 1))
Y  <- array(cmat)
df <- data.frame(pred, real, Y)

ggplot(data =  df, mapping = aes(x = real, y = pred)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") +
  theme_bw()

print(paste('Error rate :',ErrorPrune))

cv.tree<-cv.tree(tree_model, FUN=prune.tree)
cv.tree
plot(cv.tree)

prune.tree<-prune.misclass(tree_model,best = 8)
plot(prune.tree);text(prune.tree)


test_set$pred <- predict(prune.tree, test_set,type="class")
ErrorPrune<-mean(test_set$pred!=test_set$Diabetes_binary)

cm <- confusionMatrix(test_set$pred, test_set$Diabetes_binary)
accuracy <- cm$overall[1]
cmat <- cm$table

real <- factor(c(0, 0, 1, 1))
pred   <- factor(c(0, 1, 0, 1))
Y  <- array(cmat)
df <- data.frame(pred, real, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = real, y = pred)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffcc", high = "yellow") +
  theme_bw()

print(paste('New Error rate :',ErrorPrune))


print(paste('New accuracy :',((1-ErrorPrune)*100),'%'))
