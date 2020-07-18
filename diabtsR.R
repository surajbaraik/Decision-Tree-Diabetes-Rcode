
library(corrplot)
library(caret)
library(ModelMetrics)

diabts <- read.csv(file.choose())
colnames(diabts) =c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes")
View(diabts)
summary(diabts)
head(diabts)

str(diabts)

sapply(diabts, function(x) sum(is.na(x)))

pairs(diabts, panel = panel.smooth)

corrplot(cor(diabts[, -9]), type = "lower", method = "number")

##### Apply Logistic Regression model

# Preparing the DataSet
set.seed(123)
n <- nrow(diabts)
train <- sample(n, trunc(0.70*n))
diabts_training <- diabts[train, ]
diabts_testing <- diabts[-train, ]

# Training The Model
glm_fm1 <- glm(Diabetes ~., data = diabts_training, family = binomial)
summary(glm_fm1)

## Update to use only the significant variables
glm_fm2 <- update(glm_fm1, ~. - Triceps_Skin - Serum_Insulin - Age )
summary(glm_fm2)


### Now the results gives variables statiscally significance.

### Plot the new model

help("par")

par(mfrow = c(2,2))
plot(glm_fm2)

help("confusionMatrix")

#### we do not have any points considered outlier, therefore the Logistic Regression model fit perfectly.
####Apply the model to the testing sample

# Testing the Model
glm_probs <- predict(glm_fm2, newdata = diabts_testing, type = "response")
glm_pred <- ifelse(glm_probs > 0.5, 1, 0)
print("Confusion Matrix for logistic regression"); table(Predicted = glm_pred, Actual = diabts_testing$Diabetes)

confusionMatrix(glm_pred, diabts_testing$Diabetes ) # Confusion Matrix for logistic regression

acc_glm_fit <- confusionMatrix(glm_pred, diabts_testing$Diabetes )
acc_glm_fit
acc_glm_fit

n= sum(acc_glm_fit)
nc = nrow(acc_glm_fit)
diag = diag(acc_glm_fit)

accuracy = sum(diag)/n
accuracy
###The test rate error is 27.27%. In other words, the accuracy is 72.73%.


######################## Decision Trees#####################
# Preparing the DataSet:
pima <- read.csv(file.choose(), col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
pima$Diabetes <- as.factor(pima$Diabetes)

library(caret)
library(tree)
library(e1071)

set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

# Training The Model
treemod <- tree(Diabetes ~ ., data = train)

summary(treemod)

treemod # get a detailed text output.


### Now we plot of the tree, and interpret the results.

plot(treemod)
text(treemod, pretty = 0)

# Testing the Model
tree_pred <- predict(treemod, newdata = test, type = "class" )
confusionMatrix(tree_pred, test$Diabetes)

acc_treemod <- confusionMatrix(tree_pred, test$Diabetes)$overall['Accuracy']
### The test error rate is 30%. In other words, the accuracy is 70%.



################### Applying random forests model####################

# Training The Model
set.seed(123)
install.packages("randomForest")
library(randomForest)

rf_pima <- randomForest(Diabetes ~., data = diabts_training, mtry = 8, ntree=50, importance = TRUE)

# Testing the Model
rf_probs <- predict(rf_pima, newdata = diabts_testing)
rf_pred <- ifelse(rf_probs > 0.5, 1, 0)
confusionMatrix(rf_pred, diabts_testing$Diabetes )

acc_rf_pima <- confusionMatrix(rf_pred, diabts_testing$Diabetes)$overall['Accuracy']
## The test error rate is 22.94%. In other words, the accuracy is 77.06%.


####The important variable
importance(rf_pima)

## The "Plasma_Glucose" is by far the most important variable.
###A plot the Variable Importance

par(mfrow = c(1, 2))
varImpPlot(rf_pima, type = 2, main = "Variable Importance",col = 'black')
plot(rf_pima, main = "Error vs no. of trees grown")


############################# Applying Support Vector Machine - svm model33333333333

pima <- read.csv(file.choose())
colnames(pima) =c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes")
pima$Diabetes <- as.factor(pima$Diabetes)

library(e1071)

#Preparing the DataSet:
set.seed(1000)
intrain <- createDataPartition(y = pima$Diabetes, p = 0.7, list = FALSE)
train <- pima[intrain, ]
test <- pima[-intrain, ]

####Choosing Parameters: Now, we will use the tune() function to do a grid search over the supplied parameter ranges (C - cost, gamma), using the train set.
###It's important to understanding the influence of this two parameters, because the accuracy of an SVM model is largely dependent on the selection them.

tuned <- tune.svm(Diabetes ~., data = train, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned) # to show the results

## Training The Model:
svm_model  <- svm(Diabetes ~., data = train, kernel = "radial", gamma = 0.01, cost = 10) 
summary(svm_model)

## Testing the Model:
svm_pred <- predict(svm_model, newdata = test)
confusionMatrix(svm_pred, test$Diabetes)
acc_svm_model <- confusionMatrix(svm_pred, test$Diabetes)$overall['Accuracy']
### The test error rate is 23.04%. In other words, the accuracy is 76.96%.


######################### Comparison of Model Accuracy #####$

## Comparing the 04 models Logistic Regression, Decision Tree, Random Forest and Support Vector Machine

accuracy <- data.frame(Model=c("Logistic Regression","Decision Tree","Random Forest", "Support Vector Machine (SVM)"), Accuracy=c(acc_glm_fit, acc_treemod, acc_rf_pima, acc_svm_model ))
ggplot(accuracy,aes(x=Model,y=Accuracy)) + geom_bar(stat='identity') + theme_bw() + ggtitle('Comparison of Model Accuracy')


###The Decision Tree model has the lowest accuracy. 
###However the difference of accuracy between these 04 Classifiers are not significative.
