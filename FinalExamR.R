#this library contains many datasets, including Carseats
library(ISLR)
library(tree)
library(caret)

#open the dataset Carseats
attach(Carseats)
str(Carseats)
head(Carseats)
range(Sales)

#convert sales to "Yes" if sales are high (>= 9,000 Units)
# if sales are less that 9,000 unites then it is "No"
high <- ifelse(Sales >=9, "Yes", "No")

#add the field high to the Carseats dataset
Carseats <- data.frame(Carseats, high)

#convert the field high to factor
Carseats$high <- as.factor(Carseats$high)
head(Carseats)

#delete the numeric variable "Sales" from the dataset
#remember that we replaced it with the categorical variable "high"
Carseats <- Carseats[,-1]
names(Carseats)
str(Carseats)

#Split dataset
set.seed(2)
train <- sample(1:nrow(Carseats), nrow(Carseats)*0.70)
validation <- -train
training_data <- Carseats[train,]
validation_data <- Carseats[validation,]

#create a variable to hold the actual values of the validation datase
#this variable will be used latter for the confusion matrix
test_high <- high[validation]

#******************** CREATE THE CLASSIFICATION TREE ********************
#******************** NAME IT: tree_model *******************************
#*************** USE THE TRAINING DATASET *******************************
#*************** USE "high" AS DEPENDENT VARIABLE ***********************
tree_model = tree(high ~., training_data )

#plot the tree
plot(tree_model, type="uniform")

#show the text for the tree
#the pretty=0 option will show the actual values of the factors
text(tree_model, pretty=0)

#Check how the model is doing using the validation data
tree_pred <- predict(tree_model, validation_data, type="class")
mean(tree_pred != test_high)
test_high <- as.factor(test_high)

#create the confusion matrix with the prediction 
#********************* CREATE & DISPLAY THE CONFUSION MATRIX ******************
cm <- confusionMatrix(data=tree_pred, reference = test_high)
print(cm)

#******************************* PRUNE & PLOT THE TREE *******************************
#******************* NOTE: USE 7 AS THE BEST MODEL TO PRUNE **************
pruned<- prune.misclass(tree_model, best= 7)
plot(pruned, type = "uniform")
text(pruned, pretty= 0)

