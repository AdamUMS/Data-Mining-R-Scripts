###############
## Data Prep ##
###############

# Reading data
diabetes_data <- read.csv("diabetes_prediction_dataset.csv")

# INITIALIZING DPLYR - For N/A removal and encoding #
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

unique(diabetes_data$smoking_history)

# Convert "No Info" from smoking_history and "Other" in gender into NA - Using dplyr
diabetes_data <- diabetes_data %>%
  mutate(
    smoking_history = ifelse(smoking_history == "No Info", NA, smoking_history),
    gender = ifelse(gender == "Other", NA, gender)
  )

# Removing/deleting N/A data and create new data list 
diabetes_data_no_missing <- na.omit(diabetes_data)

# Checks what are the values after NA is removed
unique(diabetes_data_no_missing$smoking_history)
unique(diabetes_data_no_missing$gender)

# ENCODING CATEGORICAL DATA #

diabetes_data_encoded <- diabetes_data_no_missing %>%
  mutate(
    smoking_history = case_when(
      smoking_history == "never" ~ 0,
      smoking_history == "current" ~ 1,
      smoking_history == "former" ~ 2,
      smoking_history == "ever" ~ 3,
      smoking_history == "not current" ~ 4,
      TRUE ~ as.numeric(smoking_history)  # Keeps the original numeric values if present
    ),
    gender = recode(gender, "Male" = 0, "Female" = 1)
  )

#View Summary of Prepared data
summary(diabetes_data_encoded)  
head(diabetes_data_encoded)    
str(diabetes_data_encoded)

#Checking the encoded fields
unique(diabetes_data_encoded$gender)
unique(diabetes_data_encoded$smoking_history)

###################
## Decision Tree ##
###################

# Package for building the Decision Tree Model
if (!require(C50)) install.packages("C50")
library(C50)

# Package for splitting the training and testing data
if (!require(caret)) install.packages("caret")
library(caret)

# To view a more detailed decision tree, Partykit was used
if (!require(partykit)) install.packages("partykit")
library(partykit)

# Define predictor variables and target variable
predictors <- diabetes_data_encoded[, names(diabetes_data_encoded) != "diabetes"]
target <- diabetes_data_encoded$diabetes

# Splitting data into Training and testing - Training Data Set to 80%
set.seed(123) 
training_index <- createDataPartition(target, p = 0.8, list = FALSE)
training_data <- diabetes_data_encoded[training_index, ]
testing_data <- diabetes_data_encoded[-training_index, ]

# Ensure the target variable is a factor
training_data$diabetes <- as.factor(training_data$diabetes)
testing_data$diabetes <- as.factor(testing_data$diabetes)

# Train the model
decision_tree_model <- C5.0(x = training_data[, names(training_data) != "diabetes"], y = training_data$diabetes)

# Make predictions
predictions <- predict(decision_tree_model, testing_data)

# Evaluate the model
confusion_matrix <- confusionMatrix(predictions, testing_data$diabetes)
print(confusion_matrix)

# Plotting the decision tree - Visual representation of the tree
plot(decision_tree_model, main = 'Diabetes Prediction Decision Tree')  

# Using Partykit
party_model <- as.party(decision_tree_model)
plot(party_model, type = "simple", main = "Diabetes Prediction Decision Tree")




