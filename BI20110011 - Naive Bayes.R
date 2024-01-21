install.packages("dplyr")
library(dplyr)
install.packages("e1071")
library(e1071)


diabetes_data <- read.csv("diabetes_prediction_dataset.csv")


diabetes_data <- diabetes_data %>%
  mutate(
    smoking_history = ifelse(smoking_history == "No Info", NA, smoking_history),
    gender = ifelse(gender == "Other", NA, gender)
  )


diabetes_data_no_missing <- na.omit(diabetes_data)



diabetes_data_encoded <- diabetes_data_no_missing %>%
  mutate(
    smoking_history = case_when(
      smoking_history == "never" ~ 0,
      smoking_history == "current" ~ 1,
      smoking_history == "former" ~ 2,
      smoking_history == "ever" ~ 3,
      smoking_history == "not current" ~ 4,
      TRUE ~ as.numeric(smoking_history)  
    ),
    gender = recode(gender, "Male" = 0, "Female" = 1)
  )


summary(diabetes_data_encoded)  
head(diabetes_data_encoded)    
str(diabetes_data_encoded)

unique(diabetes_data_encoded$gender)
unique(diabetes_data_encoded$smoking_history)

set.seed(123)

index <- sample(1:nrow(diabetes_data_encoded), 0.7 * nrow(diabetes_data_encoded))
train_data <- diabetes_data_encoded[index, ]
test_data <- diabetes_data_encoded[-index, ]

nb_model <- naiveBayes(diabetes ~ ., data = train_data)
predictions <- predict(nb_model, newdata = test_data)

confusion_matrix <- table(predictions, test_data$diabetes)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

print(confusion_matrix)
print(accuracy)
