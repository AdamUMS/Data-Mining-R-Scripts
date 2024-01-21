# Reading data
diabetes_data <- read.csv("diabetes_prediction_dataset.csv")

# INITIALIZING DPLYR - For N/A removal and encoding #
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Convert "No Info" from smoking_history and "Other" in gender into NA - Using dplyr
diabetes_data <- diabetes_data %>%
  mutate(
    smoking_history = ifelse(smoking_history == "No Info", NA, smoking_history),
    gender = ifelse(gender == "Other", NA, gender)
  )

# Removing/deleting N/A data and create new data list 
diabetes_data_no_missing <- na.omit(diabetes_data)

# ENCODING CATEGORICAL DATA #

diabetes_data_encoded <- diabetes_data_no_missing %>%
  mutate(
    smoking_history = case_when(
      smoking_history == "never" ~ 0,
      smoking_history == "current" ~ 1,
      smoking_history == "former" ~ 2,
      smoking_history == "ever" ~ 3,
      smoking_history == "not current" ~ 4,
      TRUE ~ as.numeric(smoking_history)  # keeps the original numeric values if present
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

#Setting the seed for reproducibility
set.seed(123)

#To perform the clustering:
data<-diabetes_data_encoded[,c("age","hypertension","bmi","HbA1c_level","blood_glucose_level","diabetes")]

str(data)

k_3<-kmeans(data, 3)

clus<-cbind(data, clus3=k_3$cluster)

#Assuming clus is the data and k_3 is the result of k-means clustering
plot(clus$HbA1c_level, clus$blood_glucose_level, col=k_3$cluster, pch=k_3$cluster,
     main="HbA1c_level vs blood_glucose_level", xlim=c(0,10), ylim=c(0,350),
     xlab="HbA1c_level", ylab="blood_glucose_level")
