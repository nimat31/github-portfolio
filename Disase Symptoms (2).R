#Load The dataset
disease <- read.csv("Downloads/Disease_symptom_and_patient_profile_dataset.csv")


#EDA
head(disease)
tail(disease)
str(disease)
summary(disease)

#Plot for the Exploratory Data Analysis
hist(disease$Age)




#Change variables to factors and numerical values
disease$Disease <- as.factor(disease$Disease)
disease$Fever <- as.factor(disease$Fever)
disease$Cough <- as.factor(disease$Cough)
disease$Fatigue <- as.factor(disease$Fatigue)
disease$Difficulty.Breathing <- as.factor(disease$Difficulty.Breathing)
disease$Age <- as.numeric(disease$Age)
disease$Gender <- as.factor(disease$Gender)
disease$Blood.Pressure <- as.factor(disease$Blood.Pressure)
disease$Cholesterol.Level <- as.factor(disease$Cholesterol.Level)
disease$Outcome.Variable <- as.factor(disease$Outcome.Variable)
summary(disease)

#splitting the data 
index <- sample(nrow(disease),nrow(disease)*0.80)
disease_train = disease[index,]
disease_test = disease[-index,]

# Fit a logistic regression model to predict Outcome.Variable using all other variables
logistic_model <- glm(Outcome.Variable ~ ., family = binomial, data = disease)

# Display the summary of the model
summary(logistic_model)



# Create a contingency table for Fever and Outcome.Variable
contingency_table <- table(disease$Fever, disease$Outcome.Variable)

# Perform the chi-square test
chi_square_result <- chisq.test(contingency_table)

# Display the contingency table and test results
print(contingency_table)
print(chi_square_result)


# in-sample residual deviance
disease_train$deviance
