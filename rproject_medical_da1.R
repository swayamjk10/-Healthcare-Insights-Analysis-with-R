install.packages("dplyr")
install.packages("tidyr")
install.packages("data.table")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(readr)
library(corrplot)

dataset <- read.csv("medical_data.csv")

# View the structure of the dataset
str(dataset)

# Preview the first few rows
head(dataset)

# Summarize data
summary(dataset)

# Check for missing values
colSums(is.na(dataset))

# Group by Medical Condition and Gender, and count occurrences
grouped_data_mcg <- dataset %>%
  group_by(Medical_Condition, Gender) %>%
  summarise(Count = n())

# Create age groups using the cut() function
dataset$Age_Group <- cut(dataset$Age, 
                      breaks = c(18, 30, 40, 50, 60, 70, 80, 90, 100), 
                      labels = c("18-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100"),
                      right = FALSE)
head(dataset)

# Calculate the distribution of patients by age group
age_distribution <- table(dataset$Age_Group)
print(age_distribution)
age_distribution_df <- as.data.frame(age_distribution)

# bar chart for distribution of patients by age group
ggplot(age_distribution_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Freq), vjust = -0.3, size = 5) +  # Add text labels above bars
  labs(title = "Patient Distribution by Age Group",
       x = "Age Group",
       y = "Number of Patients") +
  theme_minimal()

head(dataset)


# Create a table of the distribution of medical conditions across age groups
condition_age_distribution <- table(dataset$Age_Group, dataset$Medical_Condition)

# Convert the table to a data frame for ggplot2
condition_age_df <- as.data.frame(condition_age_distribution)

# Create the bar chart
ggplot(condition_age_df, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "stack") + 
    labs(title = "Distribution of Medical Conditions Across Age Groups",
       x = "Age Group",
       y = "Number of Patients",
       fill = "Medical Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate the average income by gender
avg_income_gender <- dataset %>%
  group_by(Gender) %>%
  summarise(Average_Income = mean(Income, na.rm = TRUE)) %>%
  as.data.frame(avg_income_gender)


# Check for duplicates in the relevant columns
duplicates <- dataset[duplicated(dataset[c("Smoking_Status", "Gender", "Medical_Condition", "Treatment")]), ]
print(duplicates)

# Remove duplicates if necessary
data_cleaned <- dataset %>%
  distinct(Smoking_Status, Gender, Medical_Condition, Treatment, .keep_all = TRUE)

# Create a contingency table for smoking status and gender
smoking_gender <- table(data_cleaned$Smoking_Status, data_cleaned$Gender)
print(smoking_gender)

# Create a contingency table for smoking status and treatment type
smoking_treatment <- table(data_cleaned$Smoking_Status, data_cleaned$Treatment)
print(smoking_treatment)

# Calculate the regional distribution of patients
regional_distribution <- table(data_cleaned$Region)

# Convert the table to a data frame for ggplot2
regional_distribution_df <- as.data.frame(regional_distribution)

# bar chart for regional distribution of patients
ggplot(regional_distribution_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), vjust = -0.3, size = 3)
  labs(title = "Regional Distribution of Patients",
       x = "Region",
       y = "Number of Patients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Medical Conditions and Treatments
  
  # Create a table of the most common medical conditions
  medical_condition_count <- table(dataset$Medical_Condition)
  
  # Sort the table in descending order to show the most common conditions
  sorted_medical_conditions <- sort(medical_condition_count, decreasing = TRUE)
  
  # Display the sorted list of medical conditions
  print(sorted_medical_conditions)
  as.data.frame(sorted_medical_conditions)
  
  # Create a contingency table for medical condition and treatment type
  condition_treatment_corr <- table(dataset$Medical_Condition, dataset$Treatment)
  
  # Display the table
  print(condition_treatment_corr)
  
  # Convert the table to a data frame for ggplot2
  condition_treatment_df <- as.data.frame(condition_treatment_corr)
  
  # Create the bar chart
  ggplot(condition_treatment_df, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Medical Conditions and Treatment Types",
         x = "Medical Condition",
         y = "Number of Patients",
         fill = "Treatment Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  
  # Create a contingency table for outcome and medical condition
  outcome_condition_corr <- table(dataset$Medical_Condition, dataset$Outcome)
    
  
  # Display the table
  print(outcome_condition_corr)
  
  
  # Calculate the average length of stay by medical condition
  avg_length_of_stay <- dataset %>%
    group_by(Medical_Condition) %>%
    summarise(Average_Length_Of_Stay = mean(Length_of_Stay, na.rm = TRUE))
  
  # Display the result
  print(avg_length_of_stay)
  
  
  

  
  
# Visualize distributions
ggplot(age_distribution, aes(x = `age_distribution`)) + 
  geom_bar()


# Correlation matrix for numerical columns
correlation_matrix <- cor(dataset[sapply(dataset, is.numeric)])
print(correlation_matrix)

#summary of the dataset
summary(dataset)

table(dataset$Gender)
table(dataset$Medical_Condition)


ggplot(dataset, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count")


ggplot(dataset, aes(x = Length_of_Stay)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Length of Stay Density", x = "Length of Stay", y = "Density")


ggplot(dataset, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")


ggplot(dataset, aes(x = Outcome, fill = Insurance_Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Outcome by Insurance Type", x = "Outcome", y = "Count")



numeric_vars <- dataset[c("Age", "Income", "Length_of_Stay")]
cor_matrix <- cor(numeric_vars)
library(corrplot)
corrplot(cor_matrix, method = "number")



ggplot(dataset, aes(x = Age, y = Income)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Age vs Income", x = "Age", y = "Income")



ggplot(dataset, aes(x = Region, y = Length_of_Stay, fill = Region)) +
  geom_boxplot() +
  labs(title = "Length of Stay by Region", x = "Region", y = "Length of Stay")


#Average Income by Insurance Type
Average_Income_by_Insurance_Type <- dataset %>%
  group_by(Insurance_Type) %>%
  summarise(Average_Income = mean(Income))


ggplot(dataset, aes(x = Admission_Type, fill = Region)) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), color = "black") +
  labs(title = "Hospital Utilization by Admission Type", x = "Admission Type", y = "Count")


t.test(Income ~ Smoking_Status, data = dataset)


anova <- aov(Length_of_Stay ~ Outcome, data = dataset)
summary(anova)



