# Create a frequency table for insurance type
insurance_distribution <- table(dataset$Insurance_Type)

# Sort the table in descending order for better readability
sorted_insurance_distribution <- sort(insurance_distribution, decreasing = TRUE)

# Display the distribution
print(sorted_insurance_distribution)

# Bar chart visualization
insurance_df <- as.data.frame(insurance_distribution)
ggplot(insurance_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.3, size = 3)
  labs(title = "Distribution of Patients by Insurance Type",
       x = "Insurance Type",
       y = "Number of Patients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
# a. Income vs. Insurance Type
  
  # Calculate the average income by insurance type
  avg_income_insurance <- dataset %>%
    group_by(Insurance_Type) %>%
    summarise(Average_Income = mean(Income, na.rm = TRUE))
  
  # Display the results
  print(avg_income_insurance)

# b. Income vs. Medical Condition
  
  # Calculate the average income by medical condition
  avg_income_condition <- dataset %>%
    group_by(Medical_Condition) %>%
    summarise(Average_Income = mean(Income, na.rm = TRUE)) %>%
    arrange(desc(Average_Income))
  
  # Display the results
  print(avg_income_condition)
  
  
# c. Correlation Between Income and Insurance Type
  
  # Perform ANOVA to see if income significantly differs by insurance type
  anova_result <- aov(Income ~ Insurance_Type, data = dataset)
  summary(anova_result)
  
# 3. How does the type of insurance influence the treatment and outcome of the patients?
  
# a. Insurance Type vs. Treatment
  
  # Create a contingency table for insurance type and treatment
  insurance_treatment_corr <- table(dataset$Insurance_Type, dataset$Treatment)
  
  # Display the table
  print(insurance_treatment_corr)
  
  # Bar chart visualization
  insurance_treatment_df <- as.data.frame(insurance_treatment_corr)
  ggplot(insurance_treatment_df, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Insurance Type and Treatment Distribution",
         x = "Insurance Type",
         y = "Number of Patients",
         fill = "Treatment") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# b. Insurance Type vs. Outcome
  
  # Create a contingency table for insurance type and outcome
  insurance_outcome_corr <- table(dataset$Insurance_Type, dataset$Outcome)
  
  # Display the table
  print(insurance_outcome_corr)
  
  # Bar chart visualization
  insurance_outcome_df <- as.data.frame(insurance_outcome_corr)
  ggplot(insurance_outcome_df, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = Freq), 
              position = position_dodge(width = 0.8),  
              vjust = -0.3,                           
              size = 4) +
    labs(title = "Insurance Type and Outcome Distribution",
         x = "Insurance Type",
         y = "Number of Patients",
         fill = "Outcome") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  

  
  
  