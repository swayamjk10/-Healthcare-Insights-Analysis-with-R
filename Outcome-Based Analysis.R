# 1. What are the most common outcomes (e.g., stable, improved, worsened) for different medical conditions?
# Count outcomes by medical condition
outcome_condition <- dataset %>%
  count(Medical_Condition, Outcome)
print(outcome_condition)

# Bar plot for visualization
ggplot(outcome_condition, aes(x = Medical_Condition, y = n, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Outcomes by Medical Condition",
       x = "Medical Condition",
       y = "Number of Patients",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a data frame for the outcome distribution by region
region_outcome_df <- dataset %>%
  count(Region, Outcome)
print(region_outcome_df)

# Plot outcomes by region with text labels
ggplot(region_outcome_df, aes(x = Region, y = n, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.8),  
            vjust = -0.3,                           
            size = 4) +
  labs(title = "Outcomes by Region",
       x = "Region",
       y = "Number of Patients",
       fill = "Outcome") +
  theme_minimal()


# 3. How does the outcome differ by treatment type (e.g., Dialysis, Chemotherapy, etc.)?

# Create contingency table for treatment type and outcomes
treatment_outcome <- dataset %>%
  count(Treatment, Outcome)
print(treatment_outcome)


# Bar plot to visualize outcomes by treatment type
ggplot(dataset, aes(x = Treatment, fill = Outcome)) +
  geom_bar(position = "dodge") +
  labs(title = "Outcomes by Treatment Type",
       x = "Treatment Type",
       y = "Number of Patients",
       fill = "Outcome") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


