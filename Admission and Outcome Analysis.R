# 1. How does the type of admission (e.g., Urgent, Emergency) affect the length of stay in the hospital?

# Calculate average length of stay by admission type
admission_los <- dataset %>%
  group_by(Admission_Type) %>%
  summarize(Average_LOS = mean(Length_of_Stay, na.rm = TRUE))
print(admission_los)

# 2. What are the outcomes of patients with urgent vs. emergency admissions?

# Create a contingency table of outcomes by admission type
admission_outcome <- dataset %>%
  count(Admission_Type, Outcome)
print(admission_outcome)

# Stacked bar chart to visualize outcomes by admission type
ggplot(admission_outcome, aes(x = Admission_Type, y = n, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.8),  
            vjust = -0.3,                           
            size = 4)
  labs(title = "Outcomes by Admission Type",
       x = "Admission Type",
       y = "Number of Patients",
       fill = "Outcome") +
  theme_minimal()
  
# 3. What is the average length of stay for different medical conditions or treatment types?
  
  # Calculate average length of stay by medical condition and treatment type
  condition_treatment_los <- dataset %>%
    group_by(Medical_Condition, Treatment) %>%
    summarize(Average_LOS = mean(Length_of_Stay, na.rm = TRUE))
  
  # Heatmap for visualization
  ggplot(condition_treatment_los, aes(x = Medical_Condition, y = Treatment, fill = Average_LOS)) +
    geom_tile() +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Average Length of Stay by Medical Condition and Treatment Type",
         x = "Medical Condition",
         y = "Treatment Type",
         fill = "Average LOS (Days)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
