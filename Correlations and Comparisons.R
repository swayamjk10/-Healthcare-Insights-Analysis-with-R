# 3. Does smoking status correlate with a particular medical condition or outcome?
# Chi-square test between smoking status and medical condition
table_smoking_condition <- table(dataset$Smoking_Status, dataset$Medical_Condition)
chisq_test <- chisq.test(table_smoking_condition)
print(chisq_test)

# Bar plot for visualization
ggplot(dataset, aes(x = Medical_Condition, fill = Smoking_Status)) +
  geom_bar(position = "dodge") +
  labs(title = "Smoking Status vs. Medical Condition",
       x = "Medical Condition",
       y = "Number of Patients",
       fill = "Smoking Status") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

