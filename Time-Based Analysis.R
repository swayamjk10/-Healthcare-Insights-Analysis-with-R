# 1. What is the average length of stay in the hospital across the dataset?


# Calculate the average length of stay
avg_los <- mean(dataset$Length_of_Stay, na.rm = TRUE)
print(paste("Average Length of Stay: ", avg_los))

# 2. Is there any trend in length of stay by treatment, or medical condition?

# Box plot for length of stay by treatment type

ggplot(dataset, aes(x = Treatment, y = Length_of_Stay)) +
  geom_boxplot() +
  labs(title = "Length of Stay by Treatment Type",
       x = "Treatment Type",
       y = "Length of Stay (Days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for length of stay by medical condition
ggplot(dataset, aes(x = Medical_Condition, y = Length_of_Stay)) +
  geom_boxplot() +
  labs(title = "Length of Stay by Medical Condition",
       x = "Medical Condition",
       y = "Length of Stay (Days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
