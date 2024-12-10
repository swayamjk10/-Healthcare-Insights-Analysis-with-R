# 1. What is the distribution of patients across different hospitals (Hospital_ID)?
# Count the number of patients by Hospital_ID
hospital_distribution <- dataset %>%
  count(Hospital_ID) %>%
  filter(n > 1)
print(hospital_distribution)

# Count number of admissidataset# Count number of admissions by region
region_admissions <- dataset %>%
  count(Region)
print(region_admissions)

# Bar plot for visualization
ggplot(region_admissions, aes(x = Region, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Hospital Admissions by Region",
       x = "Region",
       y = "Number of Admissions") +
  theme_minimal()
