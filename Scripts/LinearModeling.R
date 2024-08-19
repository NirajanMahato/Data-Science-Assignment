#•	House prices vs Download Speed
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)


cleaned_housing = read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/house_selling_clean.csv")
cleaned_broadband = read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Broadband Speed/cleaned_data_broadband.csv")

View(cleaned_housing)
View(cleaned_broadband)

# Merging the datasets on the Postcode 
merged_data = cleaned_housing %>%
  inner_join(cleaned_broadband, by = "Postcode") %>%
  filter(SaleYear == 2023) %>%
  select(Price, AvgDownloadSpeed)

# Converting Price to numeric 
merged_data = merged_data %>% mutate(Price = as.numeric(Price))

merged_data = merged_data %>% drop_na(Price, AvgDownloadSpeed)

View(merged_data)

linear_model = lm(Price ~ AvgDownloadSpeed, data = merged_data)

summary(linear_model)

# Visualizing the linear model with a line of best fit
ggplot(merged_data, aes(x = Price, y = AvgDownloadSpeed)) +
  geom_point(color = "blue") +  # Scatter plot of the data
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Line of best fit
  labs(title = "House Prices vs Download Speed",
       x = "House Price",
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal()




#•	House price vs Drug Rate (2023)

library(tidyverse)
library(ggplot2)


housing_data = read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/house_selling_clean.csv")
crime_data = read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Crime/Cleaned_merged_crimedata.csv")

# Trimming the housing data postcodes to match the crime data format 
housing_data = housing_data %>%
  filter(SaleYear == 2023) %>%
  mutate(Postcode = str_extract(Postcode, "^[A-Z]+\\d+\\s*\\d*")) 

# Filtering 
crime_data = crime_data %>%
  filter(`Crime type` == 'Drugs', Year == 2023) %>%
  group_by(Postcode) %>%
  summarise(Drug_Rate = n(), .groups = 'drop')

# Merging the datasets by Postcode
merged_data = housing_data %>%
  inner_join(crime_data, by = "Postcode")

View(merged_data)

linear_model <- lm(Price ~ Drug_Rate, data = merged_data)
summary(linear_model)

# Plotting 
ggplot(merged_data, aes(x = Drug_Rate, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "House Prices vs. Drug Rate (2023)",
       x = "Total Drug Offenses",
       y = "House Price") +
  theme_minimal()