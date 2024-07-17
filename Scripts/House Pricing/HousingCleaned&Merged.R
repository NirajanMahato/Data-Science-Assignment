library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

colm_names = c('Category', 'Price', 'Date', 'PostCode', 'null1','null2','null3','POAN','SOAN','Street','Locality','Town','District','County','null4','null5')

housing20 = read.csv("D:/Sem4/Data Science/Obtained Data/Housing/pp-2020.csv", col.names = colm_names )
housing21 = read.csv("D:/Sem4/Data Science/Obtained Data/Housing/pp-2021.csv", col.names = colm_names)
housing22 = read.csv("D:/Sem4/Data Science/Obtained Data/Housing/pp-2022.csv", col.names = colm_names)
housing23 = read.csv("D:/Sem4/Data Science/Obtained Data/Housing/pp-2023.csv", col.names = colm_names)

#View(housing20)
#View(housing21)
#View(housing22)
#View(housing23)


# Merge the datasets
merged_housing = bind_rows(housing20, housing21, housing22, housing23)

# View the merged dataset
View(merged_housing)

# Optionally, you can write the merged dataset to a new CSV file
#write.csv(merged_housing, "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/mergehouse.csv", row.names = FALSE)

# Remove the specified columns
columns_to_remove = c('null1', 'null2', 'null3', 'null4', 'null5', 'Category','SOAN')
final_data = merged_housing %>% select(-one_of(columns_to_remove))


# View the updated dataset
#View(final_data)

write.csv(final_data, "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/mergehouse.csv", row.names = FALSE)

# Keep data that contains county Bristol and Cornwall using grepl
filtered_data = final_data %>%
  filter(grepl("Bristol|Cornwall", County, ignore.case = TRUE))


View(filtered_data)

write.csv(filtered_data, "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/filter.csv", row.names = FALSE)

# Remove rows with too many missing values
# Set threshold for maximum allowable NA values in a row (e.g., 50% missing)
threshold = ncol(filtered_data) / 2
filtered_data = filtered_data[rowSums(is.na(filtered_data)) <= threshold, ]

# Remove columns with too many missing values
# Set threshold for maximum allowable NA values in a column (e.g., 50% missing)
threshold_col = nrow(filtered_data) / 2
filtered_data = filtered_data %>% select_if(~sum(is.na(.)) <= threshold_col)

# Impute remaining missing values
# Example: Replace missing values in numeric columns with median
filtered_data = filtered_data %>% mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .))


# Remove duplicates
filtered_data = filtered_data %>% distinct()


# Remove the time component from the Date column
filtered_data = filtered_data %>% mutate(Date = as.Date(Date))

str(filtered_data)


average_price <- mean(filtered_data$Price, na.rm = TRUE)


# Define the path where you want to save the combined file
output_file_path = 'D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/finalHousing_file.csv'


# Save the final dataframe to a new CSV file
write.csv(filtered_data, output_file_path, row.names = FALSE)

View(filtered_data)




cat("Lowest Prices")
print(filtered_data %>%
        group_by(Town) %>%
        summarise(Avg_Price=mean(Price, na.rm=TRUE))%>%
        arrange(Avg_Price)%>%
        head(10)
)

View(filtered_data)

# Read the final filtered dataset
filtered_data <- read.csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/finalHousing_file.csv")

# Create a box plot of housing prices by town
ggplot(filtered_data, aes(x = Town, y = Price, fill = Town)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +  # Adds commas for better readability
  labs(title = "Housing Prices by Town: Bristol vs Cornwall",
       x = "Town",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

