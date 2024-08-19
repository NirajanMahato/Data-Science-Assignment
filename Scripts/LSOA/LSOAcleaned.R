library(tidyverse)

# Load the data
postcode_to_lsoa <- read.csv("D:/Sem4/Data Science/Obtained Data/Postcode to LSOA.csv")
view(postcode_to_lsoa)


# Import the cleaned house price dataset
cleaned_house_prices = read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/house_selling_clean.csv")
View(cleaned_house_prices)

# Clean and join the data using pipes
postcode_lsoa_clean = postcode_to_lsoa %>%
  # Select only the necessary columns for the mapping
  select(pcds, lsoa11cd) %>%
  # Rename columns for consistency and clarity
  rename(Postcode = pcds, `LSOA Code` = lsoa11cd) %>%
  # Perform a right join with the cleaned house prices data on the Postcode
  right_join(cleaned_house_prices, by = "Postcode") %>%
  # Select only the relevant columns for the final dataset
  select(`LSOA Code`, Postcode, `ShortPostcode`, `Town/City`, County) %>%
  # Remove rows with missing values
  drop_na() %>%
  # Remove duplicate rows
  distinct()

# View the cleaned and joined dataset
View(postcode_lsoa_clean)

# Save the cleaned dataset to a new CSV file
write.csv(postcode_lsoa_clean, "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/LSOA/Clean_Postcode_to_LSOA.csv", row.names = FALSE)

