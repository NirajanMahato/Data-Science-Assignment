# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Define the base path where all the folders are located
base_path <- "D:/Sem4/Data Science/Obtained Data/Crime Dataset"

# List all the folders within the base path
folders <- list.files(base_path, full.names = TRUE)

# Initialize empty data frames to store the combined data
avon_somerset_all <- data.frame()
devon_cornwall_all <- data.frame()

# Loop through each folder
for (folder in folders) {
  # Extract the month-year from the folder name (assumes folder name is the same as the file prefix)
  folder_name <- basename(folder)
  
  # Construct the file paths for Bristol and Cornwall datasets
  avon_somerset_file <- file.path(folder, paste0(folder_name, "-avon-and-somerset-street.csv"))
  devon_cornwall_file <- file.path(folder, paste0(folder_name, "-devon-and-cornwall-street.csv"))
  
  # Read the datasets
  avon_somerset_data <- read_csv(avon_somerset_file)
  devon_cornwall_data <- read_csv(devon_cornwall_file)
  
  # Combine them with the existing data
  avon_somerset_all <- bind_rows(avon_somerset_all, avon_somerset_data)
  devon_cornwall_all <- bind_rows(devon_cornwall_all, devon_cornwall_data)
}

# Convert 'Month' column to Date format
avon_somerset_all$Month <- as.Date(paste0(avon_somerset_all$Month, "-01"))
devon_cornwall_all$Month <- as.Date(paste0(devon_cornwall_all$Month, "-01"))

# Clean the data by removing rows with missing coordinates (optional)
avon_somerset_all <- avon_somerset_all %>% drop_na(Longitude, Latitude)
devon_cornwall_all <- devon_cornwall_all %>% drop_na(Longitude, Latitude)

# Now you have all the data from Bristol and Cornwall combined across all months



# Summarize the total number of crimes per month for each region
avon_somerset_summary <- avon_somerset_all %>%
  group_by(Month) %>%
  summarise(Total_Crimes = n())

devon_cornwall_summary <- devon_cornwall_all %>%
  group_by(Month) %>%
  summarise(Total_Crimes = n())

# Plot the crime trends using ggplot2

# Avon and Somerset (Bristol)
ggplot(avon_somerset_summary, aes(x = Month, y = Total_Crimes)) +
  geom_line(color = "blue") +
  labs(title = "Crime Trends in Avon and Somerset (Bristol)",
       x = "Month",
       y = "Number of Crimes") +
  theme_minimal()

# Devon and Cornwall
ggplot(devon_cornwall_summary, aes(x = Month, y = Total_Crimes)) +
  geom_line(color = "green") +
  labs(title = "Crime Trends in Devon and Cornwall",
       x = "Month",
       y = "Number of Crimes") +
  theme_minimal()

# To analyze and plot the most common crime types in each region:
# Avon and Somerset (Bristol) Common Crimes
avon_somerset_common <- avon_somerset_all %>%
  count(Crime.type) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(avon_somerset_common, aes(x = reorder(Crime.type, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top 10 Common Crimes in Avon and Somerset (Bristol)",
       x = "Crime Type",
       y = "Number of Incidents") +
  theme_minimal()

# Devon and Cornwall Common Crimes
devon_cornwall_common <- devon_cornwall_all %>%
  count(Crime.type) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(devon_cornwall_common, aes(x = reorder(Crime.type, n), y = n)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Top 10 Common Crimes in Devon and Cornwall",
       x = "Crime Type",
       y = "Number of Incidents") +
  theme_minimal()