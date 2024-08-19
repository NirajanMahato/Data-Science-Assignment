library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)


# Read in the CSV files
broadband1 <- read.csv("D:/Sem4/Data Science/Obtained Data/Broadband Speed/201805_fixed_pc_performance_r03.csv")
broadband2 <- read.csv("D:/Sem4/Data Science/Obtained Data/Broadband Speed/201809_fixed_pc_coverage_r01.csv")


# Perform the inner join
combined_data_broadbrand <- broadband1 %>%
  inner_join(broadband2, by = c("postcode_space" = "pcds"))

# View the combined data (optional)
View(combined_data_broadbrand)

# Perform the inner join
combined_data_broadbrand <- filtered_LSOA %>%
  inner_join(broadband1, by = c("pcds" = "postcode_space"))

# View the combined data (optional)
View(combined_data_broadbrand)

# Example: Deleting cMaximum.upload.speed..Mbit.s.# Example: Deleting cMaximum.upload.speed..Mbit.s.# Example: Deleting columns "column1", "column2", and "column3"
cleaned_data <- combined_data_broadbrand %>%
  select(-c(pcd7, pcd8,doterm, usertype, oa11cd, lsoa11cd,msoa11cd,ladcd, lsoa11nm, msoa11nm, ladnmw,dointr, postcode.area ,
            postcode))
View(cleaned_data)

print(colnames(cleaned_data))

# Rename specific columns
cleaned_data <- cleaned_data %>%
  rename(
    AvgDownloadSpeed = Average.download.speed..Mbit.s.,
    MinDownloadSpeed = Minimum.download.speed..Mbit.s.,
    MaxDownloadSpeed = Maximum.download.speed..Mbit.s.,
    MedUploadSpeed = Median.upload.speed..Mbit.s.,
    AvgUploadSpeed = Average.upload.speed..Mbit.s.,
    MinUploadSpeed = Minimum.upload.speed..Mbit.s.,
    MaxUploadSpeed = Maximum.upload.speed..Mbit.s.,
    AvgDataUsage = Average.data.usage..GB.
  )
cleaned_data_broadband<- cleaned_data %>%
  rename(
    MedDownloadSpeed = Median.download.speed..Mbit.s.
  )
cleaned_data_broadband<- cleaned_data_broadband %>%
  rename(
    Postcode = pcds,
    County = ladnm
  )
View(cleaned_data_broadband)



# Define the output file path
output_file_path <- "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Broadband Speed/cleaned_data_broadband.csv"

# Save the cleaned data to a new CSV file
write_csv(cleaned_data_broadband, output_file_path)

# Optional: View the cleaned data
View(cleaned_data_broadband)

colnames(cleaned_data_broadband)



# Select only the renamed columns and delete all others
cleaned_data_broadband <- cleaned_data_broadband %>%
  select(Postcode, County, AvgDownloadSpeed, MinDownloadSpeed, MaxDownloadSpeed, MedUploadSpeed, AvgUploadSpeed, MinUploadSpeed, MaxUploadSpeed, AvgDataUsage)

view(cleaned_data_broadband)


# Define the output file path
output_file_path <- "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Broadband Speed/cleaned_data_broadband.csv"

# Save the cleaned data to a new CSV file
write_csv(cleaned_data_broadband, output_file_path)

view(cleaned_data_broadband)


#1. Create a boxplot for Average Download Speeds in Both Counties
ggplot(cleaned_data_broadband, aes(x = County, y = AvgDownloadSpeed, fill = County)) +
  geom_boxplot() +
  labs(
    title = "Average Download Speeds in Bristol and Cornwall",
    x = "County",
    y = "Average Download Speed (Mbit/s)"
  ) +
  theme_minimal()


# 2. Bar Chart of Average and Maximum Download Speeds in Both Counties
# For Bristol
ggplot(cleaned_data_broadband %>% filter(County == "Bristol, City of"), aes(x = Postcode)) +
  geom_bar(aes(y = AvgDownloadSpeed, fill = "Avg Download Speed"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = MaxDownloadSpeed, fill = "Max Download Speed"), stat = "identity", position = "dodge") +
  labs(title = "Average and Maximum Download Speeds in Bristol",
       x = "Postcode",
       y = "Download Speed (Mbit/s)") +
  theme_minimal() +
  scale_fill_manual(name = "Speed Type", values = c("Avg Download Speed" = "blue", "Max Download Speed" = "red"))

# For Cornwall
ggplot(cleaned_data_broadband %>% filter(County == "Cornwall"), aes(x = Postcode)) +
  geom_bar(aes(y = AvgDownloadSpeed, fill = "Avg Download Speed"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = MaxDownloadSpeed, fill = "Max Download Speed"), stat = "identity", position = "dodge") +
  labs(title = "Average and Maximum Download Speeds in Cornwall",
       x = "Postcode",
       y = "Download Speed (Mbit/s)") +
  theme_minimal() +
  scale_fill_manual(name = "Speed Type", values = c("Avg Download Speed" = "blue", "Max Download Speed" = "red"))



