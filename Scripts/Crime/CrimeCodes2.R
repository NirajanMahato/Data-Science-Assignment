#cleaning crime data
library(tidyverse)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(fmsb)

crime_data = "D:/Sem4/Data Science/Obtained Data/Crime Dataset"

# Listing all subfolders
folders = list.dirs(crime_data, full.names = TRUE, recursive = FALSE)

cleaned_data_list = list()

# Function to clean individual crime data files
clean_crime_data = function(file_path) {
  
  data = read_csv(file_path, col_types = cols(
    `Crime type` = col_character(),
    Month = col_character(),
    Longitude = col_double(),
    Latitude = col_double(),
    Context = col_character()
  ))
  
  # Cleaning the data
  data_clean = data %>%
    filter(!is.na(`Crime type`)) %>%  
    
    mutate(
      Date = as.Date(paste0(Month, "-01"), format = "%Y-%m-%d")  
    ) %>%
    drop_na(Longitude, Latitude)  %>%
    distinct()
  
  # Removing unnecessary columns 
  data_clean = data_clean %>% 
    arrange(Date) %>%
    select(-`Crime ID`, -`Reported by`, -Longitude, -Latitude, -Location, -`Last outcome category`, -Date, -Context)
  
  
  return(data_clean)
}

# Loop through each folder
for (folder in folders) {
  # Get all CSV files in the folder
  files = list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Applying the cleaning function to each file and store in the list
  for (file in files) {
    cleaned_data = clean_crime_data(file)
    cleaned_data_list[[length(cleaned_data_list) + 1]] = cleaned_data
  }
}


all_cleaned_data = bind_rows(cleaned_data_list)


write_csv(all_cleaned_data, file.path(crime_data, "all_cleaned_crime_data.csv"))

summary(all_cleaned_data)

View(all_cleaned_data)
print(colnames(all_cleaned_data))


postcode_lsoa = read_csv("D:/Sem4/Data Science/Obtained Data/Postcode to LSOA.csv")
clean_lsoa = postcode_lsoa %>%
  select(lsoa11cd, lsoa11nm, ladnm, pcds) %>%
  rename(`LSOA code` = lsoa11cd, Street = lsoa11nm, County = ladnm, Postcode = pcds)

population_data = read_csv("D:/Sem4/Data Science/Obtained Data/Population2011_1656567141570.csv")
population_data = population_data %>%
  mutate(Population2023 = 1.00561255390388033 * Population) %>%
  select(Postcode, Population2023) 




# Filtering
filtered_lsoa = clean_lsoa %>%
  filter(County %in% c("Bristol, City of", "Cornwall")) %>%
  mutate(Postcode = str_trim(substring(Postcode, 1, 6)))

# Checking for duplicates
any(duplicated(all_cleaned_data$`LSOA code`))
any(duplicated(filtered_lsoa$`LSOA code`))

# Removing duplicates 
cleancrime1 = unique(all_cleaned_data, by = "LSOA code")
filtered_lsoa1 = unique(filtered_lsoa, by = "LSOA code")

# Merging the datasets 
final_cleaned_crime = cleancrime1 %>%
  left_join(filtered_lsoa1, by = "LSOA code", relationship = "many-to-many") %>%
  mutate(Year = str_trim(substring(Month, 1, 4))) %>%
  mutate(Month = str_trim(substring(Month, 6, 7))) %>%
  left_join(population_data, by = "Postcode") %>%
  filter(!is.na(`Crime type`) & !is.na(`Month`) & !is.na(`Falls within`) & !is.na(`LSOA code`) &
           !is.na(`Street`) & !is.na(`County`) & !is.na(`Population2023`))


View(final_cleaned_crime)

write_csv(final_cleaned_crime, "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Crime/Cleaned_merged_crimedata.csv")




#Visualization of crimedataset
#Q1

cleaned_crime_data = read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Crime/Cleaned_merged_crimedata.csv")

# Filtering
drug_offences_2022 = cleaned_crime_data %>%
  filter(`Crime type` == "Drugs" & Year == "2022")


drug_offence_rate = drug_offences_2022 %>%
  group_by(County, Street) %>%
  summarise(Total_Offences = n())

# Generating the boxplot 
ggplot(drug_offence_rate, aes(x = County, y = Total_Offences, fill = County)) +
  geom_boxplot() +
  labs(title = "Drug Offence Rate for Cornwall and Bristol (2023)",
       x = "County",
       y = "Total Drug Offences") +
  theme_minimal() +
  scale_fill_manual(values = c("Bristol, City of" = "#F8766D", "Cornwall" = "#00BFC4"))


#Q2
vehicle_crimes = cleaned_crime_data %>%
  filter(`Crime type` == "Vehicle crime", Year %in% 2020:2023)


vehicle_crime_rate = vehicle_crimes %>%
  group_by(Year) %>%
  summarise(
    Total_Vehicle_Crimes = n(),
    Population = mean(Population2023, na.rm = TRUE)  
  ) %>%
  mutate(Crime_Rate_Per_10000 = (Total_Vehicle_Crimes / Population) * 10000)

# Prepare the data for the radar chart
radar_data = vehicle_crime_rate %>%
  select(Year, Crime_Rate_Per_10000) %>%
  pivot_wider(names_from = Year, values_from = Crime_Rate_Per_10000)


radar_data = rbind(rep(100, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)

# Plotting the radar chart
par(mfrow = c(1, 1))
radarchart(radar_data, axistype = 1,
           pcol = c("#F8766D", "#00BFC4", "#7CAE00", "#C77CFF"),
           pfcol = c(scales::alpha("#F8766D", 0.5), scales::alpha("#00BFC4", 0.5), scales::alpha("#7CAE00", 0.5), scales::alpha("#C77CFF", 0.5)),
           plwd = 2,
           title = "Vehicle Crime Rate per 10,000 People from 2020 to 2023")



#Q3
selected_month = "04"  

# Filtering
robbery_crimes = cleaned_crime_data %>%
  filter(`Crime type` == "Robbery" & Year == "2023" & Month == selected_month)


robbery_crime_rate = robbery_crimes %>%
  group_by(County) %>%
  summarise(
    Total_Robbery_Crimes = n(),
    Population = mean(Population2023, na.rm = TRUE)  
  ) %>%
  mutate(Crime_Rate_Per_10000 = (Total_Robbery_Crimes / Population) * 10000)


pie_data = robbery_crime_rate %>%
  select(County, Crime_Rate_Per_10000)

# Generating the pie chart 
ggplot(pie_data, aes(x = "", y = Crime_Rate_Per_10000, fill = County)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = paste("Robbery Crime Rate per 10000 People in", selected_month, "2023")) +
  theme_void() +
  scale_fill_manual(values = c("Bristol, City of" = "#F8766D", "Cornwall" = "#00BFC4"))



#Q4. Filtering
drug_offenses = cleaned_crime_data %>%
  filter(`Crime type` == "Drugs" & Year == "2022")



drug_offense_rate = drug_offenses %>%
  group_by(County, Month) %>%
  summarise(
    Total_Drug_Offenses = n(),
    Population = mean(Population2023, na.rm = TRUE)  
  ) %>%
  mutate(Drug_Offense_Rate_Per_10000 = (Total_Drug_Offenses / Population) * 10000)

# Generating the line chart 
ggplot(drug_offense_rate, aes(x = Month, y = Drug_Offense_Rate_Per_10000, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Drug Offense Rate per 10000 People in Both Counties (2022)",
       x = "Month", 
       y = "Drug Offense Rate per 10000 People") +
  theme_minimal() +
  scale_color_manual(values = c("Bristol, City of" = "#F8766D", "Cornwall" = "#00BFC4"))





