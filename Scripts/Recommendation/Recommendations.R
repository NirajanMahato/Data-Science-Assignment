library(tidyverse)
library(dplyr)


#House Price Ranking
#Importing cleaned population data
cleaned_population_data = read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Population/Cleaned_Population_Data.csv")

#Importing cleaned house price data
cleaned_houseprices = read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/house_selling_clean.csv")

#Creating a new house rank table
houseprice_rank= cleaned_houseprices %>%
  group_by(`Town/City`) %>%
  summarise(Price=mean(Price),County=first(County)) %>% 
  #reducing the table by merging multiple same towns that belong to the same county
  arrange(Price) %>% #arranging price in ascending order
  mutate(`House Score`=10-(Price/100000)) %>%  
  #calculating score. We are subtracting from 10 because lower house prices need to have higher rank
  select(`Town/City`,County, `House Score`)

#defining path to save the house ranking csv
file_path <- "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Recommendation/HouseRanking.csv"

#saving the house ranking csv
write.csv(houseprice_rank, file_path, row.names = FALSE)
view(houseprice_rank)



#Download Speed Ranking

#importing the cleaned broadband speed
cleaned_broadband_speed= read_csv('D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Broadband Speed/cleaned_data_broadband.csv')

#Creating a new download speed rank table
download_speed_rank <- cleaned_broadband_speed %>%
  group_by(`Town/City`) %>%
  rename(Town=`Town/City`) %>% #renaming to maintain consistency
  summarise(`Average download speed (Mbit/s)`=`Average download speed (Mbit/s)`,County=first(County)) %>%
  arrange(desc(`Average download speed (Mbit/s)`)) %>% #arranging download speed in descending order
  mutate(`Download Score`= (`Average download speed (Mbit/s)`/100)) %>% #calculating score
  select(Town, County, `Download Score`) %>%
  distinct(Town, .keep_all = TRUE) #keeping .keep_all as true because we want to preserve other columns

#defining path to save the download speed speed ranking csv
file_path <- "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Recommendation/InternetRanking.csv"
view(download_speed_rank)

#saving the download speed ranking csv
write.csv(download_speed_rank, file_path, row.names = FALSE)



#Crime Ranking
# Load necessary libraries
library(tidyverse)
library(dplyr)

# Import the cleaned crime dataset
cleaned_crime_data <- read_csv('D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Crime/Cleaned_merged_crimedata.csv')
View(cleaned_crime_data)

# Import the cleaned population dataset
population_dataset <- read_csv('D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Population/Cleaned_Population_Data.csv')
View(population_dataset)


# Count crimes by year, LSOA, and type
crime_dataset_count <- cleaned_crime_data %>%
  group_by(ShortPostcode, CrimeType, Year, County, `Town/City`) %>%
  # Grouping by ShortPostcode, CrimeType, Year, and County
  select(ShortPostcode, CrimeType, Year, County,`Town/City`) %>%
  na.omit() %>%
  tally() %>%  # Creating the crime count column
  rename(CrimeCount = n) %>%  # Renaming for clarity
  left_join(population_dataset, by = c("ShortPostcode" = "Short Postcode")) %>%
  # Joining with population dataset using the correct column names
  select(ShortPostcode, CrimeCount, `Town/City`, County) %>%  # Selecting the required columns
  na.omit() %>% 
  distinct()

View(crime_dataset_count)

# Calculating the crime rank
crime_rank <- crime_dataset_count %>%
  rename(Town = `Town/City`) %>%  # Renaming for consistency
  group_by(Town) %>%
  summarise(MeanCrime = mean(CrimeCount), County = first(County)) %>%
  arrange(MeanCrime) %>%  # Arranging MeanCrime in ascending order
  mutate(CrimeScore = 10 - (MeanCrime / 1000)) %>%
  # Calculating the score (lower crime means higher rank)
  select(Town, County, CrimeScore)

# Define the path to save the crime rank CSV
file_path <- "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Recommendation/CrimeRanking.csv"

# Save the crime rank CSV
write.csv(crime_rank, file_path, row.names = FALSE)

# View the crime rank
view(crime_rank)





#School Ranking
# Import cleaned school dataset
cleaned_school_dataset <- read_csv('D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/School/cleanedschool.csv')

# Convert town and county names to upper case for consistency
school_rank <- cleaned_school_dataset %>%
  mutate(Town = toupper(Town), County = toupper(County)) %>%
  group_by(Town) %>%
  mutate(`Mean Attainment` = mean(`Attainment_8_Score`, na.rm = TRUE), County = first(County)) %>%
  arrange(desc(`Mean Attainment`)) %>%
  mutate(`School Score` = (`Mean Attainment` / 10)) %>%
  select(Town, County, `School Score`) %>%
  distinct() %>% 
  na.omit()


# Define path to save school rank CSV
file_path <- "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Recommendation/SchoolRanking.csv"

# Save the school rank CSV
write.csv(school_rank, file_path, row.names = FALSE)

# View the resulting school rank data
View(school_rank)



#-----------Joining all the ranking table-----------#

combined_ranking_table <-houseprice_rank %>% #starting with house price rank table
  left_join(download_speed_rank, by = c("Town/City" = "Town", "County" = "County")) %>% #joining with download speed rank table
  na.omit() %>%
  left_join(crime_rank, by = c("Town/City" = "Town", "County" = "County")) %>% #joining with crime rank table
  na.omit() %>%
  left_join(school_rank, by = c("Town/City" = "Town", "County" = "County")) %>% #joining with school rank table
  na.omit()

#-----------Calculation of total score-----------#

final_rank <- combined_ranking_table %>%
  mutate(`Total Score` = (`House Score` + `Download Score` + `CrimeScore` + `School Score`) / 4) %>%
  #creating a new column to show the total score
  select(`Town/City`, County, `House Score`, `Download Score`, `CrimeScore`,`School Score`, `Total Score`) %>% 
  #arranging the order for columns
  arrange(desc(`Total Score`)) %>%   #showing the highest score first
  mutate(Rank= row_number()) %>%
  select(Rank, everything()) #moving the serial number column at first

#defining path to save final ranks csv
file_path <- "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Recommendation/FinalCombinedTownRanking.csv"

#saving the final ranks csv
write.csv(final_rank, file_path, row.names = FALSE)
view(final_rank)
