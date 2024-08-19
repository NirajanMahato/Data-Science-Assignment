library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


#1	House prices vs Download Speed
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




#2	House price vs Drug Rate (2023)

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



#3	Attainment 8 score vs House Price (2022)

# Load the cleaned school and housing datasets
school_data <- read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/School/cleanedschool.csv", show_col_types = FALSE)
housing_data <- read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/House Pricing/house_selling_clean.csv", show_col_types = FALSE)

view(school_data)

#grouping house prices by town and county and finding average price for each group
grouped_house_data = housing_data %>%
  filter(`SaleYear`=="2022") %>%
  group_by(`Town/City`,County) %>%
  mutate(`Town/City` = tolower(`Town/City`)) %>% #converting the town from uppercase to all lowercase
  summarise(Price=mean(Price))


#grouping school data by town and county and finding average score for each group
grouped_school_data = school_data %>%
  filter(`Academic_Year`=="2021-2022") %>%
  group_by(`TOWN`,County) %>%
  mutate(Town= tolower(TOWN)) %>%  #converting the town from to all lowercase
  summarise(`Attainment Score`=mean(`ATT8SCR`))
view(grouped_school_data)



#joining school data and house price data in a single table
school_houseprice_final = grouped_school_data %>% 
  left_join(grouped_house_data,by=c("TOWN"="Town/City")) %>% 
  na.omit #removing rows with null value


#creating a linear model 
l_model = lm(data=school_houseprice_final, `Attainment Score`~Price)
#this model predicts Average attainment score as a function of Average house prices

#showing summary of the Linear Model
summary(l_model) 
options(scipen = 999)
#creating the linear model graph
ggplot(school_houseprice_final, aes(x = Price, y = `Attainment Score`)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 5)) + # Setting limits and breaks
  geom_point(color = "blue") + # All points in a single color
  geom_smooth(method = "lm", se = FALSE, color = "black") + # Adding linear regression line and omitting error bands
  labs(x = "House Price",
       y = "Attainment Score",
       title = "2022 Attainment Score vs House Prices") + # Setting labels
  theme_minimal()



#4	Average Download Speed vs Drug Offense Rate (2022)

#importing population dataset
population_data <- read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Population/Cleaned_Population_Data.csv")

# Importing the cleaned school dataset
crime_data <- read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Crime/Cleaned_merged_crimedata.csv")

#importing the cleaned broadband speed
broadband_speed= read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Broadband Speed/cleaned_data_broadband.csv")

View(broadband_speed)


#grouping broadband speed by town and county and finding average download speed for each group
grouped_broadband = broadband_speed %>%
  group_by(County) %>%
  summarise(`AvgDownloadspeed`= mean(`AvgDownloadSpeed`))
View(grouped_broadband)

colnames(population_data)


#modifyinpopulation_data#modifying our crime dataset to show drug offence rate and crime count  
crime_dataset_drugs <- crime_data %>%
  group_by(ShortPostcode, CrimeType, Year, Month, `Town/City`) %>%
  select(ShortPostcode, CrimeType, Year, Month,`Town/City`) %>%
  na.omit() %>%
  tally() %>%  # Creating crime count column
  rename(Crime_Count = n) %>%
  right_join(population_data, by = c("ShortPostcode" = "Short Postcode")) %>%
  select(ShortPostcode, CrimeType, Crime_Count, Population, Year, Month, `Town/City`) %>%
  na.omit() %>%
  filter(CrimeType == "Drugs") %>%  # Filtering to show only drug crimes of 2022
  mutate(Drug_Offence_Rate = (Crime_Count / Population))  # Calculating drug offence rate

View(crime_dataset_drugs)

#grouping the drug crime dataset by county and town and showing the rate for each group for the year 2022
grouped_drug_crime <- crime_dataset_drugs %>%
  filter(Year == "2022") %>%
  group_by(`Town/City`) %>%
  summarise(Drug_Offence_Rate = mean(Drug_Offence_Rate, na.rm = TRUE))

View(grouped_drug_crime)

#joining broadband data and drug crime rate data in a single table
broadband_crime_data = grouped_broadband %>% 
  left_join(grouped_drug_crime,by="Town/City") %>% 
  na.omit #removing null values
view(broadband_crime_data)

#creating a linear model 
l_model = lm(data=broadband_crime_data, `AvgDownloadspeed"`~`Drug_Offence_Rate`)
#this model predicts Average download speed as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 


#creating the linear model graph
ggplot(broadband_crime_data,aes(x=`Drug_Offence_Rate`,y=`AvgDownloadspeed"`)) +
  scale_y_continuous(limits=c(0,50), breaks = seq(0,50,5))+ #setting limits and breaks
  geom_point(color = "blue") + # All points in a single color
  #setting color as blue for Surrey's data point
  geom_smooth(method=lm,se=FALSE,color="green")+ #adding linear regression line and omitting error bands 
  labs(x="Drug Offence Rate",
       y="Average Download Speed (Mbit/s)",
       title="2022 Average Download Speed vs Drug Offence Rate",color="County") #setting labels


#5	Attainment 8 score vs Drug Offense Rate (2022)

# Importing population dataset
population_data <- read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Population/Cleaned_Population_Data.csv")

# Importing the cleaned school dataset
cleaned_school_data <- read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/School/cleanedschool.csv")

# Importing the cleaned crime dataset
cleaned_crime_data <- read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Crime/Cleaned_merged_crimedata.csv")


# Grouping school data by town and county and finding average score for each group
grouped_school_data <- cleaned_school_data %>%
  filter(Year == "2021-2022") %>%
  group_by(Town, County) %>%
  mutate(Town = tolower(Town)) %>%  # Converting the town to lowercase
  summarise(Attainment_Score = mean(Attainment_8_Score, na.rm = TRUE))

# Modifying crime dataset to show drug offence rate and crime count  
crime_dataset_drugs <- cleaned_crime_data %>%
  group_by(ShortPostcode, CrimeType, Year, Month, `Town/City`) %>%
  select(ShortPostcode, CrimeType, Year, Month,`Town/City`) %>%
  na.omit() %>%
  tally() %>%  # Creating crime count column
  rename(Crime_Count = n) %>%
  right_join(population_data, by = c("ShortPostcode" = "Short Postcode")) %>%
  select(ShortPostcode, CrimeType, Crime_Count, Population, Year, Month, `Town/City`) %>%
  na.omit() %>%
  filter(CrimeType == "Drugs") %>%  # Filtering to show only drug crimes of 2022
  mutate(Drug_Offence_Rate = (Crime_Count / Population))  # Calculating drug offence rate

View(crime_dataset_drugs)

# Grouping the drug crime dataset by county and town and showing the rate for each group for the year 2021
grouped_drug_crime <- crime_dataset_drugs %>%
  filter(Year == "2022") %>%
  group_by(`Town/City`) %>%
  mutate(`Town/City` = tolower(`Town/City`)) %>%  # Converting the town to lowercase
  summarise(Drug_Offence_Rate = mean(Drug_Offence_Rate, na.rm = TRUE))

View(grouped_drug_crime)

# Joining school data and drug crime data in a single table
school_drug <- grouped_school_data %>%
  left_join(grouped_drug_crime, by = c("Town" = "Town/City")) %>%
  na.omit()  # Removing rows with null values


# Creating a linear model 
l_model <- lm(data = school_drug, Attainment_Score ~ Drug_Offence_Rate) 

# This model predicts average attainment score as a function of drug offence rate

# Showing summary of the linear model
summary(l_model) 

# Creating the linear model graph
ggplot(school_drug, aes(x = Drug_Offence_Rate, y = Attainment_Score)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) +  # Setting limits and breaks
  geom_point(color = "blue") + # All points in a single color
  geom_smooth(method = lm, se = FALSE, color = "brown") +  # Adding linear regression line and omitting error bands 
  labs(x = "Drug Offence Rate",
       y = "Attainment Score",
       title = "2022 Attainment Score vs Drug Offence Rate",
       color = "County")  # Setting labels




#6	Average Download Speed vs Attainment 8 score 2022

#importing the cleaned broadband speed
broadband_speed= read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Broadband Speed/cleaned_data_broadband.csv")

View(cleaned_broadband_speed)

#importing the cleaned school dataset
cleaned_school_data <- read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/School/cleanedschool.csv")
View(cleaned_school_dataset)

#grouping broadband speed by town and county and finding average download speed for each group
grouped_broadband = broadband_speed %>%
  group_by(`Town/City`,County) %>%
  mutate(`Town/City`= tolower(`Town/City`)) %>%  #converting the town from to all lowercase
  summarise(`AvgDownloadspeed`= mean(`AvgDownloadspeed`))
View(grouped_broadband)

#grouping school data by town and county and finding average score for each group
grouped_school_data = cleaned_school_data %>%
  filter(`Year`=="2021-2022") %>%
  group_by(`Town`,County) %>%
  mutate(Town= tolower(Town)) %>%  #converting the town from to all lowercase
  summarise(`Attainment Score`=mean(`Attainment_8_Score`))
View(grouped_school_data)

#joining broadband data and school data in a single table
broadband_attainment = grouped_broadband %>% 
  left_join(grouped_school_data,by=c("Town/City"="Town")) %>% 
  na.omit #removing rows with null value

View(broadband_attainment)

#creating a linear model 
l_model = lm(data=broadband_attainment, `AvgDownloadspeed`~`Attainment Score`) #this model predicts Average download speed as a function of Drug offence rate

#showing summary of the Linear Model
summary(l_model) 

#creating the linear model graph
ggplot(broadband_attainment,aes(x=`Attainment Score`,y=`AvgDownloadspeed`)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 5)) + # Setting limits and breaks
  geom_point(color = "blue") + # All points in a single color
  geom_smooth(method=lm,se=FALSE,color="black")+ #adding linear regression line and omitting error bands 
  labs(x="Attainment Score",
       y="Average Download Speed (Mbit/s)",
       title="Average Download Speed vs Attainment Score",color="County") #setting labels
ggsave("D:/Data Assignment/Code/Linear Model/Avg_Download_Speed_vs_Attainment_Score.png", width = 10, height = 6)







