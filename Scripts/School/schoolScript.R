# Cleaning of school
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)


bristol21=read_csv("D:/Sem4/Data Science/Obtained Data/school/bristol/2021-2022/801_ks4final.csv")
bristol22=read_csv("D:/Sem4/Data Science/Obtained Data/school/bristol/2022-2023/801_ks4final.csv")
cornwall21=read_csv("D:/Sem4/Data Science/Obtained Data/school/cornwall/2021-2022/908_ks4final.csv")
cornwall22=read_csv("D:/Sem4/Data Science/Obtained Data/school/cornwall/2022-2023/908_ks4final.csv")


# cleaning
clean_data = function(df) {
  df %>%
    select(SCHNAME, PCODE, ATT8SCR, TOWN) %>% 
    mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%  
    filter(!is.na(ATT8SCR)) %>%  
    filter(!is.na(ATT8SCR) & !is.na(PCODE) & !is.na(SCHNAME) & !is.na(TOWN))  %>%
    distinct()
  
  
}

# Cleaning all datasets
bristol21_clean = clean_data(bristol21)
bristol22_clean = clean_data(bristol22)
cornwall21_clean = clean_data(cornwall21)
cornwall22_clean = clean_data(cornwall22)

# Adding academic year and county identifiers
bristol21_clean = bristol21_clean %>% mutate(Academic_Year = "2021-2022", County = "Bristol")
bristol22_clean = bristol22_clean %>% mutate(Academic_Year = "2022-2023", County = "Bristol")
cornwall21_clean = cornwall21_clean %>% mutate(Academic_Year = "2021-2022", County = "Cornwall")
cornwall22_clean = cornwall22_clean %>% mutate(Academic_Year = "2022-2023", County = "Cornwall")

# Merging
combined_schooldata = bind_rows(bristol21_clean, bristol22_clean, cornwall21_clean, cornwall22_clean)

View( combined_schooldata)

write_csv(combined_schooldata, "D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/School/cleanedschool.csv")



#Visualization of school
cleaned_schooldata = read_csv("D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/School/cleanedschool.csv")

# Filtering
data_2021_2022 = cleaned_schooldata %>% 
  filter(Academic_Year == "2021-2022")

# Calculating
average_att8scr = data_2021_2022 %>%
  group_by(County) %>%
  summarize(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE))


print(average_att8scr)

# Creating a boxplot 
ggplot(data_2021_2022, aes(x = County, y = ATT8SCR, fill = County)) +
  geom_boxplot() +
  labs(title = "Boxplot of Attainment 8 Scores (2021-2022)",
       x = "County",
       y = "Attainment 8 Score") +
  theme_minimal()


# Filtering
bristol_2021_2022 = cleaned_schooldata %>% 
  filter(County == "Bristol" & Academic_Year == "2021-2022")

# Calculating
bristol_avg_att8scr = bristol_2021_2022 %>%
  group_by(SCHNAME) %>%
  summarize(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE))


print(bristol_avg_att8scr)

# Create a line chart for Bristol schools' average Attainment 8 scores
ggplot(bristol_avg_att8scr, aes(x = SCHNAME, y = Average_ATT8SCR, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Attainment 8 Score for Bristol Schools (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate school names for better readability


# Filtering
cornwall_2021_2022 = cleaned_schooldata %>% 
  filter(County == "Cornwall" & Academic_Year == "2021-2022")

# Calculating 
cornwall_avg_att8scr = cornwall_2021_2022 %>%
  group_by(SCHNAME) %>%
  summarize(Average_ATT8SCR = mean(ATT8SCR, na.rm = TRUE))


print(cornwall_avg_att8scr)

# Creating a line chart 
ggplot(cornwall_avg_att8scr, aes(x = SCHNAME, y = Average_ATT8SCR, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Attainment 8 Score for Cornwall Schools (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

