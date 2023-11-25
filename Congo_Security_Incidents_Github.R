# Eastern Congo Security Incidents Project 
  
# Welcome to the Eastern Congo Security Incidents Capstone Project, which is my final project for the Google Data Analytics Course. 
# Since I am taking the course while I am working in eastern Congo in a peacebuilding project, I wanted to find a project related to the local context. 

# The analysis is based on the University of Uppsala Conflict Data Programm [https://ucdp.uu.se/], which records security incidents worldwide. 
# I am using the "UCDP Georeferenced Event Dataset (GED)". Unfortunately, I do not remember which version I used for the initial analysis, but it contained entries until early 2021. 
# Since the context in eastern DRC has changed significantly since then, I did an update in May/June 2023 with an updated version that contains entries until the end of 2022 (Version 23.1.). 
# When I was almost done, I realised that, in contrast to the main dataset that is updated and published once a year, there is also a continuously updated version. I thus updated the 
# analysis again and added the data from January-June 2023 to the main dataset.  

# My objective for this project was mainly to apply the skills learned during the Google course, in particular applying R to a topic I find interesting, 
# as I don't really have a background in marketing or business, which was the focus of most of the course. In particular, I wanted to:

# - clean and prepare a real life dataset
# - use R for summarising the data
# - create visualizations to present the data
# - get used to troubleshooting and problem-solving based on help from internet fora and, for the later work, ChatGPT

# The main idea of the project is to analyse the evolution of the 
# security context in eastern DRC over the last 20 years, with a particular
# focus on the last couple of years, and to formulate a few recommendations for stakeholders based on the data, if possible. 

# I hope you enjoy and please feel free to comment. 

# 1. THE DATASET

# As said above, the dataset is from UCDP and covers the period from the
# first Congo War (starting in 1996) until the end of 2022. 
# Each row contains one record of a "security incident", stipulating a
# number of information, e.g. (estimations of) date, the conflict parties, 
# the geolocation, the number of civilian or military deaths, the source
# upon which the info is based, degree of certainty etc.  

# The core of the dataset is thus dyadic, providing the conflict parties
# to specific incidents, which can either be an armed group, a regular
# army or civilians (which would rather be victims, of course). This can
# sometimes be a bit problematic / annoying, e.g. when more than two groups
# are party to a conflict, when groups change names or split up or when
# several groups are actually related (e.g. Hema - Lendu, CODECO and URPDS). 
# But anyway - let's dive into the analysis. 

-
  
# 2. PREPARING R
  

library(tidyverse)
library(janitor)
library(viridis)
library(RColorBrewer)
library(anytime)
library(ggmap)
library(RColorBrewer)
library(scales)



# 3. LOADING, CLEANING AND PREPARING THE DATASET. 

# Before loading it into R, I slightly prepared the dataset in Excel, 
# kicking out a few columns I wasn't interested in and that created problems when loading (in particular the source for security incidents). 

# Loading the data 


congo_raw <- read.csv("C:\\Users\\UserNA4944\\OneDrive - Deutsche Gesellschaft für Internationale Zusammenarbeit (GIZ) GmbH\\Privat\\Data Analytics\\Data Sets\\Congo_Security_Incidents_Dataset2023.csv", head = TRUE, sep=";")

str(congo_raw)


# This produces a data frame with 35 variables and 316814 observations (incidents), that are not restricted to DRC yet. 
# The structure and data type looks OK for most columns, except date_start
# and date_end (which are strings but should be dates or date-time) and longitude/latitude (which are strings with comma separation instead of numericals with dots). 

# Restricting the dataset to eastern DRC (country_id = 490)


congo_raw <- congo_raw %>% filter(country_id==490)


# Ensure column names don't contain empty spaces etc.:
  
congo_raw <- clean_names(congo_raw)


# Renaming of several column names for clarity and purpose. For the date, I'll use the end_date column as date.  

congo_raw <- rename(congo_raw, province=adm_province, territory=adm_territoire, deaths_civilian=deaths_civilians, deaths_total=best, date=date_end)


# Creating a few new columns 

# The data on deaths is only by side a or side b. I'm not really interested 
# in the number of deaths per warring party. Instead, I would like to get the total number of military deaths. I thus take the total amount of deaths and subtract the number of civilian deaths. 
# However, there is also deaths_unknown, which I also subtract. 

congo_raw <- congo_raw %>% mutate(deaths_military=deaths_total-deaths_civilian-deaths_unknown)


# Deleting some unnecessary columns that I don't need

congo_raw <- congo_raw %>% select(-c(conflict_new_id, dyad_new_id, side_a_new_id, side_b_new_id, high, low))


# Transform the data type for date from character into a date

congo_raw$date <- as.Date(congo_raw$date, format = "%d.%m.%Y")


# Transform longitude and latitude from character with comma separation to numeric with dot separation

congo_raw$latitude <- as.numeric(gsub(",", "\\.", congo_raw$latitude))
congo_raw$longitude <- as.numeric(gsub(",", "\\.", congo_raw$longitude))


# Replacing column entries 

# In the columns containing the conflict party names, the entries for involvement of the Congolese, Rwandan, Ugandan or Burundian 
# armed forces are displayed as "Government of Burundi/Uganda/Rwanda/DR Congo (Zaire), which makes for long and unwieldy entries. 
# I will replace them by the abbreviations of their respective armed forces: FARDC (Congo), RDF (Rwanda), UPDF (Uganda) and FDNB (Burundi). 
# Since I don't know whether they are counted as side a or b in the dataset, I will replace them for either side, and then create 
# a new column that describes the conflict parties. At the same time, I create a new column entry "ADF / IS" (since the armed group ADF is 
# counted as IS in the dataset after it swore allegiance to the Islamic State in 2019, but for my purposes remains the same group) and 
# correct some errors for group and province names where French special characters weren't properly recognised. Furthermore, due to the 
# administrative reorganisation of Congolese provinces, I introduce some changes to the province names. 

congo_raw <- congo_raw %>%
  mutate(
    side_a = if_else(side_a == "Government of DR Congo (Zaire)", "FARDC", side_a),
    side_b = if_else(side_b == "Government of DR Congo (Zaire)", "FARDC", side_b), 
    side_a = if_else(side_a == "Government of Rwanda", "RDF", side_a),
    side_b = if_else(side_b == "Government of Rwanda", "RDF", side_b), 
    side_a = if_else(side_a == "Government of Uganda", "UPDF", side_a), 
    side_b = if_else(side_b == "Government of Uganda", "UPDF", side_b), 
    side_a = if_else(side_a == "Government of Burundi", "FDNB", side_a), 
    side_b = if_else(side_b == "Government of Burundi", "FDNB", side_b), 
    side_a = if_else(side_a == "ADF", "ADF / IS", side_a),
    side_b = if_else(side_b == "ADF", "ADF / IS", side_b),
    side_a = if_else(side_a == "IS", "ADF / IS", side_a),
    side_b = if_else(side_b == "IS", "ADF / IS", side_b),
    side_a = if_else(side_a == "ADF, Vuba militia", "ADF / IS", side_a), 
    side_b = if_else(side_b == "Groupe de SÃ©curitÃ© (Nyatura Kasongo)", "Groupe de Sécurité (Nyatura Kasongo)", side_b), 
    province = if_else(province == "KasaÃ¯ Oriental", "Kasai-Oriental", province),
    province = if_else(province == "Kasai¯-Central", "Kasai-Central", province),
    province = if_else(province == "Nord Kivu region", "Nord Kivu", province),
    province = if_else(province == "Sud Kivu region", "Sud Kivu", province),
    province = if_else(province == "KasaÃ¯ Oriental", "Kasai-Oriental", province)
  )


# Create a new column conflict_dyad with the corrected names and delete dyad_name and conflict_name

congo_raw <- unite(congo_raw, "conflict_dyad", side_a, side_b, sep=" - ", remove = FALSE) %>% 
  select(-c(dyad_name, conflict_name))


# Removing some unnecessary words in the territory and province columns

congo_raw <- congo_raw %>% mutate(territory = str_remove_all(territory, "territory"))
congo_raw <- congo_raw %>% mutate(territory = str_remove_all(territory, "zone"))
congo_raw <- congo_raw %>% mutate(territory = str_remove_all(territory, "region"))
congo_raw <- congo_raw %>% mutate(province = str_remove_all(province, " city"))
congo_raw <- congo_raw %>% mutate(province = str_remove_all(province, " region"))
congo_raw <- congo_raw %>% mutate(province = str_remove_all(province, " province"))


# Trim trailing spaces in province and territory columns

congo_raw$province <- trimws(congo_raw$province, which = c("right"))
congo_raw$territory <- trimws(congo_raw$territory, which = c("right"))


# Administrative restructuring of Ituri and Tanganyika provinces

# One of the provinces I'm interested in is Ituri and, to a lesser extent, Tanganyika. However, until an administrative restructuring 
# in 2015, both were mere districts in bigger provinces (Ituri within the province "Orientale", Tanganyika within "Katanga"). For clarity and continuity, 
# I would thus like to change the entry in province from "Orientale" to "Ituri" and "Katanga" to "Tanganyika" if the incident occurred in one of the 
# 5 territories of Ituri (Djugu, Irumu, Mambasa, Aru, Mahagi) or one of the 6 territories of Tanganyika (Kabalo, Kalemie, Kongolo, Manono, Moba, Nyunzu), whereas all other entries remain the same. 

congo_raw <- congo_raw %>% mutate(province = case_when(
  territory %in% c("Irumu", "Djugu", "Aru", "Mahagi", "Mambasa") ~ "Ituri", 
  TRUE ~ province))

congo_raw <- congo_raw %>% mutate(province = case_when(
  territory %in% c("Kabalo", "Kalemie", "Kongolo", "Manono", "Moba", "Nyunzu") ~ "Tanganyika",
  TRUE ~ province))


# Rain season vs. dry season

# Besides, I would like to filter for seasonal variations later in the analysis. Since there are no seasons in the European sense, seasons of the year do not make sense. 
# However, distinguishing between rainy and dry season might make sense as inaccessability of certain areas due to degraded roads might impact armed group or army mobility. 
# There are two rain seasons in eastern DRC, one short one from October-November, a longer one from mid-March until the end of May. I thus create two categories, 
# where the date between October 1 and November 30, or between March 15 and May 31 is counted as rain season, whereas the rest is counted as dry season. 

congo_raw <- congo_raw %>% mutate(day = day(date), 
                                  month = month(date),
                                  season = case_when(
                                    month == 03 & day >= 15 | month %in% 04:05 | month %in% 10:11 ~ "rain season", 
                                    TRUE ~ "dry season"))


# Filtering for some notorious armed groups

# Besides, in the final analysis, I would also like to compare the development over time of the activity of several of DRC's most notorious armed groups, e.g. ADF/IS, FDLR, M23, CODECO, Kamuina Nsapu and Banyamulenge groups. 

congo_raw <- congo_raw %>% mutate(armed_group = case_when(
            side_a %in% c("FDLR-FOCA", "FDLR-RUD") | side_b %in% c("FDLR-FOCA", "FDLR-RUD") ~ "FDLR", 
            side_a == "ADF / IS" | side_b == "ADF / IS" ~ "ADF / IS",
            side_a == "M23" | side_b == "M23" ~ "M23",
            side_a == "Kamuina Nsapu" | side_b == "Kamuina Nsapu" ~ "Kamuina Nsapu", 
            side_a %in% c("URDPC", "Zaire self-defense group", "Hema", "Lendu", "CODECO-BTD", "FDBC", "ALC") | 
              side_b %in% c("URDPC", "Zaire self-defense group", "Hema", "Lendu", "CODECO-BTD", "FDBC", "ALC") ~ "Hema-Lendu",
            side_a %in% c("Twiganeho", "Ngumino, Twiganeho", "Ngumino") | side_b %in% c("Twiganeho", "Ngumino, Twiganeho", "Ngumino") ~ "Banyamulenge", 
            side_a == "LRA" | side_b == "LRA" ~ "LRA",))


# Reordering of the columns

congo_raw <- congo_raw %>% select(id, year, date, conflict_dyad, side_a, side_b, province, territory, deaths_total, 
                                  deaths_military, deaths_civilian, deaths_unknown, season, armed_group, latitude, longitude, everything()) %>% select(-c(country, country_id, date_start, type_of_violence))


# Producing the final, clean version

congo_clean <- congo_raw

str(congo_clean)

# Looks good!


# Export of cleaned and final dataset as CSV file (";" as separator, "," for decimals )

# the export is not included here








# 3.5 - MERGING WITH UPDATED DATASET JANUARY-JUNE 2023

# The dataset I've been using so far is one that is updated in a yearly interval, it thus doesn't contain any entries for 2023 yet. 
# However, I discovered that there is one that is updated in a monthly interval, the "UCDP Candidate Events Dataset (UCDP Candidate) version 23.0.X" dataset. 
# I will try to merge it with my base dataset (the version: January-June 2023: Version 23.01.23.06 (global)).
# In January 2024, I will merge the second half of 2023, as soon as it gets available. 

congo_raw23 <- read.csv("C:\\Users\\UserNA4944\\OneDrive - Deutsche Gesellschaft für Internationale Zusammenarbeit (GIZ) GmbH\\Privat\\Data Analytics\\Data Sets\\Congo_Security_Incidents_Dataset2023_Jan-Jul23.csv", head = TRUE, sep=";")
str(congo_raw23)

# Latitude and Longitude do not have the comma/dot delimiter, but I can get the data from the geom_wikt column
# date_end is chr instead of date
# Some of the id columns are chr instead of int (because there is text in some rows), but it doesn't matter since I don't really use them anyway 

# Repeating some of the data cleaning & preparing steps from the previous analysis 
congo_raw23 <- congo_raw23 %>% filter(country_id==490)
congo_raw23 <- rename(congo_raw23, province=adm_1, territory=adm_2, deaths_civilian=deaths_civilians, deaths_total=best, date=date_end)
congo_raw23 <- clean_names(congo_raw23)
congo_raw23 <- congo_raw23 %>% mutate(deaths_military=deaths_total-deaths_civilian-deaths_unknown)
congo_raw23 <- congo_raw23 %>% select(-c(conflict_new_id, dyad_new_id, side_a_new_id, side_b_new_id, high, low))
congo_raw23$date <- as.Date(congo_raw23$date, format = "%d.%m.%Y")

congo_raw23 <- congo_raw23 %>%
  mutate(
    side_a = if_else(side_a == "Government of DR Congo (Zaire)", "FARDC", side_a),
    side_b = if_else(side_b == "Government of DR Congo (Zaire)", "FARDC", side_b), 
    side_a = if_else(side_a == "Government of Rwanda", "RDF", side_a),
    side_b = if_else(side_b == "Government of Rwanda", "RDF", side_b), 
    side_a = if_else(side_a == "Government of Uganda", "UPDF", side_a), 
    side_b = if_else(side_b == "Government of Uganda", "UPDF", side_b), 
    side_a = if_else(side_a == "Government of Burundi", "FDNB", side_a), 
    side_b = if_else(side_b == "Government of Burundi", "FDNB", side_b), 
    side_a = if_else(side_a == "ADF", "ADF / IS", side_a),
    side_b = if_else(side_b == "ADF", "ADF / IS", side_b),
    side_a = if_else(side_a == "IS", "ADF / IS", side_a),
    side_b = if_else(side_b == "IS", "ADF / IS", side_b),
    side_a = if_else(side_a == "ADF, Vuba militia", "ADF / IS", side_a), 
    side_b = if_else(side_b == "Groupe de SÃ©curitÃ© (Nyatura Kasongo)", "Groupe de Sécurité (Nyatura Kasongo)", side_b), 
    province = if_else(province == "KasaÃ¯ Oriental", "Kasaï-Oriental", province),
    province = if_else(province == "KasaÃ¯-Central", "Kasaï-Central", province),  
    province = if_else(province == "Nord Kivu region", "Nord Kivu", province),
    province = if_else(province == "Sud Kivu region", "Sud Kivu", province),
  )
  
congo_raw23 <- unite(congo_raw23, "conflict_dyad", side_a, side_b, sep=" - ", remove = FALSE) %>% 
  select(-c(dyad_name, conflict_name))

congo_raw23 <- congo_raw23 %>% mutate(territory = str_remove_all(territory, " territory"))
congo_raw23 <- congo_raw23 %>% mutate(territory = str_remove_all(territory, " zone"))
congo_raw23 <- congo_raw23 %>% mutate(territory = str_remove_all(territory, " region"))
congo_raw23 <- congo_raw23 %>% mutate(province = str_remove_all(province, " city"))
congo_raw23 <- congo_raw23 %>% mutate(province = str_remove_all(province, " region"))
congo_raw23 <- congo_raw23 %>% mutate(province = str_remove_all(province, " province"))

congo_raw23$province <- trimws(congo_raw23$province, which = c("right"))
congo_raw23$territory <- trimws(congo_raw23$territory, which = c("right"))
```

# Since the longitude and latitude data do not have the comma or dot separator in the CSV file, I need to obtain it from the geom_wkt column: 

congo_raw23$longitude <- sub("^.*\\(([^\\s]+)\\s.*$", "\\1", congo_raw23$geom_wkt)
congo_raw23$longitude <- as.numeric(congo_raw23$longitude)
congo_raw23$latitude <- sub("^.*\\s([^\\s]+)\\).*$", "\\1", congo_raw23$geom_wkt)
congo_raw23$latitude <- as.numeric(congo_raw23$latitude)


congo_raw23 <- congo_raw23 %>% mutate(province = case_when(
  territory %in% c("Irumu", "Djugu", "Aru", "Mahagi", "Mambasa") ~ "Ituri", 
  TRUE ~ province))

congo_raw23 <- congo_raw23 %>% mutate(province = case_when(
  territory %in% c("Kabalo", "Kalemie", "Kongolo", "Manono", "Moba", "Nyunzu") ~ "Tanganyika",
  TRUE ~ province))


congo_raw23 <- congo_raw23 %>% mutate(day = day(date), 
                                  month = month(date),
                                  season = case_when(
                                      month == 03 & day >= 15 | month %in% 04:05 | month %in% 10:11 ~ "rain season", 
                                     TRUE ~ "dry season"))


congo_raw23 <- congo_raw23 %>% mutate(armed_group = case_when(
            side_a %in% c("FDLR-FOCA", "FDLR-RUD") | side_b %in% c("FDLR-FOCA", "FDLR-RUD") ~ "FDLR", 
            side_a == "ADF / IS" | side_b == "ADF / IS" ~ "ADF / IS",
            side_a == "M23" | side_b == "M23" ~ "M23",
            side_a == "Kamuina Nsapu" | side_b == "Kamuina Nsapu" ~ "Kamuina Nsapu", 
            side_a %in% c("URDPC", "Zaire self-defense group", "Hema", "Lendu", "CODECO-BTD", "FDBC", "ALC") | 
              side_b %in% c("URDPC", "Zaire self-defense group", "Hema", "Lendu", "CODECO-BTD", "FDBC", "ALC") ~ "Hema-Lendu",
            side_a %in% c("Twiganeho", "Ngumino, Twiganeho", "Ngumino") | side_b %in% c("Twiganeho", "Ngumino, Twiganeho", "Ngumino") ~ "Banyamulenge", 
            side_a == "LRA" | side_b == "LRA" ~ "LRA",))


congo_raw23 <- congo_raw23 %>% select(id, year, date, conflict_dyad, side_a, side_b, province, territory, deaths_total, deaths_military, deaths_civilian, deaths_unknown, season, armed_group, latitude, longitude, everything()) %>% select(-c(country, country_id, date_start))

congo_clean23 <- congo_raw23


# Since the column names have the same name and order, I can now merge the two dataframes: 

combined_congo <- rbind(congo_clean, congo_clean23)
str(combined_congo)

# Preparing the final combined and cleaned dataframe
congo_clean <- combined_congo


# write.csv2(congo_clean, "C:\\Users\\UserNA4944\\OneDrive - Deutsche Gesellschaft für Internationale Zusammenarbeit (GIZ) GmbH\\Privat\\Data Analytics\\Projects\\Eastern Congo Security Incidents\\congo_database_june_2023.csv", row.names=F)




# 4 - DATA ANALYSIS 

# I'm mostly interested in eastern Congo after the signature of the Sun City Agreement in April 2003 that ended the Second Congo War, 
# and even more in the developments over the last couple of years, with a particular focus on the developments in North Kivu and Ituri provinces. 

# Nonetheless, having a quick look at the whole dataset from 1996, with the beginning of the First Congo War:
  
# 4.1 General Overview

# Total no of incidents

nrow(congo_clean)

# There are 7818 incidents recorded in total. 


congo_clean %>% count(province, name="count") %>% arrange(-count)

# In terms of number of incidents, North Kivu is far ahead (3410), followed by Ituri (1842), South Kivu (1038), Orientale (338) and Tanganyika (189). 

# Since an incident in itself does not say much about its deadliness, now a quick look at total, civilian and military deaths. 

congo_clean %>% group_by(province) %>% summarise(total_deaths=sum(deaths_total),                                                total_civilian_deaths=sum(deaths_civilian),                                    total_military_deaths=sum(deaths_military)) %>% 
  arrange(-total_deaths)

# The deadliness presents the same picture: North Kivu leading by far (~46.000) in front of South Kivu (~30.000), Ituri (~21.000), Equateur (5.800) and Tanganyika (4.800). 


# A quick visualisation of the 6 deadliest provinces since 1996: 
  
congo_clean %>% filter(province %in% c("Nord Kivu", "Sud Kivu", "Ituri", "Equateur", "Tanganyika", "Maniema")) %>%
  group_by(year, province) %>% 
  ggplot(aes(x=year, y=deaths_total, fill=province))+
  geom_col()+
  labs(title="Total deaths 1996-2023", x="Year", y="Total deaths", fill="Province")+
  scale_fill_brewer(palette="Dark2", direction=1)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_minimal()+
  theme(legend.box.background = element_rect(color="black"), 
        legend.box.margin = margin(6, 6, 6, 6))

# The graph shows several things:
  
# - Violence was much worse in the 90s during/at the end of the Second Congo War
# - There were relatively lulls in the early 2000s and early 2010s, but violence never entirely stopped
# - Ituri had a huge uptick around 2003, but then calmed down until 2017
# - Violence has been increasing over the last years, especially since 2017



## 4.2 Eastern Congo 2002-2023

# I'm most interested in the period since the signature of the Sun City Agreement on 
# 2 April 2003 and the four eastern provinces of Nord Kivu, Sud Kivu, Ituri and Tanganyika. 

east <- congo_clean %>% filter(province %in% c("Nord Kivu", "Sud Kivu", "Ituri",                                      "Tanganyika"), date > "2003-04-01") %>% arrange(date)

# First, again some broad looks. Total number of incidents for the 4 provinces: 

nrow(east) 

# 5763 incidents in total. 

# Number of incidents by province: 

east %>% count(province, name = "count") %>% arrange(-count) %>% 
  ggplot(aes(x=reorder(province, -count), y=count, fill=province))+
  geom_col()+
  scale_fill_viridis(discrete=TRUE, option = "D", direction=1)+
  labs(title="Total number of incidents by province", x="Province", y="Number of incidents", fill="Province")+
  theme_minimal()

# Again, North Kivu is by far the first, before Ituri, South Kivu and, by far the least, Tanganyika. 


# Having a look at the number of incidents caused by conflict (dyad).

east %>% count(conflict_dyad, name = "count") %>% arrange(-count) %>% head(n=15)

# ADF / IS is unsurprisingly the conflict with the most incidents, in 3 different constellations (against civilians and the Congolese (FARDC) and Ugandan (UPDF) militaries in the top 15 dyads. 
# Civilians are taking the brunt of incidents, being one of the "conflict parties" in the first 4 places with most incidents. In a lot of incidents, FARDC is involved. 

# Visualizing the incidents by conflict dyad: 
east %>% count(conflict_dyad, name = "count") %>% arrange(-count) %>% head(n=8) %>% 
  ggplot(aes(x=reorder(conflict_dyad, -count), y=count, fill=conflict_dyad))+
           geom_col()+
           labs(title="Number of incidents by conflict dyad", x="Conflict dyad", y="Number of incidents", fill="Conflict Dyad")+
           scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
          theme(axis.text.x=element_text(angle=45, hjust=1))

# To calculate the number of incidents where one of the sides was civilians: 

length(which(east$side_a == "Civilians" | east$side_b == "Civilians"))

# In total, civilians were affected in 3536 out of 5763 incidents. 

# This makes for a proportion of 61% :

length(which(east$side_a == "Civilians" | east$side_b == "Civilians"))/nrow(east)*100

            
# A quick overview over most incidents per province: 

# North Kivu

east %>% select(year, conflict_dyad, province, territory, deaths_total, deaths_civilian) %>% filter(province == "Nord Kivu") %>% 
  group_by(conflict_dyad) %>% count(conflict_dyad, name="count") %>% arrange(-count) %>% head(n=10)

# Most incidents were caused by ADF / IS, FARDC and FDLR. 


# Ituri

east %>% select(year, conflict_dyad, province, territory, deaths_total, deaths_civilian) %>% filter(province == "Ituri") %>% 
  group_by(conflict_dyad) %>% count(conflict_dyad, name="count") %>% arrange(-count) %>% head(n=10)

# Most are related to the Hema Lendu conflict, ADF/IS and FARDC. 


# Tanganyika

east %>% select(year, conflict_dyad, province, territory, deaths_total, deaths_civilian) %>% filter(province == "Tanganyika") %>% 
  group_by(conflict_dyad) %>% count(conflict_dyad, name="count") %>% arrange(-count) %>% head(n=10)

# Mostly related to the Batwa conflict and the CNPSC. 


# South Kivu

east %>% select(year, conflict_dyad, province, territory, deaths_total, deaths_civilian) %>% filter(province == "Sud Kivu") %>% 
  group_by(conflict_dyad) %>% count(conflict_dyad, name="count") %>% arrange(-count) %>% head(n=10)

# Most are related to FARDC, CNPSC and FDLR. 


# Mortality of incidents 
  
# Again, incidents in themselves don't say much about mortality. To take a look at how many deaths there are in total per province:
  
east %>% group_by(province) %>% summarise(total_deaths=sum(deaths_total), 
                                          total_civilian_deaths=sum(deaths_civilian),
                                          total_military_deaths=sum(deaths_military),
                                          total_unknown_deaths=sum(deaths_unknown), 
                                          perc_civ_deaths=total_civilian_deaths/total_deaths * 100, 
                                          perc_mil_deaths=total_military_deaths/total_deaths * 100,
                                          perc_unknown_deaths=total_unknown_deaths/total_deaths * 100,
) %>% arrange(-total_deaths)

# North Kivu is again in front with ca. double the total deaths than Ituri. However, in Nord Kivu the number of civilian to military deaths is relatively equalized at 48% civilian vs. 44% military. 
# In Ituri, the picture is different, with 76% of the deaths being civilian and only 8% military. On this metric, Ituri is actually not far behind North Kivu (10.285 vc 8775 deaths). 
# South Kivu, which has less than half the total number of deaths than Ituri, has a much higher military death rate at 57%, vs. 30% civilian. 
# In Tanganyika, the total number of deaths is much lower, and pretty balanced at 27% vs. 25% between civilian and military deaths. However, about 48% of deaths are unknown, by far the highest percentage. 


proportion_civilian <- east %>% group_by(year, province) %>% summarise(total_deaths=sum(deaths_total), 
                                                                       total_civilian_deaths=sum(deaths_civilian),
                                                                       total_military_deaths=sum(deaths_military))


ggplot(proportion_civilian, aes(x = year)) +
  geom_col(aes(y = total_deaths, fill = "Total Deaths"), position = "stack") +
  geom_col(aes(y = total_civilian_deaths, fill = "Civilian Deaths"), position = "stack") +
  scale_fill_manual(values = c("Total Deaths" = "steelblue", "Civilian Deaths" = "red")) +
  labs(title = "Total Deaths and Civilian Deaths by Year and Province",
       x = "Year",
       y = "Number of Deaths",
       fill = "Type of Deaths") +
  facet_wrap(~ province) +
  theme_minimal()


# Visualising the total number of deaths per province over time

ggplot(data=east) + 
  geom_col(mapping=aes(x=year, y=deaths_total, fill=province)) + 
  facet_wrap(~province)+
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  labs(title="Total deaths per province, 2003-2023", x="Year", y="Number of deaths", fill="Province")

# What becomes evident:  
  
# - Ituri had a huge spike around 2003 when the EU deployed Opération Artemis, but then calmed down until almost 2018. 
# - SK had a spike around 2009, then calmed down, but increased again starting in 2017. 
# - Tanganyika had one outbreak around 2016/2017, but was mostly quiet otherwise. 
# - North Kivu started with relatively low levels in the early 2000s, then had huge peaks around the CNDP and M23 crises in 2009 and 2013. 
# Throughout the 2010s, it has remained at relatively high levels, with a huge increase again towards 2020. 

# One quick look at number of total deaths for the 4 eastern plus the Kasai provinces, with contribution by province:  

congo_clean %>% filter(year >= 2003, province %in% c("Nord Kivu", "Sud Kivu", "Ituri", "Tanganyika", "Kasaï", "Kasaï-Central", "Kasaï-Oriental")) %>% group_by(year, province) %>%
  summarise(total_deaths=sum(deaths_total)) %>% 
  ggplot(aes(x=year, y=total_deaths, fill=province))+
  geom_col()+
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  labs(title="Total deaths per year in eastern DRC, 2000-2021", x="Year", y="Deaths per year", fill="Province")+
  theme_minimal()+
  theme(legend.position = "right")

# The graph clearly shows the magnitude of the crisis in Ituri in 2003 and North Kivu in 2009, but also, and in particular, 
# the magnitude of the Kasai crisis in 2017 (the year with with the highest number of deaths since the end of the Congo war) 
# as well as the current level of violence in the east since 2020.  


# Finally, a look at the historical development of deadliness by some notorious armed groups: 

congo_clean %>% group_by(year, armed_group) %>% na.omit(armed_group) %>% summarise(total_deaths=sum(deaths_total)) %>% 
  ggplot(aes(x=year, y=armed_group, colour=armed_group, size=total_deaths), alpha=0.6)+
  geom_point()+
  scale_color_brewer(palette="Dark2", direction=1)+
  scale_size_area(breaks = c(500, 1000, 1500, 2000, 2500, 3000), max_size = 10)+
  labs(title="Historical total deadliness of selected conflicts", x="Year", y="Armed group", colour="Armed group", size="Total Deaths")+
  theme_minimal()+
  guides(size = "none", color = guide_legend(reverse=TRUE))


# 4.3 North Kivu & Ituri, 2018-2023

# Finally, I want to have a closer look at North Kivu and Ituri from 2018 until today, and delve deeper into the regional division of conflict and the biggest conflict parties.


# North Kivu

east_recent <- congo_clean %>% filter(province %in% c("Nord Kivu", "Ituri", "Sud Kivu"), 
                                      date >= "2018-01-01") 


# Visualization of total deaths by territory

east_recent %>% filter(province=="Nord Kivu") %>%   
  ggplot(aes(x=year, y=deaths_total, fill=territory))+
  geom_col() + 
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  labs(title="Total deaths by territory, Nord Kivu", x="Year", y="Total Deaths")+
  theme_minimal()


# Total civilian deaths by territory 

east_recent %>% filter(year>=2018, province=="Nord Kivu") %>% 
  ggplot(aes(x=year, y=deaths_civilian, fill=territory))+
  geom_col() + 
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  labs(title="Civilian deaths by territory, Nord Kivu", x="Year", y="Total Civilian Deaths")

# The dominance of the territory of Beni is standing out, though in 2022 Rutshuru has increased a lot as well. Beni is a particularly dangerous zone for civilians.

# This is mostly due to ADF activity, with civilians increasingly being attacked over the last years:  
  
east_recent %>% filter(year>=2018, territory=="Beni") %>% group_by(province, territory, conflict_dyad) %>% 
  ggplot(aes(x=year, y=deaths_civilian, fill=conflict_dyad))+
  geom_col()+
  scale_fill_viridis(discrete=TRUE, option="D", direction=-1)+
  labs(title="Civilian deaths in Beni territory by conflict", x="Year", y="Total Civilian Deaths", fill="Conflict Dyad")


# In Rutshuru, there is not one dominant group, except for 2022 with a big uptick in deaths caused by M23

east_recent %>% filter(year>=2018, territory=="Rutshuru") %>% group_by(province, territory, conflict_dyad) %>% 
  ggplot(aes(x=year, y=deaths_civilian, fill=conflict_dyad))+
  geom_col()+
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  labs(title="Civilian deaths in Rutshuru territory by conflict", x="Year", y="Total Civilian Deaths", fill="Conflict Dyad")



# Ituri

# Visualization of total deaths by territory

east_recent %>% filter(year>=2018, province=="Ituri") %>% 
  ggplot(aes(x=year, y=deaths_total, fill=territory))+
  geom_col() + 
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  labs(title="Total deaths by territory, Ituri", x="Year", y="Total Deaths")

# Djugu starts as the territory with the biggest share, but Irumu is increasingly taking up a bigger share and overtaking Djugu since 2021. Besides, violence on a lower level also spreads into Mahagi and Mambasa. 

# Visualization of total civilian deaths 

east_recent %>% filter(year>=2018, province=="Ituri") %>% 
  ggplot(aes(x=year, y=deaths_civilian, fill=territory))+
  geom_col() + 
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  labs(title="Total deaths by territory, Ituri", x="Year", y="Total Deaths", fill="Territory")

# The picture is pretty similar for civilian deaths. 

# Having a look at which conflict is responsible for civilian deaths in those two territories: 
  
east_recent %>% filter(year>=2018, territory=="Djugu") %>% group_by(province, territory, conflict_dyad) %>% 
  ggplot(aes(x=year, y=deaths_civilian, fill=conflict_dyad))+
  geom_col()+
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  labs(title="Civilian deaths in Djugu territory by conflict", x="Year", y="Total Civilian Deaths", fill="Conflict Dyad")

# The vast majority is in one way or another caused by the Hema-Lendu conflict.

east_recent %>% filter(year>=2018, territory=="Irumu") %>% group_by(province, territory, conflict_dyad) %>% 
  ggplot(aes(x=year, y=deaths_civilian, fill=conflict_dyad))+
  geom_col()+
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  labs(title="Civilian deaths in Irumu territory by conflict", x="Year", y="Total Civilian Deaths")

# In Irumu, the situation is different. Here, as in Beni, most deaths are caused by ADF activity and the FARDC's # fight against them, and really started taking up in 2020. 


# Conflict dyads

east_recent %>% group_by(conflict_dyad) %>% summarise(total_death_by_conflict=sum(deaths_total)) %>% 
  arrange(-total_death_by_conflict) %>% head(n=10) %>% 
  ggplot(aes(x=reorder(conflict_dyad, -total_death_by_conflict), y=total_death_by_conflict, fill=conflict_dyad))+
  geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Deadliest conflict dyads in eastern Congo", x="Conflicts", y="Number of deaths", fill="Conflict Dyad")


--> hier schauen ob ich Balken noch einfärben kann nach M23, ADF usw.?


  
  # Deadliest conflicts in NK

filter(east_recent, province=="Nord Kivu") %>% group_by(conflict_dyad) %>% summarise(total_death_by_conflict=sum(deaths_total)) %>% 
  arrange(-total_death_by_conflict) %>% head(n=10) %>% 
  ggplot(aes(x=reorder(conflict_dyad, -total_death_by_conflict), y=total_death_by_conflict, fill=conflict_dyad))+
  geom_col()+
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Deadliest conflicts in North Kivu", x="Conflict dyad", y="Total deaths")

# Here, it is ADF by far, followed by M23 and FARDC. 


# Deadliest conflicts in Ituri

filter(east_recent, province=="Ituri") %>% group_by(conflict_dyad) %>% summarise(total_death_by_conflict=sum(deaths_total)) %>% 
  arrange(-total_death_by_conflict) %>% head(n=10) %>% 
  ggplot(aes(x=reorder(conflict_dyad, -total_death_by_conflict), y=total_death_by_conflict, fill=conflict_dyad))+
  geom_col()+
  scale_fill_viridis(discrete=TRUE, option="D", direction=1)+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(title="Deadliest conflicts in Ituri", x="Conflict parties", y="Total deaths", fill="Conflict Dyad")

# In Ituri, it is related to ADF and the Hema-Lendu conflict. 


# Dry vs. Rainy Season

# I wanted to check whether there are seasonal variations, which in DRC makes sense mostly on whether it's rain season or dry season. 

east %>% group_by(year, province, season) %>% summarise(t_d=sum(deaths_total),
                                                        t_c_d=sum(deaths_civilian)) %>% 
  ggplot(aes(x=year, y=t_d, fill=season))+
  geom_col(position="dodge")+
  facet_wrap(~province)+
  labs(title="Total deaths by season", x="Year", y="Total Deaths", fill="Season")

# Only based on the visualization, seasonality seems to not always have an impact on the number of deaths, 
# but in tendency, there are more deaths during the dry season. The notable exception is SK, with several years with more deaths
# during the rain season. While there seems to be a relatively strong correlation
# in Ituri and Tanganyika, in NK, it is sometimes relatively equal, in other years
# there are much more deaths during the dry season. 

--> include another correlation test here?


# Seasonality in Ituri by territory

# Since I am most confidend that my rain vs. dry season is actually correct for Ituri
# and there seems to be a relatively strong effect, I'll take a look at the
# division by territory, since the uptick of violence in 2018. 

east %>% group_by(year, province, territory, season) %>% filter(province=="Ituri", year >= 2018, territory!="") %>% 
  summarise(t_d=sum(deaths_total), t_c_d=sum(deaths_civilian)) %>% 
  ggplot(aes(x=year, y=t_d, fill=season))+
  geom_col(position="dodge")+
  facet_wrap(~territory)+
  scale_fill_brewer(palette="Paired")+
  labs(title="Seasonality of deaths in Ituri territories", x="Year", y="Total Deaths", fill="Season")





  
# 5. GEOMAPPING OF INCIDENTS 
 
install.packages("ggmap", dependencies = TRUE)

# Redoing the mapping part, when preparing for upload to GitHub, stamen maps was no longer supported by ggmap

# registering online with Stadiamap

register_stadiamaps(key="cea7b75d-e47c-40c5-854e-4008b8489ea1", write=TRUE)

# fetching and trying the underlying maps with different map styles 

bbox_NK <- c(left = 26.94, bottom = -2.12, right = 30.12, top = 1.06) # North Kivu map
get_stadiamap(bbox_NK, zoom=10, maptype="outdoors") %>% ggmap() # not that great, some tiles missing

bbox_rutshuru <- c(left = 28.66, bottom = -1.78, right = 29.8, top = -0.65)
get_stadiamap(bbox_rutshuru, zoom=10, maptype="stamen_toner_lite") %>% ggmap()

bbox_beni <- c(left = 28.689, bottom = -0.299, right = 30.16, top = 1.008)
get_stadiamap(bbox_beni, zoom=10, maptype="alidade_smooth") %>% ggmap()

bbox_SK <- c(left = 26.63, bottom = -5.1, right = 29.32, top = -1.64)
get_stadiamap(bbox_SK, zoom=10, maptype="stamen_terrain") %>% ggmap()

bbox_Ituri <- c(left = 27.17, bottom = 0.45, right = 31.51, top = 3.77)
get_stadiamap(bbox_Ituri, zoom=10, maptype="stamen_terrain_background") %>% ggmap() # not that great, some tiles missing

bbox_Ituri <- c(left = 27.17, bottom = 0.38, right = 31.51, top = 3.77)
get_stadiamap(bbox_Ituri, zoom=10, maptype="stamen_terrain_lines") %>% ggmap() # not very useful

bbox_SK <- c(left = 26.63, bottom = -5.1, right = 29.32, top = -1.64)
get_stadiamap(bbox_SK, zoom=10, maptype="stamen_toner") %>% ggmap()

# Stamen Toner Lite and Alidade Smooth look best and are the fastest. I'll try them. 


# Mapping North Kivu over the last 6 years

NK_loc <- east_recent %>% filter(province=="Nord Kivu", territory != "")


qmplot(longitude, latitude, color=territory, size=deaths_total, data = NK_loc, alpha=0.5, stroke=0.3, maptype = "alidade_smooth_dark")+
  ggtitle("North Kivu indicents 2018-2023")+ 
  scale_size_area(breaks=c(20, 40, 60, 80), max_size = 5)+
  scale_color_manual(values = brewer.pal(6, "Set1")) +  
  labs(size="Total deaths", colour="Territory", alpha="")+
  theme_minimal()+
  guides(alpha="none", erritory=guide_legend(override.aes = list(shape = 15)))+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())

# Unsurprisingly, the biggest concentration of incidents is in Beni and Rutshuru,
# followed by Masisi. Lubero, Walikale and Nyiragongo with far less incidents. 


# Taking a look at two hotspots: 1) M23 since 2021 in North Kivu, 2) ADF in Beni and southern Ituri. 


# M23

M23_loc <- east_recent %>% filter(province=="Nord Kivu", armed_group == "M23", territory!="", year >= 2020)

date_breaks <- seq(as.Date("2020-06-01"), max(M23_loc$date), by = "6 months")

qmplot(longitude, latitude, color=date, size=deaths_total, data = M23_loc, alpha=0.5, stroke=0.3, maptype = "alidade_smooth_dark")+
  ggtitle("M23 violence, 2020-2023")+ 
  scale_size_area(breaks=c(20, 40, 60, 80), max_size = 5)+
  scale_color_distiller(palette = "YlOrRd", direction = 1, 
                        breaks = date_breaks, 
                        labels = label_date("%b %Y")) + 
  guides(alpha="none", erritory=guide_legend(override.aes = list(shape = 15)))+
  labs(color="Date", size="Total deaths")
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())

# Shows the evolution, going from Rutshuru's border area with Rwanda and Uganda into the area west of Virunga National Park
# around Tongo and Kishishe, before descending into Masisi. However, I would have expected more
# incidents in total to be honest.


  
# ADF violence in northern North Kivu and southern Ituri
  
ADF_loc <- east_recent %>% filter(province %in% c("Nord Kivu", "Ituri"), armed_group == "ADF / IS", territory !="", territory!="Nyiragongo", territory!="Masisi") # filtering out Nyiragongoand Masisi because there were two individual incidents there, which prevent zooming in on the map
  
date_breaks_ADF <- seq(min(ADF_loc$date), max(ADF_loc$date), by = "6 months") # specify the date breaks for the legend

custom_date_format <- function(x) format(x, "%m/%y") # date_format for the date legend

qmplot(longitude, latitude, color=date, size=deaths_total, data = ADF_loc, alpha=0.5, 
       stroke=0.3, maptype = "stamen_toner_lite")+ 
  ggtitle("ADF violence NK & Ituri, 2018-2023")+ 
  scale_color_viridis(discrete = FALSE, option = "C", direction = 1, 
                        breaks = date_breaks_ADF, 
                        labels = custom_date_format) + 
  guides(alpha="none")+
  labs(color="Date", size="Total deaths")+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())

# Clearly shows the evolution of violence, starting around Beni city in 2018, spreading out along
# the road leading north into Ituri, but also south towards Butembo and east into more rural areas of Beni territory.



# Ituri

Ituri_loc <- east_recent %>% filter(province=="Ituri", territory != "", latitude > -1.2) # filtering out one erroneous value that is actually in North Kivu

qmplot(longitude, latitude, color=territory, size=deaths_total, data = Ituri_loc, alpha=0.5, 
       stroke=0.3, maptype = "stamen_toner_lite", extent="panel")+ 
  ggtitle("Ituri incidents, 2018-2023")+ 
  scale_size_area(breaks=c(20, 40, 60, 80, 100), max_size = 8)+
  scale_color_manual(values = brewer.pal(6, "Set1")) +  
  guides(alpha="none")+
  labs(color="Territory", size="Total deaths")+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())


# different map type and different scaling of dots

max_deaths <- max(Ituri_loc$deaths_total)

qmplot(longitude, latitude, color=territory, size=deaths_total, data = Ituri_loc, alpha=0.5, 
       stroke=0.3, maptype = "alidade_smooth_dark", extent="panel")+
  ggtitle("Ituri indicents 2018-2023")+ 
  scale_size_area(breaks = c(10, 30, 50, 80, 120, max_deaths), max_size = 10)+
  scale_color_manual(values = brewer.pal(6, "Set1")) +  
  labs(size="Total deaths", colour="Territory", alpha="")+
  theme_minimal()+
  guides(alpha="none", erritory=guide_legend(override.aes = list(shape = 15)))+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())

# South Kivu

SK_loc <- east_recent %>% filter(province=="Sud Kivu", territory != "", latitude < 0.38)

aspect_ratio <- 2

qmplot(longitude, latitude, color=territory, size=deaths_total, data = SK_loc, alpha=0.5, 
       stroke=0.3, maptype = "alidade_smooth_dark", extent="panel")+
  coord_fixed(ratio = 1 / aspect_ratio) + 
  ggtitle("SK indicents 2018-2023")+ 
  scale_size_area(breaks=c(20, 40, 60, 80, 100), max_size = 5)+
  scale_color_manual(values = brewer.pal(9, "Paired")) +  
  labs(size="Total deaths", colour="Territory", alpha="")+
  theme_minimal()+
  guides(alpha="none", erritory=guide_legend(override.aes = list(shape = 15)))+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())

# different map type

qmplot(longitude, latitude, color=territory, size=deaths_total, data = SK_loc, alpha=0.5, 
       stroke=0.3, maptype = "stamen_toner_lite", extent="panel")+
  ggtitle("SK indicents 2018-2023")+ 
  scale_size_area(breaks=c(20, 40, 60, 80, 100), max_size = 5)+
  scale_color_manual(values = brewer.pal(9, "Paired")) +  
  labs(size="Total deaths", colour="Territory", alpha="")+
  theme_minimal()+
  guides(alpha="none", erritory=guide_legend(override.aes = list(shape = 15)))+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())


# Comparison of ADF lethality for civilians over different years



qmplot(longitude, latitude, size=deaths_civilian, color=territory, data = ADF_loc, alpha=0.5, 
       stroke=0.3, maptype = "alidade_smooth_dark", extent="panel")+ 
  ggtitle("ADF lethality for civilians, 2018-2023")+ 
  scale_fill_viridis(discrete = FALSE, option = "C", direction = 1)+
  scale_size_area(breaks=c(20, 40, 60, 80, 100), max_size = 5)+
  guides(alpha="none")+
  labs(color="Territory", size="Civilian deaths")+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank()) +
  facet_wrap(~year)




# Comparison of ADF vs. Hema-Lendu lethality for civilians in Ituri

Ituri_conflicts <- east_recent %>% filter(province=="Ituri", armed_group %in% c("ADF / IS", "Hema-Lendu"), territory!="")
                                         
qmplot(longitude, latitude, size=deaths_civilian, color=armed_group, data = Ituri_conflicts, alpha=0.5, 
       stroke=0.3, maptype = "alidade_smooth_dark", extent="panel")+ 
  ggtitle("ADF vs. Hema-Lendu lethality in Ituri, 2018-2023")+ 
  scale_fill_viridis(discrete = FALSE, option = "C", direction = 1)+
  scale_size_area(breaks=c(20, 40, 60), max_size = 5)+
  guides(alpha="none")+
  labs(color="Conflict", size="Civilian deaths")+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank()) +
  facet_wrap(~year)                                       


# Evolution of ADF lethality against civilians in Beni territory

ADF_NK <- east_recent %>% filter(province=="Nord Kivu", armed_group=="ADF / IS", territory=="Beni")

qmplot(longitude, latitude, size=deaths_civilian, color=armed_group, data = ADF_NK, alpha=0.5, 
       stroke=0.3, maptype = "stamen_toner", extent="panel")+ 
  ggtitle("ADF lethality for civilians in NK, 2018-2023")+ 
  scale_size_area(breaks=c(20, 40), max_size = 5)+
  guides(alpha="none", color="none")+
  labs(size="Civilian deaths", color="")+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank()) +
  facet_wrap(~year)





# Mapping the militaries active in DRC

# As a proxy to estimate the deployment of military forces in eastern DRC, especially FARDC, 
# I will map the incidents where they were one of the conflict parties. While this does not 
# precisely indicate their deployment, it nonetheless gives an indication about where they are fighting. 


# Creation of a new column "military_involvement" based on whether side a or b was a military

east_recent <- east_recent %>% mutate(military_involvement = case_when(
            side_a == "RDF" | side_b == "RDF" ~ "RDF", 
            side_a == "UPDF" | side_b == "UPDF" ~ "UPDF",
            side_a == "FDNB" | side_b == "FDNB" ~"FDNB", 
            side_a == "FARDC" | side_b == "FARDC" ~"FARDC",))

military <- east_recent %>% filter(military_involvement %in% c("FARDC", "RDF", "UPDF", "FDNB")) 

military %>% group_by(province) %>% summarise(total_death=sum(deaths_total),
                                 tot_civ_death=sum(deaths_civilian), 
                                 tot_mil_death=sum(deaths_military), 
                                 perc_civ_death=(sum(deaths_civilian)/sum(deaths_total)*100), 
                                 perc_mil_death=sum(deaths_military)/sum(deaths_total)*100) %>% 
                                 arrange(-total_death)


# A little bit surprisingly, in all provinces the percentage of civilian deaths is actually
# relatively low compared to percentage of military deaths, in the incidents involving a military. 


# Mapping incidents involving the military in North Kivu 

NK_military <- east_recent %>% filter(province=="Nord Kivu", military_involvement %in% c("RDF", "FARDC", "UPDF", "FDNB"), territory!="")

qmplot(longitude, latitude, color=military_involvement, size=deaths_total, shape=territory, 
       data = NK_military, alpha=0.5, stroke=0.3, maptype = "alidade_smooth_dark", extent="panel")+
  ggtitle("Incidents involving the military in NK, 2018-2023")+ 
  scale_size_area(breaks=c(20, 40, 60, 80), max_size = 5)+
#  scale_color_manual(values = brewer.pal(6, "Set1")) +  
  labs(size="Total deaths", color="Military", shape="Territory", alpha="")+
  guides(alpha="none")+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())+
  facet_wrap(~year)

# The visualization shows relatively little incidents (thus presence?) involving FARDC at all in NK in 2018, 
# however a number of incidents involving UPDF in Beni as well as RDF in Rutshuru. 
# In the following years, the footprint of FARDC seems to grow considerably in Beni and later in
# Rutshuru & Masisi. In 2022 and 2023, RDF presence in Rutshuru appears to grow, too. 
# I am a little surprised that the joint FARDC-UPDF operation "Shujaa" in the Grand Nord of NK
# has not resulted in more incidents involving UPDF. Not sure whether those incidents simply 
# haven't been reported or whether they are attributed to FARDC, since it is a joint operation. 


# Mapping in Ituri

Ituri_military <- east_recent %>% filter(province=="Ituri", military_involvement %in% c("RDF", "FARDC", "UPDF", "FDNB"), territory!="")

qmplot(longitude, latitude, color=military_involvement, size=deaths_total, shape=territory, 
       data = Ituri_military, alpha=0.5, stroke=0.3, maptype = "alidade_smooth_dark", extent="panel")+
  ggtitle("Incidents involving the military in Ituri, 2018-2023")+ 
  scale_size_area(breaks=c(20, 40, 60, 80), max_size = 5)+
  labs(size="Total deaths", color="Military", shape="Territory", alpha="")+
  guides(alpha="none")+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())+
  facet_wrap(~year)

# There seems to have been basically no presence in Ituri between 2018 and 2020, with a growing footprint
# in 2021 and 2022 in Irumu, most likely in relation to ADF spreading into that area.
# In 2023, that seems to have reversed and the presence diminished again, most likely because of 
# FARDC redeploying to North Kivu to fight against M23. 
# What is striking is the low number of incidents in Djugu, which stands in stark contrast
# to the escalating violence there due to the Hema-Lendu conflict. 


# Mapping in South Kivu

SK_military <- east_recent %>% filter(province=="Sud Kivu", military_involvement %in% c("RDF", "FARDC", "UPDF", "FDNB"), territory!="")

qmplot(longitude, latitude, color=territory, size=deaths_total, shape=military_involvement, 
       data = SK_military, alpha=0.5, stroke=0.3, maptype = "alidade_smooth_dark", extent="panel")+
  ggtitle("Incidents involving the military in SK, 2018-2023")+ 
  scale_size_area(breaks=c(20, 40, 60, 80), max_size = 5)+
  labs(size="Total deaths", color="Territory", shape="Military", alpha="")+
  guides(alpha="none")+
  theme(axis.title = element_blank(),     # Hide axis titles
        axis.text = element_blank(),      # Hide axis text
        axis.ticks = element_blank())+
  facet_wrap(~year)

# There seems to be very little FARDC activity compared to NK and Ituri. 
# Besides, there's no real geographic focus apparent, with sporadic incidents 
# all over the province. 2019 and 2020 saw some RDF engagements in the north parts
# of South Kivu. 
# While there had been some incidents involving the Burundian army between 2018 and 2021, 
# these were mostly concentrated on the Plaine de la Ruzizi. In 2022, the Burundian army 
# started its activities in the Hauts Plateaux of Uvira and Mwenga, leading to more incidents
# in that area. 
# In 2023, there have been (surpringly) few incidents in SK. 



# 6. Preparation of Tableau Dataset

# I want to build a dashboard and visualize some of the info in Tableau. 
# In the shapefile I found for displaying geodata for Congo, some of the province
# names are written a bit differently, so I'll prepare another dataframe with the
# corrected names and reduce some of the columns I won't need to make life easier in Tableau.
# The geofile can be found here: https://data.humdata.org/dataset/cod-ab-cod?

View(east)

unique_provinces <- unique(congo_clean$province)
unique_provinces <- sort(unique_provinces)
# comparing province names in the dataset and the geofile
print(unique_provinces)

# Changing province names to match to geofile

dataset_tableau <- congo_clean %>% 
  mutate(province = case_when(
    province == "Sud Kivu" ~ "Sud-Kivu", 
    province == "Nord Kivu" ~ "Nord-Kivu", 
    province == "Mai-Ndombe" ~ "Maï-Ndombe",
    province == "Kongo Central" ~ "Kongo-Central", 
    province == "Kasaï Oriental" ~ "Kasaï-Oriental", 
    province == "Kasaï-Central" ~ "Kasaï-Central", 
    province == "Haut-Uélé" ~ "Haut-Uele", 
    province == "Équateur" ~ "Equateur",
    province == "Bas-Uélé" ~ "Bas-Uele",
    TRUE ~ province
  ))


# Adding the militaries column

dataset_tableau <- dataset_tableau %>% 
  mutate(military_involvement = case_when(
    side_a == "RDF" | side_b == "RDF" ~ "RDF", 
    side_a == "UPDF" | side_b == "UPDF" ~ "UPDF",
    side_a == "FDNB" | side_b == "FDNB" ~ "FDNB", 
    side_a == "FARDC" | side_b == "FARDC" ~ "FARDC",
    TRUE ~ "none"  
  ))

# Adding a new column "Civilians" to indicate when either "conflict party" was Civilians

dataset_tableau <- dataset_tableau %>% 
  mutate(civilians = case_when(
    side_a == "Civilians" | side_b == "Civilians" ~ "Civilians",
    TRUE ~ "No Civilians" 
  ))

# Adding a new column "President" indicating who was president at the time - I could have thought about that for the previous analysis. 

dataset_tableau <- dataset_tableau %>%
  mutate(president = case_when(
    date >= as.Date("1965-11-24") & date <= as.Date("1997-05-16") ~ "Mobutu Sese Seko",
    date >= as.Date("1997-05-17") & date <= as.Date("2001-01-16") ~ "Laurent-Désiré Kabila",
    date >= as.Date("2001-01-17") & date <= as.Date("2019-01-24") ~ "Joseph Kabila",
    date >= as.Date("2019-01-25") ~ "Félix Tshisekedi",
    TRUE ~ "Other"))

# Adding a column for "état de siège", indicating whether for North-Kivu and Ituri, it was already declared 

dataset_tableau <- dataset_tableau %>%
  mutate(etat_de_siege = case_when(
    province %in% c("Nord-Kivu", "Ituri") & date >= as.Date("2021-05-06") ~ "Etat de siège", 
    TRUE ~ "Civilian govt")
  )

# Dropping unnecessary columns

dataset_tableau <- dataset_tableau %>%
  select(-year, -conflict_dset_id, -dyad_dset_id, -side_a_dset_id, 
         -side_b_dset_id, -where_prec, -geom_wkt, -priogrid_gid, 
         -event_clarity, -date_prec, -day, -month)


# Last check

str(dataset_tableau)

# Looks good, so I'll export as a CSV file: 

write.csv2(dataset_tableau, "C:\\Users\\UserNA4944\\OneDrive - Deutsche Gesellschaft für Internationale Zusammenarbeit (GIZ) GmbH\\Privat\\Data Analytics\\Projects\\Eastern Congo Security Incidents\\dataset_tableau.csv", row.names=F)


# 7. Conclusion

# I think this project was a really cool and interesting exercise as a Capstone Project. It allowed me to finally apply the R skills I had learned during the course, 
# learn new R syntax, try out different forms of visualisations and palettes and spend a loooot of time troubleshooting and finding fixes for stuff that didn't work, 
# in the beginning with the help of internet fora and, later on, ChatGPT, which sped up the process considerably. This was also my first occasion
# of really using ChatGPT, which was pretty cool and made me much more aware of its potential. 

# The analysis is nonetheless not perfect and not as coherent and always as visually appealing as it might have been. However, this was in part on purpose, to allow me to play with different colour palettes, map styles etc. 
# In addition, the insight derived is only as good as the quality of data. Being curated by a European university, I consider the reliability of the data collection itself
# as rather high, however it is certain that the data is not complete given the context of eastern DRC and includes entries of different levels of reliability. 
# I found that for a couple of entries, the geolocation and the definition of province or territory were not entirely correct. 
# In conclusion I would say "the usual caveats apply" for data on DRC. To simplify things, I have ignored the info on the degree of precision or reliability of time and location that is included in
# the dataset and simply treated each entry the same. This will also falsify or skew the results somewhat. 
# Besides, the dyadic nature of the conflict dyads and the fact that armed groups sometimes change names, split up into several wings or that several groups
# might be listed together as "side a/b" has also complicated things a bit. 
# Nonetheless, I am pretty happy with the result. 

# To finish, I would like to derive a few conclusions and "actionable insights" based on what the data seems to be telling: 

# 1. The conflict context in eastern DRC has fluctuated considerably over the last 20 years. Over the last couple of years, the situation got increasingly worse, especially in North Kivu and Ituri. 
# 2. In the 10-15 years of relative calm in Ituri, the opportunity to work on underlying conflict drivers seems to have been squandered. The explosion of inter-communal violence shows 
# that the potential for conflict had remained latent, though it didn't show as openly. This is a serious missed chance for stabilisation in eastern DRC. 
# 3. In the ADF territory in Ituri and ADF, civilians bear the absolute brunt of violence and deadliness. In these areas, more should be done to protect civilians. 
# 4. It seems that in the region of the Hema-Lendu conflict in Ituri, there has been little presence of the FARDC to try to put a lid on the conflict. This has probably been further 
# complicated by the emergence of M23 and the redeployment of army units to North-Kivu. More could be done in that area to try to manage the conflict and reduce violence. 
# 5. In North Kivu, the proportion of military deaths (compared to civilians in Ituri) and incidents involving the military is much higher. In this region, a stronger focus 
# could be put on trying to sensitize the military about the protection of civilians.
# 6. M23 dominates the headlines and the political discourse in DRC, besides having led to a huge humanitarian crisis due to almost a million people it has displayed. 
# When looking at deadliness itself, M23 is dwarfed by ADF and the Hema-Lendu conflict in northern North-Kivu and Ituri, which wreak havoc on civilians, especially ADF. 
# 6. Due to the mugh lower level of violence and deadliness, South Kivu seems like the most conducive place for stabilisation and the area that could best support the ongoing MONUSCO withdrawal. 
# Given this, MONUSCO will start withdrawing here earliest, planned for early 2024. Besides, in contrast to Ituri and North Kivu, the civilian government that is still in place
# (in contrast to North-Kivu and Ituri, which are under military control due to the état de siège) potentially offers a more legitimate partner for international actors, 
# which in part cannot cooperate with military authorities (e.g. the World Bank). Thus, efforts at stabilisation and DDR seem most likely to succeed there and should be scaled up, 
# especially to make up for the reduced UN presence once MONUSCO starts withdrawing.
# 7. Last but not least, I will try to use the dataset_tableau to build an interactive dashboard in Tableau. 
