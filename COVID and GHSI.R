
#----- the first part -- data preparation --------------------------------------
#rm(list = ls())
data_countries <- `2021.11.01_owid.covid`
data_index <- GHSIndex2019
dex2019

# Installing and Loading packages
install.packages("arsenal")
install.packages("daff")
library(daff)
library(arsenal)
library(readxl)

# Loading and Reading Data

getwd()
data_countries = read.csv("2021-11-01_owid-covid.csv")
head(data_countries,5)
data_index = read_excel("GHSIndex2019.xlsx", sheet = 3) 
head(data_index)

# Filtering only variables for Rapid Response
data_rapid_response <- data_index[ , c(1,2,97:139)]

#merging 2 data sets: 

#before merging we need to detect why 228 countries are in the first file  and 195 in the second, what is the reason for differencies           lets see how many of them match and then detect the reasons for differencies
# We combine them to find the number of similar list

combined = merge(data_countries,data_rapid_response,by.x="location",by.y="Country")# 177 names are the same

# in the location column of file ""2021-11-01_owid-covid.csv"we can see some extra information (other than country names), and all these extra rows have blanks in the first column, so we delete all blanks in the first column to get rid of this extra info

data_countries=data_countries[!(is.na(data_countries$continent) | data_countries$continent ==""), ] # decreased to 215

# the following function allows us to detect list of different countries in these two datastets before final merging 
# summary of countries that are different in the second file from the 1st one are
summary(comparedf(data_countries, data_rapid_response, by.x="location",by.y="Country"))# we will manually check for spelling differences

# we manually compare those different names of the countries to the other list and correct the spellings if it is the reason for differences

data_countries$location[c(36, 44, 53, 47, 52, 64, 107, 128, 168, 163, 164, 165, 194)]= c("Cabo Verde", "Congo (Brazzaville)", "Congo (Democratic Republic)" , "Côte d'Ivoire", "Czech Republic" , "eSwatini (Swaziland)","Kyrgyz Republic", "Micronesia", "São Tomé and Príncipe", "St Kitts and Nevis", "St Lucia", "St Vincent and The Grenadines", "Timor-Leste")

summary(comparedf(data_countries, data_rapid_response, by.x="location",by.y="Country")) # now the difference is only for countries which are listed either in one or the other file, not because of the spelling difference. 


# now we start combining the files. 

data_combined = merge(data_countries,data_rapid_response,by.x="location",by.y="Country") # combined data consists of 177 observations, while we have 215 countries in one file and 195 in the second. 
head(data_combined) # 188 matching combined file 


#we will focus in this project on "fully vaccinated" persons and ignore just "vaccinated". Below we will check if there are any countries which reported just one of those variables and thus we could miss that data
colnames(data_countries)
dim(data_countries[is.na(data_countries$people_fully_vaccinated),c(2,14,15)])
dim(data_countries[is.na(data_countries$people_vaccinated)&is.na(data_countries$people_fully_vaccinated),c(2,14,15)])
#only Luxembourg reported just "vaccinated" people but did not report "fully vaccinated". This means that we can ignore "vaccinated" variable

# creating people fvaccinated per million variable for a more cogerent analysis controlling for population size
data$fvaccinated_per_M <- data$people_fully_vaccinated/ (data$population/1000000)
data$vaccinated_per_M <- data$people_vaccinated/ (data$population/1000000)


#saving the merged data - optional step 
#setwd("C:/Users/user/Downloads/")
#getwd()
#write.csv(x= data_combined,
#          file="rapid_response_data.csv")

#----- the second part -- data description --------------------------------------

#loading libraries

library(tidyr)
library(ggplot2)

#getting data and environment ready
#rm(list = ls())
#for the convinience we will create a dataframe with shorter name
data <- data_combined
# or 
data <- rapid_response_data #if it was used from presaved file

#descriptive stats
str(data)
summary(data)

#Table 1 - Summary statistics of main continuous variables
#nice table of descriptive stats

library("stargazer")
mydata = cbind(data[,2], data[,3], data[,18],data[,10],data[,17],data[,13],data[,16], data[,15], data[,8])
head(mydata)
stargazer(mydata, median = TRUE, column.sep.width = "15pt", type = 'html',
     title = "Summary statistics of continous variables")

#Picture 1 - Variables distribution - whole dataset compilation
#distribution of all our variables
data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


#----- the third part -- Covid-19 data analysis --------------------------------------

#rm(list = ls())

# Task a. Which of indicators from your category of indicators is the most important when fighting covid
#pandemic? Try to examine and explain the relationship and express its strength.

#we will crate a matrix of scatterplots to quickly estimate if there are any linear patterns and correlations in the dataset
#Picture 2 - Paired variables scatter plots  - whole dataset compilation

data.numeric <- data[,sapply(data, is.numeric)]
pairs(data.numeric[,1:18])


#from the matrix of scatterplots we see following relationships:


#correlation = 0.981 for population ~ people vaccinated
cor(data$population, data$people_vaccinated, use = "complete.obs")

#correlation= 0.877 for population ~ deaths 
cor(data$population, data$total_deaths, use = "complete.obs")

#correlation = 0.43 for cases/m ~ GHSI
cor(data$total_cases_per_million, data$GHSI, use = "complete.obs")

#correlation= -0.327 for hospital patients/m ~ rapid response
cor(data$hosp_patients_per_million, data$`3) RAPID RESPONSE TO AND MITIGATION OF THE SPREAD OF AN EPIDEMIC`, use = "complete.obs")

#correlation= -0.307 for hospital patients/m ~ GHSI 
cor(data$hosp_patients_per_million, data$GHSI, use = "complete.obs")

#correlation = -0.109 for cases/m ~ people fully vaccinated 
cor(data$total_cases_per_million, data$people_fully_vaccinated, use = "complete.obs")



# Task b. Aggregate your data per continent and describe and visualize indicators and pandemic situation of whole
# continents.
# Which continent fight the covid pandemic the best? Is that caused by excellent performance of your
# indicators or maybe it is thanks to vaccination or thanks to something completely else?

#continents perfomance visualization 

#will create a file to study continents
  continent = cbind(data[,2], data[,3], data[,18],data[,10],data[,17],data[,13],data[,16])

#will clean the data
  library(tidyr)
continent <-  continent %>% drop_na()

continent <- aggregate(continent, by = list(continent$continent), FUN = mean)
# we will rename response variable for our convinience
continent$response <- continent$`3) RAPID RESPONSE TO AND MITIGATION OF THE SPREAD OF AN EPIDEMIC`
# Figures 1, 2, 3 
# Which continent fights the Covid pandemic the best?    
  

#Relationship between vaccination and cases in different continents

#install.packages("ggrepel")
library(ggplot2)
library(ggrepel)

ggplot(continent, aes(x = total_cases_per_million, y = people_fully_vaccinated, label =  Group.1)) +
  geom_point(alpha = 20)+
  labs(
    y = "People fully vaccinated", 
    x = "Total cases per million",
    title = "Which continent fight the covid pandemic the best?",
    subtitle = "Relationship between vaccination and cases",
    legend.title = "Continents",
    caption = "Source: https://ourworldindata.org/covid-deaths"
  )+
  ggrepel::geom_text_repel()

#Relationship between vaccination and deaths in different continents

ggplot(continent, aes(x = total_deaths_per_million, y = people_fully_vaccinated, label =  Group.1)) +
  geom_point(alpha = 20)+
  labs(
    y = "People fully vaccinated", 
    x = "Total deaths per million",
    title = "Which continent fight the covid pandemic the best?",
    subtitle = "Relationship between vaccination and deaths",
    caption = "Source: https://ourworldindata.org/covid-deaths"
  )+
  ggrepel::geom_text_repel()

#Relationship between rapid response to and mitigation of the spread of an epidemic and number of cases in different continents

ggplot(continent, aes(x = total_cases_per_million, y = response, label =  Group.1)) +
  geom_point(alpha = 20)+
  labs(
    y = "Response to the epidemic", 
    x = "Total cases per million",
    title = "Which continent fight the covid pandemic the best?",
    subtitle = "Relationship between response to the spread of an epidemic and cases",
    caption = "Source: https://ourworldindata.org/covid-deaths"
  )+
  ggrepel::geom_text_repel()

#----------------------------------
#World maps visualization
#-----------------------------------
#Global rapid response map


#getting world data

world <- map_data("world")

data_world <- data

# adding key variable for merge
data_world$region <- data_world$location


#we will rename the countries that have different spelling
data_world$region[c(179, 180, 91, 40, 39, 57)] <- c("UK", "USA", "Kyrgyzstan", "Democratic Republic of the Congo", "Republic of Congo", "Swaziland")


# merging the data and the map and getting rid of Antarctica
install.packages("tidytable")
library("tidytable")
world_data <- left_join(world, data, by = "region") %>% 
  filter(region != "Antarctica")

#we will check which countries did not merge successfully

summary(comparedf(world, data_world, by = "region")) #only small countries are left and countries which have NA's for rapid response GHS Index


#Figure 4
#chart of the world for total deaths

ggplot(world_data, aes(long, lat, group = group, fill = world_data[,18])) +
  geom_polygon(color = "gray90", size = 0.05 ) +
  coord_quickmap() +
  labs(
    fill = "Deaths (per M)",
    title = "Total deaths (per M) around the world",
    caption = "Source: https://ourworldindata.org/covid-deaths"
  ) +
  scale_fill_viridis_c(na.value = "white", direction = -1) +
  theme_bw() +
  theme(
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.spacing=unit(0, "lines"),
    plot.background=element_blank(),
    legend.justification = c(0,0),
    legend.position = c(0,0)
  )


#Figure 5 
#chart of the world for total vaccination

ggplot(world_data, aes(long, lat, group = group, fill = world_data[,66])) +
  geom_polygon(color = "gray90", size = 0.05 ) +
  coord_quickmap() +
  labs(
    fill = "People fully vaccinated (per M)",
    title = "Total fully vaccinated people (per M) around the world",
    caption = "Source: https://ourworldindata.org/covid-deaths"
  ) +
  scale_fill_viridis_c(na.value = "white", direction = -1) +
  theme_bw() +
  theme(
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.spacing=unit(0, "lines"),
    plot.background=element_blank(),
    legend.justification = c(0,0),
    legend.position = c(0,0)
  )


#chart of the world for number of cases

ggplot(world_data, aes(long, lat, group = group, fill = world_data[,15])) +
  geom_polygon(color = "gray90", size = 0.05 ) +
  coord_quickmap() +
  labs(
    fill = "Number of cases (per M)",
    title = "Total number of COVID-19 cases (per M) around the world",
    caption = "Source: https://ourworldindata.org/covid-deaths"
  ) +
  scale_fill_viridis_c(na.value = "white", direction = -1) +
  theme_bw() +
  theme(
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.spacing=unit(0, "lines"),
    plot.background=element_blank(),
    legend.justification = c(0,0),
    legend.position = c(0,0)
  )




# Task c. Based on 2021-11-01_owid-covid.csv data - does vaccination helps improve pandemic situation? Support
#your claim with numbers/graphs. In which area (cases, ICU, deaths.) is the improvement the strongest
#and where it is the weakest? Does it apply universally on every country? 
#Or are there any countries (or even continents) where vaccination vs. pandemic behaves radically differently 
#comparing to the majority?

#plot devided by continents and countries
#install.packages("gapminder")
library("gapminder")

#we will clean the data before making a new graph
data1 <- data %>% drop_na(continent, total_deaths_per_million, people_fully_vaccinated)

#Figure  7
#Does vaccination help to reduce number of deaths?
library("gapminder")
ggplot(data1, aes(x = fvaccinated_per_M, y= total_deaths_per_million, size = population, color = location, label =  location)) +
  geom_point(alpha = 0.8) +
  scale_size(range = c(2, 12), guide = "none") +
  scale_color_manual(values = country_colors, guide = "none") +
  facet_wrap(~continent)+
  labs(
    y = "Total deaths per million",
    x = "Number of people fully vaccinated per million",
    title = "Does vaccination help to reduce number of deaths?",
    subtitle = "The size of points corresponds to the population size",
    caption = "Source: https://ourworldindata.org/covid-deaths")+
  ggrepel::geom_text_repel()

#Global overview
#Figures 8, 9, 10

# visualization cases~vaccination
ggplot(data, aes(x = fvaccinated_per_M, y = total_cases_per_million)) +
  geom_point(alpha = 20) +
  labs(
    y = "Total cases per million", 
    x = "Fully vaccinated persons per million",
    title = "Does vaccination help to reduce number of cases?",
    caption = "Source: https://ourworldindata.org/covid-deaths" 
    )+
  geom_smooth(method='lm')+
  geom_text_repel(data = subset(data, total_cases_per_million > 150000 | fvaccinated_per_M > 750000), aes(label = location))


# visualization ICU~vaccination
ggplot(data, aes(x = fvaccinated_per_M, y = icu_patients_per_million, label = location)) +
  geom_point(alpha = 20) +
  labs(
    y = "Total number of ICU patients per million", 
    x = "Fully vaccinated persons per million",
    title = "Does vaccination help to reduce number of ICU patients?",
    caption = "Source: https://ourworldindata.org/covid-deaths" 
  )+
  geom_smooth(method='lm')+
  ggrepel::geom_text_repel()

#visualization deaths~vaccination

ggplot(data, aes(x = fvaccinated_per_M, y = total_deaths_per_million)) +
  geom_point(alpha = 20) +
  labs(
    y = "Total deaths per million", 
    x = "Fully vaccinated persons per million",
    title = "Does vaccination help to reduce number of deaths?",
    caption = "Source: https://ourworldindata.org/covid-deaths" 
  )+
  geom_smooth(method='lm')+
  geom_text_repel(data = subset(data, total_deaths_per_million > 3000
                                | fvaccinated_per_M > 750000), aes(label = location))



#----- the forth part -- GHS Index analysis --------------------------------------


# _______Descriptive Analysis of GHS index _____

#1. extract GHSI
library(dplyr)

GHSI=data %>%
  select(location, GHSI)
head(GHSI)
summary(GHSI)


# 2. Some key interest to be highlighted (top 5 and lower 5 countries)

head(GHSI[order(GHSI$GHSI,decreasing=TRUE),],n=5)
head(GHSI[order(GHSI$GHSI,decreasing=FALSE),],n=5)
Key.Country=merge(head(GHSI[order(GHSI$GHSI,decreasing=TRUE),],n=5),head(GHSI[order(GHSI$GHSI,decreasing=FALSE),],n=5),all=TRUE)$location

# The histogram shows us a distribution of GHSI index. We can see that most of the countries have GHSI index in betweeen levels of 25-50. Top 5 countries with high GHSI index are Australia, Canada, Netherlands, UK and USA wheareas countries with the lowest level of GHSI index are Kiribati, Marshall Islands, Somalia, Yemen, Equatorial Guinea. 


install.packages("ggplot2")
library(ggplot2)
library(ggpubr)
gghistogram(GHSI, x = "GHSI", y = "..count..",
            xlab = "GHSI Index",
            ylab = "Number of Countries",
            fill = "lightgray", color = "black",
            label = "location", label.select = Key.Country, repel = TRUE,
            font.label = list(color= "GHSI"),
            xticks.by = 10, # Break x ticks by 20
            gradient.cols = c("blue", "red"),
            legend = c(0.7, 0.6),                                 
            legend.title = ""       # Hide legend title
)

#world map of GHSI 

# classify countries 'Minimal preparedness'  ,'average level preparedness' , "high level of preparedness" 


quantile(GHSI$GHSI, probs =seq(0, 1, 1/3) , na.rm = FALSE) # getting quarantines to divide into groups

GHSI$level=NA #adding a column level

GHSI$level<- cut(GHSI$GHSI, breaks = c(-Inf, 32.7, 44.3, Inf), 
              labels = c("Minimal "," average ", "high "))

head(GHSI)
barplot(table(GHSI$level),labels=GHSI)
?barplot()

#5 Plot results on the map

library(ggplot2)
library(dplyr)
world <- map_data("world")
head(world)
world %>%
  merge(GHSI, by.x = "region", by.y = "location", all.y = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = level)) + geom_polygon()




#___________________________________

# Figure 6 - Comparative ranking charts
#Top countries for the number of cases, vaccinated people and deaths
#___________________________________

#extract data
library(dplyr)

head(data)
data2=data %>%
  select(location, GHSI,total_cases_per_million,total_deaths_per_million, people_fully_vaccinated, GHSI)
summary(data2)

data2 =na.omit(data2) 


# plot top 20 GHSI countries
dataGHSI=head(data2[order(data2$GHSI,decreasing=TRUE), ],20)
dataGHSI


library(scales)
theme_set(theme_classic())

ggplot(dataGHSI, aes(x=location,, y= GHSI)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=location, 
                   xend=location, 
                   y=min( GHSI), 
                   yend=max( GHSI)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="TOP 20 Countries with GHSI Index", 
    
       caption="source: GHSI 2019") +  
  coord_flip()

# plot top 20  total_cases_per_million
data.cases=head(data2[order(data2$ total_cases_per_million,decreasing=TRUE), ],20)


ggplot(data.cases, aes(x=location,, y= total_cases_per_million)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=location, 
                   xend=location, 
                   y=min( total_cases_per_million), 
                   yend=max( total_cases_per_million)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="TOP 20  total_cases_per_million", 
       
       caption="data.cases") +  
  coord_flip()


# plot top 20 total_deaths_per_million

data.death=head(data2[order(data2$total_deaths_per_million,decreasing=TRUE), ],20)


ggplot(data.death, aes(x=location,, y= total_deaths_per_million)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=location, 
                   xend=location, 
                   y=min( total_deaths_per_million), 
                   yend=max( total_deaths_per_million)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="TOP 20  total_deaths_per_million", 
       
       caption="datadeath") +  
  coord_flip()

# plot top 20 people_fully_vaccinated ------


data.vaccinated=head(data2[order(data2$people_fully_vaccinated/1000000,decreasing=TRUE), ],20)

ggplot(data.vaccinated, aes(x=location,, y=people_fully_vaccinated)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=location, 
                   xend=location, 
                   y=min( people_fully_vaccinated), 
                   yend=max( people_fully_vaccinated)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="TOP 20  people_fully_vaccinated", 
       
       caption="datadeath") +  
  coord_flip()

#Figure 14
#chart of the world for rapid response

ggplot(world_data, aes(long, lat, group = group, fill = world_data[,24])) +
  geom_polygon(color = "gray90", size = 0.05 ) +
  coord_quickmap() +
  labs(
    fill = "Rapid response",
    title = "Speed of response around the world",
    caption = "Data: GHS Index 2019"
  ) +
  scale_fill_viridis_c(na.value = "white", direction = -1) +
  theme_bw() +
  theme(
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.spacing=unit(0, "lines"),
    plot.background=element_blank(),
    legend.justification = c(0,0),
    legend.position = c(0,0)
  )
