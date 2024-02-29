#load tidyverse
require(tidyverse)
-------
-------

#Load the GCom data into R and rename the column headers for better understanding.
df = read_csv("GCoM_emissions.csv") %>%
  rename(id = 'GCoM_ID',
         city = 'signatory name',
         country = 'country code',
         hdd = 'Heating Degree-Days (HDD)',
         gdp_pc = 'GDP per capita at NUTS3 [Euro per inhabitant]',
         emissions_pc = 'GHG emissions per capita in GCoM sectors_EDGAR [tCO2-eq/year]',
         population = 'population in 2018') %>%
  select(id, city, country, hdd, gdp_pc, emissions_pc, population)

dfs = read_csv("GCoM_emissions_by_sector.csv") %>% 
  rename(id = 'GCoM_ID',
         sector = 'emission_inventory_sector') %>%
  select(id,sector,emissions)
-------
-------
# Element 1: A dataset overview.
# Filter out rows in the GCoM_emissions.csv dataset where either the emissions per capita or population is missing.
filter_df <- df[complete.cases(df$emissions_pc, df$population), ]
#Or use 
filter_df <- na.omit(df)

#Filtered out rows where either emissions or population data was missing are 8136 observations from the original data of 8140 observations.
-------
-------
# Give the number of cities and countries represented in the data.
cities <- length(unique(filter_df$city))
countries <- length(unique(filter_df$country))
#The number of cities in the dataset are 7755
#The number of countries in the dataset are 28
-------
-------
# Give the names of the countries with the least cities.
city_counts <- filter_df %>%
  group_by(country) %>%
  summarise(num_cities = n())

max_cities <- city_counts %>%
  top_n(1,num_cities)

print(max_cities)

min_cities <- city_counts %>%
  top_n(-1,num_cities)

print(min_cities)
-------
-------
# Do there seem to be any imbalances: are certain countries are overrepresented?
# Create a histogram for the country data to check imbalances if any
library(ggplot2)
ggplot(city_counts, aes(x = country, y = num_cities)) +
  geom_bar(stat = "identity", fill = "maroon") + 
  geom_text(aes(label = num_cities), vjust = -0.5, size = 3) +
  labs(title = "Country and Cities Data", x = "Countries in EU", y = "Number of Cities") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
-------
-------
# Element 2: Range and distribution of city populations.
# Make a histogram of population
populationhist <- hist(filter_df$population, xlim = c(28,12051223), col="skyblue", main= 'Population Histogram', xlab = "Population", ylab = "Frequency")
# OPTION 2
hist(log(filter_df$population), col = "darkblue", main= 'Log Transformed Population Histogram', xlab = "Log(Population)", ylab = "Count")
-------
-------
#What are the maximum and minimum city populations in the dataset
#Find out maximum population city
maximum_pop_city <- filter_df[which.max(filter_df$population), c("city", "population")]
#Find out maximum population city
minimum_pop_city <- filter_df[which.min(filter_df$population), c("city", "population")]
#Find out the median using inbuilt R function
median_pop <- median(filter_df$population)
#Find out the city(s) associated with median value.
filter_df %>%
  filter(population==4540)
#*ANSWER* The city with the least and maximum populations in this dataset are Lobera de Onsella and London with 28 and 12051223 respectively. 
-------
-------
#Make a boxplot or similar plot that shows emissions per capita for each country.

# Use ggplot to create a box plot
# In aes function, use the reorder function to show emission value per country from lowest to highest.
# Use axis.text for x axis so the values are represented in a 45degree angle for improved readability.
ggplot(filter_df, aes(x = reorder(country, emissions_pc), y = emissions_pc)) +
  geom_boxplot(fill = "darkblue", color = "black", alpha = 0.7) +
  labs(title = "Emissions per Capita by Country", x = "Country", y = "Emissions per Capita") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#identity top and bottom three countries by median emission

median_emissions <- filter_df %>%
  group_by(country) %>%
  summarise(median_emissions = median(emissions_pc)) %>%
  arrange(median_emissions)

top_3c <- filter_df %>%
  group_by(country) %>%
  summarise(median_emissions = median(emissions_pc)) %>%
  arrange(desc(median_emissions)) %>%
  head(3)

bottom_3c <- filter_df %>%
  group_by(country) %>%
  summarise(median_emissions = median(emissions_pc)) %>%
  arrange(median_emissions) %>%
  head(3)

-------
-------
#plot emission data by sector for dataset dfs
#Create a new dataset and omit missing values for two columns only
dfs1<- na.omit(dfs)
#summarize to get the sumx total of emissions per sector
sector_emission_dfs1 <- dfs1 %>%
  group_by(sector) %>%
  summarise(total=(sum(emissions, na.rm = TRUE)))
#create a bar graph by loading stringr library for formatting purpose
library(stringr)
sector_emission_dfs1$sector <- str_wrap(sector_emission_dfs1$sector, width = 15)

plot_1 <- ggplot(sector_emission_dfs1, aes(x = reorder(sector, total), y = total, fill = sector)) +
  geom_bar(stat = "identity", color = "yellow") +
  labs(title = "Emission Values by Sector",
       x = "Sectors", y = "Total Emissions") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1.15, hjust = 0.5))

print(plot_1)

max_sectoremission <- sector_emission_dfs1[which.max(sector_emission_dfs1$total), c("sector", "total")]

-------
-------
df_new <- merge(df, dfs, by = "id")
df_new_clean <- na.omit(df_new)

# Calculate total value for each sector by country
sector_totals <- df_new_clean %>%
  group_by(sector,country) %>%
  summarise(total=(sum(emissions, na.rm = TRUE)))

# Calculate total value for each sector across all countries
sector_totals_pan <- sector_totals %>%
  group_by(sector) %>%
  summarise(total_pan=(sum(total, na.rm = TRUE)))

# Calculate relative importance of sectors by country
sector_totals <- merge(sector_totals, sector_totals_pan, by = "sector", all.x = TRUE)
sector_totals$RelativeImportance <- sector_totals$total / sector_totals$total_pan

#Plot relative importance on the map
ggplot(sector_totals, aes(x = country, y = RelativeImportance, fill = sector)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Relative Importance of Sectors by Country",
       x = "Country", y = "Frequency",
       fill = "Sector") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
-------
# Element 6: Connecting emissions to heating demand.
# Step 1: create a new dataset for sector insti and add respect city and value
  
element_6 <- data.frame(df_new_clean$country,df_new_clean$city,df_new_clean$hdd,df_new_clean$emissions_pc) %>%
  mutate(Is_Scandanavian= ifelse(df_new_clean$country %in% c("se", "no", "fi", "dk"),TRUE,FALSE))%>%
  rename(country='df_new_clean.country', city='df_new_clean.city',hdd='df_new_clean.hdd',emissions_pc='df_new_clean.emissions_pc', Countries ='Is_Scandanavian')%>%
  select(country,city,hdd,emissions_pc,Countries)%>%
  distinct()

#create the scatter plot showing the relation between heating degree, emissions and city
ggplot(element_6, aes(x = hdd, y = emissions_pc, color = Countries)) +
  geom_point() +
  labs(title = "Emissions vs. Heating Degree Days",
       x = "Heating Degree Days",
       y = "Emissions per Capita") +
  scale_color_manual(values = c("skyblue", "darkblue"), 
                     breaks = c(FALSE, TRUE),
                     labels = c("Other Countries", "Scandinavian Countries")) +
  theme_minimal()

corr1 <- cor(element_6$hdd, element_6$emissions_pc, method = "pearson")
--------
#Step 1: Remove Outlier City ie. London
max_gdpcity <- df_new_clean [which.max(df_new_clean$gdp_pc), c("city", "gdp_pc")]

df_new1 <- df_new_clean[df_new_clean$city != "London", ]
df_new1
  
#Step 2: Create a new dataset for gdp and emission and removed duplicates
df_emgdp <- data.frame(df_new1$gdp_pc, df_new1$emissions_pc, df_new1$city, df_new1$country, df_new1$sector)%>%
  rename(gdp_pc='df_new1.gdp_pc', 
         emissions_pc='df_new1.emissions_pc', 
         city='df_new1.city', 
         country='df_new1.country', 
         sector='df_new1.sector') %>%
  select(gdp_pc, emissions_pc, city, country, sector)%>%
  distinct()

#Step 3: Make a scatter plot to check relation between gdp and emissions
#3.1 Scatter plot of gdp and emissions with shape and color

sector_colors <- c("Institutional/tertiary buildings and facilities" = "skyblue",
                   "Manufacturing and construction industries" = "red",
                   "Municipal buildings and facilities" = "darkgreen",
                   "Residential buildings and facilities" = "purple",
                   "Transportation" = "yellow",
                   "Waste/wastewater" = "grey")

ggplot(df_emgdp, aes(x = gdp_pc, y = emissions_pc, color = sector)) +
  geom_point() +
  scale_color_manual(values = sector_colors)+
  labs(title = "Scatter Plot of Emission per capita vs. GDP per capita",
       x = "GDP Per Capita",
       y = "Emissions Per Capita") +
  theme_minimal()

corr2 <- cor(df_emgdp$gdp_pc, df_emgdp$emissions_pc, method = "pearson")

#TYPE 2: Create subplots
# Using facet_grid to create subplots

ggplot(df_emgdp, aes(x = gdp_pc, y = emissions_pc, color = sector)) +
  geom_point() +
  facet_grid(rows = vars(sector))+
  labs(title = "Scatter Plot of Emission per capita vs. GDP per capita, sector wise",
       x = "GDP per capita",
       y = "Emissions per capita") +
  theme_minimal()

-----X------X-------
-----X------X-------

#ELEMENT 5 - NEW
#merge both datasets
newdata <- merge(df, dfs, by = "id", all.x=TRUE, all.y=TRUE)
mergeddata <- na.omit(newdata)

#Find out sector contribution of emissions in each country
mergeddata <- mergeddata %>%
  group_by(country) %>%
  mutate(fraction = emissions / sum(emissions))    
  
#Create a stacked bar to display relative importance between countries and emissions by sector
ggplot(mergeddata, aes(x = country, y = fraction, fill = sector)) +
  geom_col() +
  labs(title = "Relative Importance between countries and emission by sector",
       x = "Country",
       y = "Emissions") +
  theme_minimal() +
  theme(legend.position = "top")

-----X------X-------
-----X------X-------
#ELEMENT 6 - NEW
  
mergeddata_element6 <- mergeddata %>%
  mutate(Is_Scandinavian = ifelse(country %in% c("se", "no", "fi", "dk"), TRUE, FALSE)) %>%
  distinct(country, hdd, emissions_pc, .keep_all = TRUE)%>%
  select(country, hdd, emissions_pc, Is_Scandinavian)

ggplot(mergeddata_element6, aes(x = hdd, y = emissions_pc, color = Is_Scandinavian)) +
  geom_point() +
  labs(title = "Emissions vs. Heating Degree Days",
       x = "Heating Degree Days",
       y = "Emissions per Capita") +
  scale_color_manual(values = c("skyblue", "darkblue"), 
                     breaks = c(FALSE, TRUE),
                     labels = c("Other Countries", "Scandinavian Countries")) +
  theme_minimal()
corr1 <- cor(mergeddata_element6$hdd, mergeddata_element6$emissions_pc, method = "pearson")

  
-----X------X-------
-----X------X-------
#ELEMENT 7 - NEW
max_gdpcity <- mergeddata [which.max(mergeddata$gdp_pc), c("city", "gdp_pc")]

mergeddata_elm7 <- mergeddata %>%
  filter(city != "London")%>%
  select(emissions_pc,gdp_pc,sector)%>%
  distinct()

sector_colors <- c("Institutional/tertiary buildings and facilities" = "skyblue",
                   "Manufacturing and construction industries" = "red",
                   "Municipal buildings and facilities" = "darkgreen",
                   "Residential buildings and facilities" = "purple",
                   "Transportation" = "yellow",
                   "Waste/wastewater" = "grey")

ggplot(mergeddata_elm7, aes(x = gdp_pc, y = emissions_pc, color = sector)) +
  geom_point() +
  scale_color_manual(values = sector_colors)+
  labs(title = "Scatter Plot of Emission per capita vs. GDP per capita",
       x = "GDP Per Capita",
       y = "Emissions Per Capita") +
  theme_minimal()

corr2 <- cor(mergeddata_elm7$gdp_pc, mergeddata_elm7$emissions_pc, method = "pearson")


