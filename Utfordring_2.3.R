### Assignment 2.3


# Loading packages needed for the assignment

library(readr) # fileformat of the dataset
library(ggplot2)     # the ggplot package
library(tidyverse)  # the tidyverse package


union<- read_csv("union_unempl.csv") #Loading the data with information about the variables of interest


#Changing the name of a single observation. The below code changes all observations called "United Kingdom" to "UK" in the union data. This is done so we can merge the data
union$country <- gsub("United Kingdom", "UK", union$country)
View(union) 

# Renaming a variable. The below code renames the variable "Country" to "Region". 
names(union)[names(union) == "country"] <- "region"
View(union) 



mapdata <- map_data("world") #loading map data
view(mapdata)

mapdata <- left_join(mapdata, union, by = "region") # Combining datasets

mapdata1 <- mapdata %>% filter(!is.na(mapdata$unempl)) # Removing NA values to only plot the countries we have data for


# Lag kart over Europa som viser arbeidsledighetsrate i ulike land. #
#####

# Plotting a map that shows unemployment rates in countries in Europe. 
map1 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = unempl), col = "black")
map1

# Plotting a new map with colors that are easier to see

map1.1 <- map1 + scale_fill_gradient(name = "Arbeidsledighet i Prosent", low = "green", high = "red", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map1.1

#####

# Plotting a map showing union density

map2 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = density), col = "black")
map2

map2.1 <- map2 + scale_fill_gradient(name = "Fagforeningsdensitet i Prosent", low = "red", high = "green", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map2.1

#####


# Plotting a map showing excess coverage


map3 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = coverage), col = "black")
map3

map3.1 <- map3 + scale_fill_gradient(name = "Excess Coverage", low = "red", high = "green", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map3.1


#####

# Plotting a map showing coordination of payrate setting

map4 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = coord), col = "black")
map4

map4.1 <- map4 + (name = "Koordinering av lonnfastsettelse") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map4.1

#####

map4 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = coord), col = "black")
map4

map4.1 <- map4 +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map4.1

