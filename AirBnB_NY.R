#New York AirBnB Dataset

#Load the dataset

my_data <- read_csv("AB_NYC_2019.csv");

print(my_data)

#lets have a look at the data
head(my_data)
glimpse(my_data)
summary(my_data)

#Load Libraries I will need
library(ggplot2)
library(dplyr)
library(ggthemes)
library(caret)
library(purrr)
library(tidyverse)


#Clean data
head(my_data)
class(my_data)


#remove NA values
clean_data <- na.omit(my_data)

head(clean_data)

#Remove all the columns we do not need
good_data <- select(clean_data, -c(id, host_id,latitude,longitude))

head(good_data)

#Exploratory Data Analysis
ggplot(good_data, aes(as.factor(price))) + geom_bar()
ggplot(good_data, aes(as.factor(number_of_reviews))) + geom_bar()

ggplot(good_data, aes(price)) + geom_histogram(binwidth = 0.5)
ggplot(good_data, aes(number_of_reviews)) + geom_histogram(binwidth = 0.5)

ggplot(good_data, aes(price, color = number_of_reviews)) + geom_freqpoly(binwidth = 0.1)

ggplot(good_data, aes(number_of_reviews,price, colour = room_type)) + geom_point()

#People tend to shy away from reveiwing shared rooms
ggplot(good_data, aes(price, colour = room_type)) + geom_histogram()


ggplot(good_data,  aes(x = price)) + 
  geom_freqpoly(aes(colour = room_type), binwidth = 500)

#Prices of Private rooms and entire apartnments are fairly the same 


ggplot(good_data, aes(x = price, y = calculated_host_listings_count)) + 
  geom_freqpoly(aes(colour = room_type), binwidth = 500)

ggplot(good_data, aes(x = room_type, y = price)) +
  geom_boxplot()

ggplot(good_data, aes(x = room_type, y = price)) +
  geom_boxplot()

ggplot(good_data, aes(x = price, y = neighbourhood_group)) + 
  geom_point(aes(colour = room_type), binwidth = 500)

#The cheapest neighbourhood is the Bronx

#Visualize the prices for shared rooms in each neighbourhood_group
shared_rooms = subset(good_data, room_type == "Shared room")
shared_rooms

ggplot(shared_rooms, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot()

ggplot(shared_rooms, aes(x = neighbourhood_group, y = price)) +
  geom_point()

ggplot(shared_rooms, aes(x = neighbourhood_group, y = price)) +
  geom_jitter()



#Queens tends to have the most expensive shared rooms

#Private Rooms Analysis

private_rooms = subset(good_data, room_type == "Private room")
private_rooms

ggplot(private_rooms, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot()

ggplot(private_rooms, aes(x = neighbourhood_group, y = price, colour = minimum_nights)) +
  geom_point()

ggplot(private_rooms, aes(x = neighbourhood_group, y = price)) +
  geom_jitter()


#Entire home/apt Analysis

entire_home = subset(good_data, room_type == "Entire home/apt")
entire_home

ggplot(entire_home, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot()

ggplot(entire_home, aes(x = neighbourhood_group, y = price, colour = minimum_nights)) +
  geom_point()

ggplot(entire_home, aes(x = neighbourhood_group, y = price)) +
  geom_jitter()
