#What I need to do

#Obtain the Data
#Scrub the Data
#Explore the Data
#Model the Data
#Interpret the Data

#Set the working Environment
#Install the required packages

#Load the data

youtube_df <- read.csv("data/thisisess.csv")
youtube_df


#check if it's a data frame - beginner problems lol
class(youtube_df)

#let's have a look at the structure of the data
str(youtube_df)

#can also use glipmse
glimpse(youtube_df)


#glipse isn;t available so we load the library which should be dplyr
library(dplyr)
glimpse(youtube_df)


#we select only what we need
#but first what question are we trying to answer?
#Most Popular Video
#Most disliked Video
#Most popular day to post


#since I want about three columns of data, I will need to use the select function which is under the tidyvers package
#install.packages("tidyverse")
library(tidyverse)

clean_yt <- youtube_df %>% select("video_title","total_view", "likes", "dislikes", "date_posted") 
clean_yt

library(ggplot2)


#plot the likes against the title 
ggplot(clean_yt, aes(total_view, likes, color = video_title)) + geom_point()

ggplot(clean_yt, aes(max(total_view), max(likes), color = date_posted)) + geom_density()
