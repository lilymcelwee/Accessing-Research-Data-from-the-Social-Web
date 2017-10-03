## Loading J_tsar data:

getwd()
setwd("/Users/Xyz27/Dropbox/HILARY 2016-17/ARD/ARD W4")

## Sample load:
#twitter_data<-read.csv("J_tsar_tweets.csv")
#View(twitter_data)

library(dplyr)
library(readr)
library(tidyr)

## Sample load of several files:
# E.g. amyklobuchar, BillCassidy, catherinefornv, ChrisMurphyCT

amyklobuchar<-read.csv("amyklobuchar_tweets.csv")
BillCassidy<-read.csv("BillCassidy_tweets.csv")
catherinefornv<-read.csv("catherinefornv_tweets.csv")
ChrisMurphyCT<-read.csv("ChrisMurphyCT_tweets.csv")

## Sample load of first 10 senators:
first_10_twitter<-read.xls('senator_df_1thru10.xlsx', sheet=1)
View(first_10_twitter)
head(first_10_twitter)
first_10_twitter <- transform(first_10_twitter, created_at = colsplit(created_at, split = "", names = c('date', 'time')))
first_10_twitter %>% separate(created_at, c("date", "time"), " ", extra = "merge")

first_10_twitter$date<-as.Date(first_10_twitter$created_at)


first_10_twitter<-within(first_10_twitter, created_at<-data.frame(do.call('rbind', strsplit(as.character(created_at), ' ', fixed=TRUE))))

lapply(first_10_twitter, class) # All columns are factors
first_10_twitter$time<-as.Date(first_10_twitter$created_at)
class(first_10_twitter$username) # factor


# The first thing we need to do is subset by the time period for our analysis. Since I am examining social media use across a one-week period, I will subset the 
# data to include dates just including and after ____.
# CLASS OF AN OBJECT IN R - date, for that column.