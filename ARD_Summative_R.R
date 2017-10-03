##### R FILE
##### DATA ANALYSIS: Twitter and Facebook

## Set Working directory:
getwd()
setwd("/Users/Xyz27/Dropbox/HILARY 2016-17/ARD/ARD W4")

## Libraries:
library(dplyr)
library(readr)
library(tidyr)
library(plyr)
library(oii)

######## TWITTER ANALYSIS:
# Load the dataset, exported from Python:
tw <- read_excel('twitter_foranalysis.xlsx')

## Create numeric columns for demographic variables:
# Gender:
tw$gender_code<-ifelse(tw$gender=="male", 0, 1)
class(tw$gender_code)   
# Ethnicity
tw$ethnicity_code<-mapvalues(tw$ethnicity, c("white", "african-american", "asian-american", "hispanic-american"), c(0,1,2,3))
tw$ethnicity_code<-as.integer(tw$ethnicity_code)

## Create numeric columns for political variables:
# Party
tw$party_code<-mapvalues(tw$party, c("republican", "democrat", "independent"), c(0,1,2))

# Convert the count variable to numeric: 
tw$tw_count<-as.numeric(tw$tw_count)

## PLOTTING: Totals...
# By gender:
ggplot(tw, aes(x=gender, y=tw_count, fill=gender)) + 
  geom_bar(stat="identity") +
  xlab("Gender") +
  ylab("Total Tweets") + ggtitle ("Tweets by Gender") + 
  guides(fill=FALSE) +
  theme_bw()
# By ethnicity:
ggplot(tw, aes(x=ethnicity, y=tw_count, fill=ethnicity)) + 
  geom_bar(stat="identity") +
  xlab("Ethnicity") +
  ylab("Total Tweets") + ggtitle ("Tweets by Ethnicity") + 
  guides(fill=FALSE) +
  theme_bw()
# By party:
ggplot(tw, aes(x=party, y=tw_count, fill=party)) + 
  geom_bar(stat="identity") +
  xlab("Party") +
  ylab("Total Tweets") + ggtitle ("Tweets by Party") + 
  guides(fill=FALSE) +
  theme_bw()
## Variables for...
# VOLUME: 
# Create a new variable, assigning a senator as a 'frequent' tweeter if their number of tweets is above the average within the period.
tw$frequent <- ifelse(tw$tw_count>mean(tw$tw_count, na.rm=TRUE), "frequent", "infrequent")

# CONTENT:
tw$frequent_national <- ifelse(tw$national>mean(tw$national, na.rm=TRUE), "more national", "less national")
tw$frequent_state <- ifelse(tw$state>mean(tw$state, na.rm=TRUE), "more state", "less state")
tw$frequent_partisan <- ifelse(tw$partisan>mean(tw$partisan, na.rm=TRUE), "more partisan", "less partisan")
names(tw)[4]<-"selfpromotional"
tw$frequent_sp <- ifelse(tw$selfpromotional>mean(tw$selfpromotional, na.rm=TRUE), "more self-promotional", "less self-promotinal")

# CROSS-TAB ANALYSIS:
## Volume (Total Tweets)
# Gender:
oii.xtab(tw$gender, tw$frequent, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(tw$ethnicity, tw$frequent, row=TRUE, stats=TRUE)
# Party:
oii.xtab(tw$party, tw$frequent, row=TRUE, stats=TRUE)

## TYPE:
# National
# Gender:
oii.xtab(tw$gender, tw$frequent_national, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(tw$ethnicity, tw$frequent_national, row=TRUE, stats=TRUE)
# Party:
oii.xtab(tw$party, tw$frequent_national, row=TRUE, stats=TRUE)

# State
# Gender:
oii.xtab(tw$gender, tw$frequent_state, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(tw$ethnicity, tw$frequent_state, row=TRUE, stats=TRUE)
# Party:
oii.xtab(tw$party, tw$frequent_state, row=TRUE, stats=TRUE)

# Partisan
# Gender:
oii.xtab(tw$gender, tw$frequent_partisan, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(tw$ethnicity, tw$frequent_partisan, row=TRUE, stats=TRUE)
# Party:
oii.xtab(tw$party, tw$frequent_partisan, row=TRUE, stats=TRUE)


# Self-Promotional
# Gender:
oii.xtab(tw$gender, tw$frequent_sp, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(tw$ethnicity, tw$frequent_sp, row=TRUE, stats=TRUE)
# Party:
oii.xtab(tw$party, tw$frequent_sp, row=TRUE, stats=TRUE)


######## Facebook ANALYSIS:
# Load the dataset:
fb <- read_excel('fb_foranalysis.xlsx')
names(fb)
# Create numeric column for count variables:
fb$fb_count<-as.numeric(fb$fb_count)

## VOLUME: Create a new variable, assigning a senator as a 'frequent' Facebook poster if their number of posts is above the average within the period.
fb$frequent <- ifelse(fb$fb_count>mean(fb$fb_count, na.rm=TRUE), "frequent", "infrequent")
View(fb)

## CONTENT:
fb$frequent_national <- ifelse(fb$national>mean(fb$national, na.rm=TRUE), "more national", "less national")
fb$frequent_state <- ifelse(fb$state>mean(fb$state, na.rm=TRUE), "more state", "less state")
fb$frequent_partisan <- ifelse(fb$partisan>mean(fb$partisan, na.rm=TRUE), "more partisan", "less partisan")
names(fb)[4]<-"selfpromotional"
fb$frequent_sp <- ifelse(fb$selfpromotional>mean(fb$selfpromotional, na.rm=TRUE), "more self-promotional", "less self-promotinal")


# CROSS-TAB ANALYSIS:
## Volume (Total fbeets)
# Gender:
oii.xtab(fb$gender, fb$frequent, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(fb$ethnicity, fb$frequent, row=TRUE, stats=TRUE)
# Party:
oii.xtab(fb$party, fb$frequent, row=TRUE, stats=TRUE)

### TYPE:
## National
# Gender:
oii.xtab(fb$gender, fb$frequent_national, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(fb$ethnicity, fb$frequent_national, row=TRUE, stats=TRUE)
# Party:
oii.xtab(fb$party, fb$frequent_national, row=TRUE, stats=TRUE)

## State
# Gender:
oii.xtab(fb$gender, fb$frequent_state, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(fb$ethnicity, fb$frequent_state, row=TRUE, stats=TRUE)
# Party:
oii.xtab(fb$party, fb$frequent_state, row=TRUE, stats=TRUE)

## Partisan
# Gender:
oii.xtab(fb$gender, fb$frequent_partisan, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(fb$ethnicity, fb$frequent_partisan, row=TRUE, stats=TRUE)
# Party:
oii.xtab(fb$party, fb$frequent_partisan, row=TRUE, stats=TRUE)


## Self-Promotional
# Gender:
oii.xtab(fb$gender, fb$frequent_sp, row=TRUE, stats=TRUE)
# Ethnicity:
oii.xtab(fb$ethnicity, fb$frequent_sp, row=TRUE, stats=TRUE)
# Party:
oii.xtab(fb$party, fb$frequent_sp, row=TRUE, stats=TRUE)


## PLOTTING: Totals...
# By gender:
ggplot(fb, aes(x=gender, y=fb_count, fill=gender)) + 
  geom_bar(stat="identity") +
  xlab("Gender") +
  ylab("Total Facebook Posts") + ggtitle ("Facebook Posts by Gender") + 
  guides(fill=FALSE) +
  theme_bw()
# By ethnicity:
ggplot(fb, aes(x=ethnicity, y=fb_count, fill=ethnicity)) + 
  geom_bar(stat="identity") +
  xlab("Ethnicity") +
  ylab("Total Facebook Posts") + ggtitle ("Facebook Posts by Ethnicity") + 
  guides(fill=FALSE) +
  theme_bw()
# By party:
ggplot(fb, aes(x=party, y=fb_count, fill=party)) + 
  geom_bar(stat="identity") +
  xlab("Party") +
  ylab("Total Facebook Posts") + ggtitle ("Facebook Posts by Party") + 
  guides(fill=FALSE) +
  theme_bw()
