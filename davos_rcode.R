###########
##R Setup##
###########

#Libraries we need for the project
library(rtweet)       #twitter package
library(wordcloud)    #word cloud
library(dplyr)        #data wrangling
library(ggplot2)      #data visualization
library(stringr)      #string manipulation
library(readr)        #read in data, export data
library(tm)           #stopwords
library(janitor)      #tabyl function
library(RColorBrewer) #colors
library(maps)         #drawing map
library(countrycode)  #matching country name

#Now we need to set up a Twitter API, this is a good resource:
#https://themepacific.com/how-to-generate-api-key-consumer-token-access-key-for-twitter-oauth/994/
#Tip: if you do not have a website use your Twitter user page

#After you've set up the app, Twitter will provide you credentials to access their API
#Here is a really great, not too technical, explanation for API: 
#https://medium.freecodecamp.org/what-is-an-api-in-english-please-b880a3214a82

create_token(
  app = "Davos_Blogpost",
  consumer_key = "qzquUUhUqUZXy9DSGmkqYNbln", #API key
  consumer_secret = "VwLAoxanGylvtczZYlkHrbZcc2aKDCnDHBj3zjj2TkDjbQ36uc", #API secret key
  access_token = "309171117-agaXF4Pcwb3C7TDPJdYoHK9nb4pVajvqp8SFaHmp",
  access_secret = "Ne2M8uRh0iEkttbUTz7q8ErL8wiO26DpOkMqbYVUfDPA4"
)

##################
##Getting Tweets##
##################

#Now we can start searching for tweets
#search_tweets() is part of the rtweet package
davos_tweets <- search_tweets(q = "#Davos OR #Davos2019 OR #WEF19 OR #WEF2019", #q = is the hashtag you want to return
                               n = 18000,               #n = number of tweets (maximum number)
                               include_rts = FALSE,     #don't include retweets
                               retryonratelimit = TRUE, #if want to get more than the 18,000 tweet limit
                               lang = "en")             #lang = english

#Picking the columns that we need
davos_csv <- davos_tweets %>%
  select(text,             #text inside tweet 
         favorite_count,   
         retweet_count,
         country,          #country of tweeter
         name,             #name used by user (not username)
         description)      #description associated with user


#Saving the tweets in .csv file so that the results can be replicated
#write_csv(davos_csv, "davos_tweets.csv")

davos_csv <- read_csv("davos_tweets.csv")
davos_csv <- read_csv(file.choose())

#################
##Data Cleaning##
#################

#Get only hashtags
hashtags <- str_extract_all(davos_csv$text, "\\#\\w+") #starts with hashtag, then matches 1 or more words
hashtags <- unlist(hashtags)
hashtags <- str_replace_all(hashtags, "(#WEF19)|(#Davos)|(#wef19)|(#Davos2019)|(#WEF2019)|(#davos)|(#wef)|(#WEF)|(\\d+)", "") #erasing our original hashtag callers and numbers
hashtags <- hashtags[hashtags != ""] #erasing empty vectors
head(sort(table(hashtags), decreasing = TRUE), 10) #top 10 results

#Get most common words (excluding stop words)
common1 <- str_to_lower(davos_csv$text) #make everything lowercase
common2 <- str_replace_all(common1, c("\\#\\w+" = "", #hashtags
                                            "(https:\\/\\/t.co\\/\\w+)" = "", #urls
                                            "\\@\\w+" = "", #usernames
                                            "  " = " ", #double spaces
                                            "[[:punct:]]" = "")) #punctuation characters
common3 <- str_trim(common2) #getting rid of whitespace
common4 <- str_split(common3, pattern = "\\s") #split them into each word
common5 <- unlist(common4) #unlisting them
common6 <- tm::removeWords(common5, stopwords("english")) #erasing stopwords using tm package
common7 <- common6[common6 != ""] #eradicating empty vectors
head(sort(table(common7), decreasing = TRUE), 10) #top 10 results

######################
##Creating Wordcloud##
######################

#Creating hashtags wordcloud
hashtags_wc <- as_tibble(tabyl(hashtags)) #creating a frequency table
hashtags_wc <- hashtags_wc %>%
  arrange(desc(n)) %>%
  select(hashtags, n) 
head(hashtags_wc, 10)

wordcloud(words = hashtags_wc$hashtags, #words we want to plot 
          freq = hashtags_wc$n,         #frequency of words
          random.order = FALSE,         #plot according to frequency
          min.freq = 50,                #minimum number to be plotted (needed when you have too many words)
          colors = brewer.pal(n = 5, name = "Set1")) #coloring scheme! 

#Creating most common words worcloud
common_wc <- as_tibble(tabyl(common7))
common_wc <- common_wc %>%
  arrange(desc(n)) %>%
  select(common7, n)
head(common_wc, 10)

wordcloud(words = common_wc$common7,
          freq = common_wc$n,
          random.order = FALSE,
          min.freq = 200,
          colors = brewer.pal(n = 5, name = "Set1"))

####################
##Further Analysis##
####################

#Obtaining tweets with country data (since most are missing)
tweets_w_country <- davos_csv[!is.na(davos_csv$country),]

tweets_w_country <- tweets_w_country %>%
  group_by(country) %>%
  summarize(total_retweet = sum(retweet_count)) %>%
  arrange(desc(total_retweet)) %>%
  head(20)

ggplot(data = tweets_w_country, aes(x = reorder(country, total_retweet), y = total_retweet)) + 
  geom_histogram(stat = "identity") +
  coord_flip() +
  ggtitle("Top Countries of Twitter Users") + 
  ylab("Retweet Count") + xlab("") +
  scale_y_continuous(breaks = seq(0,600, by = 50))




