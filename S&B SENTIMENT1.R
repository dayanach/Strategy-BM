######## READ LIBRARIES ######
library(tidyverse) 
library(dplyr)
library(tidytext) #Text mining
library(tidyr)
library(widyr)
library(NLP)
#library("SnowballC")
library(RColorBrewer)
library(wordcloud)
library(RCurl)
library(SentimentAnalysis)
library(plotly)
library(tm)
library(glue)
library(stringr)
library(yarrr)
library(ggplot2)
library(devtools)
#install.packages("jpeg")

######## READ DATA #########
#reading data from folder 'Apps'
my.data.app<-readRDS("09_week32uk_app.rds")
#str(my.data.app)

#Reading data from folder Ratings
my.data.rat<-readRDS("09_week32uk_rati.rds")
#str(my.data.rat)

#reading data from folder 'Revenue'
my.data.rev<-readRDS("09_week32uk_rev.rds")
#str(my.data.rev)

######## JOIN ALL #########
join1<-inner_join(my.data.app,my.data.rat,by='my_app_id',copy=TRUE )
join2<-inner_join(join1,my.data.rev,by='my_app_id',copy=TRUE )
#head(join2)

######## CLEANING #########
#droping NA
new_app1<-drop_na(join2,description)

#droping NA
new_app2<-drop_na(new_app1,downloads)
new_app2<-drop_na(new_app1,nb_rating)
new_app2<-drop_na(new_app1,active_users)
#str(new_app2)

######## FINAL DATASET #########
dataSent<- new_app2
dataSent = dataSent[which(dataSent$price == '0'),]
#str(data)

######## APP200 #########
app200<-sample_n(dataSent,500)
#head(app200)

################### working on mean rating #######################
myvector <- c(5:1)
ratings <- app200 %>% select(rating_5,rating_4,rating_3,
                             rating_2,rating_1)

rating_wgt= (sweep(ratings,MARGIN=2,myvector,'*') )
#/app200$nb_rating)
#?sweep
'rowSums(x); colSums(x); rowMeans(x); colMeans(x)'

######## TIDY DATA : APP200 #####

tidy_app <- app200 %>% 
  mutate(rating_mean= rowSums(rating_wgt)
         /app200$nb_rating) %>%
  select(description, num_downloads_class,
         name,category, my_app_id,downloads,
         active_users,rating_mean)

tidy_app<-drop_na(tidy_app,rating_mean)
glimpse(tidy_app)

#scatter.smooth(tidy_app$rating_mean,tidy_app$active_users)
plot(tidy_app$rating_mean)
######## BY CATEGORY #######

bycategory = tidy_app %>% 
  group_by(category,description) %>% summarise(numberApps= n() )
#%>% mutate(count(my_app_id)) )
# %>% summarise(avgrat=mean(rating_5,rating_4,rating_3,rating_2,rating_1)))
glimpse(bycategory)
#head(bycategory)

######
category_tidy <- tidy_app %>%
  unnest_tokens(word, description) %>% #Break the description
  #into individual words
  # filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% 
  anti_join(stop_words) #Data provided by the tidytext package
glimpse(category_tidy)

########## word count per category #####
word_summary <- category_tidy %>%
  #mutate(decade = ifelse(is.na(decade),"NONE", decade)) %>%
  group_by(category, my_app_id) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(my_app_id, Category = category, word_count) %>%
  distinct() %>% #To obtain one record per song
  ungroup()
glimpse(word_summary)

#wtop10<- sort(word_summary$word_count)

pirateplot(formula =  word_count ~ Category , #Formula
           data = top_n(word_summary$word_count,10) , #Data frame
           xaxt = "n",
           xlab = NULL, ylab = "Distinct Word Count", #Axis labels
           main = "Lexical Diversity Per Category", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size


################ SENTIMENT #############
######## full data sentiment with library devtools ####
#library(devtools)
#install_github('trinker/sentimentr')
devtoolsent<-sentiment(tidy_app$description)
devtoolsent
######## full data sentiment ####
sentiment <- analyzeSentiment(tidy_app$description)
glimpse(sentiment)
senti<-sentiment$SentimentQDAP
convertToDirection(sentiment$SentimentQDAP)

plot(senti)
hist(tidy_app$rating_mean)
hist(tidy_app$downloads)
plot(tidy_app$downloads)
######### can´t be comapre with response
response <- tidy_app$rating_mean
compareToResponse(sentiment, response)
#cor(senti, response)
plotSentimentResponse(senti, response)


########## by lexicon / CATEGORY  #######
# Choose the bing lexicon
#get_sentiments("bing")
get_sentiments("bing")
# Choose the nrc lexicon
get_sentiments("nrc") %>%
  count(sentiment) # Count words by sentiment

head(get_sentiments("bing"))
# Choose the nrc lexicon
head(get_sentiments("nrc") %>% count(sentiment)) 
# Count words by sentiment

############### Frequency by category by words #####
freqcategory = category_tidy %>% 
  group_by(category,word) %>% 
  summarise(freq= n() )
tail(freqcategory)

# Access bing lexicon: bing
bing <- get_sentiments("bing")

# Use data frame with text data
category_bing= freqcategory %>%
  # With inner join, implement sentiment analysis using `bing`
  inner_join(bing)

# data has been pre-defined
#tweets_bing
####################### graph freq per category ###
category_bing %>% 
  # Group by two columns: state and sentiment
  group_by(category, sentiment) %>%
  # Use summarize to calculate the mean frequency for these groups
  summarize(freq = mean(freq)) %>%
  spread(sentiment, freq) %>%
  ungroup() %>%
  # Calculate the ratio of positive to negative words
  mutate(ratio = positive / negative,
         category = reorder(category, ratio)) %>%
  # Use aes() to put state on the x-axis and ratio on the y-axis
  ggplot(aes(category, ratio)) +
  # Make a plot with points using geom_point()
  geom_point() +
  coord_flip()

############### top by words #####

word_counts <- category_tidy %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count by word and sentiment
  count(word,sentiment)

top_words <- word_counts %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

?ggplot
# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top_words, aes(word, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()


########## sentiment corr #####

tidy_app2= tidy_app %>%
           group_by(category)%>%
           mutate(n_apps= n_distinct(my_app_id),
                  rating_mean_cat = mean(rating_mean),
                  #agregate the varible calculaed above.
                  downloads_categ= sum(downloads),
                  active_user_categ= sum()) %>%
          select( Category = category, rating_mean_cat, n_apps,
                  downloads_categ) %>%
          distinct() %>% #To obtain one record per category
          ungroup()

