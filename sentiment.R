## comparing word usage

## where  n is the number of times the word in question is used by each person and the total indicates the total words for each person.

library(tidyr) ##group_by count left_join summarise mutate

## tidy_tweets <- readRDS("sentiment-words.rds") %>% filter(!str_detect(word, "^@"))

## frequency <- tidy_tweets %>% 
##   group_by(user, word) %>%
##   summarise(count = n()) %>% 
##   left_join(tidy_tweets %>% 
##               group_by(user) %>% 
##               summarise(total = n())) %>%
##   mutate(freq = count/total) %>%
##   filter(total > 499) ## keep users whose tweets contain least 500 words

## return pos and neg words for user sorted from most frequent to least frequent 
wordsByUser <- function(username = "Dogbook", topN = 20) {
  
  frequency <- readRDS("sentiment-freq.rds")
  
  frequency %>% 
  filter(user == username) %>% 
  arrange(desc(freq)) %>%
  inner_join(get_sentiments("bing")) %>%
  head(topN) %>% 
  mutate(color = as.factor(ifelse(sentiment == "positive",1,0))) %>%
  ##group_by(sentiment) %>%
  ##summarise(sum(count))
  ggplot(aes(reorder(word, freq)
             , ifelse(sentiment == "positive",count,-count)
             , fill = color)) +
  geom_col(show.legend = FALSE) + 
  xlab("Word Count") + 
  ylab("Word") +
  labs(title=paste("Top", topN, "positive and negative words for user:", username))+
  coord_flip() +
  theme_classic()  
}

##Examples
#frequency %>% filter(total > 1000) %>% group_by(user) %>% summarise(sum(count))
#1 Dogbook                 1032  ## for neg example
#2 Dutchrudder             1008
#3 enamoredsoul            1002
#4 lost_dog                1098
#5 MTVnHollyWEST23         1097
#6 nuttychris              1052  ##neg
#7 SallytheShizzle         1420
#8 shanajaca               1269 ##for pos example
#9 StDAY                   1061
#10 torilovesbradie         1051 ##pos
#11 tsarnick                1052 ##pos
#12 VioletsCRUK             1818 ##pos
#13 webwoke                 1062 ##neg?
#14 what_bugs_u             1552

#################################################

library(tidytext)

## frequency_all <- tidy_tweets %>% 
##   filter(!str_detect(word, "^@")) %>%
##   group_by(word) %>%
##   summarise(count = n()) 
  
##  group_by(user) %>% 
##  count(word, sort = TRUE) %>% 
##  left_join(tidy_tweets %>% 
##              group_by(person) %>% 
##              summarise(total = n())) %>%
##  mutate(freq = n/total)

#min(frequency$total)
#max(frequency$total)
#max(frequency$freq)

#frequency %>% 
#  filter(user == "_musiclover27_") %>%
#  arrange(desc(freq))


#unique(frequency$user)

frequency_all <- readRDS("sentiment-freq-all.rds") 
 sentiments <- frequency_all %>%
   inner_join(get_sentiments("nrc"))

## sentiments %>% group_by(sentiment) %>% summarise(sum(count))

## sentiments %>% filter(sentiment == "anger") %>% arrange(desc(count))

 sentiments %>% 
   filter(sentiment == "positive") %>% 
   arrange(desc(count)) %>% 
   head(15) %>%
   ##top_n(15, sum(count)) %>%
   ggplot(aes(reorder(word, count), count)) +
   geom_col(fill = "blue") +
   xlab("Word Count") + 
   ylab("Word") +
   labs(title="Positive words")+
   coord_flip() +
   theme_classic()  


## Top N words by Sentiment 
topWordsSentiment <- function(Sent = c("anger","anticipation","disgust","fear",
                                       "joy","negative","positive",
                                       "sadness","surprise","trust"),
                              topN = 15) {
  
  frequency_all <- readRDS("sentiment-freq-all.rds") 
  sentiments <- frequency_all %>%
    inner_join(get_sentiments("nrc"))
  
  sad <- c("anger","anticipation","disgust","fear","negative","sadness") 
  col <- ifelse(Sent %in% sad,"lightsteelblue3","plum2")
  
  sentiments %>% 
    filter(sentiment == Sent) %>% 
    arrange(desc(count)) %>% 
    head(topN) %>%
    ggplot(aes(reorder(word, count), count)) +
    geom_col(fill = col) +
    xlab(NULL) + 
    ylab(NULL) +
    labs(title=paste("Top", topN, Sent,"words")) +
    coord_flip() +
    theme_classic()  

}

##########################################
