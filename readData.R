#' Read Sentiment140.csv file
#'
#' Read Sentiment140.csv file and create three tibble objects which can be used as inputs to other functions in the package.
#' The same data sets are also included in the package, so other functions have no dependency on readData 
#'
#' @param file as csv file containing tweeter messages, formatted same as Sentiment140.csv
#' @return three tibble objects: original, transformed with additional columns, and tokenized by word
#'
#' @importFrom dplyr as_tibble select mutate filter
#' @importFrom tidyerse read_csv
#' @importFrom magrittr %>%
#' @importFrom datetime as.time
#' @importFrom tidytext str_detect str_remove_all unnest_tokens stop_words
#'
#' @keywords data cleanup
#'
#' @export
#'
#' @examples 
#' readData("Sentiment140.csv")
#'

library(tidyverse) ## read_csv
library(dplyr) ## as_tibble
library(magrittr) ## %>%
library(datetime) ## as.time
library(tidytext) ##str_detect str_remove_all unnest_tokens stop_words

sentiment.orig <- tibble()
sentiment <- tibble()
sentiment.words <- tibble()

readData <- function(file = "Sentiment140.csv") {
  
  sentiment.orig <<- as_tibble(read_csv(file))
  ## head(sentiment.orig)
  names(sentiment.orig) <- c("polarity", "id", "date_orig", "query", "user", "tweet")
  ## names(sentiment.orig)
  
  ## extract date, time, hr
  sentiment <<- sentiment.orig %>% 
    mutate(
      date = as.Date(paste(substr(sentiment.orig$date_orig,5,10),substr(sentiment.orig$date_orig,25,29)), format = "%B %d %Y"),
      time = as.time(substr(sentiment.orig$date_orig, 12,19)),
      hr = substr(sentiment.orig$date_orig,12,13)
      ) %>% 
    mutate (
      weekday = weekdays(date)
      ) %>%
    select(id,user,date,time,hr,weekday)
  
  ## words
  remove_reg <- "&amp;|&lt;|&gt;"
  sentiment.words <<- sentiment.orig %>% 
    filter(!str_detect(tweet, "^RT")) %>%
    mutate(tweet = str_remove_all(tweet, remove_reg)) %>%
    unnest_tokens(word, tweet, token = "tweets") %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]")) %>%
    select(id,user,word)
  
  ## user mentions
  tweets.user <- sentiment.words %>% 
    filter(str_detect(word, "^@")) %>%
    group_by(user, word) %>%
    summarise(count = n()) %>% 
    left_join(tidy_tweets %>% 
                filter(str_detect(word, "^@")) %>%
                group_by(user) %>% 
                summarise(total = n())) %>%
    mutate(word = str_remove(word, "@"))
  
  ## word frequencies by user
  fr <- sentiment.words %>% filter(!str_detect(word, "^@"))
  
  frequency <- fr %>% 
    group_by(user, word) %>%
    summarise(count = n()) %>% 
    left_join(tidy_tweets %>% 
                group_by(user) %>% 
                summarise(total = n())) %>%
    mutate(freq = count/total) %>%
    filter(total > 499) ## keep users whose tweets contain least 500 words
  
  frequency_all <- fr %>% 
    group_by(word) %>%
    summarise(count = n()) 
  
  ##saveRDS(sentiment.orig, file = "sentiment-orig.rds")
  ##saveRDS(sentiment, file = "sentiment.rds")
  ##saveRDS(sentiment.words, file = "sentiment-words.rds")
  ##saveRDS(tweets.user, file = "sentiment-user.rds")
  ##saveRDS(frequency, file = "sentiment-freq.rds")
  ##saveRDS(frequency_all, file = "sentiment-freq-all.rds")
}