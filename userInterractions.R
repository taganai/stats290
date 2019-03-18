#' Produces graph of relationships between users based on Sentiment140.csv file
#'
#' Uses data set included in the package (sentiment-words)
#' Produces a network plot with links transparent based on how common or rare the mention of the user is
#'
#' @param all_tweets include users with more than this number of total tweets mentioning another user 
#' @param user_mentions include only user pairs that occurr more than this number of times in the data set
#' @return visualizations describing Sentiment140 data set
#'
#' @importFrom tidyr mutate
#' @importFrom dplyr filter select
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph
#' @importFrom stringr str_detect
#' 
#' @keywords visualization
#'
#' @export
#'
#' @examples 
#' userInterractions(all_tweets = 150, user_mentions = 10)
#'

library(igraph) ##graph_from_data_frame
library(tidyr) ##mutate 
library(dplyr) ##filter select
library(ggraph) ##ggraph
library(stringr) ##str_detect

userInterractions <- function(all_tweets = 150, user_mentions = 10) {
  
  tweets_user <- readRDS("sentiment-user.rds")
  
##  tweets_user <- tidy_tweets %>% 
##    filter(str_detect(word, "^@")) %>%
##    group_by(user, word) %>%
##    summarise(count = n()) %>% 
##    left_join(tidy_tweets %>% 
##                filter(str_detect(word, "^@")) %>%
##                group_by(user) %>% 
##                summarise(total = n())) 

  user_graph <- tweets_user %>%
    select(user, word, count, total) %>%
    filter(total > all_tweets & count > user_mentions) %>%
    ##filter(total > 150 & count > 10) %>%
    graph_from_data_frame()

## user_graph
  
##  ggraph(user_graph, layout = "fr") +
##    geom_edge_link() +
##    geom_node_point() +
##    geom_node_text(aes(label = name), vjust = 1, hjust = 1)

  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))  
  ggraph(user_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = count), show.legend = FALSE,
                   arrow = a, end_cap = circle(.05, 'inches')) +
    geom_node_point(color = "plum2", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}