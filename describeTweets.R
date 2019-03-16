#' Provides some descriptive information about Sentiment140.csv file
#'
#' Uses data sets included in the package (sentiment-orig, sentiment, sentiment-word)
#' Produces the following visualizations:
#' Number of tweets by day of the week
#' Number of tweets by hour of the day 
#' Top 10 users with the most number of tweets
#' Top 10 most frequently used words in tweets
#' Number of tweets by day of the week and time of day
#'
#' @return visualizations describing Sentiment140 data set
#'
#' @importFrom sqldf sqldf
#' @importFrom gridExtra grid.arrang
#' @importFrom ggplot ggplot qplot
#'
#' @keywords visualization
#'
#' @export
#'
#' @examples 
#' describeTweets()
#' countTweetsbyDayHour()
#'
library(sqldf) ##sqldf
library(gridExtra) ##grid.arrange
library(ggplot) ##ggplot qplot

describeTweets <- function() {
  
sent <- readRDS("sentiment.rds")
sent.words <- readRDS("sentiment-words.rds")

by_weeekday <- sent %>% group_by(weekday) %>% summarise(count = n()) %>% arrange(count)
by_hour <- sent %>% group_by(hr) %>% summarise(count = n()) %>% arrange(hr)
by_user <- sent %>% group_by(user) %>% summarise(count = n()) %>% arrange(desc(count))
by_word <- sent.words %>% group_by(word) %>% summarise(count = n()) %>% arrange(desc(count))
ntweets <- nrow(sent)
nusers <- length(unique(sent$user))

## plot tweets by day of the week
plot1 <- ggplot(by_weeekday,
                aes(x = reorder(weekday, count), count)) +
  geom_bar(stat = "identity", fill = "lightblue4") +
  xlab("Count") + 
  ylab("Weekday") +
  labs(title="Tweets by Day of the Week")+
  ##coord_flip() +
  theme_classic()

## plot tweets by hour of the day
plot2 <- ggplot(by_hour,
                aes(x = hr, count)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  xlab("Count") + 
  ylab("Hour") +
  labs(title="Tweets by Hour of the Day")+
  ##coord_flip() +
  theme_classic()

## list top 10 users by number of tweets
plot3 <- qplot(1:10, 1:10, geom = "blank") + theme_classic() + theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(as.data.frame(head(by_user,10)))) ## + labs(title="Top 10 Users")

## list top 10 most frequent words used
plot4 <- qplot(1:10, 1:10, geom = "blank") + theme_classic() + theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(as.data.frame(head(by_word,10)))) ## + labs(title="Top 10 Users")

grid.arrange(plot1, plot2, plot3, plot4, ncol=2, top = paste("Total number of tweets: ", format(ntweets,big.mark=",",scientific=FALSE), ", total number of users: " , format(nusers,big.mark=",",scientific=FALSE)))
}


## function2 

countTweetsByDayHour <- function () {
sent <- readRDS("sentiment.rds")

by_wk_hr <- sqldf("SELECT weekday
                  , sum(case when hr between '00' and '03' then 1 end) as '00-03'
                  , sum(case when hr between '04' and '07' then 1 end) as '04-07'
                  , sum(case when hr between '08' and '11' then 1 end) as '08-11'
                  , sum(case when hr between '12' and '15' then 1 end) as '12-15'
                  , sum(case when hr between '16' and '19' then 1 end) as '16-19'
                  , sum(case when hr between '20' and '23' then 1 end) as '20-23'
                  FROM sent 
                  GROUP BY weekday
                  ORDER BY weekday")
## barplot(graph_data, col=colors()[c(23,89,12)] , border="white", space=0.04, font.axis=2, xlab="group")
## barplot(graph_data, col=colors()[c(23,89,12)] , border="white", font.axis=2, beside=T, legend=rownames(graph_data), xlab="group", font.lab=2)

graph_data <- t(as.matrix(by_wk_hr[,-1]))
colnames(graph_data) <- by_wk_hr[,1]
graph_data
by_wk_hr
barplot(graph_data, main = "Tweets by Day of the Week, Hour", col=c("slategray4","steelblue4", "skyblue3", "slategray2",  "slategray1", "lightblue3") , border="white", font.axis=2, beside=T, legend=rownames(graph_data), font.lab=1
        ,args.legend = list(title = "Hours", x = "topright", cex = .7))
}