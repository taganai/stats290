#' Read Sentiment140.csv file
#'
#' Read Sentiment140.csv file and create two tibble objects which can be used as inputs to other functions in the package.
#' The same data sets are also included in the package, so other functions have no dependency on readData 
#'
#' @param x a numeric vector
#' @param file as csv file containing tweeter messages, formatted same as Sentiment140.csv
#' @param smoother one of the following: "loess", "lm", "glm", "gam"
#' @return two tibble objects: original, and transformed with additional columns
#'
#' @importFrom dplyr as_tibble
#' @importFrom magrittr %>%
#' @importFrom datetime as.time
#'
#' @keywords data cleanup
#'
#' @export
#'
#' @examples 
#' readData("Sentiment140.csv")
#'

##library(tidyverse)
library(dplyr) ## as_tibble
library(magrittr) ## %>%
library(datetime) ## as.time
## library(lubridate)

sentiment.orig <- tibble()
sentiment <- tibble()

readData <- function(file = "Sentiment140.csv") {
  
  sentiment.orig <<- as_tibble(read_csv(file))
  ## head(sentiment.orig)
  names(sentiment.orig) <- c("polarity", "id", "date_orig", "query", "user", "tweet")
  ## names(sentiment.orig)
  
  ## extract date, time, hr
  sentiment <<- sentiment.orig %>% mutate(
    date = as.Date(paste(substr(sentiment.orig$date_orig,5,10),substr(sentiment.orig$date_orig,25,29)), format = "%B %d %Y"),
    time = as.time(substr(sentiment.orig$date_orig, 12,19)),
    hr = substr(sentiment.orig$date_orig,12,13)
  ) %>% mutate (
    weekday = weekdays(date)
  ) 
  
  ##saveRDS(sentiment.orig, file = "sentiment-orig.rds")
  ##saveRDS(sentiment, file = "sentiment.rds")
  
}