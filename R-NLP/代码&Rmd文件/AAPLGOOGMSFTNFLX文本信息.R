library(readxl)
library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(stringr)
library(tidytext)
library(textdata)
library(tidyr)
library(gridExtra)

# AAPL ---------------------
# read data------------------
AAPL = read.csv('AAPL 03-26 to 04-02 & 04-04 to 04-09.csv')

# missing data------------------
knitr::kable(round(colSums(is.na(AAPL))/17898, 2))

# time with one-hour gap -------------
AAPL = AAPL[,c(3,5)]
str(AAPL)
AAPL$created_at = as.character(AAPL$created_at)
AAPL$created_at = strptime(gsub('T', " ",
                                substr(AAPL$created_at,1,13)), 
                           format = "%Y-%m-%d %H") # time gap with hour

text_df = tibble(time = AAPL$created_at,text = AAPL$text)
text_df$text = as.character(text_df$text)
text_df %>%
        arrange(desc(time)) 
text_df = text_df %>%
        mutate(date = as.Date(time))


# remove the ... ---------------------

Textprocessing <- function(x)
{       x = gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", x)
x = gsub('\\b+RT', '', x) ## Remove RT
x = gsub("(?:\\s*#\\w+)+\\s*$", " ", x)
x = gsub('@\\S+', '', x) ## Remove Mentions
x = gsub('[[:cntrl:]]', ' ', x) ## Remove Controls and special characters
x = gsub("\\d", '', x) ## Remove Controls and special characters
x = gsub('[[:punct:]]', ' ', x) ## Remove Punctuations
x = gsub("^[[:space:]]*"," ",x) ## Remove leading whitespaces
x = gsub("[[:space:]]*$"," ",x) ## Remove trailing whitespaces
x = gsub('[0-9]+', ' ', x) ## Remove all the number 
gsub(' +',' ',x) ## Remove extra whitespaces
}

text_df$text = Textprocessing(text_df$text)

# create the byhour-text_df -----------------
text_df$time = as.character(text_df$time)
text_df %>%
        group_by(time) %>%
        count(time)

byhour_text_df = text_df %>% 
        group_by(date,time) %>%
        summarise_all(paste, collapse = ' ')
byhour_text_df

# Tidy the text -------
tidy_books <- text_df %>%
        unnest_tokens(word, text)%>%
        anti_join(stop_words) ## omit the stop words 

tidy_books %>%
        count(word, sort = TRUE)

tidy_books %>%
        count(word, sort = TRUE) %>%
        filter(n > 1000) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)+
        theme_bw()

# sentiment analysis --------
## Bing method
bing<- get_sentiments("bing")
sentment_bing = tidy_books %>%
        inner_join(bing) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative) %>% 
        group_by(date, time) %>%
        summarise(sentiment = sum(sentiment))

ggplot(sentment_bing, aes(as.POSIXct(time), sentiment, fill = date)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(.~date, ncol = 2, scales = "free_x")+
        theme_bw()+labs(title = 'BING', x = 'Date')
p1 = ggplot(sentment_bing, aes(as.POSIXct(time), sentiment, fill = as.character(date))) +
        geom_col(show.legend = FALSE)+
        theme_bw()+labs(title = 'BING', x = 'Date')
p1
## afinn
afinn<- get_sentiments("afinn")
sentment_afinn = tidy_books %>%
        inner_join(afinn) %>%
        group_by(date, time) %>%
        summarise(sentiment = sum(value))

ggplot(sentment_afinn, aes(as.POSIXct(time), sentiment, fill = date)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(.~date, ncol = 2, scales = "free_x")+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')

p2 = ggplot(sentment_afinn, aes(as.POSIXct(time), sentiment, fill = as.character(date))) +
        geom_col(show.legend = FALSE)+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')
p2
grid.arrange(p1, p2, ncol = 1)

## nrc
nrc<- get_sentiments("nrc")
sentment_nrc = tidy_books %>%
        inner_join(nrc) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        select(-word)%>%
        group_by(date, time) %>%
        summarise_all(sum)

# GOOG ------------
# read data------------------
GOOG = read.csv('GOOG 03-25 to 04-10.csv')

# missing data------------------
knitr::kable(round(colSums(is.na(GOOG))/17898, 2))

# time with one-hour gap -------------
GOOG = GOOG[,c(3,5)]
str(GOOG)
GOOG$created_at = as.character(GOOG$created_at)
GOOG$created_at = strptime(gsub('T', " ",
                                substr(GOOG$created_at,1,13)), 
                           format = "%Y-%m-%d %H") # time gap with hour

text_df = tibble(time = GOOG$created_at,text = GOOG$text)
text_df$text = as.character(text_df$text)
text_df %>%
        arrange(desc(time)) 
text_df = text_df %>%
        mutate(date = as.Date(time))


# remove the ... ---------------------

Textprocessing <- function(x)
{       x = gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", x)
x = gsub('\\b+RT', '', x) ## Remove RT
x = gsub("(?:\\s*#\\w+)+\\s*$", " ", x)
x = gsub('@\\S+', '', x) ## Remove Mentions
x = gsub('[[:cntrl:]]', ' ', x) ## Remove Controls and special characters
x = gsub("\\d", '', x) ## Remove Controls and special characters
x = gsub('[[:punct:]]', ' ', x) ## Remove Punctuations
x = gsub("^[[:space:]]*"," ",x) ## Remove leading whitespaces
x = gsub("[[:space:]]*$"," ",x) ## Remove trailing whitespaces
x = gsub('[0-9]+', ' ', x) ## Remove all the number 
gsub(' +',' ',x) ## Remove extra whitespaces
}

text_df$text = Textprocessing(text_df$text)

# create the byhour-text_df -----------------
text_df$time = as.character(text_df$time)
text_df %>%
        group_by(time) %>%
        count(time)

byhour_text_df = text_df %>% 
        group_by(date,time) %>%
        summarise_all(paste, collapse = ' ')
byhour_text_df

# Tidy the text -------
tidy_books <- text_df %>%
        unnest_tokens(word, text)%>%
        anti_join(stop_words) ## omit the stop words 

tidy_books %>%
        count(word, sort = TRUE)

tidy_books %>%
        count(word, sort = TRUE) %>%
        filter(n > 1000) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)+
        theme_bw()

# sentiment analysis --------
## Bing method
bing<- get_sentiments("bing")
sentment_bing = tidy_books %>%
        inner_join(bing) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative) %>% 
        group_by(date, time) %>%
        summarise(sentiment = sum(sentiment))

ggplot(sentment_bing, aes(as.POSIXct(time), sentiment, fill = date)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(.~date, ncol = 2, scales = "free_x")+
        theme_bw()+labs(title = 'BING', x = 'Date')
p1 = ggplot(sentment_bing, aes(as.POSIXct(time), sentiment, fill = as.character(date))) +
        geom_col(show.legend = FALSE)+
        theme_bw()+labs(title = 'BING', x = 'Date')
p1
## afinn
afinn<- get_sentiments("afinn")
sentment_afinn = tidy_books %>%
        inner_join(afinn) %>%
        group_by(date, time) %>%
        summarise(sentiment = sum(value))

ggplot(sentment_afinn, aes(as.POSIXct(time), sentiment, fill = date)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(.~date, ncol = 2, scales = "free_x")+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')

p2 = ggplot(sentment_afinn, aes(as.POSIXct(time), sentiment, fill = as.character(date))) +
        geom_col(show.legend = FALSE)+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')
p2
grid.arrange(p1, p2, ncol = 1)

## nrc
nrc<- get_sentiments("nrc")
sentment_nrc = tidy_books %>%
        inner_join(nrc) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        select(-word)%>%
        group_by(date, time) %>%
        summarise_all(sum)


## MSFT ---------------

# read data------------------
MSFT = read.csv('MSFT 03-27 to 04-02 & 04-04 to 04-10.csv')

# missing data------------------
knitr::kable(round(colSums(is.na(MSFT))/17898, 2))

# time with one-hour gap -------------
MSFT = MSFT[,c(3,5)]
str(MSFT)
MSFT$created_at = as.character(MSFT$created_at)
MSFT$created_at = strptime(gsub('T', " ",
                                substr(MSFT$created_at,1,13)), 
                           format = "%Y-%m-%d %H") # time gap with hour

text_df = tibble(time = MSFT$created_at,text = MSFT$text)
text_df$text = as.character(text_df$text)
text_df %>%
        arrange(desc(time)) 
text_df = text_df %>%
        mutate(date = as.Date(time))


# remove the ... ---------------------

Textprocessing <- function(x)
{       x = gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", x)
x = gsub('\\b+RT', '', x) ## Remove RT
x = gsub("(?:\\s*#\\w+)+\\s*$", " ", x)
x = gsub('@\\S+', '', x) ## Remove Mentions
x = gsub('[[:cntrl:]]', ' ', x) ## Remove Controls and special characters
x = gsub("\\d", '', x) ## Remove Controls and special characters
x = gsub('[[:punct:]]', ' ', x) ## Remove Punctuations
x = gsub("^[[:space:]]*"," ",x) ## Remove leading whitespaces
x = gsub("[[:space:]]*$"," ",x) ## Remove trailing whitespaces
x = gsub('[0-9]+', ' ', x) ## Remove all the number 
gsub(' +',' ',x) ## Remove extra whitespaces
}

text_df$text = Textprocessing(text_df$text)

# create the byhour-text_df -----------------
text_df$time = as.character(text_df$time)
text_df %>%
        group_by(time) %>%
        count(time)

byhour_text_df = text_df %>% 
        group_by(date,time) %>%
        summarise_all(paste, collapse = ' ')
byhour_text_df

# Tidy the text -------
tidy_books <- text_df %>%
        unnest_tokens(word, text)%>%
        anti_join(stop_words) ## omit the stop words 

tidy_books %>%
        count(word, sort = TRUE)

tidy_books %>%
        count(word, sort = TRUE) %>%
        filter(n > 1000) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)+
        theme_bw()

# sentiment analysis --------
## Bing method
bing<- get_sentiments("bing")
sentment_bing = tidy_books %>%
        inner_join(bing) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative) %>% 
        group_by(date, time) %>%
        summarise(sentiment = sum(sentiment))

ggplot(sentment_bing, aes(as.POSIXct(time), sentiment, fill = date)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(.~date, ncol = 2, scales = "free_x")+
        theme_bw()+labs(title = 'BING', x = 'Date')
p1 = ggplot(sentment_bing, aes(as.POSIXct(time), sentiment, fill = as.character(date))) +
        geom_col(show.legend = FALSE)+
        theme_bw()+labs(title = 'BING', x = 'Date')
p1
## afinn
afinn<- get_sentiments("afinn")
sentment_afinn = tidy_books %>%
        inner_join(afinn) %>%
        group_by(date, time) %>%
        summarise(sentiment = sum(value))

ggplot(sentment_afinn, aes(as.POSIXct(time), sentiment, fill = date)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(.~date, ncol = 2, scales = "free_x")+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')

p2 = ggplot(sentment_afinn, aes(as.POSIXct(time), sentiment, fill = as.character(date))) +
        geom_col(show.legend = FALSE)+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')
p2
grid.arrange(p1, p2, ncol = 1)

## nrc
nrc<- get_sentiments("nrc")
sentment_nrc = tidy_books %>%
        inner_join(nrc) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        select(-word)%>%
        group_by(date, time) %>%
        summarise_all(sum)

## NFLX ---------------

# read data------------------
NFLX = read.csv('NFLX 03-25 to 04-10.csv')

# missing data------------------
knitr::kable(round(colSums(is.na(NFLX))/17898, 2))

# time with one-hour gap -------------
NFLX = NFLX[,c(3,5)]
str(NFLX)
NFLX$created_at = as.character(NFLX$created_at)
NFLX$created_at = strptime(gsub('T', " ",
                                substr(NFLX$created_at,1,13)), 
                           format = "%Y-%m-%d %H") # time gap with hour

text_df = tibble(time = NFLX$created_at,text = NFLX$text)
text_df$text = as.character(text_df$text)
text_df %>%
        arrange(desc(time)) 
text_df = text_df %>%
        mutate(date = as.Date(time))


# remove the ... ---------------------

Textprocessing <- function(x)
{       x = gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", x)
x = gsub('\\b+RT', '', x) ## Remove RT
x = gsub("(?:\\s*#\\w+)+\\s*$", " ", x)
x = gsub('@\\S+', '', x) ## Remove Mentions
x = gsub('[[:cntrl:]]', ' ', x) ## Remove Controls and special characters
x = gsub("\\d", '', x) ## Remove Controls and special characters
x = gsub('[[:punct:]]', ' ', x) ## Remove Punctuations
x = gsub("^[[:space:]]*"," ",x) ## Remove leading whitespaces
x = gsub("[[:space:]]*$"," ",x) ## Remove trailing whitespaces
x = gsub('[0-9]+', ' ', x) ## Remove all the number 
gsub(' +',' ',x) ## Remove extra whitespaces
}

text_df$text = Textprocessing(text_df$text)

# create the byhour-text_df -----------------
text_df$time = as.character(text_df$time)
text_df %>%
        group_by(time) %>%
        count(time)

byhour_text_df = text_df %>% 
        group_by(date,time) %>%
        summarise_all(paste, collapse = ' ')
byhour_text_df

# Tidy the text -------
tidy_books <- text_df %>%
        unnest_tokens(word, text)%>%
        anti_join(stop_words) ## omit the stop words 

tidy_books %>%
        count(word, sort = TRUE)

tidy_books %>%
        count(word, sort = TRUE) %>%
        filter(n > 1000) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)+
        theme_bw()

# sentiment analysis --------
## Bing method
bing<- get_sentiments("bing")
sentment_bing = tidy_books %>%
        inner_join(bing) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative) %>% 
        group_by(date, time) %>%
        summarise(sentiment = sum(sentiment))

ggplot(sentment_bing, aes(as.POSIXct(time), sentiment, fill = date)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(.~date, ncol = 2, scales = "free_x")+
        theme_bw()+labs(title = 'BING', x = 'Date')
p1 = ggplot(sentment_bing, aes(as.POSIXct(time), sentiment, fill = as.character(date))) +
        geom_col(show.legend = FALSE)+
        theme_bw()+labs(title = 'BING', x = 'Date')
p1
## afinn
afinn<- get_sentiments("afinn")
sentment_afinn = tidy_books %>%
        inner_join(afinn) %>%
        group_by(date, time) %>%
        summarise(sentiment = sum(value))

ggplot(sentment_afinn, aes(as.POSIXct(time), sentiment, fill = date)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(.~date, ncol = 2, scales = "free_x")+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')

p2 = ggplot(sentment_afinn, aes(as.POSIXct(time), sentiment, fill = as.character(date))) +
        geom_col(show.legend = FALSE)+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')
p2
grid.arrange(p1, p2, ncol = 1)


## nrc
nrc<- get_sentiments("nrc")
sentment_nrc = tidy_books %>%
        inner_join(nrc) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        select(-word)%>%
        group_by(date, time) %>%
        summarise_all(sum)

