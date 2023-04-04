
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
library(rpart)
library(rpart.plot)
library(caret)


AAPL = read.csv('AAPL 03-26 to 04-02 & 04-04 to 04-09.csv')

# time with one-hour gap -------------
AAPL = AAPL[,c(3,5)]
AAPL$created_at = as.character(AAPL$created_at)
text_df = tibble(time = AAPL$created_at,text = AAPL$text)
text_df$text = as.character(text_df$text)
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
text_df1= text_df %>%
        group_by(time) %>%
        count(time) %>%
        arrange(time)

byhour_text_df = text_df %>% 
        group_by(date,time) %>%
        summarise_all(paste, collapse = ' ') # paste the test together group by 1 hour


head(byhour_text_df)
paste('there are total', nrow(byhour_text_df), 'observation')

tidy_books <- text_df %>%
        unnest_tokens(word, text)%>% # separate words 
        anti_join(stop_words) ## omit the stop words 


## nrc
nrc<- get_sentiments("nrc")
sentment_nrc = tidy_books %>%
        inner_join(nrc) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        select(-word)%>%
        group_by(date, time) %>%
        summarise_all(sum)


AAPL_sample = AAPL[AAPL$created_at %in% c('2021-03-30T23:40:46Z','2021-03-31T19:23:15Z'), ]

text_df_sample = tibble(time = AAPL_sample$created_at,text = AAPL_sample$text)
text_df_sample$text = as.character(text_df_sample$text)
text_df_sample = text_df_sample %>%
        mutate(date = as.Date(time))

text_df_sample %>%
        unnest_tokens(word, text)%>% # separate words 
        anti_join(stop_words)  %>%
        inner_join(nrc) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        select(-word)%>%
        group_by(date, time) %>%
        summarise_all(sum)

