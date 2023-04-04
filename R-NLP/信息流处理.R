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

# AAPL STOCK PRICE------------
library(tidyquant)
library(quantmod)
stock_aapl = read_excel('AAPL GOOG....xlsx', sheet = 'aapl')
stock_aapl = stock_aapl[,c(3,2)]
stock_aapl$time = as.character(stock_aapl$time )
stock = stock_aapl


# sentiment ---------
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

# stock --------
stock_afinn = ggplot() +
        geom_col(show.legend = FALSE, 
                 data =sentment_afinn, aes(as.POSIXct(time), sentiment, fill = as.character(date)))+
        geom_line(data = stock, aes(x = as.POSIXct(time), (2^(price))/(exp(80))), alpha = 0.6)+
        geom_col(data = stock, aes(x = as.POSIXct(time), (2^(price))/(exp(81))))+
        geom_point(data = stock, aes(x = as.POSIXct(time), (2^(price))/(exp(80))), alpha = 0.6, pch = 2)+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')
     
stock_bing = ggplot() +
        geom_col(data = sentment_bing, aes(as.POSIXct(time), sentiment, fill = as.character(date)),
                 show.legend = FALSE)+
        geom_line(data = stock, aes(x = as.POSIXct(time), (2^(price))/(exp(80))), alpha = 0.6)+
        geom_point(data = stock, aes(x = as.POSIXct(time), (2^(price))/(exp(80))), alpha = 0.6, pch = 2)+
        geom_col(data = stock, aes(x = as.POSIXct(time), (2^(price))/(exp(81))))+
        theme_bw()+labs(title = 'BING', x = 'Date')

grid.arrange(stock_afinn, stock_bing, ncol = 1)


## build the data frame ----------
## for nrc:
colnames(stock)[1] = 'datetime'
stock$date = as.Date(stock$datetime, format = "%Y-%m-%d")
stock$datetime = as.POSIXct(stock$datetime)
stock$time_stock = format(stock$datetime, format = '%H:%M')
stock = stock[order(stock$datetime, decreasing = FALSE),]

colnames(sentment_nrc)[2] = 'datetime'
sentment_nrc$datetime = as.POSIXct(sentment_nrc$datetime)
sentment_nrc$time_se = format(sentment_nrc$datetime, format = '%H:%M')
full_nrc = full_join(stock,sentment_nrc)

full_nrc$time_stock = ifelse(is.na(full_nrc$time_stock), 
                             full_nrc$time_se, full_nrc$time_stock)
full_nrc = full_nrc[,-15]
opentime = "09:00"
closetime = "16:00"
full_nrc$state = ifelse((strptime(full_nrc$time_stock, format = '%H:%M') >= strptime(opentime, format = '%H:%M')) & (strptime(full_nrc$time_stock, format = '%H:%M') <= strptime(closetime, format = '%H:%M')), 'open', 'close' )

full_nrc_close =  full_nrc%>%
        filter(state == 'close')
full_nrc_close = na.omit(full_nrc_close)

full_nrc_open = full_nrc %>%
        filter(state == 'open')
full_nrc_open = full_nrc_open[order(full_nrc_open$datetime,decreasing = FALSE),]
full_nrc_open = full_nrc_open %>%
        fill(price, .direction = "up") 
full_nrc_open = na.omit(full_nrc_open)
full_nrc = rbind(full_nrc_close,full_nrc_open)

diff = diff(full_nrc$price)
full_nrc$diff_price = c(diff, 0)
full_nrc = full_nrc[which(full_nrc$diff_price !=0), ]


diff = diff(full_nrc_close$price)
full_nrc_close$diff_price = c(diff, 0)
full_nrc_close = full_nrc_close[which(full_nrc_close$diff_price !=0), ]

diff = diff(full_nrc_open$price)
full_nrc_open$diff_price = c(diff, 0)
full_nrc_open = full_nrc_open[which(full_nrc_open$diff_price !=0), ]

md_nrc = lm(price ~ anger+anticipation+disgust+fear+joy+negative+positive+sadness+surprise+trust, data = full_nrc)
summary(md_nrc)
md_nrc_close = lm(price ~ anger+anticipation+disgust+fear+joy+negative+positive+sadness+surprise+trust, data = full_nrc_close)
summary(md_nrc_close)
md_nrc_open = lm(price ~ anger+anticipation+disgust+fear+joy+negative+positive+sadness+surprise+trust, data = full_nrc_open)
summary(md_nrc_open)

## build the data frame and model  ----------
## for bing:
sentment_bing = tidy_books %>%
        inner_join(bing) %>%
        count(date, time, word, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        select(-word) %>%
        group_by(date, time) %>%
        summarise_all(sum)
colnames(sentment_bing)[2] = 'datetime'
sentment_bing$datetime = as.POSIXct(sentment_bing$datetime)
sentment_bing$time_se = format(sentment_bing$datetime, format = '%H:%M')
full_bing = full_join(stock,sentment_bing)
full_bing$time_stock = ifelse(is.na(full_bing$time_stock), 
                             full_bing$time_se, full_bing$time_stock)
full_bing = full_bing[,-7]
opentime = "09:00"
closetime = "16:00"
full_bing$state = ifelse((strptime(full_bing$time_stock, format = '%H:%M') >= strptime(opentime, format = '%H:%M')) & (strptime(full_bing$time_stock, format = '%H:%M') <= strptime(closetime, format = '%H:%M')), 'open', 'close' )


full_bing_close =  full_bing%>%
        filter(state == 'close')
full_bing_close = na.omit(full_bing_close)



full_bing_open = full_bing %>%
        filter(state == 'open')
full_bing_open = full_bing_open[order(full_bing_open$datetime,decreasing = FALSE),]
full_bing_open = full_bing_open %>%
        fill(price, .direction = "up") 
full_bing_open = na.omit(full_bing_open)


full_bing = rbind(full_bing_close,full_bing_open)

diff = diff(full_bing$price)
full_bing$diff_price = c(diff, 0)
full_bing = full_bing[which(full_bing$diff_price !=0), ]


diff = diff(full_bing_close$price)
full_bing_close$diff_price = c(diff, 0)
full_bing_close = full_bing_close[which(full_bing_close$diff_price !=0), ]

diff = diff(full_bing_open$price)
full_bing_open$diff_price = c(diff, 0)
full_bing_open = full_bing_open[which(full_bing_open$diff_price !=0), ]



md_bing = lm(log(price) ~ negative+positive, data = full_bing)
summary(md_bing)
md_bing_close = lm(price ~  negative+positive, data = full_bing_close)
summary(md_bing_close)
md_bing_open = lm(price ~  negative+positive, data = full_bing_open)
summary(md_bing_open)

## for afinn
sentment_afinn
colnames(sentment_afinn)[2] = 'datetime'
sentment_afinn$datetime = as.POSIXct(sentment_afinn$datetime)
sentment_afinn$time_se = format(sentment_afinn$datetime, format = '%H:%M')
full_afinn = full_join(stock,sentment_afinn)

full_afinn$time_stock = ifelse(is.na(full_afinn$time_stock), 
                              full_afinn$time_se, full_afinn$time_stock)
full_afinn = full_afinn[,-6]
opentime = "09:00"
closetime = "16:00"
full_afinn$state = ifelse((strptime(full_afinn$time_stock, format = '%H:%M') >= strptime(opentime, format = '%H:%M')) & (strptime(full_afinn$time_stock, format = '%H:%M') <= strptime(closetime, format = '%H:%M')), 'open', 'close' )

full_afinn_close =  full_afinn%>%
        filter(state == 'close')
full_afinn_close = na.omit(full_afinn_close)

full_afinn_open = full_afinn %>%
        filter(state == 'open')
full_afinn_open = full_afinn_open[order(full_afinn_open$datetime,decreasing = FALSE),]
full_afinn_open = full_afinn_open %>%
        fill(price, .direction = "up") 
full_afinn_open = na.omit(full_afinn_open)
full_afinn = rbind(full_afinn_close,full_afinn_open)

diff = diff(full_afinn$price)
full_afinn$diff_price = c(diff, 0)
full_afinn = full_afinn[which(full_afinn$diff_price !=0), ]


diff = diff(full_afinn_close$price)
full_afinn_close$diff_price = c(diff, 0)
full_afinn_close = full_afinn_close[which(full_afinn_close$diff_price !=0), ]

diff = diff(full_afinn_open$price)
full_afinn_open$diff_price = c(diff, 0)
full_afinn_open = full_afinn_open[which(full_afinn_open$diff_price !=0), ]

md_afinn = lm(log(price) ~ sentiment, data = full_afinn)
summary(md_afinn)

md_afinn_close = lm(price ~ sentiment, data = full_afinn_close)
summary(md_afinn_close)

md_afinn_open = lm(price ~ sentiment, data = full_afinn_open)
summary(md_afinn_open)


# --------

stock_afinn = ggplot(data = full_afinn_open) +
        geom_col(show.legend = FALSE, 
                 aes(as.POSIXct(datetime), sentiment, fill = 'lightblue'))+
        geom_point(aes(x = as.POSIXct(datetime), sentiment), alpha = 0.6, pch = 2)+
        geom_point(aes(x = as.POSIXct(datetime), diff_price*50), alpha = 0.6, pch = 3)+
        geom_col(aes(x = as.POSIXct(datetime), diff_price*50))+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')

stock_bing = ggplot(data = full_bing_open) +
        geom_col(show.legend = FALSE, 
                 aes(as.POSIXct(datetime), positive-negative, fill = as.character(date)))+
        geom_point(aes(x = as.POSIXct(datetime), price), alpha = 0.6, pch = 3)+
        theme_bw()+
        labs(title = 'AFINN', x = 'Date')

grid.arrange(stock_afinn, stock_bing, ncol = 1)

full_nrc_open

diff(full_nrc_open$price)




