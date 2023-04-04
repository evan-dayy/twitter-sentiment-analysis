# Import libraries
# import pandas as pd
from bs4 import BeautifulSoup
# import matplotlib.pyplot as plt
from urllib.request import urlopen
from urllib.request import Request
# from nltk.sentiment.vader import SentimentIntensityAnalyzer

# Parameters
n = 3  # the # of article headlines displayed per ticker
tickers = ['AAPL', 'TSLA', 'AMZN']

# Get Data
finviz_url = 'https://finviz.com/quote.ashx?t='
news_tables = {}

for ticker in tickers:
    url = finviz_url + ticker
    req = Request(url=url, headers={'user-agent': 'my-app/0.0.1'})
    resp = urlopen(req)
    html = BeautifulSoup(resp, features="lxml")
    news_table = html.find(id='news-table')
    news_tables[ticker] = news_table

try:
    for ticker in tickers:
        df = news_tables[ticker]
        df_tr = df.findAll('tr')

        print('\n')
        print('Recent News Headlines for {}: '.format(ticker))

        for i, table_row in enumerate(df_tr):
            a_text = table_row.a.text
            td_text = table_row.td.text
            td_text = td_text.strip()
            print(a_text, '(', td_text, ')')
            if i == n - 1:
                break
except KeyError:
    pass

# Iterate through the news
parsed_news = []
for file_name, news_table in news_tables.items():
    for x in news_table.findAll('tr'):
        text = x.a.get_text()
        date_scrape = x.td.text.split()

        if len(date_scrape) == 1:
            time = date_scrape[0]

        else:
            date = date_scrape[0]
            time = date_scrape[1]

        ticker = file_name.split('_')[0]

        parsed_news.append([ticker, date, time, text])
