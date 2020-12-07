# Load libraries
from bs4 import BeautifulSoup
import requests
import json
import time
from random import randrange

# HTML scrapper
from selenium import webdriver

# Basic text cleaning
import re

# Data wrangling
import pandas as pd
import numpy as np
from os import listdir
from datetime import datetime

# Define folder
data_folder = r'/home/zmaznevegor/PycharmProjects/defi_uncertainty/data'


# Data collection
def collect_data_decrypt(wp):
    date = []
    content_text = []

    # same range as posts per page
    for j in range(0, len(wp)):
        date.append(wp[j]['date'])
        content_text.append(wp[j]['custom_fields']['content_text'])

    df = pd.DataFrame(columns=['date', 'text'])

    df['date'] = date
    df['text'] = content_text

    return df


# step equals to the results per page
offsets = np.arange(0, 6250, 25).tolist()
frames = []

# Loop that goes through all the available news by shifting the offset
for i in offsets:
    time.sleep(randrange(5))
    print(i)
    cmc = requests.get(
        f'https://api.decrypt.co/content/posts?_minimal=true&category=news&lang=en-US&offset={i}&order=desc&orderby=date&per_page=25')
    time.sleep(5)
    webpage = cmc.json()
    df = collect_data_decrypt(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/decrypt.csv', index=False)


# Coindesk website data
def collect_data_cd(wp):
    date = []
    text = []

    # same range as posts per page
    for j in range(0, len(wp['posts'])):
        date.append(wp['posts'][j]['date'])

        cmc1 = requests.get('https://www.coindesk.com/' + wp['posts'][j]['slug'])
        soup = BeautifulSoup(cmc1.content, 'html.parser')

        data = soup.find('script',
                         id="__NEXT_DATA__",
                         type="application/json")

        news_data = json.loads(data.contents[0])

        if 'data' in news_data['props']['initialProps']['pageProps']:
            if 'body' in news_data['props']['initialProps']['pageProps']['data']:
                body = news_data['props']['initialProps']['pageProps']['data']['body']

                post = []

                if isinstance(body, list):
                    for item in body:
                        if 'content' in item:
                            post.append(item['content'])
                        elif 'data' in item:
                            if 'items' in item['data']:
                                data_text = ''.join(item['data']['items'])
                                post.append(data_text)
                            elif 'caption' in item['data']:
                                post.append(item['data']['caption'])
                            elif 'content' in item['data']:
                                post.append(item['data']['content'])
                            else:
                                continue
                        else:
                            continue

                    post = ''.join(post)
                    post = re.sub('\<.+?\>|\<\/.+?\>', ' ', post)  # clean code snippets
                    text.append(post)

                elif isinstance(body, str):
                    body = re.sub('\<.+?\>|\<\/.+?\>', ' ', body)  # clean code snippets
                    text.append(body)
            else:
                text.append('Deleted')
        else:
            text.append('Deleted')

    df = pd.DataFrame(columns=['date', 'text'])

    df['date'] = date
    df['text'] = text

    return df


# Page count is limited by news until 2016
offsets = range(0, 1101)
frames = []

# Loop that goes through all the available pages
for i in offsets:
    time.sleep(randrange(5))
    print(i)
    cmc = requests.get(f'https://www.coindesk.com/wp-json/v1/articles/format/news/{i}?mode=list')
    time.sleep(3)
    webpage = cmc.json()
    df = collect_data_cd(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/news/coindesk.csv', index=False)


# The Block scrapper
def collect_data_block(wp):
    date = []
    content_text = []

    # same range as posts per page
    for j in wp['posts']:
        date.append(j['published'])
        body = re.sub('\<.+?\>|\<\/.+?\>', ' ', j['body'])  # clean code snippets
        content_text.append(body)

    df = pd.DataFrame(columns=['date', 'text'])

    df['date'] = date
    df['text'] = content_text

    return df


# Starts from page 1 and not 0 (!) to 359
offsets = range(1, 359)
frames = []

# Loop that goes through all the available pages
for i in offsets:
    time.sleep(randrange(3))
    print(i)
    cmc = requests.get(f'https://www.theblockcrypto.com/wp-json/v1/posts/homepage?page={i}&posts_per_page=20')
    time.sleep(3)
    webpage = cmc.json()
    df = collect_data_block(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/news/block.csv', index=False)


# JSON scrapper with rendered content
def collect_data_json(wp):
    date = []
    content_text = []

    # same range as posts per page
    for j in wp:
        date.append(j['date'])
        content = re.sub('\<.+?\>|\<\/.+?\>', ' ', j['content']['rendered'])  # clean code snippets
        content_text.append(content)

    df = pd.DataFrame(columns=['date', 'text'])

    df['date'] = date
    df['text'] = content_text

    return df


# 1 to total of 163
offsets = range(1, 164)
frames = []

# Go through all the pages
for i in offsets:
    time.sleep(randrange(3))
    print(i)
    cmc = requests.get(f'https://blockonomi.com/wp-json/wp/v2/posts?page={i}&order=desc&orderby=date&per_page=25')
    time.sleep(3)
    webpage = cmc.json()
    df = collect_data_json(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/news/blockonomi.csv', index=False)


# Crypto News Flash scrapper
# 1 to 106
offsets = range(1, 107)
frames = []

# Go through all the pages
for i in offsets:
    time.sleep(randrange(3))
    print(i)
    cmc = requests.get(f'https://www.crypto-news-flash.com/wp-json/wp/v2/posts?order=desc&orderby=date&per_page=25&page={i}')
    time.sleep(3)
    webpage = cmc.json()
    df = collect_data_json(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/news/cnf.csv', index=False)

# News btc sraper
# 1 to 975
offsets = range(1, 976)
frames = []

# Go through all the pages
for i in offsets:
    time.sleep(randrange(3))
    print(i)
    cmc = requests.get(f'https://www.newsbtc.com/wp-json/wp/v2/posts?order=desc&orderby=date&per_page=25&page={i}')
    time.sleep(3)
    webpage = cmc.json()
    df = collect_data_json(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/news/newsbtc.csv', index=False)

# Cryptoslate srapper
# 1 to 206
offsets = range(1, 207)
frames = []

# Go through all the pages
for i in offsets:
    time.sleep(randrange(3))
    print(i)
    cmc = requests.get(f'https://cryptoslate.com/wp-json/wp/v2/posts?order=desc&orderby=date&per_page=25&page={i}')
    time.sleep(3)
    webpage = cmc.json()
    df = collect_data_json(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/news/slate.csv', index=False)

# Cryptonews HTML srapper
driver = webdriver.Firefox()

# Load more articles from the general news page
driver.get('https://cryptonews.com/news/')

while True:
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
    time.sleep(1)
    driver.find_element_by_link_text("Load more news...").click()

# Collect all the links
list = driver.find_elements_by_xpath('//*[@id="body-news"]//div[contains(@class, "app")]//section[contains(@class, "cn-articles-list")]//div[contains(@id, "newsContainer")]//div[contains(@class, "cn-tile article")]//div[contains(@class, "props")]/h4/a')

links=[]
for i in list:
    links.append(i.get_attribute('href'))

# Get text from the article
texts = []
date = []
for i in links:
    driver.get(i)
    article = driver.find_element_by_xpath('//*[@id="body-news"]//div[contains(@class, "app")]/article//div[contains(@class, "content")]//div[contains(@class, "cn-content")]').text
    time_published = driver.find_element_by_xpath('//*[@id="body-news"]//div[contains(@class, "app")]/article//div[contains(@class, "content")]//div[contains(@class, "cn-props-panel")]//div[contains(@class, "time")]/time').get_attribute("datetime")
    texts.append(article)
    date.append(time_published)

# Combine as dataframe
result = pd.DataFrame(columns=['date', 'text'])
result['date'] = date
result['text'] = texts

result.to_csv(data_folder + '/news/cryptonews.csv', index=False)

# News Bitcoin HTML srapper
# Collect all the article links
links = []
for i in range(1, 1380):
    driver.get(f'https://news.bitcoin.com/page/{i}/')
    list = driver.find_elements_by_xpath('//body//div[contains(@id, "td-outer-wrap")]//div[contains(@class, "td-main-content-wrap")]//div[contains(@class, "td-pb-article-list")]//div[contains(@class, "td-pb-row")]//div[contains(@class, "td-main-content")]//div[contains(@class, "td-ss-main-content")]//div[contains(@class, "standard__article standard__article__grid")]//div[contains(@class, "story story--medium")]//div[contains(@class, "story--medium__info")]/a')

    for j in list:
        links.append(j.get_attribute('href'))

# Get text from the article
texts = []
date = []
for i in links:
    driver.get(i)
    time.sleep(0.5)
    article = driver.find_elements_by_xpath('//body//div[contains(@id, "td-outer-wrap")]//div[contains(@id, "bn-ajax-load-more")]//div[contains(@id, "ajax-load-more")]//div[contains(@class, "alm-listing alm-ajax")]//div[contains(@class, "alm-single-post")]//main[contains(@class, "article full-grid")]//article[contains(@class, "article__body")]')[0].text
    time_published = driver.find_elements_by_xpath('//div[contains(@class, "article__info__right")]//time')[0].text
    texts.append(article)
    date.append(time_published)

# Combine as dataframe
result = pd.DataFrame(columns=['date', 'text'])
result['date'] = date
result['text'] = texts

result.to_csv(data_folder + '/news/newsbitcoin.csv', index=False)

# Ambcrypto html srapper
# Load the page all the way through
driver.get('https://eng.ambcrypto.com/category/news/')

while True:
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
    driver.find_element_by_xpath('//a[contains(@class, "mvp-inf-more-but")]').click()
    time.sleep(1)

# Collect all the article links
list = driver.find_elements_by_xpath('//li[contains(@class, "mvp-blog-story-wrap left relative infinite-post")]/a')

links=[]
for i in list:
    links.append(i.get_attribute('href'))

# Collect all the texts and dates
texts = []
date = []
for i in links:
    driver.get(i)
    time.sleep(1)
    article = driver.find_elements_by_xpath('//div[contains(@id, "mvp-content-main")]')[0].text
    time_published = driver.find_elements_by_xpath('//span[contains(@class, "mvp-post-date updated")]/time')[0].get_attribute("datetime")
    texts.append(article)
    date.append(time_published)

# Combine as dataframe
result = pd.DataFrame(columns=['date', 'text'])
result['date'] = date
result['text'] = texts

result.to_csv(data_folder + '/news/ambcrpyto.csv', index=False)

# Cointelegraph html srapper
# Load the page all the way through
driver.get('https://cointelegraph.com/')
links=[]
for i in list:
    links.append(i.get_attribute('href'))

news= [x for x in links if "https://cointelegraph.com/news/" in x]

# Collect texts and dates
texts = []
date = []
for i in news:
    driver.get(i)
    time.sleep(0.5)
    article = driver.find_elements_by_xpath('//div[contains(@class, "post-content")]')[0].text
    time_published = driver.find_elements_by_xpath('//div[contains(@class, "post-meta__publish-date")]/time')[0].get_attribute("datetime")
    texts.append(article)
    date.append(time_published)

# Combine as dataframe
result = pd.DataFrame(columns=['date', 'text'])
result['date'] = date
result['text'] = texts

result.to_csv(data_folder + '/news/cointelegraph.csv', index=False)

# Stop webdriver
driver.quit()

# Combine all dataframes into one
frames = []
for i in listdir(r'data'):
    df = pd.read_csv(f'data/{i}')
    source = ''.join(i.split())[:-4]
    df['source'] = source
    if source == 'newsbitcoin':
        df['date'][0:5] = 'Nov 5, 2020'
        df['date'][5:10] = 'Nov 4, 2020'
        df['date'][10:15] = 'Nov 3, 2020'
        df['date'][15:21] = 'Nov 2, 2020'
        df['date'][21:24] = 'Nov 1, 2020'
    df.date = pd.to_datetime(df.date, infer_datetime_format=True, utc = True)
    frames.append(df)


# Sort data and clean for duplicates and deleted news
result = pd.concat(frames, ignore_index=True)
result['date'] = result['date'].dt.date
result = result.sort_values("date", ascending=False)
result = result.dropna(subset=['text'])
result.drop_duplicates(subset=['text'], keep=False,inplace=True)

result.to_csv(data_folder + '/all_articles.csv', index=False)

# Number of articles per source per day
result.groupby(result.date).source.value_counts()


# TVL data collection
def collect_tvl(wp):
    time = []
    tvlusd = []
    tvleth = []
    dai = []

    for i in wp:
        time.append(datetime.fromtimestamp(i['timestamp']))
        tvlusd.append(i['tvlUSD'])
        tvleth.append(i['tvlETH'])
        dai.append(i['DAI'])

    result = pd.DataFrame(columns=['time', 'tvlusd', 'tvleth', 'dai'])
    result['time'] = time
    result['tvlusd'] = tvlusd
    result['tvleth'] = tvleth
    result['dai'] = dai

    return result


# Collect TVL data
frames = []
defi = ['maker', 'uniswap', 'compound']

for i in defi:
    cmc = requests.get(f'https://data-api.defipulse.com/api/v1/defipulse/api/GetHistory?api-key=472f113c6538b63a9ecfaebde7c1c3ede835f8a4059539015a7ff0ec95e7&project={i}&resolution=history')
    webpage = cmc.json()
    result = collect_tvl(webpage)
    result['token'] = i
    frames.append(result)

result = pd.concat(frames, ignore_index=True)
result.time = pd.to_datetime(result.time, infer_datetime_format=True, utc=True)
result.time = result.time.dt.date
result.to_csv(data_folder + '/defi/tvl_data.csv', index=False)
