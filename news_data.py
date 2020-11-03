# Load libraries
from bs4 import BeautifulSoup
import requests
import json
import time
from random import randrange

# Basic text cleaning
import re

# Data wrangling
import pandas as pd
import numpy as np
from os import listdir

# Define folder
data_folder = r'/home/zmaznevegor/PycharmProjects/defi_uncertainty/data'


# Data collection
def collect_data_decrypt(wp):
    date = []
    content_text = []
    category = []

    # same range as posts per page
    for j in range(0, len(wp)):
        date.append(wp[j]['date'])
        content_text.append(wp[j]['custom_fields']['content_text'])
        # category.append(webpage[j]['custom_fields']['primary_category']['slug'])

    df = pd.DataFrame(columns=['date', 'text'])

    df['date'] = date
    df['text'] = content_text
    # df['category'] = category

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
result.to_csv(data_folder + '/coindesk.csv', index=False)


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
result.to_csv(data_folder + '/block.csv', index=False)


# Blockonomi scrapper
def collect_data_blockonomi(wp):
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
    df = collect_data_blockonomi(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/blockonomi.csv', index=False)

# TODO: News Bitcoin scrapper !
# 1 to 123
cmc = requests.get(f'https://news.bitcoin.com/page/1377/')
webpage = cmc.json()


# Crypto News Flash scrapper
def collect_data_cnf(wp):
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


# 1 to 106
offsets = range(106, 107)
frames = []

# Go through all the pages
for i in offsets:
    time.sleep(randrange(3))
    print(i)
    cmc = requests.get(f'https://www.crypto-news-flash.com/wp-json/wp/v2/posts?order=desc&orderby=date&per_page=25&page={i}')
    time.sleep(3)
    webpage = cmc.json()
    df = collect_data_cnf(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/cnf.csv', index=False)

# Combining and cleaning all the data
frames = []
for i in listdir(r'data'):
    frames.append(pd.read_csv(f'data/{i}'))

result = pd.concat(frames, ignore_index=True)
result = result.sort_values("date", ascending=False)
result = result.dropna(subset=['text'])
result = result[result.text != 'Deleted']
result.drop_duplicates(keep=False,inplace=True)
result.to_csv(data_folder + '/all_articles.csv', index=False)
