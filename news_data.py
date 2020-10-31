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
import dill

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
offsets = range(912, 1101)
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


# TODO: The Block scrapper
# Starts from page 1 and not 0 (!)
cmc = requests.get(f'https://www.theblockcrypto.com/wp-json/v1/posts/homepage?page={359}&posts_per_page=20')
webpage = cmc.json()

# TODO: Modern Consensus scrapper
# 1 to 309
cmc = requests.get(f'https://modernconsensus.com/page/309/')
webpage = cmc.json()

# TODO: Blockonomi scrapper
# 1 to 123
cmc = requests.get(f'https://blockonomi.com/news/page/{i}/')
webpage = cmc.json()

# TODO: News Bitcoin scrapper
# 1 to 123
cmc = requests.get(f'https://news.bitcoin.com/page/1375/')
webpage = cmc.json()

# TODO: Crypto News Flash scrapper
# 1 to 123
cmc = requests.get(f'https://www.crypto-news-flash.com/news/page/239/')
webpage = cmc.json()

# TODO: Hackenoon scrapper
# Pages 1 to 49
cmc = requests.get(f'https://www.hackernoon.com/tagged/decentralization?page={i}')
webpage = cmc.json()

# TODO: pkl data

# TODO: check duplicates for the page 0 and page 1

# TODO: VAR model for the relationship capture
