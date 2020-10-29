# Load libraries
from bs4 import BeautifulSoup
import requests
import json
import time

# Data wrangling
import pandas as pd
import numpy as np
import dill

# Define folder
data_folder = r'/home/zmaznevegor/PycharmProjects/defi_uncertainty/data'

# Data collection
# TODO: Decrypt website



# TODO: define function that saves the descriptions and timestamps
# TODO: define time breaks
# TODO: construct a dataframe

cmc = requests.get(
    'https://api.decrypt.co/content/posts?_minimal=true&category=news&lang=en-US&offset=0&order=desc&orderby=date&per_page=250')
webpage = cmc.json()

offsets = np.arange(0, 6250, 250).tolist()


def collect_data(cmc):

    # Collect historical data
    date = []
    content_text = []
    category_slug = []

    for i in coins:
        time.sleep(3)
        cmc = requests.get(f'https://api.decrypt.co/content/posts?_minimal=true&category=news&lang=en-US&offset={offsets[i]}&order=desc&orderby=date&per_page=250')
        time.sleep(3)
        webpage = cmc.json()

        historical_data = json.loads(data.contents[0])
        quotes = historical_data['props']['initialState']['cryptocurrency']['ohlcvHistorical'][i]['quotes']
        info = historical_data['props']['initialState']['cryptocurrency']['info']['data'][i]

        name.append(info['name'])
        symbol.append(info['symbol'])
        slug.append(info['slug'])
        f_desc.append(info['description'])
        subreddit.append(info['urls']['reddit'])
        code.append(info['urls']['source_code'])
        category.append(info['category'])
        status.append(info['status'])

        for j in quotes:
            market_cap.append(j['quote']['USD']['market_cap'])
            volume.append(j['quote']['USD']['volume'])
            timestamp.append(j['quote']['USD']['timestamp'])

        df_hist = pd.DataFrame(columns=['marketcap',
                                        'volume',
                                        'timestamp'])

        df_hist['marketcap'] = market_cap
        df_hist['volume'] = volume
        df_hist['timestamp'] = timestamp

        hist[coins[i]] = df_hist

    df = pd.DataFrame(columns=['name',
                               'symbol',
                               'slug',
                               'description',
                               'subreddit',
                               'code',
                               'category',
                               'status'])

    df['name'] = name
    df['symbol'] = symbol
    df['slug'] = slug
    df['description'] = f_desc
    df['subreddit'] = subreddit
    df['code'] = code
    df['category'] = category
    df['status'] = status

    return df, hist


# For the first page
data = list(collect_data(cmc))

# Scrapping data for the rest of the pages
for i in range(2, 23):
    url = f'https://coinmarketcap.com/{i}/'
    cmc = requests.get(url)
    time.sleep(3)
    page_content = list(collect_data(cmc))
    time.sleep(3)
    data.append(page_content[0])
    data.append(page_content[1])

# Save textual descriptions
frames = []
for i in range(0, 44):
    if type(data[i]) == pd.DataFrame:
        frames.append(data[i])

result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder+'/crypto.csv', index= False)


# TODO: Coindesk website data
cmc = requests.get('https://headlines-api.coinmarketcap.com/articles?page=0&size=20&type=news')
webpage = cmc.json()


# TODO: pkl data