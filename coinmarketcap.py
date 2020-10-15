# Web-Scrapping
from bs4 import BeautifulSoup
import requests
import json
import time

# Data
import pandas as pd

# Define folder
data_folder = r'/home/zmaznevegor/PycharmProjects/pythonProject/cointext/data'

# Data collection
# Base website
cmc = requests.get('https://coinmarketcap.com')
soup = BeautifulSoup(cmc.content, 'html.parser')

# print(soup.title)
# print(soup.prettify())

# Finding the correct block
data = soup.find('script',
                 id="__NEXT_DATA__",
                 type="application/json")

# Creating dictionary where we will collect id's and slugs
coins = {}

coin_data = json.loads(data.contents[0])
listings = coin_data['props']['initialState']['cryptocurrency']['listingLatest']['data']

for i in listings:
    coins[str(i['id'])] = i['slug']

# Collect historical data
market_cap = []
volume = []
timestamp = []
name = []
symbol = []
slug = []
f_desc = []
subreddit = []
code = []
category = []
status = []
hist = {}

for i in coins:
    time.sleep(3)
    page = requests.get(f'https://coinmarketcap.com/currencies/{coins[i]}/historical-data/?start=20200101&end=20200923')
    time.sleep(5)
    soup = BeautifulSoup(page.content, 'html.parser')

    data = soup.find('script',
                     id="__NEXT_DATA__",
                     type="application/json")

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

# Save data
df.to_csv(data_folder+'/crypto.csv', index= False)

for d in hist:
    hist[d].to_csv(data_folder + f'/{d}.csv', index=False)
