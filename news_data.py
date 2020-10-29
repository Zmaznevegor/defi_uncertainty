# Load libraries
from bs4 import BeautifulSoup
import requests
import json
import time
from random import randrange

# Data wrangling
import pandas as pd
import numpy as np
import dill

# Define folder
data_folder = r'/home/zmaznevegor/PycharmProjects/defi_uncertainty/data'


# Data collection
def collect_data(wp):
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
    cmc = requests.get(f'https://api.decrypt.co/content/posts?_minimal=true&category=news&lang=en-US&offset={i}&order=desc&orderby=date&per_page=25')
    time.sleep(5)
    webpage = cmc.json()
    df = collect_data(webpage)
    frames.append(df)

# Combining and exporting all the results
result = pd.concat(frames, ignore_index=True)
result.to_csv(data_folder + '/decrypt.csv', index=False)

# TODO: Coindesk website data
cmc = requests.get(f'https://www.coindesk.com/wp-json/v1/articles/format/news/{1100}?mode=list', stream=True)
webpage = cmc.json()

# TODO: CCN scrapper
cmc = requests.get(f'https://www.coindesk.com/wp-json/v1/articles/format/news/{1100}?mode=list', stream=True)
webpage = cmc.json()

# TODO: The Block scrapper
cmc = requests.get(f'https://www.coindesk.com/wp-json/v1/articles/format/news/{1100}?mode=list', stream=True)
webpage = cmc.json()

# TODO: Modern Consensus scrapper
cmc = requests.get(f'https://www.coindesk.com/wp-json/v1/articles/format/news/{1100}?mode=list', stream=True)
webpage = cmc.json()

# TODO: pkl data
