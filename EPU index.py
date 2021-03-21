# Data wrangling
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import pearsonr
# from scipy import stats
# import dill

# Remove warnings
import warnings
warnings.filterwarnings('ignore')

# Define folder and load data
data_folder = r'/home/zmaznevegor/PycharmProjects/defi_uncertainty/data'
data = pd.read_csv(data_folder + '/all_articles.csv')
tvl = pd.read_csv(data_folder + '/defi/tvl_data.csv')


# Naive base method
# Construct base matching parameters for the articles
def match_df(grep, case_sense:bool = False, dataframe=data):
    grep_bool = dataframe.text.str.contains(grep, case=case_sense, regex=True)
    df = dataframe.loc[grep_bool[grep_bool==True].index,].reset_index(drop=True)
    return df


defi = match_df(grep=' DeFi |decentrali(z|s)ed financ')
defi_regulation = match_df(grep=' regulat| \bregulation\b|\blegislation\b\regulatory\b|\bdeficit\b|regulatory|White House|Federal Reserve|\bCongress\b| supreme court| government| European Commission|legislat| European Central Bank|\bESMA\b|\bECB\b|\bFCA\b|\bEBA\b|\bjurisdiction\b|\bSEC\b|\bcompliance\b', dataframe=defi)

# Base uncertainty corpus
uncertainty_words = 'uncertain'
epu_base = match_df(grep=uncertainty_words, dataframe=defi_regulation)

# Check with weekly data
epu_base.date = pd.to_datetime(epu_base.date).dt.strftime('%Y-%V')
epu_week=epu_base[['date', 'text']].groupby(by="date").count().reset_index()
epu_week.plot.line(x='date', y='text')

# Scale data
data.date = pd.to_datetime(data.date).dt.strftime('%Y-%V')
texts_per_media = data.groupby(by=["date", "source"]).count().reset_index()
unc_media = epu_base.groupby(by=["date", "source"]).count().reset_index()

# Add consequent timing (start date is taken from the launch of maker)
scale = pd.merge(texts_per_media,unc_media, on=['date', 'source'], how='left') # change to right to get just for months with epu
scale = scale.rename(columns={'text_x':'total', 'text_y':'uncertain'})

# Take only relevant to defi time (based on the launch of maker)
scale = scale[scale.date > '2017-51'].fillna(0)

# Standardize and normalize
weeks = scale.groupby(by="date").count().reset_index().date

# splitting into t1 and t2, where t1 contains 80% of observations
T1 = weeks[0:round(0.8*len(weeks))+1]
T2 = weeks[(round(0.8*len(weeks))+1):len(weeks)]

scale['scaled'] = scale.uncertain/scale.total
scale['standardized'] = scale.scaled/np.std(scale.scaled[scale.date.isin(T1)])
z = scale.groupby(by="date").mean().standardized.reset_index()
m = np.mean(scale.standardized[scale.date.isin(T2)])
z['epu'] = z.standardized*100/m

plt.bar(z.date, z.epu, yerr = None)

# Fix last week bias
z = z[z.date <'2020-45']

