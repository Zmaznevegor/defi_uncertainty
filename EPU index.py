# Data wrangling
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import pearsonr
from scipy import stats
import dill

# Preprocessing
from nltk.corpus import stopwords
import string
import re
import spacy

# Remove warnings
import warnings
warnings.filterwarnings('ignore')

# Define folder and load data
data_folder = r'/home/zmaznevegor/PycharmProjects/defi_uncertainty/data'
data = pd.read_csv(data_folder + '/all_articles.csv')
tvl = pd.read_csv(data_folder + '/tvl_data.csv')


# Naive base method
# Construct base matching parameters for the articles
def match_df(grep, case_sense:bool = False, dataframe=data):
    grep_bool = dataframe.text.str.contains(grep, case=case_sense, regex=True)
    df = dataframe.loc[grep_bool[grep_bool==True].index,].reset_index(drop=True)
    return df


defi = match_df(grep=' defi |decentrali(z|s)ed finance')
defi_regulation = match_df(grep=' regulat| supreme court| government| European Commission|legislati| European Central Bank|\bjurisdiction\b|\bSEC\b|\bcompliance\b', dataframe=defi)
uncertainty_words = '\bapparently\b|\bapparent\b|\bindication\b|\bcould\b|\bpossible\b|\bevasive\b|uncertain|\bpotential\b|\bquestionably\b|doubt|suspect|putative|would|not easily|no notion|\blikely\b|either|\bassume\b|\bhope\b|\bthink\b|\bseem\b|\bperhaps\b|\belusive\b|\bunsure\b|\bpotentially|\bappear\b|\bdoubtful\b|\bsuspecting\b|\bprobably\b|\bmay\b|\bassumption\b|\bhypothesis\|\bpotential\b|\bpossibility\b|\bunstable\b|\bunknown\b|\bpreferential\b|\bspeculate\b|\bpresumably\b|\bprobable\b|\bhypothetical\b|\bpotentially\b|\bunsettled\b|\bunfamiliar\b|\bpreferentially\b|\bspeculation\b|\bsuppose\b|\bsupposedly\b|\bmaybe\b|\bseemingly\b|\bunclear\b|\bimprobable\b|\bestimate\b|\bsuggest\b|\bexpect\b|\bpretended\b|\bperchance\b|\buncertainty\b|\bpossibly\b|\bostensibly\b|\bvague\b|\bimprobably\b|\bquestionable\b|\bdoubt\b|\bexpecting\b|\bsupposed\b|\bmight\b'
epu_base = match_df(grep=uncertainty_words, dataframe=defi_regulation)

# Make monthly count of EPU-addressed news
epu_base.date = pd.to_datetime(epu_base.date).dt.strftime('%Y-%m')
epu_month=epu_base[['date','text']].groupby(by="date").count().reset_index()
epu_month.plot.line(x='date', y='text')

# Scale data
data.date = pd.to_datetime(data.date).dt.strftime('%Y-%m')
texts_per_media = data.groupby(by=["date", "source"]).count().reset_index()
unc_media = epu_base.groupby(by=["date", "source"]).count().reset_index()

scale = pd.merge(texts_per_media,unc_media, on=['date', 'source'], how='right') # change to left to get for all
scale = scale.rename(columns={'text_x':'total', 'text_y':'uncertain'})

# Standardize and normalize
months = scale.groupby(by="date").count().reset_index().date
# splitting into t1 and t2, where t1 contains 80% of observations
T1 = months[0:22]
T2 = months[22:28]

scale['scaled'] = scale.uncertain/scale.total
scale['standardized'] = scale.scaled/np.std(scale.scaled[scale.date.isin(T1)])
z = scale.groupby(by="date").mean().standardized.reset_index()
m = np.mean(scale.standardized[scale.date.isin(T2)])
z['epu'] = z.standardized*100/m

maker = tvl[tvl.token == 'maker']
maker.time = pd.to_datetime(maker.time).dt.strftime('%Y-%m')
maker_epu = maker.groupby(by="time").head(1).reset_index(drop=True)
maker_epu[maker_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time')['tvleth'] # relevant to EPU parameter of TVL

pearsonr(z['epu'], maker_epu[maker_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time')['tvleth'])
z.to_csv(data_folder + '/epu_standard.csv', index=False)



# Check with weekly data
epu_base.date = pd.to_datetime(epu_base.date).dt.strftime('%Y-%V')
epu_month=epu_base[['date','text']].groupby(by="date").count().reset_index()
epu_month.plot.line(x='date', y='text')

# Scale data
data.date = pd.to_datetime(data.date).dt.strftime('%Y-%V')
texts_per_media = data.groupby(by=["date", "source"]).count().reset_index()
unc_media = epu_base.groupby(by=["date", "source"]).count().reset_index()

scale = pd.merge(texts_per_media,unc_media, on=['date', 'source'], how='right') # change to left to get for all
scale = scale.rename(columns={'text_x':'total', 'text_y':'uncertain'})

# Standardize and normalize
weeks = scale.groupby(by="date").count().reset_index().date
round(0.8*len(weeks))+1
# splitting into t1 and t2, where t1 contains 80% of observations
T1 = weeks[0:round(0.8*len(weeks))+1]
T2 = weeks[(round(0.8*len(weeks))+1):len(weeks)]

scale['scaled'] = scale.uncertain/scale.total
scale['standardized'] = scale.scaled/np.std(scale.scaled[scale.date.isin(T1)])
z = scale.groupby(by="date").mean().standardized.reset_index()
m = np.mean(scale.standardized[scale.date.isin(T2)])
z['epu'] = z.standardized*100/m

maker = tvl[tvl.token == 'maker']
maker.time = pd.to_datetime(maker.time).dt.strftime('%Y-%V')
maker_epu = maker.groupby(by="time").head(1).reset_index(drop=True)
maker_epu[maker_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time')['tvleth'] # relevant to EPU parameter of TVL

pearsonr(z['epu'], maker_epu[maker_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time')['tvleth'])

# Consequent timing
pd.date_range(start="2017-12-18",end="2020-11-11").strftime('%Y-%V')


# SVM Method
# TODO: randomly select 500 articles related to DeFi
# TODO: label training and test as relevant/not based on the text
# TODO: clean and lemmarize the text
# NLTK Stop words
stop = stopwords.words('english')
exclude = set(string.punctuation)


# Data preprocessing
# Define cleaning function to drop url, stopwords and numbers
def clean(text):
    text = re.sub('http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\), ]|(?:%[0-9a-fA-F][0-9a-fA-F]))+', ' ',text)  # remove urls
    text = re.sub('\d+[.,]?\d*', '', text)  # remove numbers
    text = re.sub('\[(.+?)\]|\((.+?)\)', r'\1 ', text)  # remove brackets marks
    text = re.sub('#', '', text)  # remove hashtags
    text = re.sub('\\n', '', text)  # remove hashtags
    stop_free = " ".join([i for i in text.lower().split() if i not in stop])
    processed_text = ' '.join(ch for ch in stop_free.split() if ch not in exclude)
    return processed_text


def lemmatize(text):
    preprocessed_tokens = []  # preprocessed_tokens will be lemmatized, stopwords removed and lowercased
    nlp = spacy.load("en_core_web_sm", disable=['ner', 'parser'])  # We will not use NER and syntactic parser.
    doc = nlp(text)
    for token in doc:
        if not token.is_stop and len(token) > 3:
            preprocessed_tokens.append(token.lemma_.lower())
    return ' '.join(preprocessed_tokens)


data['clean_text'] = data.text.apply(clean)  # Clean texts
data['lemmatized_text'] = data.clean_text.apply(lemmatize) # lemmatize texts
# TODO: construct tf-idf
# TODO: run SVM
# TODO: pool-based active learner
# TODO: 10K CV with AUC
# TODO: Check most discriminant words

# Extend dictionary methodology


# Pickle environment
dill.dump_session(data_folder+'/env.pkl')