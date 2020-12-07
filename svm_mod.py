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

# Modification:SVM Method
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