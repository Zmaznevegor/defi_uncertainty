# Data
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Preprocessing
from nltk.corpus import stopwords
import string
import re
import spacy

import gensim
from gensim import corpora, models
from gensim.models import CoherenceModel
from gensim.utils import simple_preprocess

# LDA Visualisation
import pyLDAvis.gensim

# Define folder and load data
data_folder = r'/home/zmaznevegor/PycharmProjects/pythonProject/cointext'
data = pd.read_csv(data_folder + '/crypto.csv')

# NLTK Stop words
stop = stopwords.words('english')
exclude = set(string.punctuation)
for i in range(0, 2200):
    stop.extend([str(data.name[i].lower())])
for i in range(0, 2200):
    stop.extend([str(data.symbol[i].lower())])
stop.extend(['cryptocurrency', 'binance', 'coinbase', 'pro', 'okex', 'kraken', 'huobi', 'global', 'bitfinex'])


# Data preprocessing
# Define cleaning function to drop url, stopwords and numbers
def clean(text):
    text = re.sub('http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\), ]|(?:%[0-9a-fA-F][0-9a-fA-F]))+', ' ',
                  text)  # remove urls
    text = re.sub('\d+[.,]?\d*', '', text)  # remove numbers
    text = re.sub('\[(.+?)\]|\((.+?)\)', r'\1 ', text)  # remove brackets marks
    text = re.sub('#', '', text)  # remove hashtags
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


data['text'] = data.description.apply(clean)  # Clean texts
data['lemmatized_text'] = data.text.apply(
    lemmatize)  # Add new column with lemmatized and preprocessed texts. This task requires a lot of time.
data.drop(['name', 'subreddit', 'category', 'status', 'code'], axis=1,
          inplace=True)  # We don't need 'keyword', 'location' columns.

data.to_pickle(data_folder + '/cleaned_lemmatized_train.pkl')
data = pd.read_pickle(data_folder + '/cleaned_lemmatized_train.pkl')
data.head(10)

# Preparing DTM
# Creating the term dictionary of our courpus, where every unique term is assigned an index
texts = [doc.split() for doc in data.lemmatized_text]
dictionary = corpora.Dictionary(texts)
dictionary.filter_extremes(no_below=2, no_above=0.4)

# Term Document Frequency
corpus = [dictionary.doc2bow(text) for text in texts]


# Model assesment
def topic_info(model, num_topics):
    word_dict = {}

    coherence_model_lda = CoherenceModel(model=model,
                                         texts=texts,
                                         dictionary=dictionary,
                                         coherence='c_v')
    coherence_lda = coherence_model_lda.get_coherence()

    print('\nPerplexity: ', model.log_perplexity(corpus))  # a measure of how good the model is. lower the better.
    print('\nCoherence Score: ', coherence_lda)  # higher the better

    for i in range(num_topics):
        words = model.show_topic(i, topn=40);
        word_dict['Topic # ' + '{:02d}'.format(i + 1)] = [i[0] for i in words];

    return pd.DataFrame(word_dict)


# Building LDA
num_topics = 6

lda = gensim.models.ldamodel.LdaModel(corpus=corpus,
                                      id2word=dictionary,
                                      num_topics=num_topics)

topic_info(lda, num_topics)

# Visualise LDA
vis = pyLDAvis.gensim.prepare(lda, corpus, dictionary)
pyLDAvis.save_html(vis, 'lda.html')

# Mallet LDA
mallet_folder = r'/home/zmaznevegor/mallet-2.0.8/bin/mallet'
ldamallet = gensim.models.wrappers.LdaMallet(mallet_folder,
                                             corpus=corpus,
                                             num_topics=num_topics,
                                             id2word=dictionary)

coherence_model_ldamallet = CoherenceModel(model=ldamallet, texts=texts, dictionary=dictionary, coherence='c_v')
coherence_ldamallet = coherence_model_ldamallet.get_coherence()
print('\nCoherence Score: ', coherence_ldamallet)


def compute_coherence_values(dictionary, corpus, texts, limit, start=3, step=1):
    coherence_values = []
    model_list = []
    for num_topics in range(start, limit, step):
        model = gensim.models.wrappers.LdaMallet(mallet_folder,
                                                 corpus=corpus,
                                                 num_topics=num_topics,
                                                 id2word=dictionary)

        model_list.append(model)
        coherencemodel = CoherenceModel(model=model,
                                        texts=texts,
                                        dictionary=dictionary,
                                        coherence='c_v')
        coherence_values.append(coherencemodel.get_coherence())

    return model_list, coherence_values


model_list, coherence_values = compute_coherence_values(dictionary=dictionary,
                                                        corpus=corpus,
                                                        texts=texts,
                                                        start=4,
                                                        limit=100,
                                                        step=2)

# Check the best topic number
limit = 20
start = 4
step = 2
x = range(start, limit, step)
plt.plot(x, coherence_values)
plt.xlabel("Num Topics")
plt.ylabel("Coherence score")
plt.legend("coherence_values", loc='best')
plt.show()

max(coherence_values)

# LDA with 8 topics
lda = model_list[2]
model_converted = gensim.models.wrappers.ldamallet.malletmodel2ldamodel(lda)
vis = pyLDAvis.gensim.prepare(model_converted, corpus, dictionary)
pyLDAvis.save_html(vis, 'lda_mallet.html')

# Data frame with topic distribution per document
doc_topic_distr = {}
topic0 = []
topic1 = []
topic2 = []
topic3 = []
topic4 = []
topic5 = []
topic6 = []
topic7 = []
dominant = []

for i in range(0, 2200):
    dominant.append(sorted(lda[corpus[i]], key=lambda x: (x[1]), reverse=True)[0][0])
    topic0.append(lda[corpus[i]][0][1])
    topic1.append(lda[corpus[i]][1][1])
    topic2.append(lda[corpus[i]][2][1])
    topic3.append(lda[corpus[i]][3][1])
    topic4.append(lda[corpus[i]][4][1])
    topic5.append(lda[corpus[i]][5][1])
    topic6.append(lda[corpus[i]][6][1])
    topic7.append(lda[corpus[i]][7][1])
    print(i)

doc_topic_distr['dominant'] = dominant
doc_topic_distr['topic0'] = topic0
doc_topic_distr['topic1'] = topic1
doc_topic_distr['topic2'] = topic2
doc_topic_distr['topic3'] = topic3
doc_topic_distr['topic4'] = topic4
doc_topic_distr['topic5'] = topic5
doc_topic_distr['topic6'] = topic6
doc_topic_distr['topic7'] = topic7

doc_topic_distr = pd.DataFrame(doc_topic_distr)
doc_topic_distr['name'] = data['slug']

doc_topic_distr = doc_topic_distr[['name', 'dominant', 'topic0', 'topic1', 'topic2', 'topic3', 'topic4', 'topic5', 'topic6', 'topic7']]
doc_topic_distr.to_csv(data_folder+'/topic_distribution.csv', index= False)



def format_topics_sentences(ldamodel=lda, corpus=corpus, texts=texts):
    # Init output
    sent_topics_df = pd.DataFrame()

    # Get main topic in each document
    for i, row in enumerate(ldamodel[corpus]):
        row = sorted(row, key=lambda x: (x[1]), reverse=True)
        # Get the Dominant topic, Perc Contribution and Keywords for each document
        for j, (topic_num, prop_topic) in enumerate(row):
            if j == 0:  # => dominant topic
                wp = ldamodel.show_topic(topic_num)
                topic_keywords = ", ".join([word for word, prop in wp])
                sent_topics_df = sent_topics_df.append(
                    pd.Series([int(topic_num), round(prop_topic, 4), topic_keywords]), ignore_index=True)
            else:
                break
    sent_topics_df.columns = ['Dominant_Topic', 'Perc_Contribution', 'Topic_Keywords']

    # Add original text to the end of the output
    contents = pd.Series(texts)
    sent_topics_df = pd.concat([sent_topics_df, contents], axis=1)
    return (sent_topics_df)


df_topic_sents_keywords = format_topics_sentences(ldamodel=lda, corpus=corpus, texts=texts)

# Clean the dataframe
df_dominant_topic = df_topic_sents_keywords.reset_index()
df_dominant_topic.columns = ['Document_No', 'Dominant_Topic', 'Topic_Perc_Contrib', 'Keywords', 'Text']

# DF with keywords and sentences on about the topics
sent_topics_sorteddf_mallet = pd.DataFrame()

sent_topics_outdf_grpd = df_topic_sents_keywords.groupby('Dominant_Topic')

for i, grp in sent_topics_outdf_grpd:
    sent_topics_sorteddf_mallet = pd.concat([sent_topics_sorteddf_mallet,
                                             grp.sort_values(['Perc_Contribution'], ascending=[0]).head(1)],
                                            axis=0)

sent_topics_sorteddf_mallet.reset_index(drop=True, inplace=True)
sent_topics_sorteddf_mallet.columns = ['Topic_Num', "Topic_Perc_Contrib", "Keywords", "Text"]
sent_topics_sorteddf_mallet.Topic_Num = sent_topics_sorteddf_mallet.Topic_Num + 1
