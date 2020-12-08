# Data wrangling
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import pearsonr
from scipy import stats
import dill

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


defi = match_df(grep=' defi |decentrali(z|s)ed finance')
defi_regulation = match_df(grep=' regulat| supreme court| government| European Commission|legislati| European Central Bank|\bjurisdiction\b|\bSEC\b|\bcompliance\b', dataframe=defi)

# Large corpus
#uncertainty_words = '\bapparently\b|\bapparent\b|\bindication\b|\bcould\b|\bpossible\b|\bevasive\b|uncertain|\bpotential\b|\bquestionably\b|doubt|suspect|putative|would|not easily|no notion|\blikely\b|either|\bassume\b|\bhope\b|\bthink\b|\bseem\b|\bperhaps\b|\belusive\b|\bunsure\b|\bpotentially|\bappear\b|\bdoubtful\b|\bsuspecting\b|\bprobably\b|\bmay\b|\bassumption\b|\bhypothesis\|\bpotential\b|\bpossibility\b|\bunstable\b|\bunknown\b|\bpreferential\b|\bspeculate\b|\bpresumably\b|\bprobable\b|\bhypothetical\b|\bpotentially\b|\bunsettled\b|\bunfamiliar\b|\bpreferentially\b|\bspeculation\b|\bsuppose\b|\bsupposedly\b|\bmaybe\b|\bseemingly\b|\bunclear\b|\bimprobable\b|\bestimate\b|\bsuggest\b|\bexpect\b|\bpretended\b|\bperchance\b|\buncertainty\b|\bpossibly\b|\bostensibly\b|\bvague\b|\bimprobably\b|\bquestionable\b|\bdoubt\b|\bexpecting\b|\bsupposed\b|\bmight\b'

# Base uncertainty corpus
uncertainty_words = 'uncertainty|uncertain| not certain'
epu_base = match_df(grep=uncertainty_words, dataframe=defi_regulation)

# Check with weekly data
epu_base.date = pd.to_datetime(epu_base.date).dt.strftime('%Y-%U')
epu_month=epu_base[['date','text']].groupby(by="date").count().reset_index()
epu_month.plot.line(x='date', y='text')

# Scale data
data.date = pd.to_datetime(data.date).dt.strftime('%Y-%U')
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

z.plot(x = 'date', y = 'epu')

# Fix last week bias
z = z[z.date <'2020-45']

# Maker data (DEX token)
maker = tvl[tvl.token == 'maker']
maker.time = pd.to_datetime(maker.time).dt.strftime('%Y-%U')
maker_epu = maker.groupby(by="time").head(1).reset_index(drop=True)
maker_epu[maker_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time') # relevant to EPU parameter of TVL
pearsonr(z['epu'], maker_epu[maker_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time')['tvleth'])

# Compound data (yeild farming)
compound = tvl[tvl.token == 'compound']
compound.time = pd.to_datetime(compound.time).dt.strftime('%Y-%U')
compound_epu = compound.groupby(by="time").head(1).reset_index(drop=True).fillna(compound.tvleth.iloc[1])
compound_epu[compound_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time') # relevant to EPU parameter of TVL
pearsonr(z[z.date > '2018-37']['epu'], compound_epu[compound_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time')['tvleth'])

# Uniswap data (representaative stablecoin)
uniswap = tvl[tvl.token == 'uniswap']
uniswap.time = pd.to_datetime(uniswap.time).dt.strftime('%Y-%U')
uniswap_epu = uniswap.groupby(by="time").head(1).reset_index(drop=True)
uniswap_epu[uniswap_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time') # relevant to EPU parameter of TVL
pearsonr(z[z.date > '2018-42']['epu'], uniswap_epu[uniswap_epu.time.isin(z.date)].reset_index(drop=True).sort_values('time')['tvleth'])

# Export for VAR in defi
maker.to_csv(data_folder + '/defi/maker.csv', index=False)
compound.to_csv(data_folder + '/defi/compound.csv', index=False)
uniswap.to_csv(data_folder + '/defi/uniswap.csv', index=False)
z.to_csv(data_folder + '/weekly_epu_base.csv', index=False)