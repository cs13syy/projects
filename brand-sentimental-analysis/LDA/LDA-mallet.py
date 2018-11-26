# runtime error solution
import multiprocessing as mp
if __name__ == '__main__':
    mp.freeze_support()

import nltk
from pprint import pprint

# Gensim
import gensim
import gensim.corpora as corpora
from gensim.utils import simple_preprocess
from gensim.models import CoherenceModel

# spacy for lemmatization
# !conda install -c spacy spacy=0.101.0 -y
# !python -m spacy.en.download
import spacy

# Plotting tools
import pyLDAvis.gensim  # don't skip this

# Enable logging for gensim - optional
import logging

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.ERROR)

import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)
warnings.filterwarnings(action='ignore', category=UserWarning, module='gensim')

# NLTK Stop words
from nltk.corpus import stopwords

# ------------------------------------------------------------------------------------------

# stopwords
stop_words = stopwords.words('english')
stop_words.extend(['movie','film','marvel','mcu','make','see','go','feel','marvel'])
# stop_words.extend(['https','world','shirt','man','iron','ironman','suit','wear','watch','lot','follow','stark',
#                    'cap','work','tony','day','mcu','state','make','http','galaxy','avengers','war','marvel','watch',
#                    'movie','get','time','film','see','new','avenger','go','office','box','good','love','bad','fuck',
#                    'batman','watch','suit','give','great','favorite','today','give','top','black','panther','downey',
#                    'movie','good','end','top','people','start','run','year','make','normal','infinity']) #여기는 우리가 불용단어 추가하면 된다.
# print(stop_words)

# ------------------------------------------------------------------------------------------

# Import Dataset
def to_table(filename):
    with open(filename, 'r') as apple:
        a = []
        for apps in apple.readlines():
            b = (apps.strip())
            b = b.strip('.,?!;-')
            a.append(b)
            line_list = []
            for line in a:
                line = line.strip('.,?!;-')
                line_list += line.split()
    return line_list

df = open('Captain+Marvel100.txt', 'rt', encoding='UTF8')
# df = to_table('avengers100.txt')
# print(df)

# ------------------------------------------------------------------------------------------

tmp = []
for i in df:
    i = i.strip('.,?!;-').lower()
    texts = nltk.word_tokenize(i)
    tmp += nltk.pos_tag(texts)

# ------------------------------------------------------------------------------------------

# http://sens.tistory.com/454 참조, 형태소따라 나누기
def pos_extractor(parsed):

    noun_list = []
    adj_list = []
    verb_list = []
    nav_list = []
    adv_list = []

    for i in parsed:

        if i[1] in ['NN', 'NNS', 'NNP', 'NNPS']:
            noun_list.append(i[0])
            nav_list.append(i[0])
        elif i[1] in ['JJ', 'JJR', 'JJS']:
            adj_list.append(i[0])
            nav_list.append(i[0])
        elif i[1] in ['VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ']:
            verb_list.append(i[0])
            nav_list.append(i[0])
        elif i[1] == ['RB', 'RBR', 'RBS']:
            adv_list.append(i[0])
            nav_list.append(i[0])

        else:
            pass

    return [nav_list, noun_list, adj_list,
            verb_list]

# print(pos_extractor(tmp))

data_words = list(pos_extractor(tmp)[0])

# ------------------------------------------------------------------------------------------

# Build the bigram and trigram models
bigram = gensim.models.Phrases(data_words, min_count=5, threshold=100)  # higher threshold fewer phrases.
trigram = gensim.models.Phrases(bigram[data_words], threshold=100)

# Faster way to get a sentence clubbed as a trigram/bigram
bigram_mod = gensim.models.phrases.Phraser(bigram)
trigram_mod = gensim.models.phrases.Phraser(trigram)

# print(data_words)
# print(trigram_mod[bigram_mod[data_words]])

# ------------------------------------------------------------------------------------------

# Define functions for stopwords, bigrams, trigrams and lemmatization
def remove_stopwords(texts):
    return [[word for word in simple_preprocess(str(doc)) if word not in stop_words] for doc in texts]


def make_bigrams(texts):
    return [bigram_mod[doc] for doc in texts]


def make_trigrams(texts):
    return [trigram_mod[bigram_mod[doc]] for doc in texts]


def lemmatization(texts, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV']):
    """https://spacy.io/api/annotation"""
    texts_out = []
    for sent in texts:
        doc = nlp(" ".join(sent))
        texts_out.append([token.lemma_ for token in doc if token.pos_ in allowed_postags])
    return texts_out

# ------------------------------------------------------------------------------------------

# Remove Stop Words
data_words_nostops = remove_stopwords(data_words)
# print(data_words_nostops)

# Form Bigrams
data_words_bigrams = make_bigrams(data_words_nostops)

# Initialize spacy 'en' model, keeping only tagger component (for efficiency)
# python3 -m spacy download en (아나콘다 관리자 권한에서 실행)
nlp = spacy.load('en', disable=['parser', 'ner'])

# Do lemmatization keeping only noun, adj, vb, adv
data_lemmatized = lemmatization(data_words_bigrams, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV'])
data_lemmatized = remove_stopwords(data_lemmatized)
print(data_lemmatized)

# ------------------------------------------------------------------------------------------

# Create Dictionary
id2word = corpora.Dictionary(data_lemmatized)
# print(id2word)

# Create Corpus
texts = data_lemmatized
for i in texts:
    try:
        i == [] or ['\n']
        texts.remove(i)
    except IndexError:
        pass

list_a = []
for i in texts:
    try:
        list_a.append(i[0])
    except IndexError:
        pass

# Term Document Frequency
corpus = [id2word.doc2bow(list_a) for text in texts]
# print(corpus)
# print(corpus[2]) # 일부가 출력되었음

# Human readable format of corpus (term-frequency)
# [[(id2word[id], freq) for id, freq in cp] for cp in corpus[:1]]

# ------------------------------------------------------------------------------------------

mallet_path = 'C:/Users/yuniv/PycharmProjects/untitled/growth-hackers/mallet/bin/mallet' # update this path
ldamallet = gensim.models.wrappers.LdaMallet(mallet_path, corpus=corpus, num_topics=5, id2word=id2word)

# Show Topics
# pprint(ldamallet.show_topics(formatted=False))

# Visualize the topics(대화형 차트) - 토픽과 키워드 검사
pyLDAvis.enable_notebook()
vis = pyLDAvis.gensim.prepare(ldamallet, corpus, id2word)
vis

# # Compute Coherence Score
# coherence_model_ldamallet = CoherenceModel(model=ldamallet, texts=data_lemmatized, dictionary=id2word, coherence='c_v')
# coherence_ldamallet = coherence_model_ldamallet.get_coherence()
# print('\nCoherence Score: ', coherence_ldamallet)
