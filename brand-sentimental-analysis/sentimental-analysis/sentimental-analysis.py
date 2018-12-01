from pandas import DataFrame
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyzer = SentimentIntensityAnalyzer()

tweets = []
compound = []
positive = []
neutral = []
negative = []

with open('Captain+Marvel100.txt', 'r', encoding='UTF-8') as f:
    for text in f:
        for tweet in text.split('\n'):
            tweets.append(tweet)

tweets = list(filter(None, tweets))

for i in range(0, len(tweets)):
    compound.append(analyzer.polarity_scores(tweets[i])['compound'])
    positive.append(analyzer.polarity_scores(tweets[i])['pos'])
    neutral.append(analyzer.polarity_scores(tweets[i])['neu'])
    negative.append(analyzer.polarity_scores(tweets[i])['neg'])

df = DataFrame({'Tweet': tweets, 'Compound': compound, 'Positive': positive, 'Neutral': neutral, 'Negative': negative})
df.to_csv('sentiment.csv', encoding='utf-8')
print(df.describe(exclude='object'))
