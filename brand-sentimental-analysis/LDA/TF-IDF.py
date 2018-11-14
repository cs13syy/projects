from sklearn.feature_extraction.text import TfidfVectorizer
import pandas as pd
import nltk
# nltk.download("stopwords")

# X = ['Tom plays soccer','Tom loves soccer and baseball','baseball is his hobby and his job']
file = open("C:\\Users\\yuniv\\OneDrive\\바탕 화면\\title2.txt","r")
file_list = file.read().splitlines()
X = list(filter(lambda item: item.strip(), file_list))
print(X)
file.close()

tfidf_vectorizer = TfidfVectorizer(stop_words='english')
tfidf_matrix = tfidf_vectorizer.fit_transform(X)

print(tfidf_matrix.toarray()) #sparse data format
print(tfidf_matrix.shape) # x: num of docs, y: num of words
print(tfidf_vectorizer.vocabulary_)

vocab = tfidf_vectorizer.get_feature_names()
print(vocab) # show me words in a list

tfidf_table = pd.DataFrame(tfidf_matrix.toarray(), columns=vocab)
print(tfidf_table.head())
tfidf_table.to_csv("C:\\Users\\yuniv\\OneDrive\\바탕 화면\\title3.csv", header=True)
