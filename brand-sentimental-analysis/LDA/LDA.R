#### load data
data <- readLines('C:\\Users\\yuniv\\OneDrive\\바탕 화면\\captain_marvel_youtube.txt')
length(data)

#### omit broken reviews
for (i in (1:length(data))){
  data[i] = iconv(data[i], from = "latin1", to = "ASCII")
}
data = na.omit(data)
length(data)

#### =================================================================

#### tdm 생성 및 전처리
library(tm)

# data를 tm 패키지가 처리할 수 있는 형태인 corpus(말뭉치) 형태로 변환
# vector이므로 VectorSource() 함수를 사용
tdm = Corpus(VectorSource(data))

# stopwords에 단어 추가
custom_stopwords = c(stopwords("english"),"movie","comment","just","scene","another","will","can","put","took","part",
                     "got","men","look","looks","know","man","still","show","see","looking","watch","cant","please",
                     "anyone","else","kinda","gonna","much","doesn","don","get","movies","people","")
tdm = tm_map(tdm, removeWords, custom_stopwords)

# tm 패키지가 분석할 수 있는 Term-Document 형식의 매트릭스로 변환
# 24602개의 단어, 81832개의 문서
tdm = TermDocumentMatrix((tdm), control = list(removeNumbers = T,# 숫자 없애기
                                        removePunctuation = T,   # 기호 없애기
                                        stopwords = T,           # 스탑워즈 없애기
                                        wordLength = c(3, Inf))) # 최소 3글자이상

#### 빈출 단어만 간추리기
library(slam)
word.count = as.array(rollup(tdm,2))   # 매트릭스 행별 합계 구하기
word.order = order(word.count, decreasing = T)[1:1000] # 많이 쓰인 단어 순서 정리(단어 번호)
freq.word = word.order[1:1000]  # 상위 1000개 단어만 freq.word에 재할당(단어 번호)
row.names(tdm[freq.word,])      # 해당 단어 번호의 단어 확인


#### LDA에 input data 만들기 (dtm 만든 후 다시 dtm2ldaformat 함수로 변환)
library(topicmodels)
dtm = as.DocumentTermMatrix(tdm[freq.word,])  # 상위 1000개 단어만 뽑아서 DTM으로 변환
dtm                                           # TDM은 단순한 빈도수만 나타내는 표
ldaform = dtm2ldaformat(dtm, omit_empty=T)    # dtm을 LDA를 돌릴 수 있는 형식의 데이터로 변환

#### =================================================================

library(lda)
t1 <- Sys.time()
result.lda = lda.collapsed.gibbs.sampler(documents = ldaform$documents,
                                         K = 10, # 토픽의 개수
                                         vocab = ldaform$vocab,
                                         num.iterations = 5000, # posteria 업데이트 횟수    
                                         burnin = 1000, # 초기값 버닝 
                                         alpha = 0.01,  # 파라미터 세팅 : 문서 내에서 토픽들의 확률분포 (1을 주면 유니폼 분포)
                                         eta = 0.01 )   # 파라미터 세팅 : 한 토픽 내에 단어들의 확률분포
t2 <- Sys.time()
t2 - t1  # about 8.4 minutes on laptop

#### =================================================================

attributes(result.lda)
dim(result.lda$topics)  # 10개 토픽에 1000개의 단어 출현   
result.lda$topics

top.topic.words(result.lda$topics) # 10개 토픽별 상위 20개 단어

result.lda$topic_sums             # 단어 합계 : 전체 토픽 중 어떤 토픽에 해당하는 단어가 제일 많은가?

dim(result.lda$document_sums)     # 각 토픽에 해당하는 단어가 몇 개씩 나왔나?

result.lda$document_sums[,1]      # 1번 문서가 각 토픽에 해당하는 단어들을 몇 개 갖고 있는지?
