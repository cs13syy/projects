library(readxl)
library(dplyr)

# 데이터 불러오기
setwd('C:\\Users\\yuniv\\OneDrive\\문서\\카카오톡 받은 파일\\전처리')
data = read.csv("data.csv")

# 동 처리하고 x 데이터 생성
dong = rep(0, length(data$대지위치))
tmp = strsplit(as.character(data$대지위치), ' ')
for (i in (1:length(data$대지위치))){
  dong[i] = tmp[[i]][4]  
}

data = cbind(data, dong)

#---------------------------------------------------------------------------

# train set 정제하기
# class 수정
data$주부속구분코드명 = as.factor(data$주부속구분코드명)
data$주용도코드명 = as.factor(data$주용도코드명)
data$`호수(호)` = as.numeric(data$`호수(호)`)
data$`가구수(가구)` = as.numeric(data$`가구수(가구)`)
data$`세대수(세대)` = as.numeric(data$`세대수(세대)`)
data$구조코드명 = as.factor(data$구조코드명)
data$지붕코드명 = as.factor(data$지붕코드명)
data$`건축면적(㎡)` = as.numeric(data$`건축면적(㎡)`)
data$`연면적(㎡)` = as.numeric(data$`연면적(㎡)`)
data$`용적률산정연면적(㎡)` = as.numeric(data$`용적률산정연면적(㎡)`)
data$`용적률(%)` = as.numeric(data$`용적률(%)`)
data$dong = as.character(data$dong)
str(data)

# guess_floor 열 생성
data['guess_floor'] = round((data$`용적률산정연면적(㎡)` * data$`용적률(%)` / 100) / 
                              data$`건축면적(㎡)`)
data$guess_floor = ifelse(data$guess_floor == Inf, 1, data$guess_floor)
data$guess_floor = ifelse(is.na(data$guess_floor), 1, data$guess_floor)
data$guess_floor = ifelse(data$guess_floor == 0, 1, data$guess_floor)

# 동 확인하기
table(data$dong)

#-----------------------------------------

# 행정동 처리하기
양학동 = c('득량동', '학잠동')
우창동 = c('우현동', '창포동')
중앙동 = c('중앙동', '동빈1가', '동빈2가', '덕산동', '덕수동', '여천동', '상원동',
        '남빈동', '신흥동', '대흥동', '항구동', '대신동', '학산동')
장량동 = c('장성동', '양덕동')
환여동 = c('환호동', '여남동')

data$dong = ifelse(data$dong == '득량동' | data$dong == '학잠동', '양학동', data$dong)
data$dong = ifelse(data$dong == '우현동' | data$dong == '창포동', '우창동', data$dong)
data$dong = ifelse(data$dong == '장성동' | data$dong == '양덕동', '장량동', data$dong)
data$dong = ifelse(data$dong == '환호동' | data$dong == '여남동', '환여동', data$dong)
data$dong = ifelse(data$dong == '중앙동' | data$dong == '동빈1가' | 
                     data$dong == '동빈2가' | data$dong == '덕산동' |
                     data$dong == '덕수동' | data$dong == '여천동' |
                     data$dong == '상원동' | data$dong == '남빈동' |
                     data$dong == '신흥동' | data$dong == '대흥동' |
                     data$dong == '항구동' | data$dong == '대신동' |
                     data$dong == '학산동', '중앙동', data$dong)

data$dong = ifelse(data$dong == '대잠동' | data$dong == '이동', '대이동', data$dong)
data$dong = ifelse(data$dong == '상도동' | data$dong == '대도동', '상대동', data$dong)
data$dong = ifelse(data$dong == '송내동' | data$dong == '송정동' | 
                     data$dong == '괴동동' | data$dong == '동촌동' |
                     data$dong == '장흥동' | data$dong == '인덕동' |
                     data$dong == '호동',  '제철동', data$dong)
data$dong = ifelse(data$dong == '청림동' | data$dong == '일월동', '청림동', data$dong)
data$dong = ifelse(data$dong == '효자동' | data$dong == '지곡동', '효곡동', data$dong)

# 동 확인하기
table(data$dong)

#-------------------------------------------

# y 데이터 정제하기
y = read.csv('ydata.csv', header=TRUE)
colnames(y)[1] = 'dong'
str(y)

# 동 기준으로 x 데이터, y 데이터 병합 
data = left_join(data, y[, -c(2, 3)])
data$dong = as.factor(data$dong)

# 최종 데이터 구조 확인
str(data)

#-------------------------------------------------------

# train set, test set 나누기
train_index = sample(1:nrow(data), size = round(0.7 * nrow(data)))
train_set = data[train_index, ]
test_set = data[-train_index,]

# 에러나는 열 이름 변경
colnames(train_set)
colnames(train_set)[4:6] = c("호수", "가구수", "세대수")
colnames(train_set)[9:12] = c("건축면적", "연면적", "용적률산정연면적", "용적률")
colnames(test_set)
colnames(test_set)[4:6] = c("호수", "가구수", "세대수")
colnames(test_set)[9:12] = c("건축면적", "연면적", "용적률산정연면적", "용적률")

#------------------------------------------------------

# train_set, test_set 열 명 확인
names(train_set)
names(test_set)

# 사용할 데이터 train, test로 따로 저자
train = train_set[,4:9]
test = test_set[,4:9]

# 선형 회귀 적합
model = lm(train_set$평균피해금액~., data = train)

# 예측
pred = predict(model, test, interval='confidence')

# 구조코드명 레벨 확인하기
levels(train$구조코드명)
levels(test$구조코드명)

