rm(list = ls())

# 필요한 모듈 불러오기

library(readxl)
library(Imap)
library(dplyr)
library(glmnet)
library(mgcv)
library(VIM)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(randomForest)

# 데이터 불러오기
data = read_excel("new_data.xlsx", col_names = T)

# 데이터 개요
head(data)

# 데이터 구조
str(data)

# 칼럼 명
colnames(data)



# 사용하지 않는 칼럼 제거
# 제거 목록
  # "시군구코드"     "법정동코드"     "대지구분코드"   "번"            
  # "지"             "관리동별개요PK" "관리허가대장PK" "건물명"        
  # "특수지명"       "블록"           "로트"           "주부속구분코드"
  # "주용도코드"     "구조코드"       "지붕코드" 


# 행 갯수 확인
nrow(data)

# 제거한 후 칼럼 명 재확인
colnames(data)


# 동 처리하고 x 데이터 생성
dong = rep(0, length(data$대지위치))
tmp = strsplit(as.character(data$대지위치), ' ')
for (i in (1:length(data$대지위치))){
  dong[i] = tmp[[i]][4]  
}
data = cbind(data, dong)
rm(tmp)
str(data)

# train set 정제하기
# class 수정
data$주부속구분코드명 = as.factor(data$주부속구분코드명)
data$주용도코드명 = as.factor(data$주용도코드명)
data$`호수(호)` = as.numeric(data$`호수(호)`)
data$`가구수(가구)` = as.numeric(data$`가구수(가구)`)
data$`세대수(세대)` = as.numeric(data$`세대수(세대)`)
data$구조코드명 = as.factor(data$구조코드명)
data$지붕코드명 = as.factor(data$지붕코드명)
data$`대지면적(㎡)` = as.numeric(data$`대지면적(㎡)`)
data$`건축면적(㎡)` = as.numeric(data$`건축면적(㎡)`)
data$`건폐율(%)` = as.numeric(data$`건폐율(%)`)
data$`연면적(㎡)` = as.numeric(data$`연면적(㎡)`)
data$`용적율산정연면적(㎡)` = as.numeric(data$`용적율산정연면적(㎡)`)
data$`용적률(%)` = as.numeric(data$`용적률(%)`)
data$Longitude = as.numeric(data$Longitude, 16)
data$Latitude = as.numeric(data$Latitude, 16)
data$dong = as.character(data$dong)
str(data)

# 행정동 처리하기

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

str(data)
#-------------------------------------------------------

# 실제착공일 > 사용승인일 > 건축허가일 순위로 결측치 채워넣기
names(data)
str(data)

for (i in (1:nrow(data))){
  if (data$사용승인일[i] == 0){
    data$사용승인일[i] <- data$실제착공일[i]}
}
for (i in (1:nrow(data))){
  if (data$사용승인일[i] == 0){data$사용승인일[i] <- data$건축허가일[i]}
}

which(data$사용승인일 == 0)
# 착공일, 사용승인일, 건축허가일이 모두 0인 행 제거
data = data[-which(data$사용승인일=='0'), ]

nrow(data)

# age를 년월일 -> 년도로 수정
for (i in (1:nrow(data))){
  data$사용승인일[i] = 2018 - floor(data$사용승인일[i] / 10000)
}
rm(i)
str(data)


#------------------------------------------------------------

# 추가 칼럼 제거
names(data)
# 연면적과 용적율산정연면적은 y=x 관계가 뚜렷해서 후자를 삭제
plot(x = log(data$`연면적(㎡)`), y = log(data$`용적율산정연면적(㎡)`))

# 건폐율은 건축면적 / 대지면적이기 때문에 건폐율 제거
# 용적률은 연면적 / 건축면적이기 때문에 용적률 삭제

data = data[, -c(11, 13:16)]
str(data)

# 면적과 관계된 남은 3개의 변수 : 건축면적, 연면적, 대지면적
# 대지면적 >= 건축면적
par(mfrow=c(1,3))
plot(x = log(data$`연면적(㎡)`), y = log(data$`건축면적(㎡)`))
plot(x = log(data$`대지면적(㎡)`), y = log(data$`건축면적(㎡)`))
plot(x = log(data$`연면적(㎡)`), y = log(data$`대지면적(㎡)`))

str(data)

names(data)

#-------------------------------------------------------------

# 칼럼 이름 영어로 변경

colnames(data) = c('address', 'codename_of_main_sub', 'codename_of_usage',
                   'household', 'family', 'generation', 'codename_of_structure',
                   'codename_of_roof', 'area_of_land', 'area_of_construction',
                   'area_of_total_building', 'addmission_to_use_date', 
                   'Latitude', 'Longitude', 'dong')

colnames(data)

# 건축면적과 주부속건물코드명 제거
data = data[,-c(2, 10)]

# 결측치 갯수 확인
colSums(is.na(data))

# the number of rows

nrow(data)
# 결측치 확인을 위한 그래프
missing_plot <- aggr(data, col=c('gray','red'), numbers=TRUE, 
                     sortVars=TRUE, labels=names(data), 
                     cex.axis=.7, gap=3, 
                     ylab=c("Histogram of missing data", "Pattern"))


# 결측치 1위 지붕코드명 2853개, 
# 3위 주용도코드명 279개, 4위 구조코드명 126개
head(data$codename_of_roof)
head(data$codename_of_usage)
head(data$codename_of_structure)
#-------------------------------------------------------------------------
# 결측치 제거한 뒤 행 갯수 확인
data = na.omit(data)
nrow(data)

#------------------------------------------------------------

# ggmap 이용 
#plot(data$Latitude, data$Longitude)

#pohang_map = get_map('texas', zoom=6, maptype='roadmap')
#qmap('seoul', zoom = 11, maptype = 'roadmap')
#?get_map
#ggmap(get_map(location='south korea', zoom=7))


# 칼럼별 결측치, 데이터 분포 확인

ggplot(data, aes(x =data$household)) +
  geom_bar(stat="count")+theme_minimal()
table(data$household)

ggplot(data, aes(x =data$family)) +
  geom_bar(stat="count")+theme_minimal()
table(data$family)

ggplot(data, aes(x =data$generation)) +
  geom_bar(stat="count")+theme_minimal()
table(data$generation)

# 결론 0이 너무 많아서 버리려고 한다.
#names(data)

#data = data[, -c(4:6)]

#str(data)
# -------------------------------------------------

# 활용??
ggplot(data, aes(x = data$dong, y = log(data$area_of_land))) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(x = data$dong, y = log(data$area_of_total_building))) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#--------------------------------------------------------------------

#---------------------------------------------------------------------------
# 거리 칼럼 생성

  # 고리 원전, 월성 원전, 지열발전소, 지진 진앙지 위경도 좌표 설정
gori_Latitude = 35.4599361760504
gori_Longitude = 129.310425957664
wolsung_Latitude = 35.614151316351
wolsung_Longitude = 129.473160943015
earth_heat_Latitude = 36.1086638	
earth_heat_Longitude = 129.3829438
Eq_Latitude = 36.109
Eq_Longitude = 129.366

set_of_Latitude = c(gori_Latitude, wolsung_Latitude, earth_heat_Latitude, Eq_Latitude)
set_of_Longitude = c(gori_Longitude, wolsung_Longitude, earth_heat_Longitude, Eq_Longitude)
#--------------------------------------------------------------
tmp = c('gori', 'wolsung', 'earth_heat',  'Epicenter')
for(k in 1:4) {
  tmp = c('gori', 'wolsung', 'earth_heat', 'Epicenter')
  ## 4.1. 위경도 - 거리 ------
  map_dist_list <- list()
  
  for (i in (1:nrow(data))) {
    
    map_dist_list[[i]] <- gdist(lon.1 = data$Longitude[i], 
                                lat.1 = data$Latitude[i], 
                                lon.2 = set_of_Longitude[k],
                                lat.2 = set_of_Latitude[k],
                                units="km")
  }
  
  ## 4.2. 리스트를 행렬로 변환 ------
  map_dist_mat <- sapply(map_dist_list, unlist)
  data[paste0('distance_from_', tmp[k])] = map_dist_mat
}

head(data)

# 진앙지에서 가까운 순으로 정렬
data = data[order(data$distance_from_Epicenter), ]


#--------------------------------------------------------
# 동 확인하기
table(data$dong)

#-----------------------------------------
#-----------------------------------------

# 구조코드명 처리하기
str(data)
levels(data$codename_of_structure)
data$codename_of_structure = as.character(data$codename_of_structure)
names(data)[6] = 'name'
data$name = ifelse(data$name == '일반목구조' | data$name == '통나무구조' | 
                     data$name == '트러스목구조', '목구조', data$name)
data$name = ifelse(data$name == '강파이프구조' | data$name == '경량철골구조' | 
                     data$name == '기타강구조' | data$name == '일반철골구조' | 
                     data$name == '철골구조', '강구조', data$name)
data$name = ifelse(data$name == '벽돌구조' | data$name == '블록구조' | 
                     data$name == '시멘트블럭조' | data$name == '석구조' | 
                     data$name == '기타조적구조' | data$name == '조적구조', '조적식', data$name)
data$name = ifelse(data$name == '철골콘크리트구조' | data$name == '철근콘크리트구조' | 
                     data$name == '기타철골철근콘크리트구조' | data$name == '프리케스트콘크리트구조' | 
                     data$name == '기타콘크리트구조' | data$name == '철골철근콘크리트구조' | 
                     data$name == '콘크리트구조', '일체식', data$name)
data$name = ifelse(data$name == '조립식판넬조' | data$name == '컨테이너조', '조립식', data$name)

data$name = as.factor(data$name)
levels(data$name)
names(data)[6] = 'codename_of_structure'


data %>% ggplot(aes(x=codename_of_structure)) + geom_bar(stat = 'count') +
  ggtitle('codename_of_structure')

colnames(data)[10] = 'building_age'
str(data)
#------------------------------------------- 

# y 데이터 정제하기
y = read_excel('C:/Users/renz/Desktop/ydata.xlsx', col_name = T)
str(y)

# 나이에 따라 재코딩
tmp = c()

for (i in (1:length(y$dong))){
  if (y$dong[i] %in% 
      c(y[which(y$average_age >= 30 & y$average_age < 40), ][, 1])[[1]]){
    tmp[i] = 1
  } else if (y$dong[i] %in% 
             c(y[which(y$average_age >= 40 & y$average_age < 50), ][, 1])[[1]]){
    tmp[i] = 2
  } else {tmp[i] = 3}
}

y = cbind(y, tmp)
colnames(y)[6] = 'factor_of_age'
y$factor_of_age = as.factor(y$factor_of_age)

str(y)
str(data)

# 동별 신고 수
y %>% ggplot(aes(x=dong, y= the_number_of_petition)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Damage for each dong')

# 동별 피해액

y %>% ggplot(aes(x = dong, y = total_damage)) + geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle('Total Damage')

str(data)
# 동 기준으로 x 데이터, y 데이터 병합 
y$dong = as.factor(y$dong)
data = left_join(data, y[, -c(2:3)])
#data$dong = as.factor(data$dong)
#data$factor_of_age = as.factor(data$factor_of_age)

# 최종 데이터 구조 확인
str(data)

colnames(data)
#-------------------------------------------------------

# train set, test set 나누기
train_index = sample(1:nrow(data), size = round(0.8 * nrow(data)))
train_set = data[train_index, ]
test_set = data[-train_index,]


#------------------------------------------------------

# train_set, test_set 열 명 확인
names(train_set)
names(test_set)

# 사용할 데이터 train, test로 따로 저장
train = train_set[, c(3:10, 13:19)]
test = test_set[,c(3:10, 13:19)]

# 시험용, 나이를 재코딩한 것을 이용
#train = train_set[, c(2:8, 14:18, 20)]
#test = test_set[,c(2:8, 14:18, 20)]


str(train)
str(test)

names(train)
names(test)

#--------------------------------------------------------------
# 변수선택 stepwise 방식
null = lm(average_damage~1, data = train)
full = lm(average_damage~. - dong, data = train)
step(null, direction = "both", scope=list(upper=full))

#Modeling

# 선형 회귀 적합
model = lm(formula = average_damage ~ distance_from_Epicenter + distance_from_gori + 
             average_age + distance_from_wolsung + distance_from_earth_heat + 
             codename_of_structure + codename_of_roof + family, data = train)

summary(model)

# 예측
pred = predict(model, test)
#---------------------------------------------------------
test %>% group_by(dong) %>% ggplot(aes(x = c(1:nrow(test)), y = pred)) + 
  geom_point(size = 1) + coord_cartesian(ylim = c(0:2500))

ggplot(test, aes(x = test$dong, y = pred)) + geom_boxplot() + 
  geom_point(aes(x = test$dong, y = test$average_damage, color = test$dong), size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle('Linear Fit')
#-----------------------------------------------------------------------


# graph

# distance from Epicenter plot
plot(-test$distance_from_Epicenter, col = test$dong)
title('Distance from Epicenter')

# test answer set
plot(test$average_damage, col = test$dong)
title('Test set real answer ')

# linear prediction
plot(pred, col = test$dong, ylim = c(0, 2500))
title('Linear')

#legend("topright", legend=unique(test$dong), pch = test$dong,
#       col=unique(test_set$dong), bg="gray")


# ridge
train_x <- model.matrix(train$average_damage~. - dong, train)
train_y <- train$average_damage
test_x <- model.matrix(test$average_damage~. - dong, test)
test_y <- test$average_damage


lambdas <- 10^seq(2, -2, length = 100)

ridge.model = glmnet(train_x, train_y, alpha = 0, lambda = lambdas)
cv.out = cv.glmnet(train_x, train_y, alpha = 0)

bestlam <- cv.out$lambda.min

ridge.pred = predict(ridge.model, s = bestlam, newx = test_x)
mean((ridge.pred - test_y)^2)

#a look at the coefficients
out = glmnet(train_x, train_y, alpha = 0)
predict(ridge.model, type = "coefficients", s = bestlam)

# graph
plot(exp(ridge.pred), col=test$dong, ylim = c(0, 2500))
title('Ridge')

test %>% group_by(dong) %>% ggplot(aes(x = c(1:nrow(test)), y = ridge.pred)) + 
  geom_point(size = 1) + coord_cartesian(ylim = c(0:2500))

ggplot(test, aes(x = test$dong, y = ridge.pred)) + geom_boxplot() + 
  geom_point(aes(x = test$dong, y = test$average_damage, color = test$dong), size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle('Ridge Regression')

# LaSSO
lasso.model = glmnet(train_x, train_y, alpha = 1, lambda = lambdas)
lasso.pred = predict(lasso.model, s = bestlam, newx = test_x)
mean((lasso.pred-test_y)^2)

plot(lasso.pred, col=test$dong, ylim = c(0, 2500))
title('LASSO')

lasso.coef  <- predict(lasso.model, type = 'coefficients', s = bestlam)
lasso.coef

test %>% group_by(dong) %>% ggplot(aes(x = c(1:nrow(test)), y = lasso.pred)) + 
  geom_point(size = 1) + coord_cartesian(ylim = c(0:2500))

ggplot(test, aes(x = test$dong, y = lasso.pred)) + geom_boxplot() + 
  geom_point(aes(x = test$dong, y = test$average_damage, color = test$dong), size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle('LASSO')
#------------------------------------------------------------------
str(train)

rf.model = randomForest(average_damage~. -dong, data = train, ntree=200, importance = T)
rf.pred = predict(rf.model, newdata = test)

importance(rf.model)
varImpPlot(rf.model)

p1 <-test %>% group_by(dong) %>% ggplot(aes(x = c(1:nrow(test)), y = rf.pred, color = test$dong)) + 
  geom_point(size = 1) + coord_cartesian(ylim = c(0:2500))

p2 <-test %>% group_by(dong) %>% ggplot(aes(x = c(1:nrow(test)), y = test$average_damage)) + 
  geom_point(size = 1) + coord_cartesian(ylim = c(0:2500))

grid.arrange(p1, p2)

ggplot(test, aes(x = test$dong, y = rf.pred)) + geom_boxplot() + 
  geom_point(aes(x = test$dong, y = test$average_damage, color = test$dong), size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#----------------------------------------------
#tmp = data[which(data$dong == '송라면'),]
#names(tmp)
#tmp[, c(14:17)]

#par(mfrow=c(1,3))
#plot(tmp$household)
#plot(tmp$family)
#plot(tmp$generation)

#nrow(tmp)

#table(data$dong)
#---------------------------------------------


#kyeongju = read_excel('C:/Users/renz/Desktop/kyeongju.xlsx', col_name = T)


# 동 처리하고 x 데이터 생성
#dong = rep(0, length(kyeongju$대지위치))
#tmp = strsplit(as.character(kyeongju$대지위치), ' ')
#@for (i in (1:length(kyeongju$대지위치))){
#  dong[i] = tmp[[i]][3]  
#}
#kyeongju = cbind(kyeongju, dong)
#rm(tmp)
#str(kyeongju)

# 
#kyeongju$주부속구분코드명 = as.factor(kyeongju$주부속구분코드명)
#kyeongju$주용도코드명 = as.factor(kyeongju$주용도코드명)
#kyeongju$`호수(호)` = as.numeric(kyeongju$`호수(호)`)
#kyeongju$`가구수(가구)` = as.numeric(kyeongju$`가구수(가구)`)
#kyeongju$`세대수(세대)` = as.numeric(kyeongju$`세대수(세대)`)
#kyeongju$구조코드명 = as.factor(kyeongju$구조코드명)
#kyeongju$지붕코드명 = as.factor(kyeongju$지붕코드명)
#kyeongju$`대지면적(㎡)` = as.numeric(kyeongju$`대지면적(㎡)`)
#kyeongju$`건축면적(㎡)` = as.numeric(kyeongju$`건축면적(㎡)`)
#kyeongju$`건폐율(%)` = as.numeric(kyeongju$`건폐율(%)`)
#kyeongju$`연면적(㎡)` = as.numeric(kyeongju$`연면적(㎡)`)
#kyeongju$`용적율산정연면적(㎡)` = as.numeric(kyeongju$`용적율산정연면적(㎡)`)
#kyeongju$`용적률(%)` = as.numeric(kyeongju$`용적률(%)`)
#kyeongju$Longitude = as.numeric(kyeongju$Longitude, 16)
#kyeongju$Latitude = as.numeric(kyeongju$Latitude, 16)
#kyeongju$dong = as.character(kyeongju$dong)
#str(kyeongju)

#table(kyeongju$dong)
#

#kyeongju$dong = ifelse(kyeongju$dong == '동부동' | kyeongju$dong == '서부동' |
#                         kyeongju$dong == '북부동' | kyeongju$dong == '노동동' | 
#                         kyeongju$dong == '노서동', '중부동', kyeongju$dong)
#kyeongju$dong = ifelse(kyeongju$dong == '성동동' | kyeongju$dong == '황오동', 
#                       '황오동', kyeongju$dong)
#kyeongju$dong = ifelse(kyeongju$dong == '성건동' | kyeongju$dong == '석장동', 
#                       '성건동', kyeongju$dong)
#kyeongju$dong = ifelse(kyeongju$dong == '황남동' | kyeongju$dong == '사정동' |
#                         kyeongju$dong == '탑동' | kyeongju$dong == '배동' | 
#                         kyeongju$dong == '율동', '황남동', kyeongju$dong)
#kyeongju$dong = ifelse(kyeongju$dong == '인왕동' | kyeongju$dong == '교동' |
#                         kyeongju$dong == '동방동' | kyeongju$dong == '도지동' | 
#                         kyeongju$dong == '남산동' | kyeongju$dong == '구황동' |
#                         kyeongju$dong == '보문동' | kyeongju$dong == '배반동',
#                       '월성동', kyeongju$dong)
#kyeongju$dong = ifelse(kyeongju$dong == '충효동' | kyeongju$dong == '서악동' |
#                         kyeongju$dong == '효현동' | kyeongju$dong == '광명동', 
#                       '선도동', kyeongju$dong)
#kyeongju$dong = ifelse(kyeongju$dong == '진현동' | kyeongju$dong == '마동' |
#                         kyeongju$dong == '하동' | kyeongju$dong == '평동' | 
#                         kyeongju$dong == '시래동' | kyeongju$dong == '구정동' |
#                         kyeongju$dong == '조양동' | kyeongju$dong == '시동',
#                       '불국동', kyeongju$dong)
#kyeongju$dong = ifelse(kyeongju$dong == '신평동' | kyeongju$dong == '손곡동' |
#                         kyeongju$dong == '북군동' | kyeongju$dong == '천군동' | 
#                         kyeongju$dong == '덕동' | kyeongju$dong == '황용동' |
#                         kyeongju$dong == '암곡동', '보덕동', kyeongju$dong)#

#table(kyeongju$dong)

#------------------------------------------------
# 실제착공일 > 사용승인일 > 건축허가일 순위로 결측치 채워넣기
#names(kyeongju)
#str(kyeongju)

#for (i in (1:nrow(kyeongju))){
# if (kyeongju$사용승인일[i] == 0){
#    kyeongju$사용승인일[i] <- kyeongju$실제착공일[i]}
#}
#for (i in (1:nrow(kyeongju))){
#  if (kyeongju$사용승인일[i] == 0){kyeongju$사용승인일[i] <- kyeongju$건축허가일[i]}
#}

#which(kyeongju$사용승인일 == 0)
# 착공일, 사용승인일, 건축허가일이 모두 0인 행 제거
#kyeongju = kyeongju[-which(kyeongju$사용승인일==0), ]



# age를 년월일 -> 년도로 수정
#for (i in (1:nrow(kyeongju))){
#  kyeongju$사용승인일[i] = 2018 - floor(kyeongju$사용승인일[i] / 10000)
#}
#rm(i)
#str(data)

#str(kyeongju)

#-------------------------------------------
#names(kyeongju)
#kyeongju = kyeongju[, -c(1, 12, 14:17)]
#str(kyeongju)
#names(kyeongju)

#colnames(kyeongju) = c('address', 'codename_of_main_sub', 'codename_of_usage',
#                   'household', 'family', 'generation', 'codename_of_structure',
#                   'codename_of_roof', 'area_of_land', 'area_of_building', 'area_of_total_building', 
#                   'addmission_to_use_date', 'Latitude', 'Longitude', 'dong')

#colnames(kyeongju)

# 건축면적과 주부속건물코드명 제거
#kyeongju = kyeongju[,-c(2, 10)]

# 결측치 갯수 확인
#colSums(is.na(kyeongju))

# the number of rows

#nrow(kyeongju)
# 결측치 확인을 위한 그래프
#missing_plot <- aggr(kyeongju, col=c('gray','red'), numbers=TRUE, 
#                     sortVars=TRUE, labels=names(kyeongju), 
#                     cex.axis=.7, gap=3, 
#                     ylab=c("Histogram of missing data", "Pattern"))

# 결측치 제거한 뒤 행 갯수 확인
#kyeongju = na.omit(kyeongju)
#nrow(kyeongju)

# 고리 원전, 월성 원전, 지열발전소, 지진 진앙지 위경도 좌표 설정
#gori_Latitude = 35.4599361760504
#gori_Longitude = 129.310425957664
#wolsung_Latitude = 35.614151316351
#wolsung_Longitude = 129.473160943015
#earth_heat_Latitude = 36.1086638	
#earth_heat_Longitude = 129.3829438
#Eq_Latitude = 36.109
#Eq_Longitude = 129.366

#set_of_Latitude = c(gori_Latitude, wolsung_Latitude, earth_heat_Latitude, Eq_Latitude)
#set_of_Longitude = c(gori_Longitude, wolsung_Longitude, earth_heat_Longitude, Eq_Longitude)
#--------------------------------------------------------------
#tmp = c('gori', 'wolsung', 'earth_heat',  'Epicenter')
#for(k in 1:4) {
#  tmp = c('gori', 'wolsung', 'earth_heat', 'Epicenter')
  ## 4.1. 위경도 - 거리 ------
#  map_dist_list <- list()
  
#  for (i in (1:nrow(kyeongju))) {
    
#    map_dist_list[[i]] <- gdist(lon.1 = kyeongju$Longitude[i], 
#                                lat.1 = kyeongju$Latitude[i], 
#                                lon.2 = set_of_Longitude[k],
#                                lat.2 = set_of_Latitude[k],
#                                units="km")
#  }
  
  ## 4.2. 리스트를 행렬로 변환 ------
#  map_dist_mat <- sapply(map_dist_list, unlist)
#  kyeongju[paste0('distance_from_', tmp[k])] = map_dist_mat
#}

#head(kyeongju)

# 진앙지에서 가까운 순으로 정렬
#kyeongju = kyeongju[order(kyeongju$distance_from_Epicenter), ]


# 구조코드명 처리하기
#str(kyeongju)
#levels(kyeongju$codename_of_structure)
#kyeongju$codename_of_structure = as.character(kyeongju$codename_of_structure)
#names(kyeongju)[6] = 'name'
#kyeongju$name = ifelse(kyeongju$name == '일반목구조' | kyeongju$name == '통나무구조' | 
#                         kyeongju$name == '트러스목구조', '목구조', kyeongju$name)
#kyeongju$name = ifelse(kyeongju$name == '강파이프구조' | kyeongju$name == '경량철골구조' | 
#                         kyeongju$name == '기타강구조' | kyeongju$name == '일반철골구조' | 
#                         kyeongju$name == '철골구조', '강구조', kyeongju$name)
#kyeongju$name = ifelse(kyeongju$name == '벽돌구조' | kyeongju$name == '블록구조' | 
#                         kyeongju$name == '시멘트블럭조' | kyeongju$name == '석구조' | 
#                         kyeongju$name == '기타조적구조' | kyeongju$name == '조적구조', '조적식', kyeongju$name)
#kyeongju$name = ifelse(kyeongju$name == '철골콘크리트구조' | kyeongju$name == '철근콘크리트구조' | 
#                         kyeongju$name == '기타철골철근콘크리트구조' | kyeongju$name == '프리케스트콘크리트구조' | 
#                         kyeongju$name == '기타콘크리트구조' | kyeongju$name == '철골철근콘크리트구조' | 
#                         kyeongju$name == '콘크리트구조', '일체식', kyeongju$name)
#kyeongju$name = ifelse(kyeongju$name == '조립식판넬조' | kyeongju$name == '컨테이너조', '조립식', kyeongju$name)

#kyeongju$name = as.factor(kyeongju$name)
#levels(kyeongju$name)
#names(kyeongju)[6] = 'codename_of_structure'
#str(kyeongju)

#kyeongju %>% ggplot(aes(x=codename_of_structure)) + geom_bar(stat = 'count') +
#  ggtitle('codename_of_structure')

#colnames(kyeongju)[10] = 'building_age'
#str(kyeongju)

#a = predict(rf.model, kyeongju)
