rm(list=ls())

# 패키지 불러오기
library(ggplot2)
library(dplyr)
library(OpenStreetMap)
library(ggmap)

# 디렉토리 설정
getwd()
setwd("C:/Users/yuniv/OneDrive/바탕 화면")

# 파일 불러오기
la_dat <- read.csv("LA_Neighborhoods_Data.csv", header=TRUE)
attach(la_dat)

# 데이터 요약 통계량 확인
str(la_dat)
summary(la_dat)

# 결측값 확인
sum(is.na(la_dat))

# colnames 수정
colnames(la_dat)
colnames(la_dat)[1] = "LA_Nbhd"

# 소득과 자가소유의 관계?
# 소득 변수 스크리닝
Inc_freq <- Income/sum(Income)*100
ggplot(data=la_dat, aes(x=Inc_freq, y=Homes, col="red")) + geom_point()
lm(Homes~Inc_freq, data=la_dat)
summary(lm(Homes~Inc_freq, data=la_dat))

# 소득과 학교점수의 관계?
ggplot(data=la_dat, aes(x=sqrt(Income), y=Schools)) + geom_point(col="green")
inc_sch = data.frame(inc=sqrt(Income), sch=Schools)
inc_sch <- subset(inc_sch, inc_sch$sch != 0) # sch 값 0인 것 제거
ggplot(data=inc_sch, aes(x=inc_sch$inc, y=inc_sch$sch)) + geom_point(col="green")
summary(lm(inc_sch$sch~inc_sch$inc))
  
# 다양성과 학교점수의 관계?
ggplot(data=la_dat, aes(x=Diversity*100, y=Schools)) + geom_point(col="blue")
new_dat <- subset(la_dat, Schools != 0)
ggplot(data=new_dat, aes(x=Diversity*100, y=Schools)) + geom_point(col="blue")

# visualizing the map
# 동네 이름, 위도, 경도 vector 생성
nbhd <- c(); lon <- c(); lat <- c()
inc <- c(); inc <- c(); inc <- c(); white <- c();
nbhd <- LA_Nbhd
lon <- Longitude
lat <- Latitude
inc <- Income
pop <- Population
asian <- Asian
white <- White

# get_googlemap 및 위경도 정보로 visualization
p1 <- get_googlemap("los angeles") %>% ggmap
locationInfo <- data.frame(
  Name = nbhd, lon = lon, lat = lat)
p1 + geom_point(data = locationInfo, size=5, aes(x = lon, y = lat, col=LA_Nbhd))

# 소득별 분포 시각화
# 연속형 변수인 Income의 factor화
la_dat$inc_fac[Income<50000]="low"
la_dat$inc_fac[Income>=50000 & Income<100000]="middle"
la_dat$inc_fac[Income>=100000 & Income<200000]="high"
la_dat$inc_fac[Income>=200000]="very high"
table(la_dat$inc_fac)
loc1 <- data.frame(Name=nbhd, lon=lon, lat=lat, inc_fac=la_dat$inc_fac)
p1 + geom_point(data=loc1, aes(x=lon, y=lat, size=inc_fac, 
                               colour=inc_fac), alpha=.7)

# 인구별 분포 시각화
# 연속형 변수인 Population의 factor화
la_dat$pop_fac[pop<30000]="low"
la_dat$pop_fac[pop>=30000 & pop<60000]="middle"
la_dat$pop_fac[pop>=60000 & pop<90000]="high"
la_dat$pop_fac[pop>=90000]="very high"
loc2 <- data.frame(Name=nbhd, lon=lon, lat=lat, pop_fac=la_dat$pop_fac)
p1 + geom_point(data=loc2, aes(x=lon, y=lat, size=4, 
                               colour=pop_fac), alpha=.7)
