# 파일 및 라이브러리 불러오기
setwd('C:/Users/yuniv/OneDrive/문서/R/explanatory-data-analysis/data')
skill <- read.csv(file = 'SkillCraft1_Dataset.csv', header=TRUE, 
                  stringsAsFactors=FALSE, na.strings=c('?'))
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# 결측값 확인 및 NA 처리
table(is.na(skill)) 
skill[skill=='?'] == NA

# 데이터 요약치 확인
dim(skill) 
str(skill) 
names(skill) 
attach(skill)
summary(skill)



# 등급 지정, 등급 별 데이터 분류
level.name = c('Bronze', 'Silver', 'Gold', 'Platinum',
               'Diamond', 'Master', 'GrandMaster', 'Pro')
level1 = skill[skill$LeagueIndex == 1, ]
level2 = skill[skill$LeagueIndex == 2, ]
level3 = skill[skill$LeagueIndex == 3, ]
level4 = skill[skill$LeagueIndex == 4, ]
level5 = skill[skill$LeagueIndex == 5, ]
level6 = skill[skill$LeagueIndex == 6, ]
level7 = skill[skill$LeagueIndex == 7, ]
level8 = skill[skill$LeagueIndex == 8, ]

# 등급 8개의 색상 지정
mycol = brewer.pal(8, 'Blues')

# 등급 분포 시각화
counts = table(skill$LeagueIndex) # 레벨 별 빈도수
freq = counts/sum(counts) # 레벨 별 상대도수
barplot(freq, main = "LeagueIndex", xlab = "LeagueIndex", 
        ylab = "freq", ylim = c(0,0.3), 
        names.arg=level.name, col=mycol)
legend("topright", level.name, col=mycol, pch=15)



# variable 1: Age
summary(skill$Age)
boxplot(Age, main = "Boxplot of Age", col="lightyellow") # 이상값 다수
nrow(skill[skill$Age == '?', ]) # 이상값 개수 : 55
skill[skill$Age == '?', "LeagueIndex"]
# 레벨 별 평균 나이 확인하기
age.mean <- aggregate(formula=Age~LeagueIndex, data=skill,
                      FUN=mean, na.rm=TRUE)
age.mean.mat <- as.matrix(age.mean)
plot(age.mean.mat, ylim=c(20,24), pch=16, 
     main = "Mean of Age by Levels", xlab = "LeagueIndex",
     ylab = "Mean of Age", col="red")
# 전반적으로 유저의 나이가 20~23세 사이에 골고루 분포하고 있으며
# 레벨 별로 큰 차이가 없으므로 중요한 설명변수는 아니라고 할 수 있다
# 레벨8은 나이가 결측값으로, 확인 안 됨



# variable 2: HoursPerWeek
summary(skill$HoursPerWeek)
boxplot(HoursPerWeek~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "HoursPerWeek",
        main="HoursPerWeek by Levels", col=mycol2)
plot(density(level1$TotalHours), xlab='HoursPerWeek', xlim=c(0, 100), ylim=c(0,0.07), 
     main='HoursPerWeek by Levels', col=mycol2[1], lty=1, lwd=2)
lines(density(level2$HoursPerWeek), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$HoursPerWeek), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$HoursPerWeek), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$HoursPerWeek), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$HoursPerWeek), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$HoursPerWeek), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$HoursPerWeek), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)

# 레벨마다 이상값들이 많이 관측되며, 초급 및 중급 레벨에서는 큰 차이는 없다
# 중급에서 고급으로 올라가는 레벨 6, 7에서 중앙값이 증가하는 것이 보인다
# 레벨 8의 값은 관찰되지 않는다



# variable 3: TotalHours
summary(skill$TotalHours)
boxplot(HoursPerWeek~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "HoursPerWeek",
        main="HoursPerWeek by Levels", col=mycol2)
boxplot(TotalHours~LeagueIndex, data=skill, col=mycol)
# min(3)과 max(1000000)의 gap이 너무 커서 boxplot에 출력 불가

TH.mean <- aggregate(formula=TotalHours~LeagueIndex, data=skill,
                     FUN=mean, na.rm=TRUE)
TH.mean.mat <- as.matrix(TH.mean)
plot(TH.mean.mat, main="TotalHours by Levels", pch=16, col="red")
# 레벨 1~4까지는 꾸준히 증가 추이를 보이나 5, 6, 7에서 불규칙
# 레벨 8의 값은 관찰되지 않는다

plot(density(level1$TotalHours), xlab='TotalHours', main='TotalHours by Levels',
     col=mycol2[1], xlim=c(0,450), ylim=c(0,0.02), lty=1, lwd=2)
lines(density(level2$APM), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$APM), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$APM), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$APM), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$APM), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$APM), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$APM), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(APM~LeagueIndex, data=skill)



# variable 4: APM
summary(skill$APM)
mycol2 = c('skyblue1', 'springgreen3', 'steelblue4', 'yellow1',
           'violetred1', 'tomato1', 'tan1', 'royalblue4')
library(colorspace)
pal <- choose_palette()
mycol2 = pal(8)

# 레벨 별 APM 확인하기
plot(density(level1$APM), xlab='APM', main='APM by Levels',
     col=mycol2[1], xlim=c(0,400), ylim=c(0,0.025), lty=1, lwd=2)
lines(density(level2$APM), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$APM), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$APM), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$APM), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$APM), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$APM), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$APM), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(APM~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "APM",
        main="APM by Levels", col=mycol2)



# variable 5: selectbyhotkeys
summary(skill$SelectByHotkeys)
summary(skill)
plot(density(level1$SelectByHotkeys), xlab='SelectByHotkeys', main='SelectByHotkeys by Levels',
     col=mycol2[1], lty=1, lwd=2)
lines(density(level2$SelectByHotkeys), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$SelectByHotkeys), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$SelectByHotkeys), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$SelectByHotkeys), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$SelectByHotkeys), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$SelectByHotkeys), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$SelectByHotkeys), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(SelectByHotkeys~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "SelectByHotkeys",
        main="SelectByHotkeys by Levels", col=mycol2)



# variable 6: assigntohotkeys
summary(skill$AssignToHotkeys)
summary(skill)
plot(density(level1$AssignToHotkeys), xlab='AssignToHotkeys', 
     main='AssignToHotkeys by Levels', xlim=c(0,0.0013), ylim=c(0,4500), 
     col=mycol2[1], lty=1, lwd=2)
lines(density(level2$AssignToHotkeys), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$AssignToHotkeys), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$AssignToHotkeys), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$AssignToHotkeys), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$AssignToHotkeys), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$AssignToHotkeys), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$AssignToHotkeys), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(AssignToHotkeys~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "AssignToHotkeys",
        main="AssignToHotkeys by Levels", col=mycol2)



# variable 7: MinimapAttacks
summary(skill$APM)

plot(density(level1$MinimapAttacks), xlab='MinimapAttacks', 
     main='MinimapAttacks by Levels',
     col=mycol2[1], lty=1, lwd=2)
lines(density(level2$MinimapAttacks), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$MinimapAttacks), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$MinimapAttacks), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$MinimapAttacks), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$MinimapAttacks), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$MinimapAttacks), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$MinimapAttacks), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(MinimapAttacks~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "MinimapAttacks",
        main="MinimapAttacks by Levels", col=mycol2)



# variable 8: MinimapRightClicks
summary(skill)
plot(density(level1$MinimapRightClicks), xlab='MinimapRightClicks', 
     main='MinimapRightClicks by Levels',
     col=mycol2[1], xlim=c(0,0.0015), lty=1, lwd=2)
lines(density(level2$MinimapRightClicks), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$MinimapRightClicks), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$MinimapRightClicks), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$MinimapRightClicks), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$MinimapRightClicks), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$MinimapRightClicks), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$MinimapRightClicks), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(MinimapRightClicks~LeagueIndex, data=skill, ylim=c(0,0.0025),
        xlab = "LeagueIndex", ylab = "MinimapRightClicks",
        main="MinimapRightClicks by Levels", col=mycol2)



# variable 9: NumberOfPACs
plot(density(level1$NumberOfPACs), xlab='NumberOfPACs', 
     main='NumberOfPACs by Levels',
     col=mycol2[1], xlim=c(0,0.008), lty=1, lwd=2)
lines(density(level2$NumberOfPACs), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$NumberOfPACs), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$NumberOfPACs), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$NumberOfPACs), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$NumberOfPACs), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$NumberOfPACs), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$NumberOfPACs), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(NumberOfPACs~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "NumberOfPACs",
        main="NumberOfPACs by Levels", col=mycol2)



# variable 10: GapBetweenPACs
plot(density(level1$GapBetweenPACs), xlab='GapBetweenPACs', main='GapBetweenPACs by Levels',
     col=mycol2[1], xlim=c(0,150), ylim=c(0,0.08), lty=1, lwd=2)
lines(density(level2$GapBetweenPACs), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$GapBetweenPACs), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$GapBetweenPACs), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$GapBetweenPACs), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$GapBetweenPACs), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$GapBetweenPACs), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$GapBetweenPACs), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(GapBetweenPACs~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "GapBetweenPACs",
        main="GapBetweenPACs by Levels", ylim=c(0,140), col=mycol2)



# variable 11: ActionLatency
plot(density(level1$ActionLatency), xlab='ActionLatency', 
     main='ActionLatency by Levels',
     col=mycol2[1], xlim=c(0,150), ylim=c(0,0.08), lty=1, lwd=2)
lines(density(level2$ActionLatency), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$ActionLatency), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$ActionLatency), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$ActionLatency), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$ActionLatency), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$ActionLatency), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$ActionLatency), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(ActionLatency~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "ActionLatency",
        main="ActionLatency by Levels", col=mycol2)



# variable 12: UniqueUnitsMade
plot(density(level1$UniqueUnitsMade), xlab='UniqueUnitsMade', 
     main='UniqueUnitsMade by Levels',
     col=mycol2[1], xlim=c(0,13), ylim=c(0,0.3), lty=1, lwd=2)
lines(density(level2$UniqueUnitsMade), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$UniqueUnitsMade), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$UniqueUnitsMade), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$UniqueUnitsMade), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$UniqueUnitsMade), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$UniqueUnitsMade), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$UniqueUnitsMade), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(UniqueUnitsMade~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "UniqueUnitsMade",
        main="UniqueUnitsMade by Levels", col=mycol2)



# variable 13: Age
plot(density(level1$Age), xlab='Age', 
     main='Age by Levels',
     col=mycol2[1], xlim=c(10,50), ylim=c(0,0.15), lty=1, lwd=2)
lines(density(level2$Age), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$Age), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$Age), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$Age), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$Age), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$Age), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$Age), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(Age~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "Age",
        main="Agee by Levels", col=mycol2)



# variable 14: ComplexUnitsMade
plot(density(level1$ComplexUnitsMade), xlab='ComplexUnitsMade', 
     main='ComplexUnitsMade by Levels',
     col=mycol2[1], ylim=c(0,50000), lty=1, lwd=2)
lines(density(level2$ComplexUnitsMade), col=mycol2[2], lty=1, lwd=2)
lines(density(level3$ComplexUnitsMade), col=mycol2[3], lty=1, lwd=2)
lines(density(level4$ComplexUnitsMade), col=mycol2[4], lty=1, lwd=2)
lines(density(level5$ComplexUnitsMade), col=mycol2[5], lty=1, lwd=2)
lines(density(level6$ComplexUnitsMade), col=mycol2[6], lty=1, lwd=2)
lines(density(level7$ComplexUnitsMade), col=mycol2[7], lty=1, lwd=2)
lines(density(level8$ComplexUnitsMade), col=mycol2[8], lty=1, lwd=2)
legend("topright", level.name, col=mycol2, pch=15)
boxplot(ComplexUnitsMade~LeagueIndex, data=skill,
        xlab = "LeagueIndex", ylab = "ComplexUnitsMade",
        main="ComplexUnitsMade by Levels", col=mycol2)


# 상관계수 구하기
cor(skill$ActionLatency, skill$LeagueIndex) #-0.673
cor(skill$APM, skill$LeagueIndex) #0.663
cor(skill$NumberOfPACs, skill$LeagueIndex) #0.612
cor(skill$GapBetweenPACs, skill$LeagueIndex) #-0.553
cor(skill$AssignToHotkeys, skill$LeagueIndex) #0.531
cor(skill$SelectByHotkeys, skill$LeagueIndex) #0.491
cor(skill$TotalHours, skill$LeagueIndex) #0.437
cor(skill$MinimapAttacks, skill$LeagueIndex) #0.324
cor(skill$MinimapRightClicks, skill$LeagueIndex) #0.232

a.vari = c("ActionLatency", "APM", "NumberOfPACs", "GapBetweenPACs",
           "AssignToHotkeys", "SelectByHotkeys", "TotalHours",
           "MinimapAttacks", "MinimapRightClicks")
a.value = c(0.673, 0.663, 0.612, 0.553, 0.531, 
            0.491, 0.437, 0.324, 0.232)  
barplot(a.value, names.arg=a.vari, main="Correlation Coefficients with LeagueIndex",
        col="springgreen3", ylim=c(0,0.8), ylab="Corr")
