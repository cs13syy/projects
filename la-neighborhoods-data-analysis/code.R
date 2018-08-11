rm(list=ls())

# working directory
getwd()
setwd("C:/Users/User/Desktop/서윤/code")

# load packages
devtools::install_github("hrbrmstr/waffle")
library(waffle)
library(ggplot2)
library(dplyr)
library(OpenStreetMap)
library(ggmap)
library(reshape2)

# load file
la_dat <- read.csv("LA_Neighborhoods_Data.csv", header=TRUE)

# stucture and summary of file
dim(la_dat)
str(la_dat)
summary(la_dat)
head <- head(la_dat)

# check missing value
sum(is.na(la_dat))

# check and change colnames
colnames(la_dat)
colnames(la_dat)[1] = "LA_Nbhd"
attach(la_dat)

# descriptive statistics
head(la_dat)
# income
subset(LA_Nbhd, Income==max(Income))
subset(LA_Nbhd, Income==min(Income))
la_dat[Income>mean(Income),]
la_dat[Income<mean(Income),]
# diversity
subset(LA_Nbhd, Diversity==max(Diversity))
subset(LA_Nbhd, Diversity==min(Diversity))
subset(Asian, Diversity==min(Diversity))
subset(Black, Diversity==min(Diversity))
subset(Latino, Diversity==min(Diversity))
subset(White, Diversity==min(Diversity))
# waffle chart for racial ratio of Gramercy Park
which(LA_Nbhd=="Gramercy_Park") # 35
race <- c("Asian", "Black", "Latino", "White")
Gramercy_Park <- la_dat[35, race]
Gramercy_Park <- data.frame(names = race, vals = c(0, 86, 12, 1))
waffle(Gramercy_Park, title="Racial Ratio of Gramercy Park")
# waffle chart for racial ratio of Beverly Crest
which(LA_Nbhd=="Beverly_Crest")
Beverly_Crest <- la_dat[7, race]
Beverly_Crest <- data.frame(names = race, vals = c(4, 2, 3, 88))
waffle(Beverly_Crest, title="Racial Ratio of Beverly_Crest")
# waffle chart for racial ratio of Boyle Heights
which(LA_Nbhd=="Boyle_Heights")
Boyle_Heights <- la_dat[10, race]
Boyle_Heights <- data.frame(names = race, vals = c(2, 1, 94, 2))
waffle(Boyle_Heights, title="Racial Ratio of Boyle_Heights")
# age
subset(LA_Nbhd, Age==max(Age))
subset(LA_Nbhd, Age==min(Age))
# population
subset(LA_Nbhd, Population==max(Population))
# races
subset(LA_Nbhd, Asian==max(Asian))
subset(LA_Nbhd, Black==max(Black))
subset(LA_Nbhd, Latino==max(Latino))
subset(LA_Nbhd, White==max(White))

# check correlation btw Income and Homes
# dependent variable Income screening 
Inc_freq <- Income/sum(Income)*100                                                       
lm(Homes~Inc_freq, data=la_dat)
ggplot(data=la_dat, aes(x=Inc_freq, y=Homes, col="red")) + geom_point() + geom_abline(intercept=0.2123, slope=0.1564)
summary(lm(Homes~Inc_freq, data=la_dat))

# check correlation btw Income and Schools
ggplot(data=la_dat, aes(x=sqrt(Income), y=Schools)) + geom_point(col="green")
inc_sch = data.frame(inc=sqrt(Income), sch=Schools)
inc_sch <- subset(inc_sch, inc_sch$sch != 0) # eliminate data with 0
ggplot(data=inc_sch, aes(x=inc_sch$inc, y=inc_sch$sch)) + geom_point(col="green")
summary(lm(inc_sch$sch~inc_sch$inc))
  
# check correlation btw Diversity and Schools
ggplot(data=la_dat, aes(x=Diversity, y=Schools)) + geom_point(col="blue")
new_dat <- subset(la_dat, Schools != 0) # eliminate data with 0
ggplot(data=new_dat, aes(x=Diversity*100, y=Schools)) + geom_point(col="blue")

# check correlation btw Age and Homes
ggplot(data=la_dat, aes(x=Age, y=Homes)) + geom_point(col="violet")

# check correlation btw Vets and Age
Vets.1 = Vets*100
summary(lm(Age~Vets.1))
ggplot(data=la_dat, aes(x=Vets.1, y=Age)) + geom_point(col="navy") + geom_abline(intercept=21.8536, slope=1.5262)

# check correlation btw Races
eth_dat <- data.frame(Asian, Black, Latino, White)
plot(eth_dat)

# check correlation btw Latino and White
summary(lm(White~Latino))
ggplot(data=la_dat, aes(x=Latino, y=White)) + geom_point(col="orange") + geom_abline(intercept=0.7145, slope=-0.9509)

# check correlation btw White and Income
Inc.1 = log(Income)
summary(lm(Inc.1~White))
ggplot(data=la_dat, aes(x=White, y=Inc.1)) + geom_point(col="coral") + geom_abline(intercept=10.42874, slope=1.28907)

# check corrlation between all variables
plot(la_dat)

# visualizing the map
nbhd <- LA_Nbhd
lon <- Longitude
lat <- Latitude
inc <- Income
pop <- Population
asian <- Asian
white <- White

# load get_googlemap for visualization
p1 <- get_googlemap("los angeles") %>% ggmap
locationInfo <- data.frame(
  Name = LA_Nbhd, lon = lon, lat = lat)
p1 + geom_point(data = locationInfo, size=5, aes(x = lon, y = lat, col=LA_Nbhd))

# change continuous variable Income to factors
la_dat.1 <- la_dat
la_dat.1$inc_fac[Income<50000]="1.low"
la_dat.1$inc_fac[Income>=50000 & Income<100000]="2.middle"
la_dat.1$inc_fac[Income>=100000 & Income<200000]="3.high"
la_dat.1$inc_fac[Income>=200000]="4.very high"
table(la_dat.1$inc_fac)
loc1 <- data.frame(Name=nbhd, lon=lon, lat=lat, inc_fac=la_dat.1$inc_fac)
p1 + geom_point(data=loc1, aes(x=lon, y=lat,
                               colour=inc_fac), alpha=.8, size=7)

# change continuous variable Population to factors
la_dat.1$pop_fac[pop<30000]="1.low"
la_dat.1$pop_fac[pop>=30000 & pop<60000]="2.middle"
la_dat.1$pop_fac[pop>=60000 & pop<90000]="3.high"
la_dat.1$pop_fac[pop>=90000]="4.very high"
loc2 <- data.frame(Name=nbhd, lon=lon, lat=lat, pop_fac=la_dat.1$pop_fac)
p1 + geom_point(data=loc2, aes(x=lon, y=lat,  
                               colour=pop_fac), alpha=.8, size=7)
