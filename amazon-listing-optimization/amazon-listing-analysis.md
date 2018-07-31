# 아마존 리스팅 최적화를 위한 텍스트 분석
- 아마존 소비자들의 70%가 검색 결과 1 페이지에서 구매 결정을 내리는 만큼, 자사 제품이 검색 결과 상위에 노출될 수 있는 방안을 마련하는 것은 브랜드에게 매우 중요한 과제다. 상품의 리스팅 최적화, 즉 소비자가 아마존 검색 결과를 통해 원하는 정보를 얻고 구매하는 데 최적화된 리스팅(이름, 가격, 리뷰 등)을 작성하는 것은 검색 결과 상위 노출을 위한 제 1 전략으로 알려져 있다.<br/>
- 따라서 본 프로젝트에서는 하나의 판매 상품을 가정하고, 해당 카테고리의 상위 10 페이지에 노출된 상품들의 리스팅 텍스트 분석을 실시한다. 더불어 분석 내용을 바탕으로 판매 상품에 적용할 수 있는 실행 방안을 제안하는 것을 목표로 한다.<br/>

## 1) 아마존 "sunscreen" 검색 결과 1~10 페이지 크롤링
- 1. 패키지 가져오기<br/>2. 베이스 url 설정 및 데이터프레임 생성<br/>3. 상품명, 가격, 링크 정보 가져오기<br/> 
<pre>
library(rvest)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

base_url_1 = "https://www.amazon.com/s/ref=sr_pg_"
base_url_2 = "?rh=i%3Aaps%2Ck%3Asunscreen&page="
base_url_3 = "&keywords=sunscreen&ie=UTF8&qid=1532086589" 
total_name <- data.frame(name=character(), stringsAsFactors=FALSE)
total_price <- data.frame(price=character(), stringsAsFactors=FALSE)
total_link <- data.frame(link=character(), stringsAsFactors=FALSE)

for (i in 1:10){
  url = paste0(base_url_1, i, base_url_2, i, base_url_3)
  print(url)
  html_sun = read_html(x = url, encoding = 'UTF-8')
  name_sun <- html_nodes(html_sun, ".a-link-normal") %>% html_attr("title")
  name_sun <- na.omit(name_sun)
  price_sun <- html_nodes(html_sun, ".a-offscreen") %>% html_text()
  price_sun <- gsub(pattern="[Sponsored]", replacement = NA, price_sun)
  price_sun <- na.omit(price_sun)
  link_sun <- html_nodes(html_sun, ".a-size-small.a-link-normal.a-text-normal") 
  %>%  html_attr("href")
  total_name <- rbind(total_name, name_sun, stringsAsFactors=FALSE)
  total_price <- rbind(total_price, price_sun, stringsAsFactors=FALSE)
  total_link <- rbind(total_link, link_sun, stringsAsFactors=FALSE)
  cat(i, "\n")
}
<code>

## 2) 상품명 전처리 및 시각화
'''
# total_name의 벡터 변환, unique 값 추출, 파싱
unlist_name <- tolower(unlist(total_name))
unq_unlist_name <- unique(unlist_name)
split_name <- strsplit(unq_unlist_name, split = ' ')
split_name_vec <- unlist(split_name)
split_name_vec <- gsub("[.|!|,|(|)|&|-]", "", split_name_vec)

# total_name의 wordcloud 생성
wordcloud(split_name_vec, min.freq=4,
          colors=brewer.pal(8, "Dark2"))

# 상품명 키워드 TOP 6 시각화
name <- c("sunscreen", "spf", "broad", "spectrum", "oz", "lotion")
num <- c(160, 114, 60, 60, 52, 50)
top_6 <- data.frame(name, num)
ggplot(data=top_6, aes(x=reorder(name, num), y=num)) + geom_bar(stat="identity", fill="violetred1") + ggtitle("Top 6 Words in Title") + coord_flip()

# 상품명 키워드 TOP 30 시각화
split_name_vec_sort <- sort(table(split_name_vec), decreasing=T)
split_name_vec_bar <- unclass(split_name_vec_sort)
split_name_vec_bar_1 <- split_name_vec_bar[7:18]
barplot(split_name_vec_bar_1,  main = "7th~18th Words in Title", 
        xlab="words", ylab = "numbers", col="springgreen3")
split_name_vec_bar_2 <- split_name_vec_bar[19:30]
barplot(split_name_vec_bar_2, main = "19th~30th Words in Title", 
        xlab="words", ylab = "numbers", ylim=c(0,40), 
        col="springgreen3")
'''

## 3) 상품가격 전처리 및 시각화
'''
# total_price의 벡터 변환, 파싱, 단일 가격 추출, 기술 통계량 확인, 시각화
unlist_price <- unlist(total_price)
unlist_price <- gsub('\\$','',unlist_price)
unlist_price <- Filter(function(x){nchar(x)<=5}, unlist_price)
unlist_price <- as.numeric(unlist_price)
summary(unlist_price)
var(unlist_price)
med = median(unlist_price)
boxplot(unlist_price, main="Boxplot of Selling Price", col="dodgerblue")
abline(h=med, col="red")
hist(unlist_price, main="Histogram and Density Plot of Price", 
     xlim=c(0,100), ylim=c(0,0.09), freq=FALSE, xlab="price", 
     breaks=50, col="mediumaquamarine")
lines(density(unlist_price), col="tomato1", lty=1, lwd=5)
'''

## 4) 아마존 "sunscreen" 검색 결과 TOP 10 상품의 리뷰 1~10 페이지 크롤링
'''
# sunscreen 카테고리 상위 10개 상품의 리뷰 10 페이지 수집
# 1 : Sun-Bum-Moisturizing-SPF-Hypoallergenic
base_url_1 = "https://www.amazon.com/Sun-Bum-Moisturizing-SPF-Hypoallergenic/
product-reviews/B004XGPMFA/ref=cm_cr_arp_d_paging_btm_"
base_url_2 = "?ie=UTF8&reviewerType=all_reviews&pageNumber="
total_rev <- data.frame(review=character(), stringsAsFactors=FALSE)

for (i in 1:10){
  url = paste0(base_url_1, i, base_url_2, i)
  print(url)
  html_rev = read_html(x = url, encoding = 'UTF-8')
  review <- html_nodes(html_rev, "span.a-size-base.review-text") %>% html_text()
  total_rev <- rbind(total_rev, review, stringsAsFactors=FALSE)
  cat(i, "\n")
}
'''

## 5) TOP 10 상품의 리뷰 데이터 전처리, 병합, 시각화
'''
names(total_rev) = c(1:10)
unlist_rev <- unlist(total_rev)
split_rev_vec <- strsplit(unlist_rev, " ", fixed=TRUE)
final_rev_1 <- tolower(unlist(split_rev_vec))
final_rev_1 <- gsub("[.|!|,]", "", final_rev_1)

# 10개 제품의 리뷰들을 하나의 벡터로 병합하고 sort
total_review <- c(final_rev_1, final_rev_2, final_rev_3, final_rev_4, 
                 final_rev_5, final_rev_6, final_rev_7, final_rev_8,
                 final_rev_9, final_rev_10)
sorted_total_review <- sort(table(total_review), decreasing=T)
sorted_total_review[1:100]
sorted_total_review[101:200]
  
# total_name의 워드클라우드
wordcloud(split_name_vec, min.freq=5,
          colors=brewer.pal(8, "Dark2"))
'''
