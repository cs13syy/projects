# 아마존 리스팅 최적화를 위한 텍스트 분석
- 아마존 소비자들의 70%가 검색 결과 1 페이지에서 구매 결정을 내리는 만큼, 자사 제품이 검색 결과 상위에 노출될 수 있는 방안을 마련하는 것은 브랜드에게 매우 중요한 과제다. 상품의 리스팅 최적화, 즉 소비자가 아마존 검색 결과를 통해 원하는 정보를 얻고 구매하는 데 최적화된 리스팅(이름, 가격, 리뷰 등)을 작성하는 것은 검색 결과 상위 노출을 위한 제 1 전략으로 알려져 있다.<br/>
- 따라서 본 프로젝트에서는 하나의 판매 상품을 가정하고, 해당 카테고리의 상위 10 페이지에 노출된 상품들의 리스팅 텍스트 분석을 실시한다. 더불어 분석 내용을 바탕으로 판매 상품에 적용할 수 있는 실행 방안을 제안하는 것을 목표로 한다.<br/>

## 1) 아마존 "sunscreen" 검색 결과 1~10 페이지 크롤링
- 1. 패키지 가져오기<br/>2. 베이스 url 설정 및 데이터프레임 생성<br/>3. 상품명, 가격, 링크 정보 가져오기<br/> 
'''R    
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
'''
