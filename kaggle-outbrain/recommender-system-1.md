## content-based recommendation
사용자가 선호하는 제품과 비슷한 특징을 가진 제품을 추천
- ex) 사용자가 스파이더맨 영화를 보았다면, 스파이더맨에 대한 설명(heroes, adventure...)을 참고하여 비슷한 영화를 추천
- 상품들 사이의 유사도를 측정하는 것이 중요
- 상품들의 특징을 수치화하는 TF-IDF를 사용
- 장점 : 다른 사용자 정보나 평가 필요 없음, 새로 추가된 제품에 대한 추천 가능
- 단점 : 명시적으로 표현된 특징만 캐치 가능하고 질적인 부분 포착 못함, 추천하는 제품이 비슷한 장르에 머무름
- 향수를 좋아하는 사용자에게 특정 자동차를 추천하고 싶다면? collaborative filtering

## collaborative filtering
- 개인의 선호도와 과거 제품 구매 이력 등을 분석하여 개인에게 최적인 제품을 추천하는 알고리즘
- collaborative의 의미 : 유저와 컨텐츠의 콜라보레이션
- 내가 향수를 좋아하고 자동차를 좋아하면, 둘이 관계가 있다고 생각하는 것

## content-based collaborative filtering
사용자에게 사용자가 구매한 것과 가장 유사한 제품을 추천하는 알고리즘
- 제품들 사이의 선호도 패턴의 유사성 측정
  - 제품 u와 v의 유사도를 피어슨 상관계수나 코사인 유사도로 계산
- 유사성을 이용하여 선호도를 측정
  - 제품 u와 유사성이 높은 제품을 이용하여 선호도 추정
  - 특정 제품과 비슷한 제품 k개 뽑아서 그 제품들의 유사도를 가중치로 한 weighted mean(similar한 제품일수록 가중치 up)
  
## user-based collaborative filtering
사용자에게 유사한 사용자가 구매한 것과 가장 유사한 제품을 추천하는 알고리즘
- 고객들 사이의 선호도 패턴의 유사성을 측정
  - 사용자 u와 v의 유사도를 피어슨 상관계수나 코사인 유사도로 계산
- 유사성을 이용하여 선호도를 측정
  - 사용자 u와 유사성이 높은 사용자를 이용하여 선호도 추정
  - 나랑 비슷한 사람 k명 뽑아서 그 사람들의 i번째 아이템에 대한 유사도를 가중치로 한 weighted mean(similar한 사람일수록 가중치 up)