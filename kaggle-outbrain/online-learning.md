## Online Learing
Online Learning이란 Offline Learning의 반대 개념으로, 데이터가 순차적으로 들어오는 환경에서의 모델링을 말한다. 즉, 지속적으로 새로운(fresh) 데이터를 모형에 적용해 모형이 항상 최신 상태로 유지되게 하는 방식이다. Online Learning에서 중요한 것은 두 가지다. 첫째, 방대한 양의 데이터를 효율적으로 모델링하는 것이다. 둘째, 신규 데이터를 반영하여 과거의 모델을 효과적으로 업데이트 시키는 것이다. 

Stochastic Gradient Descent는 loss function을 계산할 때 전체 train set을 사용하는 BGD와 달리, 일부 조그마한 데이터의 모음(mini-batch)에 대해서만 loss function을 계산하는 방법. BGD보다 다 소 부정확할 수 있지만, 계산 속도가 훨씬 빠르기 때문에 같은 시간에 더 많은 step 갈 수 있으며 여러번 반복 시 BGD와 유사한 결과로 수렵한다. 그리고 local minima에 빠지지 않고 더 좋은 방향으로 수렴할 가능성도 있다. Online Gradient Descent는 본질적으로 SGD와 동일하나, 다른 점은 데이터가 고정된 데이터셋이 아니라 지속적으로 흘러가는 streaming data라는 점이다.
***
