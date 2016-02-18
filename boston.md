# 헬로 데이터 과학: 회귀 분석
Jin Young Kim  

이번에는 R을 사용하여 기계학습을 이용한 문제 해결의 과정을 사례로 배워보도록 하자. 기계학습의 다양한 기법 가운데 수치 형태의 속성을 예측하는 회귀 분석을 수행하고 그 결과를 평가하는 방법을 배워보도록 하자. 

이번 실습에 사용할 데이터셋 역시 보스턴의 지역별 집값에 영향을 끼치는 요인을 정리한 것으로, 다양한 변인을 조합하여 각 지역의 집값을 예측하는 것이 목표다. 우선 아래와 같이 작업 파일을 R로 불러 살펴보도록 하자.


```r
source("dbook.R")
load.packages(c("plyr","dplyr", "ggplot2", "tidyr", "rpart", "randomForest"))
```

```
## [1] loading plyr
## [1] loading dplyr
## [1] loading ggplot2
## [1] loading tidyr
## [1] loading rpart
## [1] loading randomForest
```

```r
bos = read.table("boston.txt", sep="\t", header=T)
head(bos)
```

```
##      crim zn indus chas   nox    rm  age    dis rad tax ptratio  black
## 1 0.00632 18  2.31    0 0.538 6.575 65.2 4.0900   1 296    15.3 396.90
## 2 0.02731  0  7.07    0 0.469 6.421 78.9 4.9671   2 242    17.8 396.90
## 3 0.02729  0  7.07    0 0.469 7.185 61.1 4.9671   2 242    17.8 392.83
## 4 0.03237  0  2.18    0 0.458 6.998 45.8 6.0622   3 222    18.7 394.63
## 5 0.06905  0  2.18    0 0.458 7.147 54.2 6.0622   3 222    18.7 396.90
## 6 0.02985  0  2.18    0 0.458 6.430 58.7 6.0622   3 222    18.7 394.12
##   lstat medv
## 1  4.98 24.0
## 2  9.14 21.6
## 3  4.03 34.7
## 4  2.94 33.4
## 5  5.33 36.2
## 6  5.21 28.7
```

```r
summary(bos)
```

```
##       crim                zn             indus            chas        
##  Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000  
##  1st Qu.: 0.08204   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000  
##  Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000  
##  Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917  
##  3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000  
##  Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000  
##       nox               rm             age              dis        
##  Min.   :0.3850   Min.   :3.561   Min.   :  2.90   Min.   : 1.130  
##  1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100  
##  Median :0.5380   Median :6.208   Median : 77.50   Median : 3.207  
##  Mean   :0.5547   Mean   :6.285   Mean   : 68.57   Mean   : 3.795  
##  3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188  
##  Max.   :0.8710   Max.   :8.780   Max.   :100.00   Max.   :12.127  
##       rad              tax           ptratio          black       
##  Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32  
##  1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38  
##  Median : 5.000   Median :330.0   Median :19.05   Median :391.44  
##  Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67  
##  3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23  
##  Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90  
##      lstat            medv      
##  Min.   : 1.73   Min.   : 5.00  
##  1st Qu.: 6.95   1st Qu.:17.02  
##  Median :11.36   Median :21.20  
##  Mean   :12.65   Mean   :22.53  
##  3rd Qu.:16.95   3rd Qu.:25.00  
##  Max.   :37.97   Max.   :50.00
```

#### 탐색적 분석

이제 집값(medv)에 영향을 미치는 다양한 변인을 살펴보자. 우선 R에서 상관도 테이블을 만들어서 클립보드로 그 내용을 내보내자.



```r
plot(bos[,c(1:13, 14)])
```

![](boston_files/figure-html/unnamed-chunk-2-1.png) 

이제 클립보드의 상관도 테이블을 엑셀로 읽어서 (붙여넣기: Ctrl+V) 조건부 서식을 적용하도록 하자. 아래 플롯은 집값에 영향을 끼치는 변인들을 요약한다. 각 변수간의 상관도를 색상으로 표시하는데, 파란 색은 양의 상관도를, 붉은 색은 음의 상관도를 가진다. 아래 플롯을 보면 집값(medv)과 상관도가 가장 높은 속성은 역시 집의 크기를 결정하는 방의 개수(rm)인 것을 알 수 있다.


```r
ctab = round(cor(bos),3)
write.table(ctab, "clipboard")
```

또한 우리가 예측하고자 하는 집값의 분포를 살펴보자. 아래 플롯을 보면 대체로 정규 분포를 띄지만 고가의 주택이 밀집한 지역이 많이 존재하는 것을 알 수 있다. 이처럼 예측대상인 변수가 정규 분포를 따르지 않는 것은 선형 모델의 정확도에 문제를 가져올 수 있다.


```r
par(mfrow=c(1,2))
hist(bos$medv, main="Density of Median (medv)")
plot(density(bos$medv), main="Density of Median (medv)")
```

![](boston_files/figure-html/unnamed-chunk-4-1.png) 

#### 학습 모델 만들기

이제 실제 학습 모델을 만들어보자. 이 장에서는 단순하면서 해석이 용이하지만 예측 성능에 한계가 있는 선형 회귀 모델과 의사결정트리 기법, 그리고 해석이 힘들지만 높은 예측 성능을 보이는 랜덤포레스트(RandomForest) 모델을 사용하여 서로 다른 모델의 특성과 성능을 비교 분석해보자.



```r
# 주어진 데이터로 선형 모델을 만든다
lm.m <- function(tbl){
	lm(medv ~ crim + zn + indus + chas + nox + rm + age + 
	     dis + rad + tax + ptratio + black + lstat, tbl)
}

# 주어진 데이터로 의사결정트리 모델을 만든다
rpart.m <- function(tbl){
	rpart(medv ~ crim + zn + indus + chas + nox + rm + age + 
	        dis + rad + tax + ptratio + black + lstat, tbl)
}

# 주어진 데이터로 랜덤포레스트 모델을 만든다
randomForest.m <- function(tbl){
	randomForest(medv ~ crim + zn + indus + chas + nox + rm + age + 
	               dis + rad + tax + ptratio + black + lstat, tbl)
}
```

우선 간단한 선형 모델로 시작해보자. 아래는 R로 선형회귀 모델을 학습하고 각 변수의 계수(coefficient)를 시각화한 결과다. 위의 상관도 분석에서 집값과 관련성이 높은 것으로 조사된 속성들이 꼭 높은 양의 상관 관계를 보이지는 않는 것을 알 수 있다. 이는 주어진 데이터의 많은 속성들이 서로 높은 상관 관계를 가지며, 따라서 이들 중 일부만 알아도 예측에는 충분하기 때문이다.


```r
lm1 = lm.m(bos)
barplot(lm1$coefficients)
```

![](boston_files/figure-html/unnamed-chunk-6-1.png) 

이번에는 R에서 의사결정트리를 이용하여 주어진 데이터로 학습한 결과다. 의사 결정의 각 단계가 트리 형태로 표현되는 것을 볼 수 있다. 아래 트리 왼쪽의 설명은 트리의 각 노드를 따라가면서 집값이 결정되는 과정을 나타낸다. 트리의 각 노드의 기준이 차례로 적용되어 최종 집값이 계산되는 것을 볼 수 있다.

아래 학습된 트리의 모양을 보면 어떤 속성이 의사결정에서 우선시되어야 하는지를 알 수 있다. 위 트리에서는 가구당 방의 개수(rm)를 최우선 기준으로, 그리고 주민의 소득 수준(lstat) 및 범죄율(crim) 등을 2차 기준으로 하는 의사결정트리가 학습되었다. 앞에서 살펴본 선형회귀 모델에서 높은 계수를 가지는 속성이 의사결정트리에 꼭 사용되는 것은 아니라는 점을 알 수 있다. 


```r
rp1 = rpart.m(bos)
plot(rp1)
text(rp1)
```

![](boston_files/figure-html/unnamed-chunk-7-1.png) 

이처럼 직관적으로 해석할 수 있는 선형 모델과 의사결정 트리와 달리 의사결정 트리에 기반한 비선형 모델인 랜덤포레스트는 실제로 수많은 트리의 집합(forest)으로 구성되어 있기 때문에, 학습한 결과를 한눈에 살펴보기 어렵다. 

하지만 R의 랜덤포레스트 라이브러리는 학습 결과의 이해를 돕는 몇 가지 차트를 제공한다. 우선 아래 왼쪽의 차트는 학습 모델에 포함된 트리의 개수에 따른 에러율의 변화를 보여준다. 트리 개수가 200개를 넘어가면 에러율이 안정화되는 것을 확인할 수 있다. 그리고 아래 오른쪽의 차트는 학습된 트리 집합 전체를 바탕으로 계산된 각 속성의 중요도를 보여준다. 여기서 IncNodePurity는 각 속성이 전체 예측 결과의 품질을 얼마나 높이는지를 나타내는 지표로 이해하면 된다.



```r
rf1 = randomForest.m(bos)
par(mfrow=c(1,2))
plot(rf1, log="y", main="Error Rate ~ # of Trees")
varImpPlot(rf1, main="Variable Importance")
```

![](boston_files/figure-html/unnamed-chunk-8-1.png) 

#### 학습 모델 평가하기

지금까지 주어진 데이터를 가지고 다양한 모델을 만드는 방법을 알아보았다. 이제 만들어진 모델을 평가하는 방법을 알아보자. 실제 예측 결과에서 어떤 차이를 보이는 지를 알기 위해서는 학습에 사용하지 않은 데이터에 대한 예측 성능을 비교해보아야 할 것이다.

이처럼 두 모델의 예측 성능을 비교하기 위해서 앞에서 설명한 교차검증법을 사용할 수 있다. 이는 주어진 데이터를 K개의 그룹으로 나누고, 각 그룹에 대한 예측값을 (K-1)개의 그룹의 속성을 사용해서 학습한 모델을 가지고 얻어내고, 그 결과를 집계하는 방식이다. 


```r
# 주어진 데이터를 k개로 나누어 교차검증을 수행
cval.model <- function(cvt, fun.model, k, ...)
{
	# 원본 데이터의 각 항목에 1~k까지의 숫자를 배정
	cvt$fold = sample(1:k, nrow(cvt), replace=T)

	# 각 fold의 학습 모델을 만들어 결과를 모음
	adply(1:k, 1, function(i){
		cvtr = cvt[cvt$fold != i,] # 학습 데이터
		cvte = cvt[cvt$fold == i,] # 평가 데이터
		# 모델의 예측 결과를 얻음
		cvte$res = predict(fun.model(cvtr, ...), cvte) 
		cvte
	})
}
# 3차 교차검증을 세가지 모델에 대해 수행
crt1 = cval.model(bos, lm.m, 3)
crt2 = cval.model(bos, rpart.m, 3)
crt3 = cval.model(bos, randomForest.m, 3)
head(crt3)
```

```
##   X1    crim   zn indus chas   nox    rm   age    dis rad tax ptratio
## 1  1 0.02729  0.0  7.07    0 0.469 7.185  61.1 4.9671   2 242    17.8
## 2  1 0.08829 12.5  7.87    0 0.524 6.012  66.6 5.5605   5 311    15.2
## 3  1 0.14455 12.5  7.87    0 0.524 6.172  96.1 5.9505   5 311    15.2
## 4  1 0.21124 12.5  7.87    0 0.524 5.631 100.0 6.0821   5 311    15.2
## 5  1 0.11747 12.5  7.87    0 0.524 6.009  82.9 6.2267   5 311    15.2
## 6  1 1.05393  0.0  8.14    0 0.538 5.935  29.3 4.4986   4 307    21.0
##    black lstat medv fold      res
## 1 392.83  4.03 34.7    1 35.96700
## 2 395.60 12.43 22.9    1 20.81733
## 3 396.90 19.15 27.1    1 17.81627
## 4 386.63 29.93 16.5    1 16.91083
## 5 396.90 13.27 18.9    1 19.78804
## 6 386.85  6.58 23.1    1 21.21223
```

회귀 모델의 예측 성능을 알아보기 위해서는 앞서 살펴본 RMSE(root mean squared error) 지표를 사용하면 된다. 아래 코드는 선형회귀, 의사결정트리 및 랜덤포레스트 모델의 예측값을 비교한 것이다. 


```r
# RMSE지표를 계산
rmse <- function(c1, c2){
	sqrt(mean((c1 - c2) ^ 2))
}
# 세 모델의 교차검증 결과에 대한 RMSE를 계산
rmse(crt1$medv, crt1$res)
```

```
## [1] 4.900297
```

```r
rmse(crt2$medv, crt2$res)
```

```
## [1] 4.647861
```

```r
rmse(crt3$medv, crt3$res)
```

```
## [1] 3.400849
```

이제 위에서 설명한 모델 간의 예측 결과를 시각적으로 비교해보자. 아래 플롯은 지금까지 소개한 세 가지 기계학습 모델로 예측된 집값과 실제 집값의 관계를 스케터플롯으로 나타낸 것이다. 아래 플롯의 왼쪽에 있는 선형회귀 모델의 결과를 보면 낮은 가격대의 집값은 비교적 정확하게 예측하지만 집값이 올라갈수록 예측값이 실제 값보다 작아지는 경향을 알 수 있다. 이는 예측에 사용된 속성과 예측 대상이 되는 속성 간의 선형적 관계만을 표현할 수 있는 선형회귀 기법의 한계라고 할 수 있다. 

가운데의 의사결정 트리는 선형 모델에서와 같은 편향은 나타나지 않지만, 예측값이 몇 가지 값으로 계층화된 것을 볼 수 있다. 이는 업력 속성값에 따라 구간을 나누어 예측 속성값을 결정하는 의사결정트리의 특성을 보여준다. 반면에 오른쪽 랜덤포레스트 모델의 결과는 실제 값과 예측값이 거의 일치하는 것을 알 수 있다. 이는 수많은 개별 트리를 사용하여 주어진 데이터의 다양한 패턴을 포착하는 랜덤포레스트 모델에서 주어진 속성과 예측값 간의 관계를 정확히 모델링한 결과다.


```r
par(mfrow=c(1,3))
plot(crt1$medv, crt1$res, ylim=c(0,50), 
     xlab="Actual", ylab="Predicted", main="Linear Regression")
lines(loess.smooth(crt1$medv, crt1$res))
plot(crt2$medv, crt2$res,  ylim=c(0,50), 
     xlab="Actual", ylab="Predicted", main="Decision Tree")
lines(loess.smooth(crt2$medv, crt2$res))
plot(crt3$medv, crt3$res,  ylim=c(0,50), 
     xlab="Actual", ylab="Predicted", main="RandomForest")
lines(loess.smooth(crt3$medv, crt3$res))
```

![](boston_files/figure-html/unnamed-chunk-11-1.png) 
