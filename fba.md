# 헬로 데이터 과학: 통계적 추론
Jin Young Kim  

이번에는 R로 통계적 추론의 과정을 실습해보자. 우선 GitHub 페이지에서 다운받은 파일을 다음과 같이 불러들여 살펴보자.


```r
source("dbook.R")
load.packages(c("dplyr", "ggplot2", "tidyr", "knitr"))
```

```
## [1] loading dplyr
## [1] loading ggplot2
## [1] loading tidyr
## [1] loading knitr
```

```r
ht = read.table("fba.txt", header = T)
head(ht)
```

```
##   Trial Air Helium
## 1     1  25     25
## 2     2  23     16
## 3     3  18     25
## 4     4  16     14
## 5     5  35     23
## 6     6  15     29
```

#### 탐색적 분석

본격적인 통계적 추론에 나서기 이전에 탐색적 분석을 수행해 보자. 우선 두 조건 하에서 각 시도에 대한 거리를 살펴보자. 


```r
par(mfrow=c(1,2))
plot(ht$Air, xlab="Trial", ylab="Distance (air-filled)") 
lines(supsmu(ht$Trial, ht$Air))                          
plot(ht$Helium, xlab="Trial", ylab="Distance (helium-filled)") 
lines(supsmu(ht$Trial, ht$Helium))                             
```

![](fba_files/figure-html/unnamed-chunk-2-1.png) 

위 플롯에서 몇 가지 경향을 관찰할 수 있다.

1. 시도횟수가 늘어나면서 양쪽 모두 평균 거리가 늘어나는 경향을 보인다. 이는 킥을 한 사람(키커)의 학습효과 때문일 것이라고 추정해볼 수 있다.
1. 양쪽 플롯에서 모두 평균에 훨씬 못 미치는 값이 발견된다. 이는 키커의 실수에 기인했을 가능성이 있다.

위 발견은 통계적 추론을 시행하는 데 몇 가지 시사점을 준다.

1. 공의 운동거리는 시도횟수와 양의 상관 관계를 갖는다. 따라서 공기와 헬륨 공의 비교에서 시도횟수의 효과를 제거하기 위해서는 각 시도 횟수별 데이터를 묶어서 비교하는 것이 바람직할 것이다.
1. 키커의 실수가 결과에 미치는 영향을 막기 위해 일정 기준에 못 미치는 결과를 제거하고 결과를 분석하는 것을 생각해볼 수 있다.

#### 데이터 가공하기

이제 탐색적 분석의 결론을 적용해 보자. 여기서 우리가 관심을 갖는 값은 두 조건 하에서 공의 운동 거리가 갖는 차이니, 여기에 해당하는 속성을 추가하자. 그리고 위에서 설명한 대로 일정 기준에 못 미치는 결과를 제거하도록 하자.


```r
ht$Diff = ht$Helium - ht$Air
htf = filter(ht, Air >= 15 & Helium >= 15)
head(htf)
```

```
##   Trial Air Helium Diff
## 1     1  25     25    0
## 2     2  23     16   -7
## 3     3  18     25    7
## 4     5  35     23  -12
## 5     6  15     29   14
## 6     7  26     25   -1
```

```r
nrow(ht)
```

```
## [1] 39
```

```r
nrow(htf)
```

```
## [1] 35
```

#### 통계적 추론

이제 위 분석 결과를 가지고 모집단에 대한 일반화된 결론을 도출해보자.  R에서는 t.test() 함수를 통해 아래와 같이 다양한 조건으로 신뢰구간과 가설검정을 수행할 수 있다. 


```r
# 전체 데이터에 대한 통계적 추론 ()
t.test(ht$Air, ht$Helium)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  ht$Air and ht$Helium
## t = -0.3703, df = 70.666, p-value = 0.7123
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.946828  2.023751
## sample estimates:
## mean of x mean of y 
##  25.92308  26.38462
```

```r
# 전체 데이터에 대한 통계적 추론 (대응 표본)
t.test(ht$Air, ht$Helium, paired = T)
```

```
## 
## 	Paired t-test
## 
## data:  ht$Air and ht$Helium
## t = -0.4198, df = 38, p-value = 0.677
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.687423  1.764346
## sample estimates:
## mean of the differences 
##              -0.4615385
```

```r
# 가공된 부분 데이터에 대한 통계적 추론 (대응 표본)
t.test(htf$Air, htf$Helium, paired = T)
```

```
## 
## 	Paired t-test
## 
## data:  htf$Air and htf$Helium
## t = -1.5547, df = 34, p-value = 0.1293
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -3.6914943  0.4914943
## sample estimates:
## mean of the differences 
##                    -1.6
```

위 결과를 보면 대응 표본을 사용한 경우, 그리고 부분 데이터를 사용한 경우가 그렇지 않은 경우에 비해 좁은 신뢰구간과 더 낮은 p-value를 얻는 것을 확인할 수 있다.
