# Prediction Practice
Jin Kim  
Saturday, April 25, 2015  


```r
source("../dbook.R")
check.and.install.packages("dplyr")
check.and.install.packages("ggplot2")
check.and.install.packages("tidyr")
check.and.install.packages("ellipse")
check.and.install.packages("ridge")
check.and.install.packages("leaps")
check.and.install.packages("randomForest")
library(ggplot2)
library(plyr)
library(ellipse)
library(tidyr)
library(rpart)
library(MASS)
library(ridge)
library(randomForest)
library(leaps)
library(dplyr)
library(knitr)
```


```r
summary(Boston)
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

```r
#write.table(Boston, "ch3/boston.txt", sep="\t", row.names=F)
```

# Exploring the Data

![](dbook_pred_boston_files/figure-html/unnamed-chunk-3-1.png) ![](dbook_pred_boston_files/figure-html/unnamed-chunk-3-2.png) ![](dbook_pred_boston_files/figure-html/unnamed-chunk-3-3.png) ![](dbook_pred_boston_files/figure-html/unnamed-chunk-3-4.png) 

# Model Definition



```r
lm.b <- function(dt){
	res = lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, dt)
	res
}

lm.s <- function(dt){
	res = lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat, dt)
	res
}

lm.r <- function(dt){
	res = linearRidge(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + black + lstat, dt)
	res
}

rpart.b <- function(dt){
	res = rpart(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, dt)
	res
}

rf.b <- function(dt){
	res = randomForest(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, dt) #, proximity=TRUE
	res
}
```

# Linear Model


```r
lm1 = lm.b(Boston)
summary(lm1)
```

```
## 
## Call:
## lm(formula = medv ~ crim + zn + indus + chas + nox + rm + age + 
##     dis + rad + tax + ptratio + black + lstat, data = dt)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -15.595  -2.730  -0.518   1.777  26.199 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
## crim        -1.080e-01  3.286e-02  -3.287 0.001087 ** 
## zn           4.642e-02  1.373e-02   3.382 0.000778 ***
## indus        2.056e-02  6.150e-02   0.334 0.738288    
## chas         2.687e+00  8.616e-01   3.118 0.001925 ** 
## nox         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
## rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***
## age          6.922e-04  1.321e-02   0.052 0.958229    
## dis         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
## rad          3.060e-01  6.635e-02   4.613 5.07e-06 ***
## tax         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
## ptratio     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
## black        9.312e-03  2.686e-03   3.467 0.000573 ***
## lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.745 on 492 degrees of freedom
## Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
## F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16
```

```r
rpb = rpart.b(Boston)
summary(rpb)
```

```
## Call:
## rpart(formula = medv ~ crim + zn + indus + chas + nox + rm + 
##     age + dis + rad + tax + ptratio + black + lstat, data = dt)
##   n= 506 
## 
##           CP nsplit rel error    xerror       xstd
## 1 0.45274420      0 1.0000000 1.0064796 0.08321952
## 2 0.17117244      1 0.5472558 0.6332674 0.05829741
## 3 0.07165784      2 0.3760834 0.4363542 0.04846130
## 4 0.03616428      3 0.3044255 0.3444686 0.04311093
## 5 0.03336923      4 0.2682612 0.3297930 0.04348108
## 6 0.02661300      5 0.2348920 0.3237049 0.04348066
## 7 0.01585116      6 0.2082790 0.2783188 0.03941956
## 8 0.01000000      7 0.1924279 0.2566501 0.03900442
## 
## Variable importance
##      rm   lstat     dis   indus     tax ptratio     nox     age    crim 
##      33      21       7       7       6       6       6       6       4 
##      zn     rad   black 
##       2       1       1 
## 
## Node number 1: 506 observations,    complexity param=0.4527442
##   mean=22.53281, MSE=84.41956 
##   left son=2 (430 obs) right son=3 (76 obs)
##   Primary splits:
##       rm      < 6.941    to the left,  improve=0.4527442, (0 missing)
##       lstat   < 9.725    to the right, improve=0.4423650, (0 missing)
##       indus   < 6.66     to the right, improve=0.2594613, (0 missing)
##       ptratio < 19.9     to the right, improve=0.2443727, (0 missing)
##       nox     < 0.6695   to the right, improve=0.2232456, (0 missing)
##   Surrogate splits:
##       lstat   < 4.83     to the right, agree=0.891, adj=0.276, (0 split)
##       ptratio < 14.55    to the right, agree=0.875, adj=0.171, (0 split)
##       zn      < 87.5     to the left,  agree=0.862, adj=0.079, (0 split)
##       indus   < 1.605    to the right, agree=0.862, adj=0.079, (0 split)
##       crim    < 0.013355 to the right, agree=0.852, adj=0.013, (0 split)
## 
## Node number 2: 430 observations,    complexity param=0.1711724
##   mean=19.93372, MSE=40.27284 
##   left son=4 (175 obs) right son=5 (255 obs)
##   Primary splits:
##       lstat   < 14.4     to the right, improve=0.4222277, (0 missing)
##       nox     < 0.6695   to the right, improve=0.2775455, (0 missing)
##       crim    < 5.84803  to the right, improve=0.2483622, (0 missing)
##       ptratio < 19.9     to the right, improve=0.2199328, (0 missing)
##       age     < 75.75    to the right, improve=0.2089435, (0 missing)
##   Surrogate splits:
##       age   < 84.3     to the right, agree=0.814, adj=0.543, (0 split)
##       indus < 16.57    to the right, agree=0.781, adj=0.463, (0 split)
##       nox   < 0.5765   to the right, agree=0.781, adj=0.463, (0 split)
##       dis   < 2.23935  to the left,  agree=0.781, adj=0.463, (0 split)
##       tax   < 434.5    to the right, agree=0.774, adj=0.446, (0 split)
## 
## Node number 3: 76 observations,    complexity param=0.07165784
##   mean=37.23816, MSE=79.7292 
##   left son=6 (46 obs) right son=7 (30 obs)
##   Primary splits:
##       rm      < 7.437    to the left,  improve=0.5051569, (0 missing)
##       lstat   < 4.68     to the right, improve=0.3318914, (0 missing)
##       ptratio < 19.7     to the right, improve=0.2498786, (0 missing)
##       rad     < 16       to the right, improve=0.2139402, (0 missing)
##       crim    < 2.742235 to the right, improve=0.2139402, (0 missing)
##   Surrogate splits:
##       lstat   < 3.99     to the right, agree=0.776, adj=0.433, (0 split)
##       ptratio < 14.75    to the right, agree=0.671, adj=0.167, (0 split)
##       black   < 389.885  to the right, agree=0.658, adj=0.133, (0 split)
##       crim    < 0.11276  to the left,  agree=0.645, adj=0.100, (0 split)
##       indus   < 18.84    to the left,  agree=0.645, adj=0.100, (0 split)
## 
## Node number 4: 175 observations,    complexity param=0.026613
##   mean=14.956, MSE=19.27572 
##   left son=8 (74 obs) right son=9 (101 obs)
##   Primary splits:
##       crim  < 6.99237  to the right, improve=0.3370069, (0 missing)
##       nox   < 0.607    to the right, improve=0.3307926, (0 missing)
##       dis   < 2.0037   to the left,  improve=0.2927244, (0 missing)
##       tax   < 567.5    to the right, improve=0.2825858, (0 missing)
##       lstat < 19.83    to the right, improve=0.2696497, (0 missing)
##   Surrogate splits:
##       rad     < 16       to the right, agree=0.880, adj=0.716, (0 split)
##       tax     < 567.5    to the right, agree=0.857, adj=0.662, (0 split)
##       nox     < 0.657    to the right, agree=0.760, adj=0.432, (0 split)
##       dis     < 2.202    to the left,  agree=0.737, adj=0.378, (0 split)
##       ptratio < 20.15    to the right, agree=0.720, adj=0.338, (0 split)
## 
## Node number 5: 255 observations,    complexity param=0.03616428
##   mean=23.3498, MSE=26.0087 
##   left son=10 (248 obs) right son=11 (7 obs)
##   Primary splits:
##       dis   < 1.5511   to the right, improve=0.23292420, (0 missing)
##       lstat < 4.91     to the right, improve=0.22084090, (0 missing)
##       rm    < 6.543    to the left,  improve=0.21720990, (0 missing)
##       crim  < 4.866945 to the left,  improve=0.06629933, (0 missing)
##       chas  < 0.5      to the left,  improve=0.06223827, (0 missing)
##   Surrogate splits:
##       crim < 8.053285 to the left,  agree=0.984, adj=0.429, (0 split)
## 
## Node number 6: 46 observations,    complexity param=0.01585116
##   mean=32.11304, MSE=41.29592 
##   left son=12 (7 obs) right son=13 (39 obs)
##   Primary splits:
##       lstat   < 9.65     to the right, improve=0.3564426, (0 missing)
##       ptratio < 19.7     to the right, improve=0.2481412, (0 missing)
##       rad     < 7.5      to the right, improve=0.1793089, (0 missing)
##       nox     < 0.639    to the right, improve=0.1663927, (0 missing)
##       indus   < 9.5      to the right, improve=0.1521488, (0 missing)
##   Surrogate splits:
##       crim  < 0.724605 to the right, agree=0.913, adj=0.429, (0 split)
##       nox   < 0.659    to the right, agree=0.913, adj=0.429, (0 split)
##       rad   < 16       to the right, agree=0.891, adj=0.286, (0 split)
##       tax   < 534.5    to the right, agree=0.891, adj=0.286, (0 split)
##       indus < 15.015   to the right, agree=0.870, adj=0.143, (0 split)
## 
## Node number 7: 30 observations
##   mean=45.09667, MSE=36.62832 
## 
## Node number 8: 74 observations
##   mean=11.97838, MSE=14.6744 
## 
## Node number 9: 101 observations
##   mean=17.13762, MSE=11.39146 
## 
## Node number 10: 248 observations,    complexity param=0.03336923
##   mean=22.93629, MSE=14.75159 
##   left son=20 (193 obs) right son=21 (55 obs)
##   Primary splits:
##       rm      < 6.543    to the left,  improve=0.3896273, (0 missing)
##       lstat   < 7.685    to the right, improve=0.3356012, (0 missing)
##       nox     < 0.5125   to the right, improve=0.1514349, (0 missing)
##       ptratio < 18.35    to the right, improve=0.1212960, (0 missing)
##       indus   < 4.1      to the right, improve=0.1207036, (0 missing)
##   Surrogate splits:
##       lstat < 5.055    to the right, agree=0.839, adj=0.273, (0 split)
##       crim  < 0.017895 to the right, agree=0.794, adj=0.073, (0 split)
##       zn    < 31.5     to the left,  agree=0.790, adj=0.055, (0 split)
##       dis   < 10.648   to the left,  agree=0.782, adj=0.018, (0 split)
## 
## Node number 11: 7 observations
##   mean=38, MSE=204.1457 
## 
## Node number 12: 7 observations
##   mean=23.05714, MSE=61.85673 
## 
## Node number 13: 39 observations
##   mean=33.73846, MSE=20.24391 
## 
## Node number 20: 193 observations
##   mean=21.65648, MSE=8.23738 
## 
## Node number 21: 55 observations
##   mean=27.42727, MSE=11.69398
```

```r
plot(rpb)
text(rpb)
```

![](dbook_pred_boston_files/figure-html/unnamed-chunk-5-1.png) 

```r
#fancyRpartPlot(rpb)
rfb = rf.b(Boston)
summary(rfb)
```

```
##                 Length Class  Mode     
## call              3    -none- call     
## type              1    -none- character
## predicted       506    -none- numeric  
## mse             500    -none- numeric  
## rsq             500    -none- numeric  
## oob.times       506    -none- numeric  
## importance       13    -none- numeric  
## importanceSD      0    -none- NULL     
## localImportance   0    -none- NULL     
## proximity         0    -none- NULL     
## ntree             1    -none- numeric  
## mtry              1    -none- numeric  
## forest           11    -none- list     
## coefs             0    -none- NULL     
## y               506    -none- numeric  
## test              0    -none- NULL     
## inbag             0    -none- NULL     
## terms             3    terms  call
```

```r
#rfb$importance
par(mfrow=c(1,2))
plot(rfb, log="y", main="Error Rate ~ # of Trees")
varImpPlot(rfb, main="Variable Importance")
```

![](dbook_pred_boston_files/figure-html/unnamed-chunk-5-2.png) 

```r
#MDSplot(rfb, Boston$medv)
```



```r
par(mfrow=c(1,3))
plot(Boston$medv, lm1$fitted.values, ylim=c(0,50), xlab="Actual Housing Prices", ylab="Predicted Housing Prices", main="Linear Regression")
lines(loess.smooth(Boston$medv, lm1$fitted.values))
plot(Boston$medv, predict(rpb),  ylim=c(0,50), xlab="Actual Housing Prices", ylab="Predicted Housing Prices", main="Decision Tree")
lines(loess.smooth(Boston$medv, predict(rpb)))
plot(Boston$medv, rfb$predicted,  ylim=c(0,50), xlab="Actual Housing Prices", ylab="Predicted Housing Prices", main="RandomForest")
lines(loess.smooth(Boston$medv, rfb$predicted))
```

![](dbook_pred_boston_files/figure-html/unnamed-chunk-6-1.png) 

```r
rmse(Boston$medv, lm1$fitted.values)
```

```
## [1] 4.679191
```

```r
rmse(Boston$medv, predict(rpb))
```

```
## [1] 4.030468
```

```r
rmse(Boston$medv, rfb$predicted)
```

```
## [1] 3.161456
```



## Variable Selection

Best Subset Selection


```r
vs.full = regsubsets(medv ~ ., Boston, nvmax = 13 )
vss.full = plot.vss(vs.full)
```

![](dbook_pred_boston_files/figure-html/unnamed-chunk-8-1.png) 

```r
coef(vs.full, 11)
```

```
##   (Intercept)          crim            zn          chas           nox 
##  36.341145004  -0.108413345   0.045844929   2.718716303 -17.376023429 
##            rm           dis           rad           tax       ptratio 
##   3.801578840  -1.492711460   0.299608454  -0.011777973  -0.946524570 
##         black         lstat 
##   0.009290845  -0.522553457
```



## Cross-validation


```r
crt1 = cval.model(Boston, lm.b, 5)
crt2 = cval.model(Boston, lm.s, 5)
crt3 = cval.model(Boston, lm.r, 5)
par(mfrow=c(1,3))
plot(crt1$medv, crt1$res)
plot(crt2$medv, crt2$res)
plot(crt3$medv, crt3$res)
```

![](dbook_pred_boston_files/figure-html/unnamed-chunk-9-1.png) 

```r
rmse(crt1$medv, crt1$res)
```

```
## [1] 4.85876
```

```r
rmse(crt2$medv, crt2$res)
```

```
## [1] 4.85596
```

```r
rmse(crt3$medv, crt3$res)
```

```
## [1] 4.885313
```

# Linear Model vs. RandomForest


## Cross-validation

```r
crt1 = cval.model(Boston, lm.b, 3)
crt2 = cval.model(Boston, rpart.b, 3)
crt3 = cval.model(Boston, rf.b, 3)
par(mfrow=c(1,3))
plot(crt1$medv, crt1$res)
plot(crt2$medv, crt2$res)
plot(crt3$medv, crt3$res)
```

![](dbook_pred_boston_files/figure-html/unnamed-chunk-10-1.png) 

```r
rmse(crt1$medv, crt1$res)
```

```
## [1] 4.871832
```

```r
rmse(crt2$medv, crt2$res)
```

```
## [1] 4.539602
```

```r
rmse(crt3$medv, crt3$res)
```

```
## [1] 3.351729
```


```r
params = data.frame(k.fold = c(2, 3, 5, 10))
res = mdply(params, function(k.fold){
	cm1 = cval.model(Boston, lm.b, k.fold)
	cm2 = cval.model(Boston, rpart.b, k.fold)
	cm3 = cval.model(Boston, rf.b, k.fold)
	data.frame(rmse.lm = rmse(cm1$medv, cm1$res), rmse.rp = rmse(cm2$medv, cm2$res), rmse.rf = rmse(cm3$medv, cm3$res))
})
kable(res)
```



 k.fold    rmse.lm    rmse.rp    rmse.rf
-------  ---------  ---------  ---------
      2   4.863574   5.022667   4.063593
      3   5.310622   4.820947   3.518162
      5   4.826905   4.940673   3.278589
     10   4.876840   4.747295   3.160403

## Varying the Amount of Training Data


```r
rt = mdply(data.frame(rep = 1:10), function(rep){
		mdply(data.frame(ratio.train = 1:19/20), function(ratio.train){
		cm1 = eval.model(Boston, 14, lm.b, ratio.train)
		cm2 = eval.model(Boston, 14, rpart.b, ratio.train)
		cm3 = eval.model(Boston, 14, rf.b, ratio.train)
		#browser()
		data.frame(rmse.elm = rmse(cm1$e$medv, cm1$e$res), rmse.erp = rmse(cm2$e$medv, cm2$e$res), rmse.erf = rmse(cm3$e$medv, cm3$e$res),
				   rmse.rlm = rmse(cm1$r$medv, cm1$r$res), rmse.rrp = rmse(cm2$r$medv, cm2$r$res), rmse.rrf = rmse(cm3$r$medv, cm3$r$res))
	})
})
```

```
## Warning in loop_apply(n, do.ply): prediction from a rank-deficient fit may
## be misleading
```

```
## Warning in loop_apply(n, do.ply): prediction from a rank-deficient fit may
## be misleading
```

```
## Warning in loop_apply(n, do.ply): prediction from a rank-deficient fit may
## be misleading
```

```
## Warning in loop_apply(n, do.ply): prediction from a rank-deficient fit may
## be misleading
```

```r
rts = group_by(rt, ratio.train) %>%
	summarise_each(funs(mean))
par(mfrow=c(1,3))
plot(rts$ratio.train, rts$rmse.elm, ylim=c(0,10), lty = 1, 
	 xlab = "% Train Set", ylab = "RMSE", main = "Linear Model") #Train vs. Test Set Errors
lines(rts$ratio.train, rts$rmse.rlm, lty=2)
plot(rts$ratio.train, rts$rmse.erp, ylim=c(0,10), lty = 1, 
	 xlab = "% Train Set", ylab = "RMSE", main = "Decision Tree") #Train vs. Test Set Errors
lines(rts$ratio.train, rts$rmse.rrp, lty=2)
plot(rts$ratio.train, rts$rmse.erf, ylim=c(0,10), lty = 1, 
	 xlab = "% Training Data", ylab = "RMSE", main = "RandomForest") #Train vs. Test Set Errors
lines(rts$ratio.train, rts$rmse.rrf, lty=2)
```

![](dbook_pred_boston_files/figure-html/unnamed-chunk-12-1.png) 


# Deprecated



```r
k.fold = 3
cvt = Boston
cvt$fold = sample(1:k.fold, nrow(cvt), replace=T)
crt = data.frame()
for(i in 1:k.fold){
	cvtr = cvt[cvt$fold != i,]
	cvte = cvt[cvt$fold == i,]
	cvte$res.b = predict(lm.b(cvtr, 14), cvte)
	cvte$res.e = predict(rf.b(cvtr, 14), cvte)
	crt = rbind(crt, cvte) 
}
par(mfrow=c(1,2))
plot(crt$medv, crt$res.b)
plot(crt$medv, crt$res.e)
cor(crt[,c("medv", "res.b", "res.e")])
rmse(crt$medv, crt$res.b)
rmse(crt$medv, crt$res.e)
```



```r
lm.r <- function(dt, idx.tgt){
	res = linearRidge(build.model(colnames(dt)[-idx.tgt], colnames(dt)[idx.tgt]), dt)
	res
}
lm.r(Boston, 14)
rt.b <- function(dt, idx.tgt){
	res = rpart(build.model(colnames(dt)[-idx.tgt], colnames(dt)[idx.tgt]), dt)
	res
}
rt.b(Boston, 14)
```

