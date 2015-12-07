# From Excel to R
Jin Kim  
Saturday, May 02, 2015  
# Preparing Environemnt


```r
setwd("c:/src/root/dbook")
source("dbook.R")
load.packages(c("stringr", "ggplot2", "dplyr"))
```

```
## [1] loading stringr
## [1] loading ggplot2
## [1] loading dplyr
```


# Defining Variables


```r
v1 = 1
v2 = "abc"
v3 = c(1,2,3)

df1 = data.frame(
	Name=c("Jerry","Tom","Smith"),
	Math=c(50,60,75))
df1
```

# Reading & Writing Data


```r
#write.table(mtcars, "clipboard")
write.table(mtcars, "mtcars_new.txt")
mcars = read.table("mtcars_new.txt", header=T)
```


# Learning about DataFrame


```r
head(mcars)
```

```
##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

```r
tail(mcars)
```

```
##                 mpg cyl  disp  hp drat    wt qsec vs am gear carb
## Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.7  0  1    5    2
## Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2
## Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.5  0  1    5    4
## Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.5  0  1    5    6
## Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.6  0  1    5    8
## Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.6  1  1    4    2
```

```r
rownames(mcars)
```

```
##  [1] "Mazda RX4"           "Mazda RX4 Wag"       "Datsun 710"         
##  [4] "Hornet 4 Drive"      "Hornet Sportabout"   "Valiant"            
##  [7] "Duster 360"          "Merc 240D"           "Merc 230"           
## [10] "Merc 280"            "Merc 280C"           "Merc 450SE"         
## [13] "Merc 450SL"          "Merc 450SLC"         "Cadillac Fleetwood" 
## [16] "Lincoln Continental" "Chrysler Imperial"   "Fiat 128"           
## [19] "Honda Civic"         "Toyota Corolla"      "Toyota Corona"      
## [22] "Dodge Challenger"    "AMC Javelin"         "Camaro Z28"         
## [25] "Pontiac Firebird"    "Fiat X1-9"           "Porsche 914-2"      
## [28] "Lotus Europa"        "Ford Pantera L"      "Ferrari Dino"       
## [31] "Maserati Bora"       "Volvo 142E"
```

```r
colnames(mcars)
```

```
##  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
## [11] "carb"
```

```r
summary(mcars)
```

```
##       mpg             cyl             disp             hp       
##  Min.   :10.40   Min.   :4.000   Min.   : 71.1   Min.   : 52.0  
##  1st Qu.:15.43   1st Qu.:4.000   1st Qu.:120.8   1st Qu.: 96.5  
##  Median :19.20   Median :6.000   Median :196.3   Median :123.0  
##  Mean   :20.09   Mean   :6.188   Mean   :230.7   Mean   :146.7  
##  3rd Qu.:22.80   3rd Qu.:8.000   3rd Qu.:326.0   3rd Qu.:180.0  
##  Max.   :33.90   Max.   :8.000   Max.   :472.0   Max.   :335.0  
##       drat             wt             qsec             vs        
##  Min.   :2.760   Min.   :1.513   Min.   :14.50   Min.   :0.0000  
##  1st Qu.:3.080   1st Qu.:2.581   1st Qu.:16.89   1st Qu.:0.0000  
##  Median :3.695   Median :3.325   Median :17.71   Median :0.0000  
##  Mean   :3.597   Mean   :3.217   Mean   :17.85   Mean   :0.4375  
##  3rd Qu.:3.920   3rd Qu.:3.610   3rd Qu.:18.90   3rd Qu.:1.0000  
##  Max.   :4.930   Max.   :5.424   Max.   :22.90   Max.   :1.0000  
##        am              gear            carb      
##  Min.   :0.0000   Min.   :3.000   Min.   :1.000  
##  1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:2.000  
##  Median :0.0000   Median :4.000   Median :2.000  
##  Mean   :0.4062   Mean   :3.688   Mean   :2.812  
##  3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:4.000  
##  Max.   :1.0000   Max.   :5.000   Max.   :8.000
```

# Preparing the DataFrame


```r
mcars$model = rownames(mcars)
mcars$maker = word(mcars$model, 1)
rownames(mcars) = NULL
head(mcars)
```

```
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb             model
## 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4         Mazda RX4
## 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4     Mazda RX4 Wag
## 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1        Datsun 710
## 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1    Hornet 4 Drive
## 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2 Hornet Sportabout
## 6 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1           Valiant
##     maker
## 1   Mazda
## 2   Mazda
## 3  Datsun
## 4  Hornet
## 5  Hornet
## 6 Valiant
```

# Summarizing the Data


```r
table(mcars$cyl)
```

```
## 
##  4  6  8 
## 11  7 14
```

```r
table(mcars$gear, mcars$cyl)
```

```
##    
##      4  6  8
##   3  1  2 12
##   4  8  4  0
##   5  2  1  2
```

# Working with DataFrame
## Working with DataFrame (basic)


```r
mcars$cyl == 4
```

```
##  [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE
## [12] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE
## [23] FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE
```

```r
mcars.small = mcars[mcars$cyl == 4, ] # filter rows
mcars.small.narrow = mcars.small[, c("model","maker", "mpg", "cyl")] # select columns by name
mcars.small.narrow = mcars.small[, c(12:13, 1:2)] # select columns by index
mcars.small.narrow
```

```
##             model   maker  mpg cyl
## 3      Datsun 710  Datsun 22.8   4
## 8       Merc 240D    Merc 24.4   4
## 9        Merc 230    Merc 22.8   4
## 18       Fiat 128    Fiat 32.4   4
## 19    Honda Civic   Honda 30.4   4
## 20 Toyota Corolla  Toyota 33.9   4
## 21  Toyota Corona  Toyota 21.5   4
## 26      Fiat X1-9    Fiat 27.3   4
## 27  Porsche 914-2 Porsche 26.0   4
## 28   Lotus Europa   Lotus 30.4   4
## 32     Volvo 142E   Volvo 21.4   4
```

## Working with DataFrame (dplyr)


```r
mcars.small.narrow =               # 결과를 mcars.small.narrow라는 변수에 저장한다.
	mcars %>%                        #  mcars 데이터를 사용한다.
	filter(cyl == 4) %>%             # cyl 값이 4인 데이터만 남긴다.
	select(maker, model, mpg, cyl) # # 주어진 네 속성만 선택한다.
mcars.small.narrow
```

```
##      maker          model  mpg cyl
## 1   Datsun     Datsun 710 22.8   4
## 2     Merc      Merc 240D 24.4   4
## 3     Merc       Merc 230 22.8   4
## 4     Fiat       Fiat 128 32.4   4
## 5    Honda    Honda Civic 30.4   4
## 6   Toyota Toyota Corolla 33.9   4
## 7   Toyota  Toyota Corona 21.5   4
## 8     Fiat      Fiat X1-9 27.3   4
## 9  Porsche  Porsche 914-2 26.0   4
## 10   Lotus   Lotus Europa 30.4   4
## 11   Volvo     Volvo 142E 21.4   4
```

```r
makers = 
	mcars %>% 
	group_by(maker) %>% # 제조사 기준으로 데이터를 집계한다.
	summarize( maker.mpg = mean(mpg), maker.count = n()) # 제조사별 평균 mpg및 차종수
head(makers)
```

```
## Source: local data frame [6 x 3]
## 
##      maker maker.mpg maker.count
##      (chr)     (dbl)       (int)
## 1      AMC      15.2           1
## 2 Cadillac      10.4           1
## 3   Camaro      13.3           1
## 4 Chrysler      14.7           1
## 5   Datsun      22.8           1
## 6    Dodge      15.5           1
```

```r
mcars.makers = merge(mcars, makers, by="maker")
head(mcars.makers)
```

```
##      maker  mpg cyl disp  hp drat    wt  qsec vs am gear carb
## 1      AMC 15.2   8  304 150 3.15 3.435 17.30  0  0    3    2
## 2 Cadillac 10.4   8  472 205 2.93 5.250 17.98  0  0    3    4
## 3   Camaro 13.3   8  350 245 3.73 3.840 15.41  0  0    3    4
## 4 Chrysler 14.7   8  440 230 3.23 5.345 17.42  0  0    3    4
## 5   Datsun 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## 6    Dodge 15.5   8  318 150 2.76 3.520 16.87  0  0    3    2
##                model maker.mpg maker.count
## 1        AMC Javelin      15.2           1
## 2 Cadillac Fleetwood      10.4           1
## 3         Camaro Z28      13.3           1
## 4  Chrysler Imperial      14.7           1
## 5         Datsun 710      22.8           1
## 6   Dodge Challenger      15.5           1
```

# Plotting
## Basic Plotting

```r
hist(mcars$mpg)
```

![](mtcars_files/figure-html/ch2_plot_basic-1.png) 

```r
plot(mcars[,1:5])
```

![](mtcars_files/figure-html/ch2_plot_basic-2.png) 

```r
plot(mcars$wt, mcars$mpg)
```

![](mtcars_files/figure-html/ch2_plot_basic-3.png) 

```r
par(mfrow=c(1,2))
plot(mpg ~ cyl, mcars)
plot(mpg ~ factor(cyl), mcars)
```

![](mtcars_files/figure-html/ch2_plot_basic-4.png) 

## Advanced Plotting

```r
qplot(wt, mpg, data=mcars, color=wt)
```

![](mtcars_files/figure-html/ch2_plot_adv-1.png) 

```r
qplot(wt, mpg, data=mcars, color=factor(cyl))
```

![](mtcars_files/figure-html/ch2_plot_adv-2.png) 

```r
qplot(wt, mpg, data=mcars, geom = c("point", "smooth"))
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](mtcars_files/figure-html/ch2_plot_adv-3.png) 
