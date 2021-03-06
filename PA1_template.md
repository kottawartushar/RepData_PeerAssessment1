---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity_data <- read.csv("activity.csv")
head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity_data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## What is mean total number of steps taken per day?

```r
sum_steps <- tapply(activity_data$steps, activity_data$date, FUN = sum, na.rm = TRUE)
sum_steps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##          0        126      11352      12116      13294      15420      11015 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##          0      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##      10139      15084      13452      10056      11829      10395       8821 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##      13460       8918       8355       2492       6778      10119      11458 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##       5018       9819      15414          0      10600      10571          0 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##      10439       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336          0         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##       8841       4472      12787      20427      21194      14478      11834 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##      11162      13646      10183       7047          0
```

```r
hist(sum_steps, xlab = "Total no. of steps", ylab = "Frequency", 
     main = "Histogram showing Total no. of steps taken each day", breaks = 30)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dev.copy(png, file = "plot1.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
mean_steps = mean(sum_steps, na.rm = TRUE)
mean_steps
```

```
## [1] 9354.23
```

```r
median_steps = median(sum_steps, na.rm = TRUE)
median_steps
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
average_steps <- aggregate(x = list(total_steps = activity_data$steps),
                           by = list(interval = activity_data$interval), 
                           FUN = mean, na.rm = TRUE)
average_steps
```

```
##     interval total_steps
## 1          0   1.7169811
## 2          5   0.3396226
## 3         10   0.1320755
## 4         15   0.1509434
## 5         20   0.0754717
## 6         25   2.0943396
## 7         30   0.5283019
## 8         35   0.8679245
## 9         40   0.0000000
## 10        45   1.4716981
## 11        50   0.3018868
## 12        55   0.1320755
## 13       100   0.3207547
## 14       105   0.6792453
## 15       110   0.1509434
## 16       115   0.3396226
## 17       120   0.0000000
## 18       125   1.1132075
## 19       130   1.8301887
## 20       135   0.1698113
## 21       140   0.1698113
## 22       145   0.3773585
## 23       150   0.2641509
## 24       155   0.0000000
## 25       200   0.0000000
## 26       205   0.0000000
## 27       210   1.1320755
## 28       215   0.0000000
## 29       220   0.0000000
## 30       225   0.1320755
## 31       230   0.0000000
## 32       235   0.2264151
## 33       240   0.0000000
## 34       245   0.0000000
## 35       250   1.5471698
## 36       255   0.9433962
## 37       300   0.0000000
## 38       305   0.0000000
## 39       310   0.0000000
## 40       315   0.0000000
## 41       320   0.2075472
## 42       325   0.6226415
## 43       330   1.6226415
## 44       335   0.5849057
## 45       340   0.4905660
## 46       345   0.0754717
## 47       350   0.0000000
## 48       355   0.0000000
## 49       400   1.1886792
## 50       405   0.9433962
## 51       410   2.5660377
## 52       415   0.0000000
## 53       420   0.3396226
## 54       425   0.3584906
## 55       430   4.1132075
## 56       435   0.6603774
## 57       440   3.4905660
## 58       445   0.8301887
## 59       450   3.1132075
## 60       455   1.1132075
## 61       500   0.0000000
## 62       505   1.5660377
## 63       510   3.0000000
## 64       515   2.2452830
## 65       520   3.3207547
## 66       525   2.9622642
## 67       530   2.0943396
## 68       535   6.0566038
## 69       540  16.0188679
## 70       545  18.3396226
## 71       550  39.4528302
## 72       555  44.4905660
## 73       600  31.4905660
## 74       605  49.2641509
## 75       610  53.7735849
## 76       615  63.4528302
## 77       620  49.9622642
## 78       625  47.0754717
## 79       630  52.1509434
## 80       635  39.3396226
## 81       640  44.0188679
## 82       645  44.1698113
## 83       650  37.3584906
## 84       655  49.0377358
## 85       700  43.8113208
## 86       705  44.3773585
## 87       710  50.5094340
## 88       715  54.5094340
## 89       720  49.9245283
## 90       725  50.9811321
## 91       730  55.6792453
## 92       735  44.3207547
## 93       740  52.2641509
## 94       745  69.5471698
## 95       750  57.8490566
## 96       755  56.1509434
## 97       800  73.3773585
## 98       805  68.2075472
## 99       810 129.4339623
## 100      815 157.5283019
## 101      820 171.1509434
## 102      825 155.3962264
## 103      830 177.3018868
## 104      835 206.1698113
## 105      840 195.9245283
## 106      845 179.5660377
## 107      850 183.3962264
## 108      855 167.0188679
## 109      900 143.4528302
## 110      905 124.0377358
## 111      910 109.1132075
## 112      915 108.1132075
## 113      920 103.7169811
## 114      925  95.9622642
## 115      930  66.2075472
## 116      935  45.2264151
## 117      940  24.7924528
## 118      945  38.7547170
## 119      950  34.9811321
## 120      955  21.0566038
## 121     1000  40.5660377
## 122     1005  26.9811321
## 123     1010  42.4150943
## 124     1015  52.6603774
## 125     1020  38.9245283
## 126     1025  50.7924528
## 127     1030  44.2830189
## 128     1035  37.4150943
## 129     1040  34.6981132
## 130     1045  28.3396226
## 131     1050  25.0943396
## 132     1055  31.9433962
## 133     1100  31.3584906
## 134     1105  29.6792453
## 135     1110  21.3207547
## 136     1115  25.5471698
## 137     1120  28.3773585
## 138     1125  26.4716981
## 139     1130  33.4339623
## 140     1135  49.9811321
## 141     1140  42.0377358
## 142     1145  44.6037736
## 143     1150  46.0377358
## 144     1155  59.1886792
## 145     1200  63.8679245
## 146     1205  87.6981132
## 147     1210  94.8490566
## 148     1215  92.7735849
## 149     1220  63.3962264
## 150     1225  50.1698113
## 151     1230  54.4716981
## 152     1235  32.4150943
## 153     1240  26.5283019
## 154     1245  37.7358491
## 155     1250  45.0566038
## 156     1255  67.2830189
## 157     1300  42.3396226
## 158     1305  39.8867925
## 159     1310  43.2641509
## 160     1315  40.9811321
## 161     1320  46.2452830
## 162     1325  56.4339623
## 163     1330  42.7547170
## 164     1335  25.1320755
## 165     1340  39.9622642
## 166     1345  53.5471698
## 167     1350  47.3207547
## 168     1355  60.8113208
## 169     1400  55.7547170
## 170     1405  51.9622642
## 171     1410  43.5849057
## 172     1415  48.6981132
## 173     1420  35.4716981
## 174     1425  37.5471698
## 175     1430  41.8490566
## 176     1435  27.5094340
## 177     1440  17.1132075
## 178     1445  26.0754717
## 179     1450  43.6226415
## 180     1455  43.7735849
## 181     1500  30.0188679
## 182     1505  36.0754717
## 183     1510  35.4905660
## 184     1515  38.8490566
## 185     1520  45.9622642
## 186     1525  47.7547170
## 187     1530  48.1320755
## 188     1535  65.3207547
## 189     1540  82.9056604
## 190     1545  98.6603774
## 191     1550 102.1132075
## 192     1555  83.9622642
## 193     1600  62.1320755
## 194     1605  64.1320755
## 195     1610  74.5471698
## 196     1615  63.1698113
## 197     1620  56.9056604
## 198     1625  59.7735849
## 199     1630  43.8679245
## 200     1635  38.5660377
## 201     1640  44.6603774
## 202     1645  45.4528302
## 203     1650  46.2075472
## 204     1655  43.6792453
## 205     1700  46.6226415
## 206     1705  56.3018868
## 207     1710  50.7169811
## 208     1715  61.2264151
## 209     1720  72.7169811
## 210     1725  78.9433962
## 211     1730  68.9433962
## 212     1735  59.6603774
## 213     1740  75.0943396
## 214     1745  56.5094340
## 215     1750  34.7735849
## 216     1755  37.4528302
## 217     1800  40.6792453
## 218     1805  58.0188679
## 219     1810  74.6981132
## 220     1815  85.3207547
## 221     1820  59.2641509
## 222     1825  67.7735849
## 223     1830  77.6981132
## 224     1835  74.2452830
## 225     1840  85.3396226
## 226     1845  99.4528302
## 227     1850  86.5849057
## 228     1855  85.6037736
## 229     1900  84.8679245
## 230     1905  77.8301887
## 231     1910  58.0377358
## 232     1915  53.3584906
## 233     1920  36.3207547
## 234     1925  20.7169811
## 235     1930  27.3962264
## 236     1935  40.0188679
## 237     1940  30.2075472
## 238     1945  25.5471698
## 239     1950  45.6603774
## 240     1955  33.5283019
## 241     2000  19.6226415
## 242     2005  19.0188679
## 243     2010  19.3396226
## 244     2015  33.3396226
## 245     2020  26.8113208
## 246     2025  21.1698113
## 247     2030  27.3018868
## 248     2035  21.3396226
## 249     2040  19.5471698
## 250     2045  21.3207547
## 251     2050  32.3018868
## 252     2055  20.1509434
## 253     2100  15.9433962
## 254     2105  17.2264151
## 255     2110  23.4528302
## 256     2115  19.2452830
## 257     2120  12.4528302
## 258     2125   8.0188679
## 259     2130  14.6603774
## 260     2135  16.3018868
## 261     2140   8.6792453
## 262     2145   7.7924528
## 263     2150   8.1320755
## 264     2155   2.6226415
## 265     2200   1.4528302
## 266     2205   3.6792453
## 267     2210   4.8113208
## 268     2215   8.5094340
## 269     2220   7.0754717
## 270     2225   8.6981132
## 271     2230   9.7547170
## 272     2235   2.2075472
## 273     2240   0.3207547
## 274     2245   0.1132075
## 275     2250   1.6037736
## 276     2255   4.6037736
## 277     2300   3.3018868
## 278     2305   2.8490566
## 279     2310   0.0000000
## 280     2315   0.8301887
## 281     2320   0.9622642
## 282     2325   1.5849057
## 283     2330   2.6037736
## 284     2335   4.6981132
## 285     2340   3.3018868
## 286     2345   0.6415094
## 287     2350   0.2264151
## 288     2355   1.0754717
```

```r
plot(average_steps$interval, average_steps$total_steps, xlab = "5-minute interval", 
     ylab = "Average no. of steps taken" , type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dev.copy(png, file = "plot2.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
max_average <- average_steps[which.max(average_steps$total_steps),]
max_average
```

```
##     interval total_steps
## 104      835    206.1698
```

## Imputing missing values

```r
missing_values <- sum(is.na(activity_data))
missing_values
```

```
## [1] 2304
```

```r
library(imputeTS)
```

```
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
```

```r
library(ggplot2)

replaced_values <- na_mean(activity_data)
head(replaced_values)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

```r
summary(replaced_values)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 37.38   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

```r
#missing_values_2 <- sum(is.na(replaced_values))
#missing_values_2

new_sum_steps <- tapply(replaced_values$steps, replaced_values$date, FUN = sum, na.rm = TRUE)
new_sum_steps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00   11015.00 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##   10766.19   12811.00    9900.00   10304.00   17382.00   12426.00   15098.00 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##   10139.00   15084.00   13452.00   10056.00   11829.00   10395.00    8821.00 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##   13460.00    8918.00    8355.00    2492.00    6778.00   10119.00   11458.00 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##    5018.00    9819.00   15414.00   10766.19   10600.00   10571.00   10766.19 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##   10439.00    8334.00   12883.00    3219.00   10766.19   10766.19   12608.00 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##   10765.00    7336.00   10766.19      41.00    5441.00   14339.00   15110.00 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##    8841.00    4472.00   12787.00   20427.00   21194.00   14478.00   11834.00 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##   11162.00   13646.00   10183.00    7047.00   10766.19
```

```r
qplot(new_sum_steps, xlab = "Total no. of steps taken each day", ylab = "Frequency", 
      main = "Histogram with replaced missing values", binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.copy(png, file = "plot3.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
new_mean_steps = mean(new_sum_steps, na.rm = TRUE)
new_mean_steps
```

```
## [1] 10766.19
```

```r
new_median_steps = median(new_sum_steps, na.rm = TRUE)
new_median_steps
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
replaced_values$date <- as.Date(replaced_values$date)
replaced_values$day <- ifelse(weekdays(replaced_values$date) == "Saturday" | 
                              weekdays(replaced_values$date) == "Sunday", "weekend", "weekday")
head(replaced_values)
```

```
##     steps       date interval     day
## 1 37.3826 2012-10-01        0 weekday
## 2 37.3826 2012-10-01        5 weekday
## 3 37.3826 2012-10-01       10 weekday
## 4 37.3826 2012-10-01       15 weekday
## 5 37.3826 2012-10-01       20 weekday
## 6 37.3826 2012-10-01       25 weekday
```

```r
summary(replaced_values)
```

```
##      steps             date               interval          day           
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 37.38   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

```r
new_average_steps <- aggregate(x = list(total_steps = replaced_values$steps),
                               by = list(days = replaced_values$day,
                                         interval = replaced_values$interval), 
                               FUN = mean, na.rm = TRUE)
new_average_steps
```

```
##        days interval total_steps
## 1   weekday        0    7.006569
## 2   weekend        0    4.672825
## 3   weekday        5    5.384347
## 4   weekend        5    4.672825
## 5   weekday       10    5.139902
## 6   weekend       10    4.672825
## 7   weekday       15    5.162124
## 8   weekend       15    4.672825
## 9   weekday       20    5.073235
## 10  weekend       20    4.672825
## 11  weekday       25    6.295458
## 12  weekend       25    7.922825
## 13  weekday       30    5.606569
## 14  weekend       30    4.672825
## 15  weekday       35    6.006569
## 16  weekend       35    4.672825
## 17  weekday       40    4.984347
## 18  weekend       40    4.672825
## 19  weekday       45    6.584347
## 20  weekend       45    5.047825
## 21  weekday       50    5.339902
## 22  weekend       50    4.672825
## 23  weekday       55    4.984347
## 24  weekend       55    5.110325
## 25  weekday      100    5.362124
## 26  weekend      100    4.672825
## 27  weekday      105    4.984347
## 28  weekend      105    6.922825
## 29  weekday      110    5.162124
## 30  weekend      110    4.672825
## 31  weekday      115    5.384347
## 32  weekend      115    4.672825
## 33  weekday      120    4.984347
## 34  weekend      120    4.672825
## 35  weekday      125    6.295458
## 36  weekend      125    4.672825
## 37  weekday      130    6.962124
## 38  weekend      130    5.172825
## 39  weekday      135    4.984347
## 40  weekend      135    5.235325
## 41  weekday      140    5.184347
## 42  weekend      140    4.672825
## 43  weekday      145    5.184347
## 44  weekend      145    5.360325
## 45  weekday      150    5.295458
## 46  weekend      150    4.672825
## 47  weekday      155    4.984347
## 48  weekend      155    4.672825
## 49  weekday      200    4.984347
## 50  weekend      200    4.672825
## 51  weekday      205    4.984347
## 52  weekend      205    4.672825
## 53  weekday      210    6.228791
## 54  weekend      210    4.922825
## 55  weekday      215    4.984347
## 56  weekend      215    4.672825
## 57  weekday      220    4.984347
## 58  weekend      220    4.672825
## 59  weekday      225    5.139902
## 60  weekend      225    4.672825
## 61  weekday      230    4.984347
## 62  weekend      230    4.672825
## 63  weekday      235    5.251013
## 64  weekend      235    4.672825
## 65  weekday      240    4.984347
## 66  weekend      240    4.672825
## 67  weekday      245    4.984347
## 68  weekend      245    4.672825
## 69  weekday      250    6.806569
## 70  weekend      250    4.672825
## 71  weekday      255    6.095458
## 72  weekend      255    4.672825
## 73  weekday      300    4.984347
## 74  weekend      300    4.672825
## 75  weekday      305    4.984347
## 76  weekend      305    4.672825
## 77  weekday      310    4.984347
## 78  weekend      310    4.672825
## 79  weekday      315    4.984347
## 80  weekend      315    4.672825
## 81  weekday      320    4.984347
## 82  weekend      320    5.360325
## 83  weekday      325    5.717680
## 84  weekend      325    4.672825
## 85  weekday      330    6.006569
## 86  weekend      330    7.172825
## 87  weekday      335    5.428791
## 88  weekend      335    5.360325
## 89  weekday      340    5.339902
## 90  weekend      340    5.297825
## 91  weekday      345    5.073235
## 92  weekend      345    4.672825
## 93  weekday      350    4.984347
## 94  weekend      350    4.672825
## 95  weekday      355    4.984347
## 96  weekend      355    4.672825
## 97  weekday      400    5.095458
## 98  weekend      400    8.297825
## 99  weekday      405    6.095458
## 100 weekend      405    4.672825
## 101 weekday      410    6.873235
## 102 weekend      410    7.860325
## 103 weekday      415    4.984347
## 104 weekend      415    4.672825
## 105 weekday      420    5.384347
## 106 weekend      420    4.672825
## 107 weekday      425    4.984347
## 108 weekend      425    5.860325
## 109 weekday      430    7.806569
## 110 weekend      430   10.360325
## 111 weekday      435    5.117680
## 112 weekend      435    6.485325
## 113 weekday      440    8.295458
## 114 weekend      440    6.922825
## 115 weekday      445    5.762124
## 116 weekend      445    5.235325
## 117 weekday      450    6.917680
## 118 weekend      450    9.547825
## 119 weekday      455    5.562124
## 120 weekend      455    6.735325
## 121 weekday      500    4.984347
## 122 weekend      500    4.672825
## 123 weekday      505    6.828791
## 124 weekend      505    4.672825
## 125 weekday      510    8.517680
## 126 weekend      510    4.672825
## 127 weekday      515    6.873235
## 128 weekend      515    6.797825
## 129 weekday      520    8.762124
## 130 weekend      520    5.047825
## 131 weekday      525    7.295458
## 132 weekend      525    7.985325
## 133 weekday      530    7.451013
## 134 weekend      530    4.672825
## 135 weekday      535   12.117680
## 136 weekend      535    4.672825
## 137 weekday      540   23.251013
## 138 weekend      540    6.360325
## 139 weekday      545   26.184347
## 140 weekend      545    5.797825
## 141 weekday      550   50.073235
## 142 weekend      550    8.547825
## 143 weekday      555   55.317680
## 144 weekend      555   10.485325
## 145 weekday      600   42.073235
## 146 weekend      600    4.672825
## 147 weekday      605   63.006569
## 148 weekend      605    4.672825
## 149 weekday      610   67.895458
## 150 weekend      610    5.860325
## 151 weekday      615   73.673235
## 152 weekend      615   21.672825
## 153 weekday      620   62.251013
## 154 weekend      620    9.110325
## 155 weekday      625   58.739902
## 156 weekend      625    9.422825
## 157 weekday      630   64.473235
## 158 weekend      630   10.110325
## 159 weekday      635   47.717680
## 160 weekend      635   14.797825
## 161 weekday      640   54.784347
## 162 weekend      640   10.422825
## 163 weekday      645   53.962124
## 164 weekend      645   13.235325
## 165 weekday      650   47.051013
## 166 weekend      650   10.110325
## 167 weekday      655   58.873235
## 168 weekend      655   15.547825
## 169 weekday      700   49.739902
## 170 weekend      700   23.922825
## 171 weekday      705   49.895458
## 172 weekend      705   25.360325
## 173 weekday      710   60.273235
## 174 weekend      710   16.485325
## 175 weekday      715   67.051013
## 176 weekend      715   10.672825
## 177 weekday      720   61.428791
## 178 weekend      720   11.297825
## 179 weekday      725   57.295458
## 180 weekend      725   26.422825
## 181 weekday      730   63.784347
## 182 weekend      730   23.735325
## 183 weekday      735   53.428791
## 184 weekend      735   15.235325
## 185 weekday      740   60.739902
## 186 weekend      740   20.985325
## 187 weekday      745   79.095458
## 188 weekend      745   26.610325
## 189 weekday      750   65.006569
## 190 weekend      750   27.485325
## 191 weekday      755   64.073235
## 192 weekend      755   24.485325
## 193 weekday      800   77.917680
## 194 weekend      800   42.610325
## 195 weekday      805   67.851013
## 196 weekend      805   53.797825
## 197 weekday      810  131.739902
## 198 weekend      810   76.922825
## 199 weekday      815  165.962124
## 200 weekend      815   73.735325
## 201 weekday      820  182.739902
## 202 weekend      820   71.672825
## 203 weekday      825  167.873235
## 204 weekend      825   61.297825
## 205 weekday      830  180.228791
## 206 weekend      830   99.110325
## 207 weekday      835  207.873235
## 208 weekend      835  116.985325
## 209 weekday      840  197.762124
## 210 weekend      840  111.485325
## 211 weekday      845  166.695458
## 212 weekend      845  144.672825
## 213 weekday      850  171.762124
## 214 weekend      850  143.110325
## 215 weekday      855  159.806569
## 216 weekend      855  122.485325
## 217 weekday      900  153.517680
## 218 weekend      900   62.110325
## 219 weekday      905  114.228791
## 220 weekend      905  108.297825
## 221 weekday      910   84.384347
## 222 weekend      910  142.797825
## 223 weekday      915   77.873235
## 224 weekend      915  157.797825
## 225 weekday      920   94.695458
## 226 weekend      920   95.922825
## 227 weekday      925   84.651013
## 228 weekend      925   98.485325
## 229 weekday      930   54.673235
## 230 weekend      930   84.235325
## 231 weekday      935   34.806569
## 232 weekend      935   70.610325
## 233 weekday      940   29.139902
## 234 weekend      940   18.860325
## 235 weekday      945   40.673235
## 236 weekend      945   32.672825
## 237 weekday      950   39.451013
## 238 weekend      950   23.610325
## 239 weekday      955   19.806569
## 240 weekend      955   32.735325
## 241 weekday     1000   37.451013
## 242 weekend     1000   47.735325
## 243 weekday     1005   19.606569
## 244 weekend     1005   52.922825
## 245 weekday     1010   38.406569
## 246 weekend     1010   51.172825
## 247 weekday     1015   45.784347
## 248 weekend     1015   64.360325
## 249 weekday     1020   30.139902
## 250 weekend     1020   62.860325
## 251 weekday     1025   33.362124
## 252 weekend     1025   93.110325
## 253 weekday     1030   32.206569
## 254 weekend     1030   74.797825
## 255 weekday     1035   24.251013
## 256 weekend     1035   74.422825
## 257 weekday     1040   23.873235
## 258 weekend     1040   66.485325
## 259 weekday     1045   27.117680
## 260 weekend     1045   36.297825
## 261 weekday     1050   23.673235
## 262 weekend     1050   35.235325
## 263 weekday     1055   23.984347
## 264 weekend     1055   57.047825
## 265 weekday     1100   22.495458
## 266 weekend     1100   59.297825
## 267 weekday     1105   26.117680
## 268 weekend     1105   43.547825
## 269 weekday     1110   13.828791
## 270 weekend     1110   50.422825
## 271 weekday     1115   17.851013
## 272 weekend     1115   53.110325
## 273 weekday     1120   25.384347
## 274 weekend     1120   41.297825
## 275 weekday     1125   25.184347
## 276 weekend     1125   35.547825
## 277 weekday     1130   33.295458
## 278 weekend     1130   35.797825
## 279 weekday     1135   48.517680
## 280 weekend     1135   47.797825
## 281 weekday     1140   43.939902
## 282 weekend     1140   34.360325
## 283 weekday     1145   46.962124
## 284 weekend     1145   34.360325
## 285 weekday     1150   48.962124
## 286 weekend     1150   33.485325
## 287 weekday     1155   53.228791
## 288 weekend     1155   65.047825
## 289 weekday     1200   52.184347
## 290 weekend     1200   83.485325
## 291 weekday     1205   66.139902
## 292 weekend     1205  123.172825
## 293 weekday     1210   75.984347
## 294 weekend     1210  119.172825
## 295 weekday     1215   67.895458
## 296 weekend     1215  135.047825
## 297 weekday     1220   45.251013
## 298 weekend     1220  101.422825
## 299 weekday     1225   45.117680
## 300 weekend     1225   57.985325
## 301 weekday     1230   60.295458
## 302 weekend     1230   29.547825
## 303 weekday     1235   31.406569
## 304 weekend     1235   37.735325
## 305 weekday     1240   23.428791
## 306 weekend     1240   40.672825
## 307 weekday     1245   29.273235
## 308 weekend     1245   61.360325
## 309 weekday     1250   31.762124
## 310 weekend     1250   78.610325
## 311 weekday     1255   52.606569
## 312 weekend     1255   93.610325
## 313 weekday     1300   23.939902
## 314 weekend     1300   91.610325
## 315 weekday     1305   25.406569
## 316 weekend     1305   79.360325
## 317 weekday     1310   23.784347
## 318 weekend     1310   95.110325
## 319 weekday     1315   15.162124
## 320 weekend     1315  111.797825
## 321 weekday     1320   34.451013
## 322 weekend     1320   74.985325
## 323 weekday     1325   42.317680
## 324 weekend     1325   86.610325
## 325 weekday     1330   31.051013
## 326 weekend     1330   72.985325
## 327 weekday     1335   24.939902
## 328 weekend     1335   31.797825
## 329 weekday     1340   24.895458
## 330 weekend     1340   81.047825
## 331 weekday     1345   38.028791
## 332 weekend     1345   89.110325
## 333 weekday     1350   24.251013
## 334 weekend     1350  107.235325
## 335 weekday     1355   33.206569
## 336 weekend     1355  126.735325
## 337 weekday     1400   44.473235
## 338 weekend     1400   78.297825
## 339 weekday     1405   37.606569
## 340 weekend     1405   85.047825
## 341 weekday     1410   31.295458
## 342 weekend     1410   75.047825
## 343 weekday     1415   43.539902
## 344 weekend     1415   57.547825
## 345 weekday     1420   27.739902
## 346 weekend     1420   58.172825
## 347 weekday     1425   30.739902
## 348 weekend     1425   56.610325
## 349 weekday     1430   30.895458
## 350 weekend     1430   70.422825
## 351 weekday     1435   15.828791
## 352 weekend     1435   65.297825
## 353 weekday     1440   14.251013
## 354 weekend     1440   35.297825
## 355 weekday     1445   23.495458
## 356 weekend     1445   38.985325
## 357 weekday     1450   41.028791
## 358 weekend     1450   47.797825
## 359 weekday     1455   37.428791
## 360 weekend     1455   58.422825
## 361 weekday     1500   31.851013
## 362 weekend     1500   28.547825
## 363 weekday     1505   35.228791
## 364 weekend     1505   39.110325
## 365 weekday     1510   30.206569
## 366 weekend     1510   51.297825
## 367 weekday     1515   31.717680
## 368 weekend     1515   58.172825
## 369 weekday     1520   38.717680
## 370 weekend     1520   62.047825
## 371 weekday     1525   35.962124
## 372 weekend     1525   75.735325
## 373 weekday     1530   40.695458
## 374 weekend     1530   63.672825
## 375 weekday     1535   47.206569
## 376 weekend     1535  102.297825
## 377 weekday     1540   84.495458
## 378 weekend     1540   55.672825
## 379 weekday     1545   87.695458
## 380 weekend     1545   98.860325
## 381 weekday     1550   85.317680
## 382 weekend     1550  116.985325
## 383 weekday     1555   64.095458
## 384 weekend     1555  116.547825
## 385 weekday     1600   43.584347
## 386 weekend     1600  101.922825
## 387 weekday     1605   41.628791
## 388 weekend     1605  114.047825
## 389 weekday     1610   51.651013
## 390 weekend     1610  120.360325
## 391 weekday     1615   32.695458
## 392 weekend     1615  135.985325
## 393 weekday     1620   24.206569
## 394 weekend     1620  139.110325
## 395 weekday     1625   26.539902
## 396 weekend     1625  142.047825
## 397 weekday     1630   21.651013
## 398 weekend     1630  103.110325
## 399 weekday     1635   21.673235
## 400 weekend     1635   85.485325
## 401 weekday     1640   24.895458
## 402 weekend     1640   96.610325
## 403 weekday     1645   30.917680
## 404 weekend     1645   82.297825
## 405 weekday     1650   26.451013
## 406 weekend     1650   97.360325
## 407 weekday     1655   31.584347
## 408 weekend     1655   74.547825
## 409 weekday     1700   22.339902
## 410 weekend     1700  110.297825
## 411 weekday     1705   42.428791
## 412 weekend     1705   85.860325
## 413 weekday     1710   32.406569
## 414 weekend     1710   95.547825
## 415 weekday     1715   44.895458
## 416 weekend     1715   95.235325
## 417 weekday     1720   55.406569
## 418 weekend     1720  103.735325
## 419 weekday     1725   66.828791
## 420 weekend     1725   92.235325
## 421 weekday     1730   51.939902
## 422 weekend     1730  100.985325
## 423 weekday     1735   62.851013
## 424 weekend     1735   39.547825
## 425 weekday     1740   77.851013
## 426 weekend     1740   48.485325
## 427 weekday     1745   56.784347
## 428 weekend     1745   46.172825
## 429 weekday     1750   34.851013
## 430 weekend     1750   35.860325
## 431 weekday     1755   37.584347
## 432 weekend     1755   37.047825
## 433 weekday     1800   26.206569
## 434 weekend     1800   79.735325
## 435 weekday     1805   43.873235
## 436 weekend     1805   87.485325
## 437 weekday     1810   62.251013
## 438 weekend     1810   91.047825
## 439 weekday     1815   76.251013
## 440 weekend     1815   86.860325
## 441 weekday     1820   58.473235
## 442 weekend     1820   50.547825
## 443 weekday     1825   69.406569
## 444 weekend     1825   47.985325
## 445 weekday     1830   73.851013
## 446 weekend     1830   68.360325
## 447 weekday     1835   76.584347
## 448 weekend     1835   49.235325
## 449 weekday     1840   85.317680
## 450 weekend     1840   61.422825
## 451 weekday     1845  107.184347
## 452 weekend     1845   46.672825
## 453 weekday     1850   94.739902
## 454 weekend     1850   39.047825
## 455 weekday     1855   84.162124
## 456 weekend     1855   65.547825
## 457 weekday     1900   81.228791
## 458 weekend     1900   71.360325
## 459 weekday     1905   71.828791
## 460 weekend     1905   74.485325
## 461 weekday     1910   59.628791
## 462 weekend     1910   43.235325
## 463 weekday     1915   52.251013
## 464 weekend     1915   48.485325
## 465 weekday     1920   38.028791
## 466 weekend     1920   32.047825
## 467 weekday     1925   22.784347
## 468 weekend     1925   23.235325
## 469 weekday     1930   30.428791
## 470 weekend     1930   23.860325
## 471 weekday     1935   45.628791
## 472 weekend     1935   22.922825
## 473 weekday     1940   31.006569
## 474 weekend     1940   31.547825
## 475 weekday     1945   20.162124
## 476 weekend     1945   46.610325
## 477 weekday     1950   43.162124
## 478 weekend     1950   48.547825
## 479 weekday     1955   27.806569
## 480 weekend     1955   51.547825
## 481 weekday     2000   15.762124
## 482 weekend     2000   39.360325
## 483 weekday     2005    8.006569
## 484 weekend     2005   59.172825
## 485 weekday     2010    9.228791
## 486 weekend     2010   56.797825
## 487 weekday     2015   14.651013
## 488 weekend     2015   87.922825
## 489 weekday     2020   10.117680
## 490 weekend     2020   79.047825
## 491 weekday     2025    7.873235
## 492 weekend     2025   66.672825
## 493 weekday     2030   11.117680
## 494 weekend     2030   77.860325
## 495 weekday     2035    9.295458
## 496 weekend     2035   63.235325
## 497 weekday     2040   11.339902
## 498 weekend     2040   51.547825
## 499 weekday     2045   15.251013
## 500 weekend     2045   46.422825
## 501 weekday     2050   26.651013
## 502 weekend     2050   50.735325
## 503 weekday     2055   19.606569
## 504 weekend     2055   30.297825
## 505 weekday     2100   14.228791
## 506 weekend     2100   31.485325
## 507 weekday     2105   21.584347
## 508 weekend     2105   15.047825
## 509 weekday     2110   30.362124
## 510 weekend     2110   10.985325
## 511 weekday     2115   21.362124
## 512 weekend     2115   22.360325
## 513 weekday     2120   17.606569
## 514 weekend     2120   10.422825
## 515 weekday     2125   11.962124
## 516 weekend     2125   11.610325
## 517 weekday     2130   15.828791
## 518 weekend     2130   22.735325
## 519 weekday     2135   19.317680
## 520 weekend     2135   18.360325
## 521 weekday     2140   10.962124
## 522 weekend     2140   16.610325
## 523 weekday     2145   11.539902
## 524 weekend     2145   12.047825
## 525 weekday     2150   12.162124
## 526 weekend     2150   11.422825
## 527 weekday     2155    8.073235
## 528 weekend     2155    4.672825
## 529 weekday     2200    6.317680
## 530 weekend     2200    5.735325
## 531 weekday     2205    8.917680
## 532 weekend     2205    5.797825
## 533 weekday     2210   10.651013
## 534 weekend     2210    4.672825
## 535 weekday     2215   15.006569
## 536 weekend     2215    4.672825
## 537 weekday     2220   13.317680
## 538 weekend     2220    4.672825
## 539 weekday     2225   14.673235
## 540 weekend     2225    6.235325
## 541 weekday     2230   16.473235
## 542 weekend     2230    4.672825
## 543 weekday     2235    7.584347
## 544 weekend     2235    4.672825
## 545 weekday     2240    4.984347
## 546 weekend     2240    5.735325
## 547 weekday     2245    5.117680
## 548 weekend     2245    4.672825
## 549 weekday     2250    6.673235
## 550 weekend     2250    5.235325
## 551 weekday     2255    6.384347
## 552 weekend     2255   15.985325
## 553 weekday     2300    8.095458
## 554 weekend     2300    6.860325
## 555 weekday     2305    8.339902
## 556 weekend     2305    4.672825
## 557 weekday     2310    4.984347
## 558 weekend     2310    4.672825
## 559 weekday     2315    5.962124
## 560 weekend     2315    4.672825
## 561 weekday     2320    6.117680
## 562 weekend     2320    4.672825
## 563 weekday     2325    6.651013
## 564 weekend     2325    5.235325
## 565 weekday     2330    7.673235
## 566 weekend     2330    5.735325
## 567 weekday     2335    6.606569
## 568 weekend     2335   15.672825
## 569 weekday     2340    6.784347
## 570 weekend     2340   10.547825
## 571 weekday     2345    5.162124
## 572 weekend     2345    6.297825
## 573 weekday     2350    5.251013
## 574 weekend     2350    4.672825
## 575 weekday     2355    6.251013
## 576 weekend     2355    4.672825
```

```r
ggplot(new_average_steps, aes(x = interval, y = total_steps, color = days)) + geom_line() +
facet_grid(days ~.) + xlab("Average no. of steps taken") + ylab("5-minute interval") +
ggtitle("Activity pattern comparision between weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
dev.copy(png, file = "plot4.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
