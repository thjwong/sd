## originally from:
## subset(effortpropmean_stage1_zz, L1 %in% c("om41"))$value/100

optimalSecPro14 <- read.table(header = TRUE, text = "
position value
1 0.03242787
2 0.06512226
3 0.09809715
4 0.13136792
5 0.16495158
6 0.19886700
7 0.23313530
8 0.26778017
9 0.30282839
10 0.33831049
11 0.37426150
12 0.41072204
13 0.44773969
14 0.48537088
15 0.52368351
16 0.56276067
17 0.60270619
18 0.64365311
19 0.68577739
20 0.72932155
21 0.77463897
22 0.82228693
23 0.87325910
24 0.92975823
25 1.00000000
")

## originally from:
## subset(effortpropmean_stage1_zz, L1 %in% c("com41"))$value/100

optimalCayley14 <- read.table(header = TRUE, text = "
position value
1 0.046105
2 0.091782
3 0.137020
4 0.181810
5 0.226130
6 0.269980
7 0.313330
8 0.356180
9 0.398500
10 0.440280
11 0.481500
12 0.522130
13 0.562150
14 0.601540
15 0.640270
16 0.678300
17 0.715610
18 0.752150
19 0.787890
20 0.822780
21 0.856800
22 0.889930
23 0.922260
24 0.954340
25 1.000000
")

plot(optimalCayley14$position,optimalCayley14$value,xlab="position",ylab="percentile")
lines(optimalSecPro14$position,optimalSecPro14$value)

ks.test(optimalSecPro14$value, optimalCayley14$value)
