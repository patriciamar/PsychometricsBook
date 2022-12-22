#-----------------------------------------------------------------
# Chapter 1 - Introduction
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Plot settings
#-----------------------------------------------------------------

theme_fig <- function(base_size = 17, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_rect(fill = "white", colour = NA),
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_blank(),
      legend.background = element_blank()
    )
}

#-----------------------------------------------------------------
# 1.5 Exploring measurement data
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 1.5.1 Item scores
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# Nominal items
#-----------------------------------------------------------------

#--------------
# loading data
data(HCItest, package = "ShinyItemAnalysis")

# view data head
head(HCItest, n = 2)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9 ...
## 1      D      B      A      D      B      B      B      C      D ...
## 2      D      B      A      D      B      C      B      C      D ...
#--------------

#--------------
table(HCItest$"Item 1")
##  A   B   C   D 
## 27  59 110 455

proportions(table(HCItest$"Item 1"))
prop.table(table(HCItest$"Item 1"))
##      A      B      C      D 
## 0.0415 0.0906 0.1690 0.6989 
#--------------

#--------------
data("HCIkey", package = "ShinyItemAnalysis")
unlist(HCIkey)
##  key1  key2  key3  key4  key5  key6  key7  key8  key9 key10 key11
##     D     B     A     D     B     C     C     C     D     A     A
## key12 key13 key14 key15 key16 key17 key18 key19 key20
##     D     A     A     C     A     C     C     C     D
## Levels: A B C D
#--------------

#--------------
HCIscored <- as.data.frame(mirt::key2binary(HCItest[, 1:20], HCIkey))
head(HCIscored, n = 2)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9 ...
## 1      1      1      1      1      1      0      0      1      1 ...
## 2      1      1      1      1      1      1      0      1      1 ...
#--------------

#--------------
data(HCI, package = "ShinyItemAnalysis")
head(HCI, n = 2)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9 ...
## 1      1      1      1      1      1      0      0      1      1 ...
## 2      1      1      1      1      1      1      0      1      1 ...
#--------------

#-----------------------------------------------------------------
# Binary items
#-----------------------------------------------------------------

#--------------
table(HCI$"Item 1")
##   0   1 
## 196 455 
#--------------

#--------------
proportions(table(HCI$"Item 1"))
prop.table(table(HCI$"Item 1"))
##      0      1
## 0.3011 0.6989
#--------------

#--------------
summary(HCI$"Item 1")
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.0000  0.0000  1.0000  0.6989  1.0000  1.0000 
#--------------

#--------------
# summary statistics for all variables (not shown in the book)
summary(HCI)
##      Item1           Item2            Item3            Item4
## Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000
## 1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:0.000
## Median :1.0000   Median :1.0000   Median :1.0000   Median :0.000
## Mean   :0.6989   Mean   :0.7527   Mean   :0.8479   Mean   :0.404
## 3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.000
## Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.000
## ...
#--------------

#-----------------------------------------------------------------
# Ordinal items
#-----------------------------------------------------------------

#--------------
data("BFI2", package = "ShinyItemAnalysis")
head(BFI2, n = 2)
##   i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 ...
## 1  5  5  2  3  5  4  4  3  4   5   5   4   3   4   2   3   5   3 ...
## 2  4  5  4  3  3  3  5  3  2   4   4   2   4   2   3   4   5   4 ...
#--------------

#--------------
table(BFI2$i1)
##  1   2   3   4   5
## 50 264 321 758 340
#--------------

#--------------
BFI2binary <- 1 * as.data.frame(BFI2[, 1:60] >= 3)
head(BFI2binary, n = 2)
##   i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 ...
## 1  1  1  0  1  1  1  1  1  1   1   1   1   1   1   0   1   1   1 ...
## 2  1  1  1  1  1  1  1  1  0   1   1   0   1   0   1   1   1   1 ...
#--------------

#-----------------------------------------------------------------
# Continuous items
#-----------------------------------------------------------------

#--------------
data(EPIA, package = "ShinyItemAnalysis")
head(EPIA, n = 2)
##   Item 1 Item 2 Item 3 Item 4 Item 5 score
## 1     96     36     80     78     79   369
## 2     42      2      1      1      1    47
#--------------

#--------------
summary(EPIA$"Item 1")
##   Min.   1st Qu.  Median     Mean  3rd Qu.     Max. 
## 1.0000  40.0000  63.0000  62.8296  89.0000 111.0000
#--------------

#-----------------------------------------------------------------
# 1.5.2 Test scores
#-----------------------------------------------------------------

#--------------
HCI$total
# [1] 16 19 17 20 19 20 20 14 18 17 17 16 15 12 17 ...
rowSums(HCI[, 1:20])
# [1] 16 19 17 20 19 20 20 14 18 17 17 16 15 12 17 ...
apply(X = HCI[, 1:20], MARGIN = 1, FUN = sum)
# [1] 16 19 17 20 19 20 20 14 18 17 17 16 15 12 17 ...
#--------------

#--------------
# summary of total score
summary(HCI$total)
##   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
## 3.0000  10.0000  12.0000  12.2120  15.0000  20.0000
#--------------

#--------------
c(Min = min(HCI$total), Max = max(HCI$total), Mean = mean(HCI$total), 
  Med = median(HCI$total), Var = var(HCI$total), SD = sd(HCI$total),
  Skew = moments::skewness(HCI$total), Kurt = moments::kurtosis(HCI$total))
##     Min     Max    Mean     Med     Var      SD    Skew    Kurt
##  3.0000 20.0000 12.2120 12.0000 13.2473  3.6397 -0.1982  2.3474
#--------------

#--------------
psych::describe(HCI$total, type = 1)
##    vars   n  mean   sd median trimmed  mad min max range skew kurtosis   se
## X1    1 651 12.21 3.64     12   12.32 4.45   3  20    17 -0.2    -0.65 0.14
#--------------

#--------------
# obtaining the same value for kurtosis, not shown in the book
moments::kurtosis(HCI$total) - 3
## [1] -0.6526
#--------------

#-----------------------------------------------------------------
# Standardizations and other transformations
#-----------------------------------------------------------------

#--------------
# Z-score
(zscore <- as.vector(scale(HCI$total)))
## [1]  1.0408  1.8650  1.3155  2.1398  1.8650  2.1398  2.1398 ...

# T-score
(tscore <- 10 * zscore + 50)
## [1] 60.4075 68.6500 63.1550 71.3975 68.6500 71.3975 71.3975 ...

# success rate
(success_rate <- 100 * (HCI$total / max(HCI$total)))
## [1]  80  95  85 100  95 100 100 ...
#--------------

#--------------
plot(ecdf(HCI$total), xlab = "HCI score", ylab = "Percentile")
ecdf(HCI$total)(HCI$total)
## [1] 0.8725 0.9923 0.9401 1.0000 0.9923 1.0000 1.0000 ...
(percentiles <- round(100 * ecdf(HCI$total)(HCI$total))) # percentiles
## [1]  87  99  94 100  99 100 100 ...
#--------------

#--------------
# Summarizing the score, its standardizations and transformations
# (code not displayed in the book)
head(data.frame(score = HCI$total, zscore, tscore, percentiles, success_rate), n = 4)
##   score zscore  tscore percentiles success_rate
## 1    16 1.0408 60.4075          87           80
## 2    19 1.8650 68.6500          99           95
## 3    17 1.3155 63.1550          94           85
## 4    20 2.1398 71.3975         100          100
#--------------

#-----------------------------------------------------------------
# 1.5.3 Covariates
#-----------------------------------------------------------------

#--------------
table(HCI$"Item 1", HCI$major)
##    0   1
## 0  97  99
## 1 168 287
#--------------

#--------------
proportions(table(HCI$"Item 1", HCI$major), margin = 2)
prop.table(table(HCI$"Item 1", HCI$major), margin = 2)
##        0      1
## 0 0.3660 0.2565
## 1 0.6340 0.7435
#--------------

#--------------
table(HCItest$gender, HCItest$"Item 1")
##     A   B   C   D
## 0  16  34  65 290
## 1  11  25  45 165
proportions(table(HCItest$gender, HCItest$"Item 1"), margin = 1)
prop.table(table(HCItest$gender, HCItest$"Item 1"), margin = 1)
##        A      B      C      D
## 0 0.0395 0.0840 0.1605 0.7160
## 1 0.0447 0.1016 0.1829 0.6707

by(HCI$"Item 1", HCI$gender, FUN = summary)
## HCI$gender: 0
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.000   0.000   1.000   0.716   1.000   1.000
## ----------------------------------------------------
## HCI$gender: 1
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.0000  0.0000  1.0000  0.6707  1.0000  1.0000
#--------------

#-----------------------------------------------------------------
# 1.6 Modelling measurement data
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 1.6.1 Discrete random variables
#-----------------------------------------------------------------

#--------------
# Bernoulli distribution
dbinom(x = 1, size = 1, prob = 0.3)
## [1] 0.3
dbinom(x = 0, size = 1, prob = 0.3)
## [1] 0.7
#--------------

#--------------
# simulating a Bernoulli trial
set.seed(42)
(item1 <- rbinom(n = 100, size = 1, prob = 0.3))
##  [1] 0 0 0 0 1 1 0 0 0 0 0 1 0 0 0 1 0 0 1 1 0 0 0 0 0 1 0 1 1 1 1 0 0 0 0 0
## [37] 0 0 1 0 0 0 1 0 0 0 1 0 1 1 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0
## [73] 1 0 1 1 0 0 0 1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 1 0 1 1 0 0
# sample mean and variance
mean(item1)
## [1] 0.3400
var(item1)
## [1] 0.2267
#--------------

#--------------
# probability of gaining total score of exact value
dbinom(5, size = 10, prob = 0.5)
## [1] 0.2461
dbinom(10, size = 10, prob = 0.5)
## [1] 0.0010
#--------------

#--------------
# probability of gaining total score at least of given value or in range
pbinom(5, size = 10, prob = 0.5)
## [1] 0.6230
pbinom(10, size = 10, prob = 0.5)
## [1] 1.0000
pbinom(8, size = 10, prob = 0.5) - pbinom(2, size = 10, prob = 0.5)
## [1] 0.9346
#--------------

#--------------
# simulating from a binomial distribution
score <- rbinom(n = 100, size = 20, prob = 0.7)

mean(score)
## [1] 13.9600
20 * 0.7
## [1] 14.0000

sd(score)
## [1] 1.9065
sqrt(20 * 0.7 * (1 - 0.7))
## [1] 2.0494
#--------------

#-----------------------------------------------------------------
# 1.6.2 Continuous random variables
#-----------------------------------------------------------------

#--------------
dnorm(x = 5, mean = 10 * 0.5, sd = sqrt(10 * 0.5 * 0.5))
## [1] 0.2523
dnorm(x = 10, mean = 10 * 0.5, sd = sqrt(10 * 0.5 * 0.5))
## [1] 0.0017

pnorm(q = 5, mean = 10 * 0.5, sd = sqrt(10 * 0.5 * 0.5))
## [1] 0.5000
pnorm(q = 10, mean = 10 * 0.5, sd = sqrt(10 * 0.5 * 0.5))
## [1] 0.9992
#--------------

#--------------
qnorm(0.975)
## [1] 1.9600
qnorm(0.025)
## [1] -1.9600
#--------------

#-------------
# Histogram with an estimated normal density
library(ggplot2)
ggplot(data = HCI, aes(total)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, 
                 col = "black", fill = "gold") +
  stat_function(fun = dnorm, colour = "red", linewidth = 0.8, 
                args = list(mean = mean(HCI$total), sd = sd(HCI$total))) +
  xlab("Total score") + ylab("Density") + theme_fig()
#--------------

#--------------
# QQ plot in base 
qqnorm(HCI$total)
qqline(HCI$total)
#--------------

#--------------
# QQ plot in ggplot 
ggplot(HCI, aes(sample = total)) +
  stat_qq(size = 2, shape = 1) +
  stat_qq_line() + 
  theme_fig() +
  ylab("Sample Quantiles") + 
  xlab("Theoretical Quantiles") 
#--------------

#-----------------------------------------------------------------
# 1.7 ShinyItemAnalysis interactive application
#-----------------------------------------------------------------

ShinyItemAnalysis::run_app()
