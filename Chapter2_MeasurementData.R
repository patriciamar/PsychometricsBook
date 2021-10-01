#-----------------------------------------------------------------
# Chapter 2 - Measurement data
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(ggplot2)
library(moments)
library(mirt)
library(psych)
library(ShinyItemAnalysis)
library(Cairo)
library(tidyverse)
library(psycho)

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

par(mgp = c(2.1, 0.7, 0), mar = c(3.4, 3.4, 1.3, 0.9), cex.axis = 1.2,
    cex.lab = 1.2, ann = FALSE, lwd = 0.6)

#-----------------------------------------------------------------
# 2.2.1. Binary items
#-----------------------------------------------------------------

#--------------
# loading data
data(HCI, package = "ShinyItemAnalysis")

# data dimension (number of rows, columns)
dim(HCI)
## [1] 651  22

# variable names
colnames(HCI)
##  [1] "Item 1"  "Item 2"  "Item 3"  "Item 4"  "Item 5"  "Item 6"
##  [7] "Item 7"  "Item 8"  "Item 9"  "Item 10" "Item 11" "Item 12"
## [13] "Item 13" "Item 14" "Item 15" "Item 16" "Item 17" "Item 18"
## [19] "Item 19" "Item 20" "gender"  "major"

# view data head
head(HCI, n = 3)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## 1      1      1      1      1      1      0      0      1      1
## 2      1      1      1      1      1      1      0      1      1
## 3      1      1      1      1      0      1      0      1      1
## ...
#--------------

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

#--------------
summary(HCI$"Item 6")
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.0000  0.0000  0.0000  0.3625  1.0000  1.0000

summary(HCI[, 6])
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.0000  0.0000  0.0000  0.3625  1.0000  1.0000
#--------------

#-----------------------------------------------------------------
# 2.2.2. Nominal items
#-----------------------------------------------------------------

#--------------
# loading data
data(HCItest, package = "ShinyItemAnalysis")

# view data head
head(HCItest, n = 3)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## 1      D      B      A      D      B      B      B      C      D
## 2      D      B      A      D      B      C      B      C      D
## 3      D      B      A      D      C      C      B      C      D
## ...
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
head(HCIscored, n = 3)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## 1      1      1      1      1      1      0      0      1      1
## 2      1      1      1      1      1      1      0      1      1
## 3      1      1      1      1      0      1      0      1      1
## ...
#--------------

#-----------------------------------------------------------------
# 2.2.3. Ordinal items
#-----------------------------------------------------------------

#--------------
data("BFI2", package = "ShinyItemAnalysis")

head(BFI2, n = 3)
##   i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 
## 1  5  5  2  3  5  4  4  3  4   5   5   4   3   4   2   3   5   3 
## 2  4  5  4  3  3  3  5  3  2   4   4   2   4   2   3   4   5   4 
## 3  1  4  3  5  2  1  3  1  5   4   4   1   5   5   2   1   5   5   
##  ...
#--------------

#--------------
table(BFI2$i1)
##  1   2   3   4   5
## 50 264 321 758 340
#--------------

#--------------
BFI2binary <- 1 * as.data.frame(BFI2[, 1:60] >= 3)

head(BFI2binary, n = 3)
##   i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18
## 1  1  1  0  1  1  1  1  1  1   1   1   1   1   1   0   1   1   1
## 2  1  1  1  1  1  1  1  1  0   1   1   0   1   0   1   1   1   1
## 3  0  1  1  1  0  0  1  0  1   1   1   0   1   1   0   0   1   1 
## ...
#--------------

#-----------------------------------------------------------------
# 2.2.4. Continuous items
#-----------------------------------------------------------------

#--------------
data(EPIA, package = "EstCRM")

head(EPIA, n = 2)
##   Item 1 Item 2 Item 3 Item 4 Item 5
## 1     96     36     80     78     79
## 2     42      2      1      1      1
#--------------

#--------------
summary(EPIA$"Item 1")
##   Min.   1st Qu.  Median     Mean  3rd Qu.     Max. 
## 1.0000  40.0000  63.0000  62.8296  89.0000 111.0000
#--------------

#-----------------------------------------------------------------
# 2.3. Total scores
#-----------------------------------------------------------------

#--------------
# total score calculation
HCI$score <- rowSums(HCI[, 1:20])
HCI$score <- apply(HCI[, 1:20], 1, sum)
#--------------

#--------------
# summary of total score
summary(HCI$score)
##   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
## 3.0000  10.0000  12.0000  12.2120  15.0000  20.0000
#--------------

#--------------
c(Min = min(HCI$score), Max = max(HCI$score),
  Mean = mean(HCI$score), Med = median(HCI$score),
  Var = var(HCI$score), SD = sd(HCI$score),
  Skew = moments::skewness(HCI$score),
  Kurt = moments::kurtosis(HCI$score))
##     Min     Max    Mean     Med     Var      SD    Skew    Kurt
##  3.0000 20.0000 12.2120 12.0000 13.2473  3.6397 -0.1982  2.3474

kurtosis(HCI$score) - 3
## [1] -0.6526

psych::describe(HCI$score, type = 1)
##    vars   n  mean   sd median trimmed  mad min max range
## X1    1 651 12.21 3.64     12   12.32 4.45   3  20    17
##    skew kurtosis   se
## X1 -0.2    -0.65 0.14
#--------------

#--------------
# histogram
hist(HCI$score, breaks = seq(3, 20, 1), col = "gold",
     main = "", xlab = "Total score", ylab = "Number of respondents")
#--------------

#--------------
ggplot(data = HCI, aes(score)) +
  geom_histogram(binwidth = 1, fill = "gold", col = "black") +
  xlab("Total score") + ylab("Number of respondents") + 
  scale_y_continuous(breaks = seq(0, 70, 10)) + 
  theme_fig()
#--------------

#-----------------------------------------------------------------
# 2.3.1 Standardized scores
#-----------------------------------------------------------------

#--------------
zscore <- scale(HCI$score) # Z-score
tscore <- 10 * zscore + 50 # T-score
centiles <- 100 * round(ecdf(HCI$score)(HCI$score), 2) # percentiles
success_rate <- 100 * (HCI$score / max(HCI$score)) # success rate

head(data.frame(score = HCI$score, zscore, tscore,
                centiles, success_rate), n = 4)
##   score zscore  tscore centiles success_rate
## 1    16 1.0408 60.4075       87           80
## 2    19 1.8650 68.6500       99           95
## 3    17 1.3155 63.1550       94           85
## 4    20 2.1398 71.3975      100          100
#--------------

#-----------------------------------------------------------------
# 2.4 Covariates
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 2.4.1 Criterion variable
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

#-----------------------------------------------------------------
# 2.4.2 Observed score
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# 2.4.3 Grouping variable
#-----------------------------------------------------------------

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
# 2.5 Wide and long data format
#-----------------------------------------------------------------

#--------------
HCI$person <- as.factor(1:nrow(HCI))
HCI$zscore <- scale(HCI$score)

# converting data to the long format
HCI.long <- reshape(
  data = HCI,
  varying = list(paste("Item", 1:20)), timevar = "item", v.names = "rating",
  idvar = c("person", "gender", "major", "zscore"),
  direction = "long", new.row.names = 1:13020
)

head(HCI.long, n = 3)
##   gender major score person  zscore item rating
## 1      0     1    16      1  1.0408    1      1
## 2      0     1    19      2  1.8650    1      1
## 3      1     1    17      3  1.3155    1      1
# HCI.long$item <- as.factor(HCI.long$item)
#--------------

# tidyverse approach
library(tidyverse)

HCI.long <- pivot_longer(
  data = HCI,
  cols = starts_with("Item"),
  names_to = "item", values_to = "rating"
)


#--------------
HCI.wide <- reshape(
  data = HCI.long,
  v.names = "rating", timevar = "item",
  idvar = c("person", "gender", "major", "zscore"),
  direction = "wide"
)

head(HCI.wide, n = 3)
##   gender major score person zscore rating.1 rating.2 
## 1      0     1    16      1 1.0409        1        1 
## 2      0     1    19      2 1.8650        1        1 
## 3      1     1    17      3 1.3155        1        1 
## ...
#--------------

# tidyverse approach
HCI.wide <- pivot_wider(data = HCI.long, names_from = item, values_from = rating)


#-----------------------------------------------------------------
# 2.6 Random variables
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 2.6.1 Discrete random variables
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
##   [1] 0 0 1 0 0 1 0 1 0 0 1 0 0 1 1 0 0 1 1 1 0 1 0 0 1 1 1
##  [28] 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 0 1 0 0 0 0 0 1 1 1 0
##  [55] 1 0 0 1 1 1 0 0 0 1 0 1 1 0 0 1 1 1 1 1 1 0 1 1 1 1 1
##  [82] 1 1 0 0 1 1 1 1 1 0 1 1 0 0 0 1 1 0 0

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
# [1] 13.9600
20 * 0.7
# [1] 14.0000

sd(score)
## [1] 1.9065
sqrt(20 * 0.7 * (1 - 0.7))
## [1] 2.0494
#--------------

#-----------------------------------------------------------------
# 2.6.2 Continuous random variables
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
ggplot(data = HCI, aes(score)) +
  geom_histogram(aes(y = ..density..), binwidth = 1,
                 col = "black", fill = "gold") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(HCI$score),
                            sd = sd(HCI$score)), size = 0.8) +
  xlab("Total score") + ylab("Density") +
  theme_fig()
#--------------

#--------------
# QQ plot in base 
qqnorm(HCI$score)
qqline(HCI$score)
#--------------

#--------------
# QQ plot in ggplot 
ggplot(HCI, aes(sample = score)) +
  stat_qq(size = 2,
          shape = 1) +
  stat_qq_line() + 
  theme_fig() +
  ylab("Sample Quantiles") + 
  xlab("Theoretical Quantiles") 
#--------------
