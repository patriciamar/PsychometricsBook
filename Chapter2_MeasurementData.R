#-----------------------------------------------------------------
# Chapter 2 - Measurement data
# Introduction to psychometric methods
# in education, psychology, and health.
# With examples in R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(CTT)
library(difNLR)
library(ggplot2)
library(moments)
library(ShinyItemAnalysis)

#-----------------------------------------------------------------
# Selected R code
#-----------------------------------------------------------------

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
##  [13] "Item 13" "Item 14" "Item 15" "Item 16" "Item 17" "Item 18"
##  [19] "Item 19" "Item 20" "gender"  "major"

# view data head
head(HCI, n = 3)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## 1      1      1      1      1      1      0      0      1      1
## 2      1      1      1      1      1      1      0      1      1
## 3      1      1      1      1      0      1      0      1      1
## ...
#--------------

#--------------
table(HCI$`Item 1`)
##   0   1
## 196 455
#--------------

#--------------
table(HCI$`Item 1`, HCI$major)
##    0   1
## 0  97  99
## 1 168 287
#--------------

#--------------
prop.table(table(HCI$`Item 1`, HCI$major), margin = 2)
##        0      1
## 0 0.3660 0.2565
## 1 0.6340 0.7435
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
summary(HCI$`Item 6`)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.0000  0.0000  0.0000  0.3625  1.0000  1.0000

summary(HCI[,6])
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.0000  0.0000  0.0000  0.3625  1.0000  1.0000
#--------------

#--------------
by(HCI$`Item 1`, HCI$gender, summary, simplify = TRUE)
## HCI$gender: 0
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.000   0.000   1.000   0.716   1.000   1.000
## ----------------------------------------------------
## HCI$gender: 1
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.0000  0.0000  1.0000  0.6707  1.0000  1.0000
#--------------

#-----------------------------------------------------------------
# 2.2.2. Nominal items
#-----------------------------------------------------------------

#--------------
# loading data
data(HCItest, package = "ShinyItemAnalysis")

# view data head and tail
head(HCItest, n = 3)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## 1      D      B      A      D      B      B      B      C      D
## 2      D      B      A      D      B      C      B      C      D
## 3      D      B      A      D      C      C      B      C      D
## ...
#--------------

#--------------
table(HCItest$`Item 1`)
## 27  59 110 455

table(HCItest$gender, HCItest$`Item 1`)
##     A   B   C   D
## 0  16  34  65 290
## 1  11  25  45 165

prop.table(table(HCItest$gender, HCItest$`Item 1`), margin = 1)
##        A      B      C      D
## 0 0.0395 0.0840 0.1605 0.7160
## 1 0.0447 0.1016 0.1829 0.6707
#--------------

#--------------
data("HCIkey", package = "ShinyItemAnalysis")
unlist(HCIkey)
## key1  key2  key3  key4  key5  key6  key7  key8  key9 key10 key11
##    D     B     A     D     B     C     C     C     D     A     A
## key12 key13 key14 key15 key16 key17 key18 key19 key20
##     D     A     A     C     A     C     C     C     D
## Levels: A B C D

HCIscored <- score(items = HCItest[, 1:20],
                   key = unlist(HCIkey),
                   output.scored = TRUE)
head(HCIscored$scored, n = 3)
##      Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## [1,]      1      1      1      1      1      0      0      1      1
## [2,]      1      1      1      1      1      1      0      1      1
## [3,]      1      1      1      1      0      1      0      1      1
## ...
summary(HCIscored$score)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##  3.0    10.0    12.0    12.2    15.0    20.0
#--------------

#-----------------------------------------------------------------
# 2.2.3. Ordinal items
#-----------------------------------------------------------------


#-----------------------------------------------------------------
# 2.2.4. Continuous items
#-----------------------------------------------------------------


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
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##  3.0    10.0    12.0    12.2    15.0    20.0

c(min(HCI$score), max(HCI$score),
  mean(HCI$score), median(HCI$score),
  var(HCI$score), sd(HCI$score),
  skewness(HCI$score), kurtosis(HCI$score))
## [1] 3.0000 20.0000 12.2120 12.0000 13.2473  3.6397 -0.1982  2.3474
#--------------

#--------------
# histogram
hist(HCI$score, breaks = seq(3, 20, 1), col = "gold",
     main = "", xlab = "Total score", ylab = "Number of respondents")

# ggsave("figures/data_histogram_HCI_base.png",
#       plot = hist(HCI$score, breaks = seq(3, 20, 1), col = "gold",
#                   main = "",
#                   xlab = "HCI Total score", ylab = "Number of respondents"),
#       width = 6, height = 5, dpi = 300, bg = "transparent")

ggplot(data = HCI, aes(score)) +
  geom_histogram(binwidth = 1, col = "black") +
  xlab("Total score") +
  ylab("Number of respondents") +
  theme_app()

# ggsave("figures/data_histogram_HCI_ggplot.png",
#        width = 6, height = 5, dpi = 300, bg = "transparent")
#--------------

#-----------------------------------------------------------------
# 2.3.1 Standardized total scores
#-----------------------------------------------------------------

#--------------
zscore <- scale(HCI$score) # Z-score
tscore <- 10 * zscore + 50 # T-score
percentiles <- 100 * round(ecdf(HCI$score)(HCI$score), 2) # percentiles
success_rate <- 100 * (HCI$score / max(HCI$score)) # success rate

head(data.frame(score = HCI$score, zscore, tscore, percentiles, success_rate))
##   score zscore tscore percentiles success_rate
## 1    16 1.0408 60.408          87           80
## 2    19 1.8650 68.650          99           95
## 3    17 1.3155 63.155          94           85
## 4    20 2.1398 71.398         100          100
## 5    19 1.8650 68.650          99           95
## 6    20 2.1398 71.398         100          100
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

head(HCI.long, n = 2)
##   gender major score person zscore item rating
## 1      0     1    16      1  1.041    1      1
## 2      0     1    19      2  1.865    1      1

HCI.long$item <- as.factor(HCI.long$item)
#--------------

#--------------
HCI.wide <- reshape(
  data = HCI.long,
  v.names = "rating", timevar = "item",
  idvar = c("person", "gender", "major", "zscore"),
  direction = "wide"
)

head(HCI.wide, n = 2)
##   gender major score person zscore rating.1 rating.2
## 1      0     1    16      1  1.041        1        1
## 2      0     1    19      2  1.865        1        1
#--------------
