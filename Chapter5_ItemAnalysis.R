#-----------------------------------------------------------------
# Chapter 5 - Traditional item analysis
# Introduction to psychometric methods
# in education, psychology, and health.
# With examples in R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(naniar)
library(ShinyItemAnalysis)

#-----------------------------------------------------------------
# 5.2.1  Item difficulty in binary items
#-----------------------------------------------------------------

#--------------
# loading data
data(HCI, package = "ShinyItemAnalysis")

# item difficulty / average score
sapply(HCI[, 1:20], mean)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8
## 0.6989  0.7527  0.8479  0.4040  0.4424  0.3625  0.5469  0.7051
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16
## 0.4332  0.6482  0.7788  0.5730  0.6083  0.7588  0.4439  0.6068
## Item 17 Item 18 Item 19 Item 20
## 0.2965  0.7972  0.7849  0.7220

# item standard deviation
sapply(HCI[, 1:20], sd)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8
## 0.4591  0.4318  0.3594  0.4911  0.4971  0.4811  0.4982  0.4564
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16
## 0.4959  0.4779  0.4154  0.4950  0.4885  0.4281  0.4972  0.4888
## Item 17 Item 18 Item 19 Item 20
## 0.4571  0.4024  0.4112  0.4484
#--------------

#-----------------------------------------------------------------
# 5.2.2  Item difficulty in ordinal items
#-----------------------------------------------------------------

#--------------
data(dataMedicalgraded, package = "ShinyItemAnalysis")

# average score for item 1
mean(dataMedicalgraded[, 1])
## [1] 2.6590

# standard deviation of item 1
sd(dataMedicalgraded[, 1])
## [1] 0.9905

# binarizing item 1
bin_item1 <- as.numeric(dataMedicalgraded[, 1] == 4)
# difficulty of binarized item 1
mean(bin_item1)
## [1] 0.3181

# calculation of difficulty for item 1
# minimum possible score was 0, maximum was 4 points
(mean(dataMedicalgraded[, 1]) - 0) / (4 - 0)
## [1] 0.6647
#--------------

#-----------------------------------------------------------------
# 5.3  Item discrimination
#-----------------------------------------------------------------
# 5.3.1 Correlation between item and total score
#-----------------------------------------------------------------

#--------------
# calculation of total scores
total_score <- rowSums(dataMedicalgraded[, 1:100])

# RIT
sapply(dataMedicalgraded[, 1:5], function(i) cor(i, total_score))
##  X2001  X2002  X2003  X2004  X2005
## 0.1769 0.3012 0.5481 0.3230 0.2854

# RIR
dataR <- total_score - dataMedicalgraded[, 1:5]
diag(cor(dataMedicalgraded[, 1:5], dataR))
## X2001  X2002  X2003  X2004  X2005
## 0.1528 0.2784 0.5297 0.2979 0.2616
#--------------

#-----------------------------------------------------------------
# 5.3.2 Upper-lower index
#-----------------------------------------------------------------

#--------------
gDiscrim(HCI[, 1:20])
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8
## 0.4194  0.3041  0.3272  0.3088  0.4562  0.5069  0.2350  0.4470
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16
## 0.3548  0.4608  0.3318  0.4793  0.5438  0.4378  0.4885  0.5576
## Item 17 Item 18 Item 19 Item 20
## 0.1659  0.4286  0.4147  0.4562

gDiscrim(HCI[, 1:20], k = 5, l = 4, u = 5)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8
## 0.07692 0.07692 0.01538 0.27692 0.19231 0.15385 0.19231 0.12308
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16
## 0.30000 0.15385 0.06923 0.13846 0.11538 0.03077 0.23077 0.07692
## Item 17 Item 18 Item 19 Item 20
## 0.24615 0.02308 0.05385 0.08462
#--------------

#--------------
DDplot(HCI[, 1:20], discrim = "ULI")
DDplot(HCI[, 1:20],
  discrim = "ULI", k = 5, l = 4, u = 5,
  thr = 0.1
)
#--------------

#--------------
# complex item analysis
ItemAnalysis(HCI[, 1:20], k = 5, l = 4, u = 5)[
  ,
  c("diff", "SD", "ULI", "gULI", "RIT", "RIR")
]

##           diff     SD    ULI    gULI    RIT    RIR
## Item 1  0.6989 0.4591 0.4194 0.07692 0.4019 0.2884
## Item 2  0.7527 0.4318 0.3041 0.07692 0.3320 0.2206
## Item 3  0.8479 0.3594 0.3272 0.01538 0.4352 0.3500
## Item 4  0.4040 0.4911 0.3088 0.27692 0.3023 0.1730
## Item 5  0.4424 0.4971 0.4562 0.19231 0.4005 0.2768
## Item 6  0.3625 0.4811 0.5069 0.15385 0.4560 0.3419
## Item 7  0.5469 0.4982 0.2350 0.19231 0.2355 0.1009
## Item 8  0.7051 0.4564 0.4470 0.12308 0.4350 0.3252
## Item 9  0.4332 0.4959 0.3548 0.30000 0.3411 0.2130
## Item 10 0.6482 0.4779 0.4608 0.15385 0.3844 0.2644
## Item 11 0.7788 0.4154 0.3318 0.06923 0.3882 0.2851
## Item 12 0.5730 0.4950 0.4793 0.13846 0.4354 0.3156
## Item 13 0.6083 0.4885 0.5438 0.11538 0.4751 0.3612
## Item 14 0.7588 0.4281 0.4378 0.03077 0.4406 0.3386
## Item 15 0.4439 0.4972 0.4885 0.23077 0.4223 0.3006
## Item 16 0.6068 0.4888 0.5576 0.07692 0.4940 0.3822
## Item 17 0.2965 0.4571 0.1659 0.24615 0.1684 0.0434
## Item 18 0.7972 0.4024 0.4286 0.02308 0.5063 0.4171
## Item 19 0.7849 0.4112 0.4147 0.05385 0.4623 0.3665
## Item 20 0.7220 0.4484 0.4562 0.08462 0.4463 0.3396
#--------------

#-----------------------------------------------------------------
# 5.4 Distractor analysis
#-----------------------------------------------------------------

#--------------
data(HCIkey, HCItest, package = "ShinyItemAnalysis")

DistractorAnalysis(HCItest[, 1:20], key = unlist(HCIkey))$`Item 1`
## score.level
## response Group1 Group2 Group3
##        A     20      7      0
##        B     26     20     13
##        C     71     30      9
##        D    104    171    180
#--------------

#--------------
plotDistractorAnalysis(
  HCItest[, 1:20],
  key = unlist(HCIkey),
  num.group = 5, item = 3,
  multiple.answers = TRUE
)
plotDistractorAnalysis(
  HCItest[, 1:20],
  key = unlist(HCIkey),
  num.group = 5, item = 17,
  multiple.answers = TRUE
)
#--------------

#-----------------------------------------------------------------
# 5.5 Further issues
#-----------------------------------------------------------------
# 5.5.1 Item reliability
#-----------------------------------------------------------------

#--------------
# Cronbach's alpha
psych::alpha(HCI[, 1:20])$total[1]
#  raw_alpha
#  0.715314

# calculation of alpha drop
psych::alpha(HCI[, 1:20])$alpha.drop[, 1]
## [1] 0.7042 0.7099 0.7007 0.7151 0.7054 0.6991 0.7220 0.7009 0.7114
## [10] 0.7064 0.7047 0.7016 0.6972 0.7001 0.7030 0.6952 0.7253 0.6943
## [19] 0.6981 0.6997
#--------------

#--------------
ItemAnalysis(HCI[, 1:20], y = HCI$major)[, c("rel")]
## [1] 0.1844 0.1432 0.1563 0.1484 0.1989 0.2192 0.1172 0.1984 0.1690
## [10] 0.1835 0.1611 0.2154 0.2319 0.1885 0.2098 0.2413 0.0769 0.2036
## [19] 0.1899 0.1999
#--------------

#-----------------------------------------------------------------
# 5.5.2 Item validity
#-----------------------------------------------------------------

#--------------
ItemAnalysis(HCI[, 1:20], y = HCI$major)[, "itemCritCor"]
## [1]  0.1173  0.1265  0.1541  0.0832  0.1274  0.0980 -0.0194  0.0949
## [9]  0.0681  0.0968  0.0782  0.0938  0.0909  0.0664  0.0859  0.0499
## [17]  0.0312  0.1187  0.1295  0.0650
#--------------

#--------------
ItemAnalysis(HCI[, 1:20], y = HCI$major)[, "valInd"]
## [1]  0.0538  0.0546  0.0553  0.0408  0.0633  0.0471 -0.0096  0.0433
## [9]  0.0337  0.0462  0.0325  0.0464  0.0444  0.0284  0.0427  0.0244
## [17]  0.0143  0.0477  0.0532  0.0291
#--------------

#--------------
DDplot(HCI[, 1:20], criterion = HCI$major, thr = NULL, val_type = "simple")
#--------------

#--------------
plotDistractorAnalysis(HCItest[, 1:20],
  key = unlist(HCIkey), item = 5,
  matching = HCI$major, match.discrete = TRUE
)
#--------------

#-----------------------------------------------------------------
# 5.5.2 Missed items
#-----------------------------------------------------------------

#--------------
sum(is.na(HCI[, 1:20])) # no NAs, check original data
sum(is.na(dataMedicalgraded[, 1:100])) # no NAs, need to be moved back
sum(is.na(dataMedical[, 1:100])) # no NAs, need to be moved back

## Simulated missed items:
HCImissed <- HCI
set.seed(4211)
# simulate skipped (missed) and not-reached items in HCI dataset
for (i in 1:150) {
  HCImissed[sample(1:nrow(HCImissed), 1), seq(sample(10:20, 1), 20)] <-
    NA # not-reached (minimum at 10th item, maximum at 20th)
  HCImissed[sample(1:nrow(HCImissed), 1), sample(1:20, 1)] <-
    NA # missed with random location
}
# visualize missingness
vis_miss(HCImissed)

#--------------
# number of missed answers in item 1
sum(is.na(HCImissed[, 1]))
## [1] 7
# number of respondents
length(HCImissed[, 1])
## [1] 651
# percentage of those who did not answer item 1
100 * sum(is.na(HCImissed[, 1])) / length(HCImissed[, 1])
## [1] 1.075

sapply(HCImissed[, 1:20], function(x) {
  100 * sum(is.na(x)) / length(x)
})
ItemAnalysis(HCImissed[, 1:20])$"missedPerc"
## [1]  1.0753  0.6144  0.7680  1.0753  1.0753  0.9217  0.9217
## [8]  1.3825  1.2289  2.7650  5.2227  6.2980  7.8341  9.5238
## [15] 11.2135 13.8249 16.5899 18.5868 19.2012 22.2734
#--------------
# ALL INDICES
ItemAnalysis(HCI[, 1:20], y = HCI$major)[1:5, ]
##          diff avgScore     SD min max obtMin obtMax cutScore   gULI
## Item 1 0.6989   0.6989 0.4591   0   1      0      1        1 0.4194
## Item 2 0.7527   0.7527 0.4318   0   1      0      1        1 0.3041
## Item 3 0.8479   0.8479 0.3594   0   1      0      1        1 0.3272
## Item 4 0.4040   0.4040 0.4911   0   1      0      1        1 0.3088
## Item 5 0.4424   0.4424 0.4971   0   1      0      1        1 0.4562
##           ULI    RIT    RIR itemCritCor  valInd    rel relDrop
## Item 1 0.4194 0.4019 0.2884     0.11734 0.05383 0.1844 0.13231
## Item 2 0.3041 0.3320 0.2206     0.12655 0.05460 0.1432 0.09518
## Item 3 0.3272 0.4352 0.3500     0.15412 0.05534 0.1563 0.12570
## Item 4 0.3088 0.3023 0.1730     0.08321 0.04083 0.1484 0.08488
## Item 5 0.4562 0.4005 0.2768     0.12739 0.06327 0.1989 0.13747
##        alphaDrop missedPerc notReachPerc
## Item 1    0.7042          0            0
## Item 2    0.7099          0            0
## Item 3    0.7007          0            0
## Item 4    0.7151          0            0
## Item 5    0.7054          0            0
