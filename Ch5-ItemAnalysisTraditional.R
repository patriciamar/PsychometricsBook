#-----------------------------------------------------------------
# Chapter 5 - Traditional item analysis
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(ggplot2)

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
# 5.2   Item difficulty
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 5.2.1 Difficulty in binary items 
#-----------------------------------------------------------------

#--------------
data(HCI, package = "ShinyItemAnalysis")
#--------------

#--------------
# item difficulty (average item score)
sapply(HCI[, 1:20], mean)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8
## 0.6989  0.7527  0.8479  0.4040  0.4424  0.3625  0.5469  0.7051
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16
## 0.4332  0.6482  0.7788  0.5730  0.6083  0.7588  0.4439  0.6068
## Item 17 Item 18 Item 19 Item 20
## 0.2965  0.7972  0.7849  0.7220
#--------------

#--------------
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
# 5.2.2 Difficulty in ordinal items
#-----------------------------------------------------------------

#--------------
data(Anxiety, package = "ShinyItemAnalysis")
mean(Anxiety[, "R25"])
## [1] 2.4047
sd(Anxiety[, "R25"])
## [1] 1.2124
#--------------

#--------------
# accounting for minimum item score of 1, maximum score of 5 points
(mean(Anxiety[, "R25"]) - 1) / (5 - 1)
## [1] 0.3512
#--------------

#--------------
# difficulty of item 1 under different binarizations
mean(Anxiety[, "R25"] >= 5)
# [1] 0.0561
mean(Anxiety[, "R25"] >= 4)
# [1] 0.1919
mean(Anxiety[, "R25"] >= 3)
## [1] 0.4661
mean(Anxiety[, "R25"] >= 2)
## [1] 0.6906
#--------------

#--------------
library(ShinyItemAnalysis)
Anxiety_items <- Anxiety[, paste0("R", 1:29)]
ItemAnalysis(Data = Anxiety_items)[, c("Difficulty", "Mean", "SD", 
                                       "Min.score", "Max.score", 
                                       "Prop.max.score")]
##      Difficulty  Mean      SD Min.score Max.score Prop.max.score
## R1       0.1230 1.4922 0.8303         1         5         0.0078
## ...
## R25      0.3512 2.4047 1.2124         1         5         0.0561
## ...
#--------------

#-----------------------------------------------------------------
# 5.3  Item discrimination
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 5.3.1 Correlation between item and total score (RIT)
#-----------------------------------------------------------------

#--------------
# RIT index by hand
total_score <- rowSums(HCI[, 1:20])
sapply(HCI[, 1:20], function(i) cor(i, total_score))
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8  ...
## 0.4019  0.3320  0.4352  0.3023  0.4005  0.4560  0.2355  0.4350  ...
#--------------

#--------------
# RIT index with ItemAnalysis() function
ItemAnalysis(HCI[, 1:20])$RIT
## [1] 0.4019 0.3320 0.4352 0.3023 0.4005 0.4560 0.2355 0.4350 ...
#--------------

#-----------------------------------------------------------------
# Correlation between item and total score (RIR)
#-----------------------------------------------------------------

#--------------
# RIR index by hand
dataR <- total_score - HCI[, 1:20]
diag(cor(HCI[, 1:20], dataR))
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8  ...
## 0.2884  0.2206  0.3500  0.1730  0.2768  0.3419  0.1009  0.3252  ...
#--------------

#--------------
# RIR index with ItemAnalysis() function
ItemAnalysis(HCI[, 1:20])$RIR
## [1] 0.2884 0.2206 0.3500 0.1730 0.2768 0.3419 0.1009 0.3252 ...
#--------------

#-----------------------------------------------------------------
# 5.3.2 Difference between upper and lower group (ULI)
#-----------------------------------------------------------------

#--------------
gDiscrim(Data = HCI[, 1:20])
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8
## 0.4194  0.3041  0.3272  0.3088  0.4562  0.5069  0.2350  0.4470
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16
## 0.3548  0.4608  0.3318  0.4793  0.5438  0.4378  0.4885  0.5576
## Item 17 Item 18 Item 19 Item 20
## 0.1659  0.4286  0.4147  0.4562
#--------------

#--------------
gDiscrim(Data = HCI[, 1:20], k = 5, l = 4, u = 5)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8
## 0.07692 0.07692 0.01538 0.27692 0.19231 0.15385 0.19231 0.12308
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16
## 0.30000 0.15385 0.06923 0.13846 0.11538 0.03077 0.23077 0.07692
## Item 17 Item 18 Item 19 Item 20
## 0.24615 0.02308 0.05385 0.08462
#--------------

#--------------
# Summary table (not shown in the book)
# ItemAnalysis() with all above indices
ItemAnalysis(Data = HCI[, 1:20], k = 5, l = 4, u = 5)[
  , c("Difficulty", "SD", "ULI", "gULI", "RIT", "RIR")
]
##        Difficulty     SD    ULI    gULI    RIT    RIR
## Item 1     0.6989 0.4591 0.4194 0.07692 0.4019 0.2884
## Item 2     0.7527 0.4318 0.3041 0.07692 0.3320 0.2206
## ...
## Item 20    0.7220 0.4484 0.4562 0.08462 0.4463 0.3396
#--------------

#--------------
DDplot(Data = HCI[, 1:20], discrim = "ULI")
#--------------

#--------------
DDplot(Data = HCI[, 1:20], discrim = "ULI", 
       k = 5, l = 4, u = 5, thr = 0.1)
#--------------

#-----------------------------------------------------------------
# 5.5 Distractor analysis
#-----------------------------------------------------------------

#--------------
data(HCIkey, HCItest, package = "ShinyItemAnalysis")
DistractorAnalysis(Data = HCItest[, 1:20], key = HCIkey, 
                   item = c(3, 17), num.group = 5)
## $`Item 3`
## score.level
## response Group1 Group2 Group3 Group4 Group5
##        A     92    104    156    117     83
##        B     30     11      4      0      0
##        C     30     12     10      2      0
## 
## $`Item 17`
## score.level
## response Group1 Group2 Group3 Group4 Group5
##        A     41     36     62     38     25
##        B     63     31     45     23     12
##        C     36     33     38     44     42
##        D     12     27     25     14      4
#--------------

#--------------
plotDistractorAnalysis(Data = HCItest[, 1:20], key = HCIkey, 
                       num.group = 5, item = c(3, 17), 
                       multiple.answers = TRUE)
#--------------

#-----------------------------------------------------------------
# 5.6 Item reliability
#-----------------------------------------------------------------

#--------------
psych::alpha(x = HCI[, 1:20])$total[1]
#  raw_alpha
#  0.7153
psych::alpha(x = HCI[, 1:20])$alpha.drop[, 1]
##  [1] 0.7042 0.7099 0.7007 0.7151 0.7054 0.6991 0.7220 0.7009 0.7114
## [10] 0.7064 0.7047 0.7016 0.6972 0.7001 0.7030 0.6952 0.7253 0.6943
## [19] 0.6981 0.6997
#--------------

#--------------
ItemAnalysis(Data = HCI[, 1:20])$Alpha.drop
##  [1] 0.7042 0.7099 0.7007 0.7151 0.7054 0.6991 0.7220 0.7009 0.7114
## [10] 0.7064 0.7047 0.7016 0.6972 0.7001 0.7030 0.6952 0.7253 0.6943
## [19] 0.6981 0.6997
#--------------

#--------------
ItemAnalysis(Data = HCI[, 1:20], criterion = HCI$major)[, "Index.rel"]
## [1]  0.1844 0.1432 0.1563 0.1484 0.1989 0.2192 0.1172 0.1984 0.1690
## [10] 0.1835 0.1611 0.2154 0.2319 0.1885 0.2098 0.2413 0.0769 0.2036
## [19] 0.1899 0.1999
#--------------

#-----------------------------------------------------------------
# 5.7 Item validity
#-----------------------------------------------------------------

#--------------
ItemAnalysis(Data = HCI[, 1:20], criterion = HCI$major)$Corr.criterion
##  [1] 0.1173  0.1265  0.1541  0.0832  0.1274  0.0980 -0.0194  0.0949
##  [9] 0.0681  0.0968  0.0782  0.0938  0.0909  0.0664  0.0859  0.0499
## [17] 0.0312  0.1187  0.1295  0.0650
#--------------

#--------------
DDplot(Data = HCI[, 1:20], criterion = HCI$major, thr = NULL)
#--------------

#--------------
plotDistractorAnalysis(Data = HCItest[, 1:20], key = HCIkey, item = 5, 
                       criterion = HCI$major, crit.discrete = TRUE)
#--------------

#-----------------------------------------------------------------
# 5.8 Missed items
#-----------------------------------------------------------------

#--------------
data("HeightInventory")
sum(is.na(HeightInventory[, 1]))
## [1] 9
length(HeightInventory[, 1])
## [1] 4885
100 * sum(is.na(HeightInventory[, 1])) / length(HeightInventory[, 1])
## [1] 0.1842
#--------------

#--------------
ItemAnalysis(Data = HeightInventory[, 1:26])$"Perc.miss"
##  [1] 0.1842 1.4125 0.6141 0.1024 0.1433 0.2047 0.0614 0.2047
##  [9] 0.1638 0.1433 0.0614 0.0819 0.2252 1.5353 0.1842 0.1638
## [17] 0.4504 0.0409 0.1228 0.1024 0.1638 0.1638 0.1638 0.1433
## [25] 0.0819 0.1228
#--------------

#--------------
# Table of missingness in Item 2 vs gender (not shown in book)
table(is.na(HeightInventory[, 2]), HeightInventory[,28])
#--------------

#--------------
ItemAnalysis(Data = HeightInventory[, 1:26])$"Perc.nr"
##  [1] 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
##  [9] 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
## [17] 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
## [25] 0.0000 0.1228
#--------------

#-----------------------------------------------------------------
# 5.9. Item analysis in interactive application
#-----------------------------------------------------------------

startShinyItemAnalysis()



