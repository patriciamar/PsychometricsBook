#-----------------------------------------------------------------
# Chapter 4 - Traditional item analysis
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(ggplot2)
library(naniar)
library(ShinyItemAnalysis)

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
# 4.2   Item difficulty
#-----------------------------------------------------------------
# 4.2.1 Difficulty in binary items 
#-----------------------------------------------------------------

#--------------
# loading data
data(HCI, package = "ShinyItemAnalysis")

# item difficulty (average item score)
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
# 4.2.2 Difficulty in ordinal items
#-----------------------------------------------------------------

#--------------
data(dataMedicalgraded, package = "ShinyItemAnalysis")

# average score for item 1
mean(dataMedicalgraded[, 1])
## [1] 2.6590

# standard deviation of item 1
sd(dataMedicalgraded[, 1])
## [1] 0.9905
#--------------

#--------------
# accounting for minimum item score of 0, maximum score of 4 points
(mean(dataMedicalgraded[, 1]) - 0) / (4 - 0)
## [1] 0.6647
ItemAnalysis(Data = dataMedicalgraded[, 1:100])
##       Difficulty  Mean     SD Prop.max.score Min.score Max.score Obs.min
## X2001     0.6647 2.659 0.9905         0.3181         0         4       0
## X2002     0.5348 2.139 0.9911         0.0874         0         4       0
## X2003     0.7477 2.991 1.0298         0.3880         0         4       0
## ...
#--------------

#--------------
ItemAnalysis(Data = dataMedicalgraded[,1:100], 
             maxscore = 4, minscore = 0, cutscore = 2, bin = TRUE)
##       Difficulty  Mean     SD SD.bin Prop.max.score Min.score Max.score Obs.min Obs.max Cut.score
## X2001     0.6647 2.659 0.9905 0.4964         0.3181         0         4       0       4         4
## ... 
#--------------

#--------------
# difficulty of item 1 under different binarizations
mean(dataMedicalgraded[, 1] >= 4)
# [1] 0.3181
mean(dataMedicalgraded[, 1] >= 3)
## [1] 0.3934
mean(dataMedicalgraded[, 1] >= 2)
## [1] 0.9544
mean(dataMedicalgraded[, 1] >= 1)
## [1] 0.9929
#--------------

#--------------
ItemAnalysis(Data = dataMedicalgraded[, 1:100], 
             maxscore = 4, minscore = 0, cutscore = 2, bin = TRUE)
##       Difficulty  Mean     SD SD.bin Prop.max.score Min.score Max.score Obs.min Obs.max Cut.score
## X2001     0.6647 2.659 0.9905 0.4964         0.3181         0         4       0       4         2
## ... 
#--------------


#-----------------------------------------------------------------
# 4.3  Item discrimination
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 4.3.1 Correlation between item and total score (RIT)
#-----------------------------------------------------------------

#--------------
# RIT index with ItemAnalysis() function
ItemAnalysis(HCI[, 1:20])$RIT
## [1] 0.4019 0.3320 0.4352 0.3023 0.4005 0.4560 0.2355 0.4350 ...

# RIT index by hand
total_score <- rowSums(HCI[, 1:20])
sapply(HCI[, 1:20], function(i) cor(i, total_score))
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8  ...
## 0.4019  0.3320  0.4352  0.3023  0.4005  0.4560  0.2355  0.4350  ...
#--------------

#-----------------------------------------------------------------
# Correlation between item and total score (RIR)
#-----------------------------------------------------------------

#--------------
# RIR index with ItemAnalysis() function
ItemAnalysis(HCI[, 1:20])$RIR
## [1] 0.2884 0.2206 0.3500 0.1730 0.2768 0.3419 0.1009 0.3252 ...

# RIR index by hand
dataR <- total_score - HCI[, 1:20]
diag(cor(HCI[, 1:20], dataR))
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8  ...
## 0.2884  0.2206  0.3500  0.1730  0.2768  0.3419  0.1009  0.3252  ...
#--------------

#-----------------------------------------------------------------
# 4.3.2 Difference between upper and lower group (ULI)
#-----------------------------------------------------------------

#--------------
HCI$HCItotal  <- rowSums(HCI[, 1:20])
HCIcolor      <- c(rep("red", 8), rep("#FED600", 4), rep("#00CC00", 6))

ggplot(HCI, aes(HCItotal)) + 
  geom_histogram(binwidth = 1, fill = HCIcolor, col = "black") + 
  xlab("Total score") + 
  ylab("Number of respondents") + 
  theme_fig()
#--------------

#--------------
gDiscrim(Data = HCI[, 1:20])
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8
## 0.4194  0.3041  0.3272  0.3088  0.4562  0.5069  0.2350  0.4470
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16
## 0.3548  0.4608  0.3318  0.4793  0.5438  0.4378  0.4885  0.5576
## Item 17 Item 18 Item 19 Item 20
## 0.1659  0.4286  0.4147  0.4562

gDiscrim(Data = HCI[, 1:20], k = 5, l = 4, u = 5)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8
## 0.07692 0.07692 0.01538 0.27692 0.19231 0.15385 0.19231 0.12308
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16
## 0.30000 0.15385 0.06923 0.13846 0.11538 0.03077 0.23077 0.07692
## Item 17 Item 18 Item 19 Item 20
## 0.24615 0.02308 0.05385 0.08462
#--------------

#--------------
# ItemAnalysis() with all above indices
ItemAnalysis(Data = HCI[, 1:20], k = 5, l = 4, u = 5)[
  , c("Difficulty", "SD", "ULI", "gULI", "RIT", "RIR")
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

#--------------
DDplot(Data = HCI[, 1:20], discrim = "ULI")
#--------------

#--------------
DDplot(
  Data = HCI[, 1:20], discrim = "ULI", k = 5, l = 4, u = 5,
  thr = 0.1
)
#--------------

#-----------------------------------------------------------------
# 4.4 Item characteristic curve
#-----------------------------------------------------------------

#--------------# illustrative plot
totalscore <- (0:10)
item1 <- c(0.00, 0.05, 0.12, 0.30, 0.50, 0.70, 0.90, 1.00, 1.00, 1.00, 1.00)
item2 <- c(0.00, 0.02, 0.05, 0.12, 0.30, 0.50, 0.70, 0.90, 1.00, 1.00, 1.00)
item3 <- rep(0.5, 11)
item4 <- 1 - item2
item5 <- c(rep(0, 5), rep(1, 6))

df <- data.frame(
  x = totalscore,
  icc = c(item1, item2, item3, item4, item5),
  Item = as.factor(rep(paste("Item", 1:5), each = 11))
)

ggplot(df, aes(x = x, y = icc, col = Item, shape = Item)) +
  geom_point(size = 1.8) +
  geom_line(size = 0.8) +
  xlab("Total score") +
  ylab("Proportion of correct answer") +
  theme_fig() + 
  theme(legend.title = element_blank(),
        legend.position = c(0.84, 0.28),
        legend.margin = margin(0, 0, 0, 0))
#--------------

#-----------------------------------------------------------------
# 4.5 Distractor analysis
#-----------------------------------------------------------------

#--------------
data(HCIkey, HCItest, package = "ShinyItemAnalysis")

DistractorAnalysis(Data = HCItest[, 1:20], key = unlist(HCIkey))$`Item 1`
## score.level
## response Group1 Group2 Group3
##        A     20      7      0
##        B     26     20     13
##        C     71     30      9
##        D    104    171    180
#--------------

#--------------
plotDistractorAnalysis(
  Data = HCItest[, 1:20],
  key = unlist(HCIkey),
  num.group = 5, item = 3,
  multiple.answers = TRUE
)
#--------------

#--------------
plotDistractorAnalysis(
  Data = HCItest[, 1:20],
  key = unlist(HCIkey),
  num.group = 5, item = 17,
  multiple.answers = TRUE
)
#--------------

#-----------------------------------------------------------------
# 4.6 Item reliability
#-----------------------------------------------------------------

#--------------
# Cronbach's alpha
psych::alpha(x = HCI[, 1:20])$total[1]
#  raw_alpha
#  0.715314

# calculation of alpha drop
psych::alpha(x = HCI[, 1:20])$alpha.drop[, 1]
## [1] 0.7042 0.7099 0.7007 0.7151 0.7054 0.6991 0.7220 0.7009 0.7114
## [10] 0.7064 0.7047 0.7016 0.6972 0.7001 0.7030 0.6952 0.7253 0.6943
## [19] 0.6981 0.6997
#--------------

#--------------
ItemAnalysis(Data = HCI[, 1:20])$Alpha.drop
## [1] 0.7042 0.7099 0.7007 0.7151 0.7054 0.6991 0.7220 0.7009 0.7114
## [10] 0.7064 0.7047 0.7016 0.6972 0.7001 0.7030 0.6952 0.7253 0.6943
## [19] 0.6981 0.6997
#--------------             

#--------------             
#ItemAnalysis(Data = HCI[, 1:20], criterion = HCI$major)[, "Index.rel"]
### [1]  0.1844 0.1432 0.1563 0.1484 0.1989 0.2192 0.1172 0.1984 0.1690
### [10] 0.1835 0.1611 0.2154 0.2319 0.1885 0.2098 0.2413 0.0769 0.2036
### [19] 0.1899 0.1999
#--------------

#-----------------------------------------------------------------
# 4.7 Item validity
#-----------------------------------------------------------------

#--------------
ItemAnalysis(Data = HCI[, 1:20], criterion = HCI$major)$Corr.criterion
## [1]  0.1173  0.1265  0.1541  0.0832  0.1274  0.0980 -0.0194  0.0949
## [9]  0.0681  0.0968  0.0782  0.0938  0.0909  0.0664  0.0859  0.0499
## [17] 0.0312  0.1187  0.1295  0.0650
#--------------

#-------------- 
# ItemAnalysis(Data = HCI[, 1:20], criterion = HCI$major)[, "Index.val"]
### [1]  0.0538  0.0546  0.0553  0.0408  0.0633  0.0471 -0.0096  0.0433
### [9]  0.0337  0.0462  0.0325  0.0464  0.0444  0.0284  0.0427  0.0244
### [17] 0.0143  0.0477  0.0532  0.0291
#--------------

#--------------
DDplot(Data = HCI[, 1:20], criterion = HCI$major, thr = NULL)
#--------------

#--------------
plotDistractorAnalysis(
  Data = HCItest[, 1:20], key = unlist(HCIkey),
  item = 5, criterion = HCI$major,
  crit.discrete = TRUE
)
#--------------

#-----------------------------------------------------------------
# 4.8 Missed items
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
naniar::vis_miss(HCImissed)

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

ItemAnalysis(Data = HCImissed[, 1:20])$"Perc.miss"
## [1]   1.0753  0.6144  0.7680  1.0753  1.0753  0.9217  0.9217
## [8]   1.3825  1.2289  2.7650  5.2227  6.2980  7.8341  9.5238
## [15] 11.2135 13.8249 16.5899 18.5868 19.2012 22.2734

ItemAnalysis(Data = HCImissed[, 1:20])$"Perc.nr"
## [1]   0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
## [8]   0.0000  0.1536  1.8433  4.4547  5.0691  7.2197  8.7558
## [15] 10.1382 12.9032 15.0538 16.8971 18.5868 22.2734
#--------------

# #--------------
# # ALL INDICES
# ItemAnalysis(Data = HCI[, 1:20], criterion = HCI$major)[1:5, ]
# ## Difficulty   Mean     SD Prop.max.score Min.score Max.score
# ## Item 1     0.6989 0.6989 0.4591         0.6989         0         1
# ## Item 2     0.7527 0.7527 0.4318         0.7527         0         1
# ## Item 3     0.8479 0.8479 0.3594         0.8479         0         1
# ## Item 4     0.4040 0.4040 0.4911         0.4040         0         1
# ## Item 5     0.4424 0.4424 0.4971         0.4424         0         1
# ## Obs.min Obs.max   gULI    ULI    RIT    RIR Corr.criterion
# ## Item 1       0       1 0.4194 0.4194 0.4019 0.2884        0.11734
# ## Item 2       0       1 0.3041 0.3041 0.3320 0.2206        0.12655
# ## Item 3       0       1 0.3272 0.3272 0.4352 0.3500        0.15412
# ## Item 4       0       1 0.3088 0.3088 0.3023 0.1730        0.08321
# ## Item 5       0       1 0.4562 0.4562 0.4005 0.2768        0.12739
# ## Index.val Index.rel Index.rel.drop Alpha.drop Perc.miss
# ## Item 1   0.05383    0.1844        0.13231     0.7042         0
# ## Item 2   0.05460    0.1432        0.09518     0.7099         0
# ## Item 3   0.05534    0.1563        0.12570     0.7007         0
# ## Item 4   0.04083    0.1484        0.08488     0.7151         0
# ## Item 5   0.06327    0.1989        0.13747     0.7054         0
# ## Perc.nr
# ## Item 1       0
# ## Item 2       0
# ## Item 3       0
# ## Item 4       0
# ## Item 5       0
# #--------------

#-----------------------------------------------------------------
# 4.9. Item analysis in interactive application
#-----------------------------------------------------------------

startShinyItemAnalysis()

