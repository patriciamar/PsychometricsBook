#-----------------------------------------------------------------
# Chapter 4 - Item analysis
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(ggplot2)
library(msm)
library(naniar)
library(nnet)
library(ShinyItemAnalysis)
library(VGAM)

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
# 4.2.1  Item difficulty
#-----------------------------------------------------------------
# Difficulty in binary items 
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
# Difficulty in ordinal items
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
ItemAnalysis(Data = dataMedicalgraded[,1:100])
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
ItemAnalysis(Data = dataMedicalgraded[,1:100], 
             maxscore = 4, minscore = 0, cutscore = 2, bin = TRUE)
##       Difficulty  Mean     SD SD.bin Prop.max.score Min.score Max.score Obs.min Obs.max Cut.score
## X2001     0.6647 2.659 0.9905 0.4964         0.3181         0         4       0       4         2
## ... 
#--------------


#-----------------------------------------------------------------
# 4.2.2  Item discrimination
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Correlation between item and total score (RIT)
#-----------------------------------------------------------------

#--------------
# RIT index with ItemAnalysis() function
ItemAnalysis(HCI[,1:20])$RIT
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
ItemAnalysis(HCI[,1:20])$RIR
## [1] 0.2884 0.2206 0.3500 0.1730 0.2768 0.3419 0.1009 0.3252 ...

# RIR index by hand
dataR <- total_score - HCI[, 1:20]
diag(cor(HCI[, 1:20], dataR))
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8  ...
## 0.2884  0.2206  0.3500  0.1730  0.2768  0.3419  0.1009  0.3252  ...
#--------------

#-----------------------------------------------------------------
# Upper-lower index (ULI)
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
# 4.2.3 Item characteristic curve
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
# 4.2.4 Distractor analysis
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
# 4.2.5 Item reliability
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

#-----------------------------------------------------------------
# 4.2.6 Item validity
#-----------------------------------------------------------------

#--------------
ItemAnalysis(Data = HCI[, 1:20], criterion = HCI$major)$Corr.criterion
## [1]  0.1173  0.1265  0.1541  0.0832  0.1274  0.0980 -0.0194  0.0949
## [9]  0.0681  0.0968  0.0782  0.0938  0.0909  0.0664  0.0859  0.0499
## [17] 0.0312  0.1187  0.1295  0.0650
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
# 4.2.7 Missed items
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
# 4.3.  Regression models for item description
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 4.3.2  Models for continuous items
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 4.3.2.1  Linear regression
#-----------------------------------------------------------------

#--------------
data(EPIA, package = "EstCRM")
head(EPIA, n = 2)
##   Item 1 Item 2 Item 3 Item 4 Item 5
## 1     96     36     80     78     79
## 2     42      2      1      1      1

EPIA$score <- rowSums(EPIA) # total scores
#--------------

#--------------
# linear model for Item 1 on total scores
summary(lm(`Item 1` ~ score, data = EPIA))
## Call:
## lm(formula = `Item 1` ~ score, data = EPIA)
##
## Residuals:
##     Min      1Q  Median      3Q     Max
## -95.461 -15.981  -0.639  17.072  69.689
##
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)   15.5293     2.1678   7.164  1.5e-12 ***
## score          0.1878     0.0081  23.246  < 2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 24.03 on 1031 degrees of freedom
## Multiple R-squared:  0.3439,	Adjusted R-squared:  0.3433
## F-statistic: 540.4 on 1 and 1031 DF,  p-value: < 2.2e-16
#--------------

#--------------
# plot of estimated curve
ggplot(data = EPIA, aes(x = `score`, y = `Item 1`)) +
  geom_point(
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5, size = 1.8
  ) +
  geom_smooth(
    method = "lm",
    size = 0.8, color = "darkblue"
  ) +
  xlab("Total score") +
  ylab("Score of item 1") +
  ggtitle("Item 1") + 
  theme_fig()
#--------------

#--------------
coef(lm(`Item 1` ~ score, data = EPIA))
## (Intercept)   score
##     15.5292  0.1876
sqrt(diag(vcov(lm(`Item 1` ~ score, data = EPIA))))
## (Intercept)    score
##      2.1676   0.0081
#--------------

#--------------
mean(EPIA$score) # average total score
## [1] 251.8964
sd(EPIA$score) # standard deviation of total score
## [1] 92.5968

EPIA$zscore <- scale(EPIA$score) # Z-scores

# linear model for Item 1 on Z-scores
coef(lm(`Item 1` ~ zscore, data = EPIA))
## (Intercept)      zscore
##     62.8296     17.3876

sqrt(diag(vcov(lm(`Item 1` ~ zscore, data = EPIA))))
## (Intercept)      zscore
##   0.7476159   0.7479780

# Correspondence to classical item parameters:
mean(EPIA$`Item 1`)
## [1] 62.8296

cor(EPIA$`Item 1`, EPIA$zscore) * sd(EPIA$`Item 1`)
## [1,] 17.3876
#--------------

#--------------
# plot of estimated curve
ggplot(data = EPIA, aes(x = `zscore`, y = `Item 1`)) +
  geom_point(
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5, size = 1.8
  ) +
  geom_smooth(
    method = "lm",
    size = 0.8, color = "darkblue"
  ) +
  xlab("Z-score") +
  ylab("Score of item 1") +
  ggtitle("Item 1") +
  theme_fig()
#--------------

#-----------------------------------------------------------------
# 4.3.3  Models for binary items
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 4.3.3.1  Logistic regression
#-----------------------------------------------------------------

#--------------
data(HCI, package = "ShinyItemAnalysis")
zscore <- scale(rowSums(HCI[, 1:20])) # Z-score

# logistic model for item 13
fit1 <- glm(HCI[, 13] ~ zscore, family = binomial)

# coefficients
coef(fit1)
## (Intercept)   zscore
##      0.5552   1.1824 

# standard errors
sqrt(diag(vcov(fit1)))
## (Intercept)   zscore
##      0.0932   0.1084
#--------------

#--------------
# probability of answering item 13 with Z-score 0
exp(coef(fit1)[1]) / (1 + exp(coef(fit1)[1]))
## (Intercept)
##      0.6353

# log-odds of answering item 13 correctly with Z-score 1
coef(fit1)[1] + coef(fit1)[2]
## (Intercept)
##      1.7377

# probability of answering item 13 with Z-score 1
exp(coef(fit1)[1] + coef(fit1)[2]) / (1 + exp(coef(fit1)[1] + coef(fit1)[2]))
## (Intercept)
##      0.8504
#--------------

#-----------------------------------------------------------------
# 4.3.3.2  Other link functions, probit regression model
#-----------------------------------------------------------------
#--------------
# probit link
fit2 <- glm(HCI[, 13] ~ zscore, family = binomial(link = "probit"))

# coefficients
coef(fit2)
## (Intercept)   zscore
##      0.3440   0.7122
# standard errors
sqrt(diag(vcov(fit2)))
## (Intercept)   zscore
##      0.0549   0.0608
#--------------

#--------------
# probability of correct answer for respondent with Z-score 0
pnorm(coef(fit2)[1])
## (Intercept)
##      0.6346

# probability of correct answer for respondent with Z-score 1
pnorm(coef(fit2)[1] + coef(fit2)[2])
## (Intercept)
##      0.8546
#--------------

#--------------
coef(fit2)[2] * 1.7
##  zscore
##  1.2108

coef(fit1)[2] / coef(fit2)[2]
##  zscore
##  1.6601
#--------------

#--------------
# function for plot (logit)
mod_logit <- function(x, b0, b1) {
  exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
}

# function for plot (probit)
mod_probit <- function(x, b0, b1) {
  pnorm(b0 + b1 * x)
}

df <- data.frame(
  x = sort(unique(zscore)),
  y = tapply(HCI[, 13], zscore, mean),
  Count = as.numeric(table(zscore))
)
#--------------

#--------------
# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = Count),
             color = "darkblue",
             fill = "darkblue",
             shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = mod_logit, geom = "line",
    args = list(
      b0 = coef(fit1)[1],
      b1 = coef(fit1)[2]
    ),
    size = 0.8,
    aes(color = "logit", linetype = "logit")
  ) +
  stat_function(
    fun = mod_probit, geom = "line",
    args = list(
      b0 = coef(fit2)[1],
      b1 = coef(fit2)[2]
    ),
    size = 0.8,
    aes(color = "probit", linetype = "probit")
  ) +
  xlab("Z-score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  scale_colour_manual("Link", values = c("darkblue", "darkgoldenrod2")) + 
  scale_linetype_manual("Link", values = c("solid", "dashed")) + 
  ggtitle("Item 13") + 
  theme_fig()
#--------------

#-----------------------------------------------------------------
# 4.3.3.3  IRT parametrization
#-----------------------------------------------------------------

#--------------
# coefficients in IRT parametrization
IRTpars13 <- c(coef(fit1)[2], -coef(fit1)[1] / coef(fit1)[2])
setNames(IRTpars13, c("a", "b"))
##      a       b
## 1.1824 -0.4696

# delta method to calculate SE (with the deltamethod() function)
msm::deltamethod(
  list(~x2, ~ -x1 / x2),
  mean = coef(fit1),
  cov = vcov(fit1)
)
## [1] 0.1084 0.0823

# delta method to calculate SE (by hand)
g <- list(~ beta1, ~ -beta0 / beta1) # formula
# covariance matrix of original item parameters
Sigma <- matrix(vcov(fit1), ncol = 2, nrow = 2,
                dimnames = list(c("beta0", "beta1"), 
                                c("beta0", "beta1")))
syms <- colnames(Sigma)
coefs_fit1 <- coef(fit1)
for (i in 1:2) assign(syms[i], coef(fit1)[i])
# calculation of gradient
nabla <- t(sapply(g, function(form) {
  as.numeric(attr(eval(deriv(form, syms)), "gradient"))
}))
new.covar <- nabla %*% Sigma %*% t(nabla)

# new standard errors
sqrt(diag(new.covar))
## [1] 0.1084 0.0823
#--------------

#-----------------------------------------------------------------
# 4.3.3.4  Nonlinear regression models
#-----------------------------------------------------------------

#--------------
# NLR 3P model for item 13
mod_3PL <- function(x, a, b, c) {
  c + (1 - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

fit3 <- nls(
  HCI[, 13] ~ mod_3PL(zscore, a, b, c),
  algorithm = "port",
  start = c(a = 0.7, b = -0.9, c = 0),
  lower = c(-Inf, -Inf, 0),
  upper = c(Inf, Inf, 1)
)

# coefficients
coef(fit3)
##      a      b      c
## 2.6692 0.2708 0.3214 
#--------------

#--------------
# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = Count),
             color = "darkblue",
             fill = "darkblue",
             shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = mod_3PL, geom = "line",
    args = list(
      a = coef(fit3)[1],
      b = coef(fit3)[2],
      c = coef(fit3)[3]
    ),
    size = 0.8,
    color = "darkblue"
  ) +
  xlab("Z-score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 13")  +
  theme_fig() + 
  theme(legend.position = c(0.88, 0.23))
#--------------

#--------------
# NLR 4P model for item 13
mod_4PL <- function(x, a, b, c, d) {
  c + (d - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

fit4 <- nls(
  HCI[, 13] ~ mod_4PL(zscore, a, b, c, d),
  algorithm = "port",
  start = c(a = 0.7, b = -0.9, c = 0, d = 1),
  lower = c(-Inf, -Inf, 0, 0),
  upper = c(Inf, Inf, 1, 1)
)

# coefficients
coef(fit4)
##      a      b      c      d
## 3.3405 0.2326 0.3379 0.9569 
#--------------

#-----------------------------------------------------------------
# 6.4.2.2  Model selection and model fit
#-----------------------------------------------------------------

#--------------
# AIC
AIC(fit1, fit2, fit3, fit4)
##      df      AIC
## fit1  2 715.1369
## fit2  2 713.7691
## fit3  4 740.8746
## fit4  5 742.4880

# BIC
BIC(fit1, fit2, fit3, fit4)
##      df      BIC
## fit1  2 724.0939
## fit2  2 722.7261
## fit3  4 758.7886
## fit4  5 764.8806
#--------------

#-----------------------------------------------------------------
# 6.5.1  Ordinal regression models
#-----------------------------------------------------------------

#--------------
data("Anxiety", package = "lordif")
data <- Anxiety[, paste0("R", 1:29)] - 1

zscore <- scale(rowSums(data)) # Z-scores
maxval <- max(data[, 18]) # maximal number of points for item 18
# reordering item 18
data[, 18] <- ordered(factor(data[, 18], levels = 0:maxval))

# cumulative logit model for item 18
fit.cum <- vglm(
  data[, 18] ~ zscore,
  family = cumulative(reverse = TRUE, parallel = TRUE)
)

# coefficients for item 18
coef(fit.cum)
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##        0.4155       -1.2807       -3.4374       -5.1978    1.7325
# SE
sqrt(diag(vcov(fit.cum)))
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##        0.0865        0.1031        0.1825        0.2880    0.0955

# IRT parametrization
c(-coef(fit.cum)[1:4] / coef(fit.cum)[5], coef(fit.cum)[5])
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##       -0.2398        0.7392        1.9841        3.0002    1.7325
# SE using delta method
msm::deltamethod(
  list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
  mean = coef(fit.cum),
  cov = vcov(fit.cum)
)
## [1] 0.0486 0.0643 0.1033 0.1610 0.0955
#--------------

#--------------
# plotting cumulative probabilities
plotCumulative(
  fit.cum,
  type = "cumulative", matching.name = "Z-score"
)  +
  theme_fig() + 
  xlim(-1.1, 5.2) + 
  theme(legend.position = c(0.79, 0.23),
        legend.box = "horizontal",
        legend.margin = margin(0, -5, 0, 0),
        legend.background = element_blank())  
#--------------

#--------------
# plotting category probabilities
plotCumulative(
  fit.cum,
  type = "category", matching.name = "Z-score"
)   +
  theme_fig() + 
  theme(legend.position = c(0.87, 0.5),
        legend.box = "horizontal",
        legend.margin = margin(0, -5, 0, 0),
        legend.background = element_blank()) +
  scale_size_continuous(breaks = c(0, 5, 10, 15)) + 
  guides(size = FALSE)
#--------------

#--------------
# adjacent category logit model for item 18
fit.adj <- vglm(
  data[, 18] ~ zscore,
  family = acat(reverse = FALSE, parallel = TRUE)
)

# coefficients for item 18
coef(fit.adj)
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##       -0.2322       -0.5296       -2.2146       -2.7410    1.0109
# SE
sqrt(diag(vcov(fit.adj)))
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
## 0.0976        0.1183        0.2111        0.3347           0.0722
#--------------

#--------------
# IRT parametrization
c(-coef(fit.adj)[1:4] / coef(fit.adj)[5], coef(fit.adj)[5])
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##        0.2296        0.5239        2.1906        2.7113    1.0109
# SE using delta method
msm::deltamethod(
  list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
  mean = coef(fit.adj),
  cov = vcov(fit.adj)
)
## [1] 0.1035 0.1163 0.1874 0.2769 0.0722
#--------------

#--------------
# plotting category probabilities
plotAdjacent(fit.adj, matching.name = "Z-score")   +
  theme_fig() + 
  scale_size_continuous(breaks = c(5, 10, 30, 50)) + 
  theme(legend.position = c(0.8, 0.7),
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0),
        legend.background = element_blank())
#--------------

#-----------------------------------------------------------------
# 6.5.2  Nominal response models
#-----------------------------------------------------------------

#--------------
# multinominal model
data(HCItest, HCIkey, package = "ShinyItemAnalysis")
key <- unlist(HCIkey)
zscore <- scale(rowSums(HCI[, 1:20])) # Z-score

# re-leveling item 13
HCItest[, 13] <- relevel(HCItest[, 13], ref = paste(key[13]))

# multinomial model for item 13
fit.mult <- multinom(HCItest[, 13] ~ zscore)

# coefficients for item 13
coef(fit.mult)
##   (Intercept)  zscore
## B     -2.3463 -1.4044
## C     -1.6241 -1.1924
## D     -1.2820 -1.0514
# SE
matrix(sqrt(diag(vcov(fit.mult))), ncol = 2, byrow = TRUE)
##        [,1]   [,2]
## [1,] 0.1976 0.1860
## [2,] 0.1410 0.1458
## [3,] 0.1187 0.1289

# IRT parametrization - difficulties
cbind(-coef(fit.mult)[, 1] / coef(fit.mult)[, 2], coef(fit.mult)[, 2])
##      [,1]    [,2]
## B -1.6484 -1.4234
## C -1.3253 -1.2354
## D -1.2052 -1.0560
# SE using delta method
subst_vcov <- function(vcov, cat) {
  ind <- grep(cat, colnames(vcov))
  vcov[ind, ind]
}
t(sapply(
  rownames(coef(fit.mult)),
  function(.x) {
    vcov_subset <- subst_vcov(vcov(fit.mult), .x)
    msm::deltamethod(
      list(~ -x1 / x2, ~x2),
      mean = coef(fit.mult)[.x, ],
      cov = vcov_subset,
      ses = TRUE
    )
  }
))
##     [,1]   [,2]
## B 0.1819 0.1860
## C 0.1537 0.1458
## D 0.1611 0.1289
#--------------

#--------------
plotMultinomial(
  fit.mult,
  matching = zscore, matching.name = "Standardized total score"
)  +
  theme_fig() + 
  scale_size_continuous(breaks = c(5, 10, 30, 50)) + 
  theme(legend.position = c(0.22, 0.75),
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0),
        legend.background = element_blank())
#--------------

#-----------------------------------------------------------------
# 6.6.1  Joint model
#-----------------------------------------------------------------

#--------------
# Joint model for all items: data preparation
HCI$zscore <- scale(rowSums(HCI[, 1:20])) # Z-score
HCI$person <- 1:nrow(HCI)

# converting data to the long format
HCI.long <- reshape(
  data = HCI,
  varying = list(paste("Item", 1:20)), timevar = "item", v.names = "rating",
  idvar = c("person", "gender", "major", "zscore"),
  direction = "long", new.row.names = 1:13020
)

head(HCI.long, n = 2)
##   gender major   zscore person item rating
## 1      0     1 1.040755      1    1      1
## 2      0     1 1.865002      2    1      1

summary(HCI.long)
# person ID and item ID as factors:
HCI.long$person <- as.factor(HCI.long$person)
HCI.long$item <- as.factor(HCI.long$item)
#--------------

#--------------
# Joint model for all items: data preparation
# 1PL model for all items
fit_glm1PL <- glm(rating ~ -1 + item,
                  data = HCI.long,
                  family = binomial
)
coef(fit_glm1PL)
##   item1   item2   item3   item4   item5   item6   item7   item8
##  0.8422  1.1130  1.7184 -0.3889 -0.2314 -0.5644  0.1880  0.8716
##   item9  item10  item11  item12  item13  item14  item15  item16
## -0.2689  0.6113  1.2587  0.2940  0.4402  1.1463 -0.2252  0.4337
##  item17  item18  item19  item20
## -0.8642  1.3691  1.2947  0.9542

# 2PL model for all items
fit_glm2PL <- glm(rating ~ -1 + item + zscore:item,
                  data = HCI.long,
                  family = binomial
)
coef(fit_glm2PL)
##         item1         item2         item3         item4         item5
##        1.0118        1.2723        2.3746       -0.4340       -0.2873
##         item6         item7         item8         item9        item10
##       -0.7498        0.1972        1.0867       -0.3114        0.7139
##        item11        item12        item13        item14        item15
##        1.5345        0.3508        0.5552        1.4739       -0.2879
##        item16        item17        item18        item19        item20
##        0.5587       -0.8941        2.0232        1.7478        1.2137
##  item1:zscore  item2:zscore  item3:zscore  item4:zscore  item5:zscore
##  0.9925        0.8294        1.4842        0.6751        0.9399
##  item6:zscore  item7:zscore  item8:zscore  item9:zscore item10:zscore
##  1.1988        0.4931        1.1130        0.7690        0.9007
## item11:zscore item12:zscore item13:zscore item14:zscore item15:zscore
##  1.0535        1.0329        1.1824        1.2166        1.0093
## item16:zscore item17:zscore item18:zscore item19:zscore item20:zscore
##  1.2543        0.3844        1.6575        1.3748        1.1766
#--------------

#--------------
anova(fit_glm1PL, fit_glm2PL, test = "Chisq")
## Analysis of Deviance Table
##
## Model 1: rating ~ -1 + item
## Model 2: rating ~ -1 + item + zscore:item
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1     13000      15956                          
## 2     12980      13632 20   2323.4 < 2.2e-16 ***

BIC(fit_glm1PL, fit_glm2PL)
##            df      BIC
## fit_glm1PL 20 16145.37
## fit_glm2PL 40 14011.40

AIC(fit_glm1PL, fit_glm2PL)
##            df      AIC
## fit_glm1PL 20 15995.88
## fit_glm2PL 40 13712.43
#--------------

#-----------------------------------------------------------------
# 5.7. ShinyItemAnalysis interactive application
#-----------------------------------------------------------------

startShinyItemAnalysis()
