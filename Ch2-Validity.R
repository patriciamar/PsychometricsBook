#-----------------------------------------------------------------
# Chapter 2 - Validity
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(Cairo)
library(corrplot)
library(ggdendro)
library(ggplot2)
library(lme4)
library(lmerTest)
library(psych)
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
# 2.2.1  Inference based on ratios
#-----------------------------------------------------------------

#-------------- 
Y <- c(34, 20, 34, 26, 27, 17, 22)
n <- 37
(Z <- (Y - n / 2) / (sqrt(n) / 2))
## [1]  5.0964  0.4932  5.0964  2.4660  2.7948 -0.4932  1.1508
Z^2
## [1] 25.9730  0.2432 25.9730  6.0811  7.8108  0.2432  1.3243
#--------------

#--------------
prop.test(x = 34, n = 37, alternative = "greater", correct = FALSE)
##         1-sample proportions test without continuity correction
##
## data:  34 out of 37, null probability 0.5
## X-squared = 25.973, df = 1, p-value = 1.731e-07
## alternative hypothesis: true p is not equal to 0.5
## 95 percent confidence interval:
##  0.8136 1.0000
## sample estimates:
##      p
## 0.9189
#--------------

#--------------
proptests <- lapply(Y, prop.test, n = n, alternative = "greater", 
                    correct = FALSE)
# p-values
sapply(proptests, function(x) x$p.value)
## [1] 0.0000 0.3109 0.0000 0.0068 0.0026 0.6891 0.1249
# confidence intervals
sapply(proptests, function(x) x$conf.int)
##        [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]
## [1,] 0.8136 0.4077 0.8136 0.5688 0.5971 0.3321 0.4598
## [2,] 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#--------------

#-----------------------------------------------------------------
# 2.3.1  Correlation coefficients
#-----------------------------------------------------------------

#--------------
data("MSclinical", package = "ShinyItemAnalysis")
head(MSclinical, n = 3)
##   LCLA  MI  MAS   BBS    T   DD DM PRs  KH   NHPT T25FW PASAT3 EDSS
## 1 44.0 313 19.5 51.25  4.0 6.25  3  22 5.5 20.125  5.05     59 4.75
## 2 28.0 307 16.0 46.25  6.5 6.25  4  21 4.5 27.525  5.20     33 3.50
## 3 35.3 274 18.0 38.75 11.0 6.75  5  35 5.0 28.125 11.10     53 3.50
#--------------

#--------------
ggplot(MSclinical, aes(x = MI, y = EDSS)) +
  geom_point() + theme_fig()
#--------------

#--------------
cor(MSclinical$MI, MSclinical$EDSS)
## [1] -0.4622
cor.test(MSclinical$MI, MSclinical$EDSS)
##         Pearson's product-moment correlation
##
## data:  MSclinical$MI and MSclinical$EDSS
## t = -2, df = 15, p-value = 0.06
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## -0.7714  0.0238
## sample estimates:
##     cor
## -0.4622
#--------------

#--------------
data(HCI, package = "ShinyItemAnalysis")
score <- rowSums(HCI[, 1:20])

cor.test(score, HCI$major)
##         Pearson's product-moment correlation
##
## data:  score and HCI$major
## t = 5.7, df = 649, p-value = 2e-08
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##   0.1432 0.2897
## sample estimates:
##   cor
## 0.2177

barX0 <- mean(score[HCI$major == 0])
barX1 <- mean(score[HCI$major == 1])
n <- length(score)
n0 <- length(score[HCI$major == 0])
n1 <- length(score[HCI$major == 1])
sn <- sd(score) * sqrt((n - 1) / n)
(barX1 - barX0) / sn * sqrt(n0 * n1 / (n0 + n1)^2)
## [1] 0.2177
#--------------

#--------------
ggplot(MSclinical, aes(x = rank(MI), y = rank(EDSS))) +
  geom_point() + theme_fig()
#--------------

#--------------
cor(MSclinical$MI, MSclinical$EDSS, method = "spearman")
## [1] -0.5996
cor.test(MSclinical$MI, MSclinical$EDSS, method = "spearman")
##         Spearman's rank correlation rho
##
## data:  MSclinical$MI and MSclinical$EDSS
## S = 1305, p-value = 0.01
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##    rho
## -0.5996
#--------------

#--------------
cor(MSclinical$MI, MSclinical$EDSS, method = "kendall")
## [1] -0.4275
cor.test(MSclinical$MI, MSclinical$EDSS, method = "kendall")
##         Kendall's rank correlation tau
##
## data:  MSclinical$MI and MSclinical$EDSS
## z = -2.3, p-value = 0.02
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##     tau
## -0.4275
#--------------

#--------------
psych::tetrachoric(table(HCI$"Item 1", HCI$"Item 2"))
## [1] 0.23
##
##  with tau of
##     0     0
## -0.52 -0.68
cor(HCI$"Item 1", HCI$"Item 2")
## [1] 0.1360
#--------------

#--------------
data(Anxiety, package = "lordif")
psych::polychoric(table(Anxiety$R1, Anxiety$R2))
## $rho
## [1] 0.8334
##
## $objective
## [1] 1.5587
##
## $tau.row
## 1      2      3      4
## 0.4608 1.1583 1.8434 2.6068
##
## $tau.col
## 1      2      3      4
## 0.5236 1.2898 2.0104 3.2148
cor(Anxiety$R1, Anxiety$R2)
## [1] 0.7813
#--------------

#-----------------------------------------------------------------
# 2.3.2  t-tests
#-----------------------------------------------------------------

#--------------
# set.seed(987)
data(HCIprepost, package = "ShinyItemAnalysis")
ggplot(data.frame(
  score = c(HCIprepost$score.pre, HCIprepost$score.post),
  group = factor(rep(c("Pre", "Post"), each = 16),
                 levels = c("Pre", "Post"))),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() + ylab("Total score") + xlab("") +
  theme_fig() + theme(legend.position = "none")
#--------------

#--------------
ggplot(data.frame(
  score = HCIprepost$score.post - HCIprepost$score.pre,
  group = factor(rep(c("Post-Pre"), each = 16), levels = "Post-Pre")),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  ylab("Posttest - pretest score") + xlab("") +
  theme_fig() +
  theme(legend.position = "none")
#--------------

#--------------
# differences between post-test and pre-test
(dif <- HCIprepost$score.post - HCIprepost$score.pre)
## [1]  7  2  0  2  4  1  0  5  3  1  5 -2  4  8  1 -4
# mean difference
(barX <- mean(dif))
## [1] 2.3125
# standard deviation of difference
(s <- sd(dif))
## [1] 3.1563
# number of observations
(n <- length(dif))
## [1] 16
# t-value
(t <- barX / (s / sqrt(n)))
## [1] 2.9306
# p-value
2 * pt(-abs(t), df = n - 1)
## [1] 0.0103
# confidence interval
barX - qt(1 - 0.05 / 2, df = n - 1) * s / sqrt(n)
## [1] 0.6306
barX + qt(1 - 0.05 / 2, df = n - 1) * s / sqrt(n)
## [1] 3.9944
#--------------

#--------------
# one sample t-test
t.test(dif, mu = 0)
##         One Sample t-test
##
## data:  dif
## t = 2.9306, df = 15, p-value = 0.0103
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##   0.6306 3.9944
## sample estimates:
##   mean of x
## 2.3125
#--------------

#--------------
# paired t-test
t.test(HCIprepost$score.post, HCIprepost$score.pre, paired = TRUE)
##         Paired t-test
##
## data:  dataPrePost$score.post and dataPrePost$score.pre
## t = 2.9306, df = 15, p-value = 0.0103
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   0.6306 3.9944
## sample estimates:
##   mean of the differences
## 2.3125
#--------------

#--------------
data(HCIgrads, package = "ShinyItemAnalysis")
score_grads <- rowSums(HCIgrads[, paste0("QR", 1:20)])
score_undergrads <- rowSums(HCI[, 1:20])
#--------------

#--------------
# two sample t-test
t.test(score_grads, score_undergrads, alternative = "greater")
##         Welch Two Sample t-test
##
## data:  score_grads and score_undergrads
## t = 2.2, df = 9.3, p-value = 0.03
## alternative hypothesis: true difference in means is greater than 0
## 95 percent confidence interval:
##  0.38  Inf
## sample estimates:
## mean of x mean of y
## 14.50     12.21
#--------------

#--------------
mean(score_grads)
## [1] 14.5000
sd(score_grads)
## [1] 3.2745
mean(score_undergrads)
## [1] 12.2120
sd(score_undergrads)
## [1] 3.6397
#--------------

#--------------
df <- data.frame(
  score = c(score_grads, score_undergrads),
  group = as.factor(c(rep("Graduate", length(score_grads)),
                      rep("Undergraduate", length(score_undergrads))))
)
#--------------

#--------------
ggplot(df, aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  xlab("") + ylab("Total score") +
  theme_fig() +
  theme(legend.position = "none")
#--------------

#--------------
# the same figure with jittered points of observed values included
set.seed(978)
ggplot(df, aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.25) +
  xlab("") + ylab("Total score") +
  theme_fig() +
  theme(legend.position = "none")
#--------------

#-----------------------------------------------------------------
# 2.3.3 ANOVA
#-----------------------------------------------------------------

#--------------
data(HCIdata, package = "ShinyItemAnalysis")
#--------------

#--------------
#set.seed(978)
ggplot(HCIdata, aes(x = typeSCH, y = total, fill = typeSCH)) +
  geom_boxplot() +
  #geom_jitter(height = 0, width = 0.25) +
  xlab("") + ylab("Total score") +
  theme_fig() +
  theme(legend.position = "none")
#--------------

#--------------
# density plot
ggplot(HCIdata, aes(total, fill = typeSCH)) +
  geom_density(aes(y = ..density.., color = typeSCH,
                   linetype = typeSCH),
               position = "identity", alpha = 0.5, size = 1) +
  xlab("Total score on HCI") + ylab("Density") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.16)) +
  theme_fig() + theme(legend.position = c(0.15, 0.8),
                      legend.title = element_blank())
#--------------

#--------------
anovaHCI <- aov(total ~ typeSCH, data = HCIdata)
summary(anovaHCI)
##              Df Sum Sq Mean Sq F value  Pr(>F)
## typeSCH       3    603   201.0    16.1 4.2e-10 ***
## Residuals   665   8306    12.5
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#--------------

#--------------
TukeyHSD(anovaHCI)
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
##
## Fit: aov(formula = total ~ typeSCH, data = HCIdata)
##
## $typeSCH
##             diff     lwr     upr  p adj
## BCAS-AC  -0.6563 -1.7003  0.3877 0.3685
## R1-AC     1.1537  0.2457  2.0617 0.0062
## MCU-AC   -1.4024 -2.3982 -0.4065 0.0018
## R1-BCAS   1.8100  0.7699  2.8500 0.0001
## MCU-BCAS -0.7461 -1.8637  0.3715 0.3143
## MCU-R1   -2.5561 -3.5478 -1.5643 0.0000
#--------------

#-----------------------------------------------------------------
# 2.3.4  Regression models
#-----------------------------------------------------------------

# Simple linear regression

#--------------
(b1 = cov(MSclinical$MI, MSclinical$EDSS) / var(MSclinical$MI))
## [1] -0.0159
(b0 = mean(MSclinical$EDSS) - b1 * mean(MSclinical$MI))
## [1] 8.6970  
#--------------

#--------------
lmMS <- lm(EDSS ~ MI, data = MSclinical)
anova(lmMS)
summary(lmMS)
## Call:
## lm(formula = EDSS ~ MI, data = MSclinical)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.04667 -0.31690  0.06461  0.89697  1.11951 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  8.697029   2.476136   3.512  0.00314 **
## MI          -0.015896   0.007876  -2.018  0.06180 . 
## ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 0.9437 on 15 degrees of freedom
## Multiple R-squared:  0.2136,	Adjusted R-squared:  0.1612 
## F-statistic: 4.074 on 1 and 15 DF,  p-value: 0.0618
#--------------

#--------------
ggplot(MSclinical, aes(x = MI, y = EDSS)) + geom_point() + 
  geom_smooth(method = "lm") + theme_fig()
#--------------

#--------------
lmMS$residuals
##      2       6      10      14      18      22      25      30 ...
## 1.0285 -0.3169 -0.8415  0.1354  1.0877  0.8970 -0.2287  0.0646 ...
MSclinical$EDSS - predict(lmMS)
##      2       6      10      14      18      22      25      30 ...
## 1.0285 -0.3169 -0.8415  0.1354  1.0877  0.8970 -0.2287  0.0646 ...
residuals(lmMS)
##      2       6      10      14      18      22      25      30 ...
## 1.0285 -0.3169 -0.8415  0.1354  1.0877  0.8970 -0.2287  0.0646 ...
summary(lmMS$residuals)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -2.047  -0.317   0.065   0.000   0.897   1.120 
#--------------

#--------------
# R2 = SSR/SST
SSR = sum((predict(lmMS) - mean(MSclinical$EDSS))^2)
SST = sum((MSclinical$EDSS - mean(MSclinical$EDSS))^2)
SSR / SST
## [1] 0.2136
cor(MSclinical$EDSS, MSclinical$MI)^2
## [1] 0.2136
#--------------

#--------------
# diagnostic plots
plot(lmMS$residuals ~ predict(lmMS), ylab = "Residuals")
abline(h = 0, lty = "dotted")

qqnorm(lmMS$residuals)
qqline(lmMS$residuals, lty = "dotted")
#--------------

#--------------
#plot(lmMS)
plot(lmMS, which = 1)
plot(lmMS, which = 2)
plot(lmMS, which = 3)
plot(lmMS, which = 4)
#--------------


# Multiple linear regression model
#--------------
lmF <- lm(total ~ gender + major + as.factor(yearc5) +
            minority + EnglishF + typeSCH, data = HCIdata)
anova(lmF)
## Analysis of Variance Table
## 
## Response: total
##                    Df Sum Sq Mean Sq F value    Pr(>F)    
## gender              2  260.7  130.36 13.2054 2.385e-06 ***
## major               1  393.8  393.84 39.8952 4.947e-10 ***
## as.factor(yearc5)   4  647.3  161.82 16.3920 8.300e-13 ***
## minority            2  529.8  264.91 26.8346 6.291e-12 ***
## EnglishF            1  319.7  319.72 32.3876 1.906e-08 ***
## typeSCH             3  291.6   97.19  9.8449 2.339e-06 ***
## Residuals         655 6466.0    9.87                      
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#--------------

#--------------
summary(lmF)
## Call:
## lm(formula = total ~ gender + major + as.factor(yearc5) + 
##    minority + EnglishF + typeSCH, data = HCIdata)
##
## Residuals:
##    Min     1Q Median     3Q    Max
## -8.871 -2.020  0.108  2.131  7.362
##
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)
##   (Intercept)          10.902      0.532   20.48  < 2e-16 ***
##   genderF              -0.809      0.260   -3.11  0.00193 **
##   gendernone           -2.228      0.831   -2.68  0.00754 **
##   major                 1.287      0.266    4.84  1.6e-06 ***
##   as.factor(yearc5)2    2.042      0.510    4.00  7.0e-05 ***
##   as.factor(yearc5)3    1.191      0.495    2.41  0.01641 *
##   as.factor(yearc5)4    1.794      0.522    3.44  0.00063 ***
##   as.factor(yearc5)5    3.703      0.589    6.28  6.0e-10 ***
##   minoritymin          -1.593      0.326   -4.89  1.3e-06 ***
##   minoritynone         -2.300      0.628   -3.66  0.00027 ***
##   EnglishFno           -1.416      0.315   -4.50  8.1e-06 ***
##   typeSCHBCAS           0.257      0.406    0.63  0.52583
##   typeSCHR1             0.735      0.368    2.00  0.04619 *
##   typeSCHRMCU          -1.303      0.420   -3.10  0.00203 **
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 3.14 on 655 degrees of freedom
## Multiple R-squared:  0.274,	Adjusted R-squared:  0.26
## F-statistic:   19 on 13 and 655 DF,  p-value: <2e-16
#--------------


#-----------------------------------------------------------------
# 2.4.1  Correlation structure
#-----------------------------------------------------------------

#--------------
# polychoric correlation calculation
corP <- psych::polychoric(HCI[, 1:20])
# correlation matrix
corP$rho
##         Item 1  Item 2 Item 3  Item 4 Item 5 Item 6  Item 7
## Item 1  1.0000  0.2338 0.2817  0.0707 0.1424 0.3370  0.0576
## Item 2  0.2338  1.0000 0.4800  0.1272 0.2136 0.1365 -0.0240
## Item 3  0.2817  0.4800 1.0000  0.0508 0.2843 0.2734  0.0679
## Item 4  0.0707  0.1272 0.0508  1.0000 0.1165 0.0806  0.0118
## ...
# correlation plot
ShinyItemAnalysis::plot_corr(HCI[, 1:20], cor = "polychoric")
#--------------

#-----------------------------------------------------------------
# 2.4.2  Cluster analysis
#-----------------------------------------------------------------

#--------------
# hierarchical clustering
hc <- hclust(as.dist(1 - corP$rho), method = "ward.D2")
# dendrogram
ggdendrogram(hc)
#--------------

#--------------
ShinyItemAnalysis::plot_corr(HCI[, 1:20], cor = "poly",
                             clust_method = "ward.D2")
#--------------

