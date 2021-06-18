#-----------------------------------------------------------------
# Chapter 3 - Validity
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
library(GPArotation)
library(lavaan)
library(lme4)
library(lmerTest)
library(psych)
library(semPlot)
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
# 3.2.1  Inference based on ratios
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
# 3.3.1  Correlation coefficients
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
  geom_point() + geom_smooth(method = "lm") +
  theme_fig()
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
sn <- sd(score)
n0 <- length(score[HCI$major == 0])
n1 <- length(score[HCI$major == 1])
(barX1 - barX0) / sn * sqrt(n0 * n1 / (n0 + n1)^2)
## [1] 0.2175
#--------------

#--------------
# ggplot(
#   data.frame(
#     score = score,
#     major = as.factor(HCI$major)
#   ),
#   aes(x = major, y = score)
# ) +
#   geom_point() +
#   ylab("Total score") +
#   xlab("Plan to major in science") +
#   theme_fig()

# # Save plot
# ggsave("figures/chapter3/validity_scatterplot_HCI.png",
#        width = 6, height = 4, dpi = 300, bg = "transparent")
#--------------

#--------------
ggplot(MSclinical, aes(x = rank(MI), y = rank(EDSS))) +
  geom_point() + geom_smooth(method = "lm") +
  theme_fig()
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
# 3.3.2  Student's t-tests
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
  theme_fig() +
  theme(legend.position = "none")
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
(M <- mean(dif))
## [1] 2.3125
# standard deviation of difference
(s <- sd(dif))
## [1] 3.1563
# number of observations
(n <- length(dif))
## [1] 16
# t-value
(t <- M / (s / sqrt(n)))
## [1] 2.9306
# p-value
2 * pt(-abs(t), df = n - 1)
## [1] 0.0103
# confidence interval
M - qt(1 - 0.05 / 2, df = n - 1) * s / sqrt(n)
## [1] 0.6306
M + qt(1 - 0.05 / 2, df = n - 1) * s / sqrt(n)
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
# 3.3.3 ANOVA
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
  geom_density(aes(y = ..density..,
                   color = typeSCH,
                   linetype = typeSCH),
               position = "identity",
               alpha = 0.5,
               size = 1) +
  xlab("Total score on HCI") +
  ylab("Density") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.16)) +
  theme_fig() +
  theme(legend.position = c(0.15, 0.8),
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
# 3.3.4.1  Simple linear regression model
#-----------------------------------------------------------------

#--------------
(b1 = cov(MSclinical$MI, MSclinical$EDSS) / var(MSclinical$MI))
## [1] -0.0159
(b0 = mean(MSclinical$EDSS - b1 * mean(MSclinical$MI)))
## [1] 8.6970  
#--------------

#--------------
lmMS <- lm(EDSS ~ MI, data = MSclinical)
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

#--------------
MSclinical$EDSS - predict(lmMS)
##      2       6      10      14      18      22      25      30 ...
## 1.0285 -0.3169 -0.8415  0.1354  1.0877  0.8970 -0.2287  0.0646 ...
lmMS$residuals
residuals(lmMS)
#      2       6      10      14      18      22      25      30 ...
# 1.0285 -0.3169 -0.8415  0.1354  1.0877  0.8970 -0.2287  0.0646 ...
summary(lmMS$residuals)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -2.047  -0.317   0.065   0.000   0.897   1.120 
#--------------

#--------------
# R2
sum((predict(lmMS) - mean(MSclinical$EDSS))^2) / 
  sum((MSclinical$EDSS - mean(MSclinical$EDSS))^2)
## [1] 0.2136
cor(MSclinical$EDSS, MSclinical$MI)^2
## [1] 0.2136
#--------------

#-----------------------------------------------------------------
# 3.3.4.2  Multiple linear regression model
#-----------------------------------------------------------------

#--------------
lmMSall <- lm(EDSS ~ LCLA + MI + MAS + BBS + T + DD + DM + PRs + 
                KH + NHPT + T25FW + PASAT3, data = MSclinical)
summary(lmMSall)
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  6.61783    5.88387    1.12     0.46
## LCLA         0.07964    0.04994    1.59     0.36
## MI          -0.00735    0.01535   -0.48     0.72
## MAS          0.02468    0.07222    0.34     0.79
## BBS         -0.03290    0.11994   -0.27     0.83
## T            0.27494    0.16062    1.71     0.34
## DD          -0.54930    0.22516   -2.44     0.25
## DM          -0.46387    0.46518   -1.00     0.50
## PRs         -0.26211    0.05238   -5.00     0.13
## KH           0.07203    0.31748    0.23     0.86
## NHPT         0.15995    0.05550    2.88     0.21
## T25FW        0.11935    0.16802    0.71     0.61
## PASAT3       0.05426    0.02758    1.97     0.30
## 
## Residual standard error: 0.417 on 1 degrees of freedom
## (3 observations deleted due to missingness)
## Multiple R-squared:  0.987,	Adjusted R-squared:  0.829 
## F-statistic: 6.27 on 12 and 1 DF,  p-value: 0.303
#--------------

#--------------
lmF <- lm(total ~ gender + major + as.factor(yearc5) +
            minority + EnglishF + typeSCH, data = HCIdata)
anova(lmF)
summary(lmF)
## Call:
## lm(formula = total ~ gender + major + as.factor(yearc5) + minority +
##    EnglishF + typeSCH, data = HCIdata)
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
# 3.4.1  Correlation structure
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
# 3.4.2  Cluster analysis
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

#-----------------------------------------------------------------
# 3.4.3. Factor analysis
# 3.4.3.1 Exploratory factor analysis
#-----------------------------------------------------------------

#--------------
# Single factor model
# with fa() of the psych package
(FA1 <- psych::fa(HCI[, 1:20], cor = "tetrachoric", nfactors = 1, 
                  fm = "ml"))
summary(FA1)
print(FA1$loadings, cutoff = 0)
## Loadings:
##         ML1  
## Item 1  0.444
## Item 2  0.386
## Item 3  0.615
## Item 4  0.199
## Item 5  0.416
## Item 6  0.513
## Item 7  0.152
## Item 8  0.530
## Item 9  0.283
## Item 10 0.419
## Item 11 0.476
## Item 12 0.448
## Item 13 0.565
## Item 14 0.539
## Item 15 0.460
## Item 16 0.552
## Item 17 0.045
## Item 18 0.697
## Item 19 0.607
## Item 20 0.499
## 
##                  ML1
## SS loadings    4.418
## Proportion Var 0.221
#--------------

#--------------
round(FA1$communality, 3)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8 
##  0.197   0.149   0.378   0.039   0.173   0.263   0.023   0.280 
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 
##  0.080   0.175   0.227   0.200   0.319   0.290   0.212   0.305 
## Item 17 Item 18 Item 19 Item 20 
##   0.002   0.485   0.368   0.249 
#--------------

#--------------
FA1$loadings[1]^2
## [1] 0.1973
#--------------

#--------------
round(FA1$uniquenesses, 3)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8 
##  0.803   0.851   0.622   0.961   0.827   0.737   0.977   0.720 
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 
##  0.920   0.825   0.773   0.800   0.681   0.710   0.788   0.695 
## Item 17 Item 18 Item 19 Item 20 
##   0.998   0.515   0.632   0.751 
#--------------

#--------------
sum(FA1$loadings^2)
## [1] 4.418
#--------------

#--------------
## Reproduced correlation
# loadings(FA1) %*% t(loadings(FA1)) transposed vector cross-product
HCI.rcor <- tcrossprod(loadings(FA1)) + diag(FA1$uniquenesses)
round(HCI.rcor, d = 2)
##         Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 ...
## Item 1    1.00   0.17   0.27   0.09   0.18   0.23 ...
## Item 2    0.17   1.00   0.24   0.08   0.16   0.20 ...
## Item 3    0.27   0.24   1.00   0.12   0.26   0.32 ...
## ...
#--------------

#--------------
round(FA1$model, 2)
round(tcrossprod(loadings(FA1)), 2)
var(HCI[,1])
#--------------

#--------------
## Residual matrix
HCI.resid <- corP$rho - HCI.rcor
round(HCI.resid, d = 2)
#--------------

#--------------
(FA1c <- psych::fa(corP$rho, nfactors = 1, fm = "ml", n.obs = 651))
#--------------

#--------------
# with factanal()
(FA1b <- factanal(x = HCI[, 1:20], covmat = corP$rho, factors = 1, 
                 rotation = "none"))
names(FA1b)
#--------------

#-----------------------------------------------------------------
# 3.4.3.1.2 General linear factor model
#-----------------------------------------------------------------

#--------------
data(TestAnxietyCor, package = "ShinyItemAnalysis")
#--------------

#--------------
# FA unrotated:
FA2_tAnxiety <- psych::fa(TestAnxietyCor, nfactors = 2, 
                          n.obs = 335, rotate = "none")
FA2_tAnxiety
## Factor Analysis using method =  minres
## Call: psych::fa(r = TestAnxietyCor, nfactors = 2, n.obs = 335, rotate = "none")
## Standardized loadings (pattern matrix) based upon correlation matrix
##      MR1   MR2   h2   u2 com
## i1  0.62 -0.08 0.39 0.61 1.0
## i2  0.62 -0.17 0.41 0.59 1.1
## i3  0.54  0.24 0.35 0.65 1.4
## i4  0.65  0.09 0.44 0.56 1.0
## i5  0.52  0.49 0.50 0.50 2.0
## ...
#--------------

#--------------
# communalities are sum of squared loadings:
FA2_tAnxiety$communalities
apply(FA2_tAnxiety$loadings^2, 1, sum)
#--------------

#--------------
plot(FA2_tAnxiety, xlim = c(-.5, 1), ylim = c(-.5, 1))

# label unrotated axes
text(x = 0.95, y = -0.05, expression(paste(hat(alpha), "i1")))
text(x = -0.05, y = 0.95, expression(paste(hat(alpha), "i2")))
#--------------

#-----------------------------------------------------------------
# Factor rotation
#-----------------------------------------------------------------
# FA oblimin rotation
FA2_tAnxiety_obl <- psych::fa(TestAnxietyCor, nfactors = 2, 
                              n.obs = 335, rotate = "oblimin")
print(FA2_tAnxiety_obl$loadings, cutoff = 0.4)
## Loadings:
##        MR1    MR2   
## i1   0.565       
## i2   0.662       
## i3          0.473
## i4   0.400     
## i5          0.795
## ...
#--------------

#--------------
FA2_tAnxiety_obl$rot.mat
##         [,1]   [,2]
## [1,]  0.7725 0.3022_obl
## [2,] -1.0996 1.3094  
FA2_tAnxiety$loadings %*% FA2_tAnxiety_obl$rot.mat
#--------------

#--------------
FA2_tAnxiety_obl$rot.mat
solve(FA2_tAnxiety_obl$rot.mat)

# rotated oblique axes
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[1,1]), c(0,solve(FA2_tAnxiety_obl$rot.mat)[1,2]), lty = 3)
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[2,1]), c(0,solve(FA2_tAnxiety_obl$rot.mat)[2,2]), lty = 3)

# label rotated axes
text(x = 0.75, y = 0.6, labels = expression(paste(hat(alpha), "i2r")))
text(x = 0.9, y = - 0.25, labels = expression(paste(hat(alpha), "i1r")))
#--------------

#--------------
# rotated loadings
# points(loadings(FA2_tAnxiety_obl), pch = 16, col = "grey")
plot(FA2_tAnxiety_obl, xlim = c(-.5,1), ylim = c(-.5, 1))
text(x = 0.95, y = -0.05, expression(paste(hat(alpha), "i1r")))
text(x = -0.07, y = 0.95, expression(paste(hat(alpha), "i2r")))
#--------------

#--------------
# with factanal() and GPArotation()
?GPArotation::rotations
(FA2b_tAnxiety <- factanal(covmat = TestAnxietyCor, factors = 2, 
                           rotation = "none", n.obs = 335))
(FA2b_tAnxiety_obl <- factanal(covmat = TestAnxietyCor, factors = 2, 
                               rotation = "oblimin", n.obs = 335))
update(FA2b_tAnxiety, rotation = "oblimin")
#--------------

#-----------------------------------------------------------------
# Factor scores
#-----------------------------------------------------------------

#--------------
# inverse matrix
solve(corP$rho)
# product of inverse and original matrix gives identity matrix as expected
round(solve(corP$rho) %*% corP$rho, 2) 
#------

#------
# factor score coefficients (weights)
(fscore.coef <- solve(corP$rho) %*% FA1$loadings)
##              ML1
## Item 1  0.075394
## Item 2  0.061870
## Item 3  0.134698
## Item 4  0.028183
## ...
# factor scores
FSa <- scale(HCI[,1:20]) %*% fscore.coef
head(FSa, n = 3)
##         ML1
## [1,] 0.8109
## [2,] 1.1817
## [3,] 0.8838
#--------------

#--------------
(FS <- psych::factor.scores(HCI[,1:20], FA1, 
                     rho = corP$rho, method = "Thurstone"))
## $scores
##            ML1
## [1,]  0.810914
## [2,]  1.181697
## [3,]  0.883752
## [4,]  1.224317
## ...
## $weights
##              ML1
## Item 1  0.075394
## Item 2  0.061870
## Item 3  0.134698
## Item 4  0.028183
## ...
#--------------

#--------------
FA1 <- psych::fa(HCI[,1:20], cor = "tet", nfactors = 1, 
                   fm = "ml", scores = "Thurstone")
# ## [1] 1.203982 1.806541 1.331732 1.875942
# seems to give different results?? Somewhat different setting?
FA1$scores[1:4]
plot(FS$scores ~ FA1$scores)
#--------------

#--------------
hist(FS$scores)
plot(FS$scores ~ score)
mean(FS$scores)
sd(FS$scores)
#--------------

#-----------------------------------------------------------------
# Number of factors
#-----------------------------------------------------------------

#--------------
# eigen values of the original cor. matrix
eigen(TestAnxietyCor)$values
##  [1] 8.7790 1.3495 0.9710 0.8880 0.7744 0.7416 0.7062
psych::scree(TestAnxietyCor)
#--------------

#--------------
# eigen values on the common factor solution
FA1_tAnxiety <- psych::fa(TestAnxietyCor, nfactors = 1, n.obs = 335)
FA1_tAnxiety$e.values
## [1] 8.7790 1.3495 0.9710 0.8880 0.7744 0.7416 0.7062 
FA1_tAnxiety$values
eigen(tcrossprod(loadings(FA1_tAnxiety)))$values
eigen(FA1_tAnxiety$model)$values
#--------------
fa.parallel(TestAnxietyCor, n.obs = 335)
## Parallel analysis suggests that the number of factors =  1  
## and the number of components =  1
#--------------

#--------------
fa_parallel(TestAnxietyCor, n_obs = 335, method = "pca")
## The input was recognized as a correlation matrix.
## Assuming 335 observations in the original data.
## According to the parallel analysis, the optimal number of principal components is 1. 
## Following the Kaiser rule, 2 components are recommended.
#--------------

#--------------
VSS(TestAnxietyCor, n.obs = 335)
## Very Simple Structure
## Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
##           n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
## VSS complexity 1 achieves a maximimum of 0.9  with  1  factors
## VSS complexity 2 achieves a maximimum of 0.92  with  2  factors
## 
## The Velicer MAP achieves a minimum of 0.01  with  2  factors 
## BIC achieves a minimum of  -590  with  2  factors
## Sample Size adjusted BIC achieves a minimum of  -132.5  with  4  factors
#--------------

#-----------------------------------------------------------------
# 3.4.3.2. Confirmatory factor analysis
#-----------------------------------------------------------------

#--------------
data(BFI2, package = "ShinyItemAnalysis")
head(BFI2, n = 2)
summary(BFI2)
#--------------

#--------------
model_EN <- 'E =~ i1 + i6 + i11 + i16 + i21 + i26 + 
                  i31 + i36 + i41 + i46 + i51 + i56
             N =~ i4 + i9 + i14 + i19 + i24 + i29 + 
                  i34 + i39 + i44 + i49 + i54 + i59'
fit_EN <- lavaan::cfa(model_EN, data = BFI2)
#--------------

#--------------
lavaan::parTable(fit_EN)
##    id lhs op rhs user block group free ustart exo label plabel start ...
## 1   1   E =~  i1    1     1     1    0      1   0         .p1. 1.000 ...
## 2   2   E =~  i6    1     1     1    1     NA   0         .p2. 0.813 ...
## ...
## 51 51   E ~~   N    0     1     1   49     NA   0        .p51. 0.000 ...
lavaan::summary(fit_EN)
lavaan::summary(fit_EN, fit.measures = TRUE, standardized = TRUE)
#--------------

#--------------
lavaan::parameterEstimates(fit_EN)
##    lhs op rhs    est    se       z pvalue ci.lower ci.upper
## 1    E =~  i1  1.000 0.000      NA     NA    1.000    1.000
## 2    E =~  i6  0.969 0.041  23.611      0    0.889    1.049
## ...
## 13   N =~  i4  1.000 0.000      NA     NA    1.000    1.000
## 14   N =~  i9  0.833 0.039  21.509      0    0.757    0.909
## ...
## 25  i1 ~~  i1  0.617 0.023  26.337      0    0.571    0.663
## 26  i6 ~~  i6  0.647 0.024  26.664      0    0.599    0.694
## ...
## 49   E ~~   E  0.489 0.033  14.870      0    0.425    0.554
## 50   N ~~   N  0.587 0.039  15.191      0    0.512    0.663
## 51   E ~~   N -0.196 0.017 -11.514      0   -0.229   -0.162

lavaan::parameterEstimates(fit_EN, ci = FALSE, standardized = TRUE)
##    lhs op rhs    est    se       z pvalue std.lv std.all std.nox
## 1    E =~  i1  1.000 0.000      NA     NA  0.699   0.665   0.665
## 2    E =~  i6  0.969 0.041  23.611      0  0.678   0.644   0.644
## 3    E =~ i11  0.470 0.041  11.423      0  0.329   0.296   0.296
## 4    E =~ i16  1.420 0.052  27.066      0  0.993   0.757   0.757
## 5    E =~ i21  1.150 0.047  24.647      0  0.804   0.677   0.677
## 6    E =~ i26  0.510 0.034  14.790      0  0.357   0.387   0.387
## 7    E =~ i31  1.265 0.050  25.257      0  0.885   0.697   0.697
## 8    E =~ i36  0.547 0.039  14.137      0  0.382   0.369   0.369
## 9    E =~ i41  0.813 0.039  20.583      0  0.568   0.552   0.552
## 10   E =~ i46  1.126 0.045  25.099      0  0.787   0.692   0.692
## 11   E =~ i51  1.052 0.045  23.559      0  0.736   0.643   0.643
## 12   E =~ i56  0.669 0.036  18.468      0  0.468   0.491   0.491
## 13   N =~  i4  1.000 0.000      NA     NA  0.766   0.672   0.672
## 14   N =~  i9  0.833 0.039  21.509      0  0.639   0.569   0.569
## 15   N =~ i14  1.069 0.044  24.397      0  0.819   0.653   0.653
## 16   N =~ i19  0.817 0.037  22.095      0  0.626   0.585   0.585
## 17   N =~ i24  0.807 0.039  20.685      0  0.618   0.545   0.545
## 18   N =~ i29  1.171 0.042  27.874      0  0.898   0.761   0.761
## 19   N =~ i34  0.828 0.038  21.852      0  0.634   0.578   0.578
## 20   N =~ i39  1.155 0.043  26.963      0  0.885   0.732   0.732
## 21   N =~ i44  0.730 0.038  19.372      0  0.560   0.508   0.508
## 22   N =~ i49  0.836 0.039  21.611      0  0.641   0.571   0.571
## 23   N =~ i54  1.176 0.044  26.900      0  0.901   0.730   0.730
## 24   N =~ i59  0.974 0.041  23.968      0  0.747   0.641   0.641
#--------------

#--------------
model_ENs <- 'E =~ NA*i1 + i6 + i11 + i16 + i21 + i26 + 
                   i31 + i36 + i41 + i46 + i51 + i56
              N =~ NA*i4 + i9 + i14 + i19 + i24 + i29 + 
                   i34 + i39 + i44 + i49 + i54 + i59
              E ~~ 1*E
              N ~~ 1*N'
fit_ENs <- lavaan::cfa(model_ENs, data = BFI2)
lavaan::parameterEstimates(fit_ENs, ci = FALSE, standardized = TRUE)
##    lhs op rhs    est    se       z pvalue std.lv std.all std.nox
## 1    E =~  i1  0.699 0.024  29.740      0  0.699   0.665   0.665
## 2    E =~  i6  0.678 0.024  28.552      0  0.678   0.644   0.644
## ...
#--------------

#--------------
lavaan::inspect(fit_EN)
## $lambda
##      E  N
## i1   0  0
## i6   1  0
## i11  2  0
## ...
## $theta
##     i1 i6 i11 i16 i21 i26 i31 i36 i41 i46 i51 i56 i4 i9 i14 i19 i24 i29 i34 i39 i44 i49 i54 i59
## i1  23                                                                                         
## i6   0 24                                                                                      
## i11  0  0 25
## ...
## $psi
##    E  N 
## E 47   
## N 49 48
lavaan::lavInspect(fit_EN, what = "est")$theta
lavaan::lavInspect(fit_EN, what = "est")$lambda
lavaan::lavInspect(fit_EN, what = "std")$lambda
lavaan::lavInspect(fit_EN, what = "est")$psi
lavaan::lavInspect(fit_EN, what = "std")$psi
#--------------

#--------------
psych::lavaan.diagram(fit_ENs)
semPlot::semPaths(fit_EN, what = "stdest", rotation = 4)
semPlot::semPaths(fit_ENs, what = "est", rotation = 4)
#--------------

#--------------
FS <- lavaan::predict(fit_EN)
head(FS, n = 3)
##            E       N
## [1,]  0.5944  0.2344
## [2,]  0.6298 -0.6944
## [3,] -1.4920  1.6955
#--------------

#-----------------------------------------------------------------
# Hierarchical CFA
#-----------------------------------------------------------------

#--------------
model_EN_hier <- 'Escb =~ i1 + i16 + i31 + i46
                  Easr =~ i6 + i21 + i36 + i51
                  Eenl =~ i11 + i26 + i41 + i56
                  Nanx =~ i4 + i19 + i34 + i49
                  Ndep =~ i9 + i24 + i39 + i54
                  Nemt =~ i14 + i29 + i44 + i59
                  E =~ Escb + Easr + Eenl
                  N =~ Nanx + Ndep + Nemt'
fit_EN_hier <- lavaan::cfa(model_EN_hier, data = BFI2)
#--------------

#--------------
lavaan::summary(fit_EN_hier, fit.measures = TRUE, standardized = TRUE)
lavaan::parTable(fit_EN_hier)
lavaan::parameterEstimates(fit_EN_hier)
semPlot::semPaths(fit_EN_hier, what = "std.est", rotation = 4)
#--------------

#--------------
FSh <- lavaan::predict(fit_EN_hier)
head(FSh, n = 3)
##         Escb    Easr    Eenl    Nanx    Ndep    Nemt       E       N
## [1,]  0.4401  0.6603  0.5546 -0.1261 -0.1289  0.8747  0.5131  0.0041
## [2,]  0.7308  0.4984  0.3823 -0.5643 -0.7703 -0.5638  0.6025 -0.6832
## [3,] -1.5153 -1.4035 -0.5333  1.7516  1.3825  1.4776 -1.2671  1.6671
#--------------

#--------------
lavaan::fitMeasures(fit_EN, c("cfi", "tli", "rmsea", "bic"))
##   cfi        tli      rmsea        bic 
## 0.778      0.756      0.092 114975.452 
lavaan::fitMeasures(fit_EN_hier, c("cfi", "tli", "rmsea", "bic"))
##   cfi        tli      rmsea        bic 
## 0.880      0.865      0.069 113303.631 
#--------------
