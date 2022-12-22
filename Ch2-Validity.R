#-----------------------------------------------------------------
# Chapter 2 - Validity
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
# 2.3.1  Inference based on ratios
#-----------------------------------------------------------------

#-------------- 
Y <- c(34, 20, 34, 26, 27, 17, 22)
n <- 37
(p <- Y/n)
## [1] 0.9189 0.5405 0.9189 0.7027 0.7297 0.4595 0.5946
#-------------- 

#-------------- 
(Z <- (Y - n / 2) / (sqrt(n) / 2))
## [1]  5.0964  0.4932  5.0964  2.4660  2.7948 -0.4932  1.1508
qnorm(1 - 0.05)
## [1] 1.6449
#-------------- 

#-------------- 
# not shown in the book
(1 - pnorm(Z)) / 2 # p value 
pnorm(Z, lower.tail = FALSE)
# lower bound of one-sided confidence interval
p - qnorm(1 - 0.05) * sqrt(p * (1 - p) / n)
## [1] 0.8451 0.4058 0.8451 0.5791 0.6096 0.3247 0.4618
#-------------- 

#-------------- 
Z^2
## [1] 25.9730  0.2432 25.9730  6.0811  7.8108  0.2432  1.3243
#-------------- 

#-------------- 
# not shown in the book
# p value:
(1 - pchisq(Z^2, df = 1)) # p value (alternative: greater)
## [1] 3.462e-07 6.219e-01 3.462e-07 1.366e-02 5.193e-03 6.219e-01 2.498e-01
(1 - pchisq(Z^2, df = 1)) / 2 # p value (alternative: two sided)
## [1] 1.731e-07 3.109e-01 1.731e-07 6.832e-03 2.597e-03 3.109e-01 1.249e-01 
#--------------

#--------------
prop.test(x = 34, n = 37, alternative = "greater", correct = FALSE)
## 1-sample proportions test without continuity correction
## 
## data:  34 out of 37, null probability 0.5
## X-squared = 25.973, df = 1, p-value = 2e-07
## alternative hypothesis: true p is greater than 0.5
## 95 percent confidence interval:
##   0.8136 1.0000
## sample estimates:
##   p 
## 0.9189 
#--------------

#--------------
# not shown in the book
prop.test(x = 34, n = 37, alternative = "two.sided", correct = FALSE)
##         1-sample proportions test without continuity correction
##
## data:  34 out of 37, null probability 0.5
## X-squared = 25.973, df = 1, p-value = 1.731e-07
## alternative hypothesis: true p is not equal to 0.5
## 95 percent confidence interval:
##  0.787 0.972
## sample estimates:
##      p
## 0.9189
#--------------

#--------------
proptests <- lapply(Y, prop.test, n = n, alternative = "greater", correct = FALSE)
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
# 2.3.2  t-tests
#-----------------------------------------------------------------

#--------------
data(HCIprepost, package = "ShinyItemAnalysis")
library(ggplot2)
ggplot(data.frame(
  score = c(HCIprepost$score.pre, HCIprepost$score.post),
  group = factor(rep(c("Pre", "Post"), each = 16),
                 levels = c("Pre", "Post"))),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() + ylab("Total score") + xlab("") +
  theme_fig() + theme(legend.position = "none")
#--------------

#--------------
ggplot(data.frame(score = HCIprepost$score.post - HCIprepost$score.pre,
                  group = factor(rep(c("Post-Pre"), each = 16), 
                                 levels = "Post-Pre")),
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
(barX <- mean(dif))          # mean difference
## [1] 2.3125
(sX <- sd(dif))              # standard deviation of difference
## [1] 3.1563
(n <- length(dif))           # number of observations
## [1] 16
(t <- barX / (sX / sqrt(n))) # t-value
## [1] 2.9306
2 * pt(-abs(t), df = n - 1)  # p-value
## [1] 0.0103
# confidence interval
barX - qt(1 - 0.05 / 2, df = n - 1) * sX / sqrt(n)
## [1] 0.6306
barX + qt(1 - 0.05 / 2, df = n - 1) * sX / sqrt(n)
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

#-----------------------------------------------------------------
# 2.3.3 Two sample t test
#-----------------------------------------------------------------

#--------------
data(HCIgrads, package = "ShinyItemAnalysis")
data(HCI, package = "ShinyItemAnalysis")
score_grads <- HCIgrads$total
score_undergrads <- HCI$total
#--------------

#--------------
# two sample t-test
t.test(score_grads, score_undergrads, alternative = "greater")
##         Welch Two Sample t-test
##
## data:  score_grads and score_undergrads
## t = 2.1889, df = 9.3448, p-value = 0.0276
## alternative hypothesis: true difference in means is greater than 0
## 95 percent confidence interval:
##  0.3800  Inf
## sample estimates:
## mean of x   mean of y
##   14.5000     12.2120
#--------------

#--------------
# mean and sd for groups (code not shown in the book)
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
# boxplots (code not shown in the book)
df <- data.frame(
  score = c(score_grads, score_undergrads),
  group = as.factor(c(rep("Graduate", length(score_grads)),
                      rep("Undergraduate", length(score_undergrads))))
)

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
# 2.3.4 More samples - ANOVA
#-----------------------------------------------------------------

#--------------
data(HCIdata, package = "ShinyItemAnalysis")
#--------------

#--------------
# boxplots (code not shown in the book)
ggplot(HCIdata, aes(x = typeSCH, y = total, fill = typeSCH)) +
  geom_boxplot() +
  xlab("") + ylab("Total score") +
  theme_fig() +
  theme(legend.position = "none")
#--------------

#--------------
# density plot (code not shown in the book)
ggplot(HCIdata, aes(total, fill = typeSCH)) +
  geom_density(aes(y = ..density.., color = typeSCH,
                   linetype = typeSCH),
               position = "identity", alpha = 0.5, linewidth = 1) +
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
# 2.3.5  Correlation coefficients
#-----------------------------------------------------------------

#--------------
data("HeightInventory", package = "ShinyItemAnalysis")
summary(HeightInventory) # code not shown in the book
HI <- na.omit(HeightInventory)
#--------------

#--------------
HI$total <- rowSums(HI[, 1:26])
summary(HI$total)
##    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 26.0000  54.0000  67.0000  65.6517  78.0000 104.0000 
summary(HI$HeightCM)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 145.0000 165.0000 171.0000 172.2278 179.0000 207.0000 
#--------------

#--------------
ggplot(HI, aes(x = total, y = HeightCM)) +
  geom_point() + theme_fig() +
  xlab("HI score") + ylab("Height [cm]")
#--------------

#--------------
cor(HI$total, HI$HeightCM)
## [1] 0.8728
cor.test(HI$total, HI$HeightCM)
##         Pearson's product-moment correlation
## 
## data:  HI$total and HI$HeightCM
## t = 120, df = 4497, p-value <2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##   0.8656 0.8795
## sample estimates:
##   cor 
## 0.8728 
#--------------

#--------------
HI$GenderM <- 1 * (HI$Gender == "M")
# (code not shown in the book)
summary(HI$Gender)
str(HI$Gender)
summary(HI$GenderM)
str(HI$GenderM)
#--------------

#--------------
cor.test(HI$total, HI$GenderM)
##         Pearson's product-moment correlation
##
## data:  HI$total and HI$GenderM
## t = 29.093, df = 4497, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## 0.3731 0.4223
## sample estimates:
##   cor
## 0.3980
#--------------

#--------------
barX0 <- mean(HI[HI$GenderM == 0, "total"])
barX1 <- mean(HI[HI$GenderM == 1, "total"])
n <- nrow(HI)
n0 <- sum(HI$GenderM == 0)
n1 <- sum(HI$GenderM == 1)
sn <- sd(HI$total) * sqrt((n - 1) / n)
(barX1 - barX0) / sn * sqrt(n0 * n1 / (n0 + n1)^2)
## [1] 0.3980
#--------------

#--------------
# Spearman's correlation (not shown in the book)
cor.test(HI$total, HI$HeightCM, method = "spearman")
## 	Spearman's rank correlation rho
## data:  HI$total and HI$HeightCM
## S = 1.64e+09, p-value <2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##   rho 
## 0.89163 
ggplot(HI, aes(x = rank(total), y = rank(HeightCM))) +
  geom_point() + theme_fig()
#--------------

#--------------
# Kendall's correlation (not shown in the book)
cor.test(HI$total, HI$HeightCM, method = "kendal")
## 		Kendall's rank correlation tau
## 
## data:  HI$total and HI$HeightCM
## z = 71.3, p-value <2e-16
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##   tau 
## 0.7263 
#--------------

#-----------------------------------------------------------------
# 2.3.6  Regression models
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 2.3.6.1 Simple linear regression
#-----------------------------------------------------------------

#--------------
(b1 <- cov(HI$total, HI$HeightCM) / var(HI$total))
## [1] 0.5019
(b0 <- mean(HI$HeightCM) - b1 * mean(HI$total))
## [1] 139.2778  
#--------------

#--------------
ggplot(HI, aes(x = total, y = HeightCM)) + geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +
  xlab("HI score") + ylab("Height [cm]") + theme_fig()
#--------------

#--------------
lmHI <- lm(HeightCM ~ total, data = HI)
summary(lmHI)
## Call:
## lm(formula = HeightCM ~ total, data = HI)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.3875  -3.3989  -0.4026   3.1219  22.0576 
##
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.39e+02   2.84e-01     491   <2e-16 ***
## total       5.02e-01   4.19e-03     120   <2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 4.7299 on 4497 degrees of freedom
## Multiple R-squared:  0.7617,	Adjusted R-squared:  0.7617
## F-statistic: 14374 on 1 and 4497 DF,  p-value: <2.22e-16
#--------------

#--------------
lmHI$residuals[1:20]
##       3        5        9       10       11       12       14   ... 
## -6.3724  -0.8894  -3.8818   8.6579  -2.4026  -1.4556  -3.3686   ... 
#--------------

#--------------
# alternative calulation (code not shown in the book)
HI$HeightCM - predict(lmHI)
##       3        5        9       10       11       12       14   ... 
## -6.3724  -0.8894  -3.8818   8.6579  -2.4026  -1.4556  -3.3686   ... 

summary(residuals(lmHI))
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -22.3875  -3.3989  -0.4026   0.0000   3.1219  22.0576 
#--------------

#--------------
# ANOVA table by hand (code not shown in the book)
(SSR <- sum((predict(lmHI) - mean(HI$HeightCM))^2))
## [1] 321583
(SST <- sum((HI$HeightCM - mean(HI$HeightCM))^2))
## [1] 422189
(SSE <- SST - SSR)
## [1] 100607
#--------------

#--------------
# R^2 calculated by hand (code not shown in the book)
SSR / SST # R^22
## [1] 0.7617
#--------------

#--------------
# F calculated by hand (code not shown in the book)
SSR / (SST / (nrow(HI) - 2))
## [1] 0.7617
#--------------

#--------------
# calculation of t by hand (not shown in the book)
# compare with output of summary(lmHI)

# residual standard error
(S <- sqrt(SSE / (nrow(HI) - 2)))
## [1] 4.7299

# std. error
(seBeta1 <- S / sqrt(sum((HI$total - mean(HI$total))^2)))
## [1] 0.0042  

# t value
b1 / seBeta1  
## [1] 119.8931

# df
nrow(HI) - 2
## [1] 4497
#--------------

#--------------
cor(HI$HeightCM, HI$total)^2
## [1] 0.7617
#--------------

#--------------
anova(lmHI)
## Analysis of Variance Table
## 
## Response: HeightCM
##             Df Sum Sq Mean Sq F value Pr(>F)    
## total        1 321583  321583   14374 <2e-16 ***
## Residuals 4497 100607      22                   
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#--------------

#--------------
# Checking model assumptions (diagnostic plots)
plot(lmHI$residuals ~ predict(lmHI), ylab = "Residuals")
abline(h = 0, lty = "dotted")

qqnorm(lmHI$residuals)
qqline(lmHI$residuals, lty = "dotted")
#--------------

#--------------
# more diagnostic plots (code not shown in the book)
# plot(lmHI)
plot(lmHI, which = 1)
plot(lmHI, which = 2)
plot(lmHI, which = 3)
plot(lmHI, which = 4)
#--------------

#-----------------------------------------------------------------
# 2.3.6.2 Multiple linear regression model
#-----------------------------------------------------------------

lmF <- lm(total ~ gender + major + factor(yearc5) + minority + EnglishF + typeSCH, 
          data = HCIdata)
anova(lmF)
## Analysis of Variance Table
## 
## Response: total
##                    Df    Sum Sq  Mean Sq F value     Pr(>F)    
## gender              2  260.7217 130.3608 13.2054 2.3855e-06 ***
## major               1  393.8370 393.8370 39.8952 4.9471e-10 ***
## factor(yearc5)      4  647.2713 161.8178 16.3920 8.2999e-13 ***
## minority            2  529.8117 264.9059 26.8347 6.2913e-12 ***
## EnglishF            1  319.7232 319.7232 32.3876 1.9058e-08 ***
## typeSCH             3  291.5606  97.1869  9.8449 2.3389e-06 ***
## Residuals         655 6466.0192   9.8718                        
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#--------------

#--------------
summary(lmF)
## Call:
## lm(formula = total ~ gender + major + factor(yearc5) + minority + 
##     EnglishF + typeSCH, data = HCIdata)
##
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.8708 -2.0195  0.1084  2.1314  7.3625 
##
## Coefficients:
##                   Estimate Std. Error t value  Pr(>|t|)    
## (Intercept)        10.9021     0.5322 20.4849 < 2.2e-16 ***
## genderF            -0.8086     0.2597 -3.1132    0.0019 ** 
## gendernone         -2.2278     0.8311 -2.6805    0.0075 ** 
## major               1.2866     0.2657  4.8424 1.602e-06 ***
## factor(yearc5)2     2.0424     0.5102  4.0034 6.959e-05 ***
## factor(yearc5)3     1.1911     0.4951  2.4058    0.0164 *  
## factor(yearc5)4     1.7943     0.5221  3.4366    0.0006 ***
## factor(yearc5)5     3.7029     0.5893  6.2839 6.024e-10 ***
## minoritymin        -1.5932     0.3258 -4.8905 1.267e-06 ***
## minoritynone       -2.2996     0.6276 -3.6642    0.0003 ***
## EnglishFno         -1.4160     0.3147 -4.4993 8.066e-06 ***
## typeSCHBCAS         0.2575     0.4056  0.6347    0.5258    
## typeSCHR1           0.7350     0.3680  1.9974    0.0462 *  
## typeSCHMCU         -1.3028     0.4205 -3.0986    0.0020 ** 
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 3.14 on 655 degrees of freedom
## Multiple R-squared:  0.274,	Adjusted R-squared:  0.26
## F-statistic:   19 on 13 and 655 DF,  p-value: <2e-16
#--------------

#-----------------------------------------------------------------
# 2.4.2 Model selection, model fit
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 2.4.2.1 Likelihood-ratio test
#-----------------------------------------------------------------

#--------------
lmFrestr <- lm(total ~ gender + factor(yearc5) + minority + EnglishF + typeSCH, 
               data = HCIdata)
anova(lmFrestr, lmF, test = "Chisq")
## Analysis of Variance Table
## 
## Model 1: total ~ gender + factor(yearc5) + minority + EnglishF + typeSCH
## Model 2: total ~ gender + major + factor(yearc5) + minority + EnglishF + typeSCH
##   Res.Df  RSS Df Sum of Sq Pr(>Chi)    
## 1    656 6698                          
## 2    655 6466  1       232  1.3e-06 ***
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#--------------

#--------------
# LRT with lrtest() function
library(lmtest)
lrtest(lmFrestr, lmF)
## Likelihood ratio test
##
## Model 1: total ~ gender + factor(yearc5) + minority + EnglishF + typeSCH
## Model 2: total ~ gender + major + factor(yearc5) + minority + EnglishF + typeSCH
##   #Df LogLik Df Chisq Pr(>Chisq)    
## 1  14  -1720                        
## 2  15  -1708  1  23.5    1.2e-06 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#--------------

#--------------
# LRT calculation by hand (code not shown in the book)
logLik.diff <- logLik(lmFrestr) - logLik(lmF)
df.diff <- lmFrestr$df.residual - lmF$df.residual
pchisq(as.numeric(logLik.diff) * (-2), df = df.diff, lower.tail = FALSE)
## [1] 1.229e-06
#--------------

#-----------------------------------------------------------------
# 2.4.2.2 Akaike information criterion
#-----------------------------------------------------------------

#--------------
AIC(lmF, lmFrestr)
##          df       AIC
## lmF      15 3446.1876
## lmFrestr 14 3467.7192
#--------------

#-----------------------------------------------------------------
# 2.4.2.1 Bayesian information criterion
#-----------------------------------------------------------------

#--------------
BIC(lmF, lmFrestr)
##          df       BIC
## lmF      15 3513.7743
## lmFrestr 14 3530.8001
#--------------

#-----------------------------------------------------------------
# 2.4.3  Correction for range restriction
#-----------------------------------------------------------------

#--------------
set.seed(987)
x <- rnorm(1500); e <- rnorm(1500); r <- 0.729
b1 <- r / sqrt(1 - r^2)
y <- b1 * x + e
cor(x, y)
## [1] 0.7236

xvar <- x * 10 + 50
yvar <- y * 2.3 + 20
cor(xvar, yvar)
## [1] 0.7236
plot(x, y)
#-------------- 

#-------------- 
df <- data.frame(xvar, yvar)
# only admitted students with X > 58
admitted <- subset(df, xvar > 58)
cor(admitted$xvar, admitted$yvar)
## [1] 0.3633
#-------------- 

#-------------- 
# code not shown in the book:
# Plot a: All observations, r = 0.72
(scatter_all <- ggplot(df) + 
   geom_point(aes(x = xvar, y = yvar),
              size = 1.8, alpha = 0.5, shape = 19) +
   theme_fig() + xlab("X") + ylab("Y") +
   scale_x_continuous(limits = c(25, 75)) +
   scale_y_continuous(limits = c(5, 31))
)
#-------------- 

#--------------
psych::rangeCorrection(r = cor(admitted$xvar, admitted$yvar),
                       sdu = sd(xvar), sdr = sd(admitted$xvar))
## [1] 0.6548
#--------------

#--------------
# code not shown in the book:
# Plot b: Only those with high scores (admitted), r = 0.65
(scatter_admitted <- ggplot(admitted) +
   geom_point(aes(x = xvar, y = yvar),
              size = 1.8, alpha = 0.5, shape = 19) +
   theme_fig() +
   xlab("X") +
   ylab("Y") +
   scale_x_continuous(limits = c(25, 75)) +
   scale_y_continuous(limits = c(5, 31))
)
#--------------

#-------------- 
# Only those above 58 or bellow 43
admitted2 <- subset(df, xvar > 58 | xvar < 43)
cor(admitted2$xvar, admitted2$yvar)
## [1] 0.8128

psych::rangeCorrection(r = cor(admitted2$xvar, admitted2$yvar),
                       sdu = sd(xvar), sdr = sd(admitted2$xvar))
## [1] 0.7097
#-------------- 

#-------------- 
# code not shown in the book:
# Plot c: Only those with low and high scores, r = 0.71
(scatter_second <- ggplot(admitted2) +
   geom_point(aes(x = xvar, y = yvar),
              size = 1.8,
              alpha = 0.5,
              shape = 19
   ) +
   theme_fig() +
   xlab("X") +
   ylab("Y") +
   scale_x_continuous(limits = c(25, 75)) +
   scale_y_continuous(limits = c(5, 31))
)
#-------------- 

#-----------------------------------------------------------------
# 2.5 Validity in interactive application
#-----------------------------------------------------------------

ShinyItemAnalysis::run_app()
