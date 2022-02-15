#-----------------------------------------------------------------
# Chapter 8 - Differential item functioning
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(aod)
library(deltaPlotR)
library(DFIT)
# remotes::install_github("adelahladka/difNLR")
library(difNLR)
library(difR)
library(ggdendro)
library(ggplot2)
library(lavaan)
library(lordif)
library(ltm)
library(mirt)
library(semPlot)
library(ShinyItemAnalysis)

library(cowplot)
library(Cairo)
# remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

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

theme_fig_title <- function(base_size = 17, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_rect(fill = "white", colour = NA),
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      # plot.title = element_blank(),
      legend.background = element_blank()
    )
}

#-----------------------------------------------------------------
# 8.3.1 Delta method
#-----------------------------------------------------------------

#--------------
# loading data
data(MSATB, package = "difNLR")
head(MSATB, n = 2)
##   Item49 Item27 Item41 ...
## 1      1      0      0 ...
## 2      1      0      1 ...
## ...
#--------------

#--------------
# calculating proportions of correct answer per group
(pi0 <- colMeans(MSATB[MSATB$gender == 0, -21]))
## Item49 Item27 Item41 ...
## 0.8161 0.2335 0.3843 ...
(pi1 <- colMeans(MSATB[MSATB$gender == 1, -21]))
## Item49 Item27 Item41 ...
## 0.8776 0.2470 0.3803 ...
#--------------

#--------------
# calculation of standard normal quantiles
(z0 <- qnorm(1 - pi0))
##  Item49  Item27  Item41 ...
## -0.9007  0.7275  0.2942 ...
(z1 <- qnorm(1 - pi1))
##  Item49  Item27  Item41 ...
## -1.1629  0.6839  0.3047 ...

# transformation into delta scores
(delta0 <- 4 * z0 + 13)
## Item49  Item27  Item41 ...
## 9.3974 15.9099 14.1769 ...
(delta1 <- 4 * z1 + 13)
## Item49  Item27  Item41 ...
## 8.3482 15.7356 14.2190 ...
#--------------

#--------------
s0 <- sd(delta0) # SD of delta scores - males
s1 <- sd(delta1) # SD of delta scores - females
s01 <- cov(delta0, delta1) # covariance of delta scores
mean_delta0 <- mean(delta0) # mean of delta scores - males
mean_delta1 <- mean(delta1) # mean of delta scores - females

# calculation of parameters a and b of major axis
(b <- (s1^2 - s0^2 + sqrt((s1^2 - s0^2)^2 + 4 * s01^2)) / (2 * s01))
## [1] 0.9784
(a <- mean_delta1 - b * mean_delta0)
## [1] 0.3787
#--------------

#--------------
# calculation of distances of delta scores from major axis
(D <- (b * delta0 + a - delta1) / (sqrt(b^2 + 1)))
## Item49 Item27 Item41 ...
## 0.8753 0.1493 0.0214 ...
#--------------

#--------------
# delta plot using fixed threshold
(DP_fixed <- deltaPlotR::deltaPlot(data = MSATB, group = "gender",
                                   focal.name = 1, thr = 1.5))
## ...
##        Prop.Ref Prop.Foc Delta.Ref Delta.Foc   Dist.
## Item1    0.8161   0.8776    9.3974    8.3482  0.8753
## Item2    0.2335   0.2470   15.9099   15.7356  0.1493
## Item3    0.3843   0.3803   14.1769   14.2190  0.0214
## ...
## Code: '***' if item is flagged as DIF
## Parameters of the major axis:
##      a     b
##  0.379 0.978
## ...
#--------------

#--------------
# delta plot using normal approximation threshold
(DP_norm <- deltaPlotR::deltaPlot(data = MSATB, group = "gender",
                                  focal.name = 1, thr = "norm"))
## ...
##        Prop.Ref Prop.Foc Delta.Ref Delta.Foc   Dist.
## Item1    0.8161   0.8776    9.3974    8.3482  0.8753 ***
## Item2    0.2335   0.2470   15.9099   15.7356  0.1493
## Item3    0.3843   0.3803   14.1769   14.2190  0.0214
## ...
## Code: '***' if item is flagged as DIF
##
## Parameters of the major axis:
##      a     b
##  0.379 0.978
##
## Detection threshold: 0.606 (significance level: 5%)
## Items detected as DIF items:
## Item1
## ...
#--------------

#--------------
# diagonal plots
deltaPlotR::diagPlot(DP_fixed, thr.draw = TRUE)
deltaPlotR::diagPlot(DP_norm, thr.draw = TRUE)
#--------------

#-----------------------------------------------------------------
# 8.3.2 Mantel-Haenszel test
#-----------------------------------------------------------------

#--------------
score <- rowSums(MSATB[, 1:20]) # total score
MSATB$Item49 <- factor(MSATB$Item49, levels = c(1, 0))

# contingency table for item 49 and score 5
(tab1 <- table(MSATB[score == 5, c("gender", "Item49")]))
##       Item49
## gender  1  0
##      0 10  8
##      1 23  9
# odds ratio in contingency table above
n_item49_01_5 <- tab1[1, 1]
n_item49_00_5 <- tab1[1, 2]
n_item49_11_5 <- tab1[2, 1]
n_item49_10_5 <- tab1[2, 2]
(n_item49_01_5 * n_item49_10_5) / (n_item49_00_5 * n_item49_11_5)
## [1] 0.4891 

# contingency table for item 49 and score 12
(tab2 <- table(MSATB[score == 12, c("gender", "Item49")]))
##       Item49
## gender  1  0
##      0 30  6
##      1 86  8
# odds ratio in contingency table above
n_item49_01_12 <- tab2[1, 1]
n_item49_00_12 <- tab2[1, 2]
n_item49_11_12 <- tab2[2, 1]
n_item49_10_12 <- tab2[2, 2]
(n_item49_01_12 * n_item49_10_12) / (n_item49_00_12 * n_item49_11_12)
## [1] 0.9701
#--------------

#--------------
# contingency table for item 1 and all levels of total score
tabs <- xtabs(~ gender + Item49 + score, data = MSATB)

n_item49_01 <- sapply(1:dim(tabs)[3], function(i) tabs[1, 1, i])
n_item49_00 <- sapply(1:dim(tabs)[3], function(i) tabs[1, 2, i])
n_item49_11 <- sapply(1:dim(tabs)[3], function(i) tabs[2, 1, i])
n_item49_10 <- sapply(1:dim(tabs)[3], function(i) tabs[2, 2, i])
n_item49 <- n_item49_01 + n_item49_00 + n_item49_11 + n_item49_10

# alphaMH
(alphaMH <- sum(n_item49_01 * n_item49_10 / n_item49) /
  sum(n_item49_00 * n_item49_11 / n_item49))
## [1] 0.5430
#--------------

#--------------
# deltaMH
-2.35 * log(alphaMH)
## [1] 1.4352
#--------------

#--------------
n_item49_R <- n_item49_01 + n_item49_00 # reference group
n_item49_F <- n_item49_11 + n_item49_10 # focal group
n_item49_1 <- n_item49_01 + n_item49_11 # correct answers
n_item49_0 <- n_item49_00 + n_item49_10 # incorrect answers

# MH test statistic
(MHstat <- (abs(sum(n_item49_01 - n_item49_R * n_item49_1 / n_item49))
- 0.5)^2 /
  sum((n_item49_R * n_item49_F * n_item49_1 * n_item49_0) /
    (n_item49^2 * (n_item49 - 1))))
## [1] 12.4456
# critical value on 0.05 significance level
qchisq(p = 0.95, df = 1)
## [1] 3.8415
# p-value
(pvalue <- 1 - pchisq(MHstat, df = 1))
## [1] 0.0004

MSATB$Item49 <- as.numeric(paste(MSATB$Item49))
#--------------

#--------------
difR::difMH(Data = MSATB, group = "gender", focal.name = 1)
## ...
##        Stat.   P-value
## Item49 12.4456  0.0004 ***
## Item27  0.9159  0.3386
## ...
## Item68  5.0871  0.0241 *
## ...
## Items detected as DIF items:
## Item49
## Item68
##
## Effect size (ETS Delta scale):
## Effect size code:
## 'A': negligible effect
## 'B': moderate effect
## 'C': large effect
##
##        alphaMH deltaMH
## Item49  0.5430  1.4352 B
## Item27  0.8546  0.3693 A
## ...
## Item68  1.3659 -0.7328 A
## ...
## Effect size codes: 0 'A' 1.0 'B' 1.5 'C'
## (for absolute values of 'deltaMH')
## ...
#--------------

#-----------------------------------------------------------------
# 8.3.3 SIBTEST
#-----------------------------------------------------------------

#--------------
difR::difSIBTEST(MSATB, group = "gender", focal.name = 1)
## ...
##           Beta      SE X2 Stat. P-value
## Item49 -0.0871  0.0231 14.1465   0.0002 ***
## Item27 -0.0179  0.0230  0.6073   0.4358
## Item41 -0.0053  0.0246  0.0471   0.8282
## Item7   0.0356  0.0261  1.8647   0.1721
## Item38 -0.0321  0.0266  1.4620   0.2266
## ...
## Item2  -0.0213  0.0238  0.8000   0.3711
## ...
## Detection threshold: 3.841 (significance level: 0.05)
## Items detected as DIF items:
##   Item49
#--------------

#--------------
difR::difSIBTEST(MSATB, group = "gender", focal.name = 1, 
                 type = "nudif")
## ...
##           Beta    SE X2 Stat. P-value
## Item49  0.0871    NA 14.1465   0.0002 ***
## Item27  0.0179    NA  0.6073   0.4358
## Item41  0.0229    NA  0.8891   0.6411
## Item7   0.0356    NA  1.8647   0.1721
## Item38  0.0591    NA  7.2962   0.0260 *
## ...
## Item2   0.0175      NA  0.8053   0.6685
## ...
## Detection threshold: 3.841 (significance level: 0.05)
## ...
## Items detected as DIF items:
##   Item49
##   Item38
##   Item76
#--------------

#--------------
lapply(1:20, function(i)
  mirt::SIBTEST(dat = MSATB[, 1:20], group = MSATB$gender,
                suspect_set = i)
)
## [[1]]
##                     focal_group n_matched_set n_suspect_set  beta
## SIBTEST                       0            19             1 0.087
## CSIBTEST                      0            19             1 0.087
##                        SE     X2 df     p
## SIBTEST             0.023 14.146  1 0.000
## CSIBTEST               NA 14.146  1 0.000
## ...
#--------------

#-----------------------------------------------------------------
# 8.4.1. Logistic regression
#-----------------------------------------------------------------

#--------------
zscore <- as.vector(scale(rowSums(MSATB[, 1:20]))) # Z-score
fit_alt <- glm(Item49 ~ zscore * gender, data = MSATB, family = binomial)
fit_null <- glm(Item49 ~ zscore, data = MSATB, family = binomial)
anova(fit_null, fit_alt, test = "LRT")
## Analysis of Deviance Table
##
## Model 1: Item49 ~ score
## Model 2: Item49 ~ score * gender
## Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      1405      982.09
## 2      1403      967.33  2    14.76 0.0006 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#--------------

#--------------
aod::wald.test(Sigma = vcov(fit_alt), b = coef(fit_alt), Terms = c(3, 4))
## Wald test:
## ----------
##   
## Chi-squared test:
## X2 = 14.8975, df = 2, P(> X2) = 0.0006
#--------------

#--------------
predict(fit_alt, newdata = data.frame(zscore = c(-1, 0, 1), gender = 0),
        type = "response")
##      1      2      3
## 0.6457 0.8660 0.9582
predict(fit_alt, newdata = data.frame(zscore = c(-1, 0, 1), gender = 1),
        type = "response")
##      1      2      3
## 0.7819 0.9215 0.9747
#--------------

#--------------
summary(fit_null)
## ...
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept)    2.2231     0.1060   20.97   <2e-16 ***
## zscore         1.1860     0.1026   11.56   <2e-16 ***
## ...
summary(fit_alt)
## ...
##               Estimate Std. Error z value Pr(>|z|)
## (Intercept)     1.8659     0.1582   11.79  < 2e-16 ***
## zscore          1.2659     0.1671    7.58  3.6e-14 ***
## gender          0.5973     0.2147    2.78   0.0054 **
## zscore:gender  -0.0797     0.2142   -0.37   0.7099
## ...
#--------------

#--------------
# parameters for item 49 in IRT parametrization
a <- coef(fit_alt)[2]
b <- -coef(fit_alt)[1] / coef(fit_alt)[2]
aDIF <- coef(fit_alt)[4]
bDIF <- (coef(fit_alt)[1] * coef(fit_alt)[4] - coef(fit_alt)[2] * 
           coef(fit_alt)[3]) / 
  (coef(fit_alt)[2] * (coef(fit_alt)[2] + coef(fit_alt)[4]))
setNames(c(a, b, aDIF, bDIF), c("a", "b", "aDIF", "bDIF"))
##      a       b    aDIF    bDIF 
## 1.2659 -1.4740 -0.0797 -0.6025 
#--------------

#--------------
# delta method to compute standard errors of parameters for item 49
msm::deltamethod(list(~x2, ~ -x1 / x2,
                      ~x4, ~ (x1 * x4 - x2 * x3) / (x2 * (x2 + x4))),
                 mean = coef(fit_alt),
                 cov = vcov(fit_alt))
## [1] 0.1671 0.1584 0.2142 0.2374
#--------------

#--------------
# DIF detection with logistic regression model
(fit.LR <- difR::difLogistic(Data = MSATB, group = "gender", 
                             focal.name = 1, match = zscore))
## ... 
##        Stat.   P-value    
## Item49 14.7603  0.0006 ***
## Item27  1.2130  0.5453    
## Item41  0.6366  0.7274    
## ...
## Items detected as DIF items:
## Item49
## 
## Effect size (Nagelkerke's R^2): 
## Effect size code: 
##  'A': negligible effect 
##  'B': moderate effect 
##  'C': large effect 
##        R^2    ZT JG
## Item49 0.0164 A  A 
## Item27 0.0007 A  A 
## Item41 0.0003 A  A 
## ...
## Effect size codes: 
##  Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
##  Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
## ...
difR::difLogistic(Data = MSATB, group = "gender", 
                  focal.name = 1, match = zscore, criterion = "Wald")
## ...
##          Stat. P-value    
## Item49 14.8975  0.0006 ***
## Item27  1.1941  0.5504    
## Item41  0.6420  0.7254
## ...
#--------------

#--------------
# parameters for item 49
fit.LR$logitPar[1, ]
## (Intercept)       SCORE       GROUP SCORE:GROUP 
##      1.8659      1.2659      0.5973     -0.0797
#--------------

#--------------
# plot of characteristic curves for item 49
ShinyItemAnalysis::plotDIFLogistic(
  fit.LR, item = 1, Data = MSATB[, 1:20], 
  group = MSATB[, 21], match = zscore
)
#--------------

#--------------
# DIF detection with 2PL model
(fit.NLR.2PL <- difNLR::difNLR(Data = MSATB, group = "gender", 
                               focal.name = 1, model = "2PL", 
                               method = "irls"))
## ...
##        Chisq-value P-value
## Item49 14.7603      0.0006 ***
## Item27  1.2130      0.5453    
## Item41  0.6366      0.7274 
## ...
## Items detected as DIF items:
## Item49

# parameters for item 49
coef(fit.NLR.2PL)$Item49
##               a      b     aDif    bDif
## estimate 1.2659 -1.474 -0.07965 -0.6025
## CI2.5    0.9384 -1.785 -0.49939 -1.0678
## CI97.5   1.5934 -1.163  0.34009 -0.1371
#--------------

#--------------
# plot of characteristic curves for item 49
plot(fit.NLR.2PL, item = "Item49")
#--------------

#-----------------------------------------------------------------
# 8.4.2. Generalized logistic regression models
#-----------------------------------------------------------------

#--------------
# DIF detection with 3PL model with freely estimated guessing parameter
(fit.NLR.3PL <- difNLR::difNLR(Data = MSATB, group = "gender", 
                               focal.name = 1,  model = "3PLc"))
## ...
##        Chisq-value P-value
## Item49 22.3978      0.0001 ***
## Item27  1.0883      0.7799
## ...
## Item47  9.5320      0.0230 *
## ...
## Items detected as DIF items:
## Item49
## Item47
#--------------

#--------------
# parameters for item 47
coef(fit.NLR.3PL, SE = TRUE)$Item47
##               a       b       c    aDif    bDif    cDif
## estimate 3.9562 -1.4195  0.4807 -2.7970 -1.0999 -0.4807
## SE       1.3838  0.1689  0.1284  1.4310  1.8441  1.3225
## CI2.5    1.2439 -1.7505  0.2290 -5.6017 -4.7142 -3.0727
## CI97.5   6.6684 -1.0886  0.7325  0.0077  2.5144  2.1112

# plot of characteristic curves for item 47
plot(fit.NLR.3PL, item = "Item47", group.names = c("Males", "Females"),
     draw.CI = TRUE)
#--------------

#-----------------------------------------------------------------
# 8.4.3. Cumulative logit model
#-----------------------------------------------------------------

#--------------
# loading data
data(Anxiety, package = "lordif")
Anxiety_items <- Anxiety[, paste0("R", 1:29)]
#--------------

#--------------
# DIF detection with cumulative logit regression model
(fit.ORD1 <- difNLR::difORD(Data = Anxiety_items, group = Anxiety$education,
                            focal.name = 1, model = "cumulative"))
## ...
##     Chisq-value P-value 
## ...
## R6  13.8917      0.0010 ***
## R7   9.3795      0.0092 **
## R8   1.2370      0.5388
## ...
## R19  9.0748      0.0107 *
## R20 10.6796      0.0048 **
## R21  5.9576      0.0509 .
## ...
## Items detected as DIF items:
## R6
## R7
## R10
## R19
## R20
#--------------

#--------------
# parameters for item R6
coef(fit.ORD1, SE = TRUE)$R6
##              b2     b3     b4     b5     a
## estimate 0.2248 1.1264 2.1714 3.2289 2.1390
## SE       0.0640 0.0899 0.1393 0.2235 0.1490
##           bDIF2  bDIF3  bDIF4  bDIF5   aDIF
## estimate 0.3112 0.2811 0.2462 0.2109 0.0738
## SE       0.0821 0.1049 0.1711 0.2498 0.1791
#--------------

#--------------
# plot of cumulative probabilities for item R6
plot(fit.ORD1, item = "R6", plot.type = "cumulative", 
     group.names = c("Males", "Females"))
# plot of category probabilities for item R6
plot(fit.ORD1, item = "R6", plot.type = "category", 
     group.names = c("Males", "Females"))
#--------------

#--------------
# predicted values (category probabilities) for item R6
predict(fit.ORD1, item = "R6", match = 0, group = c(0, 1))
##   P(Y = 1) P(Y = 2) P(Y = 3) P(Y = 4)  P(Y = 5)
## 1   0.6179   0.2996   0.0729   0.0085    0.0010
## 2   0.7660   0.1915   0.0378   0.0042    0.0005
# predicted values (cumulative probabilities) for item R6
predict(fit.ORD1, item = "R6", match = 0, group = c(0, 1), 
        type = "cumulative")
##   P(Y <= 1) P(Y <= 2) P(Y <= 3) P(Y <= 4) P(Y <= 5)
## 1         1    0.3821    0.0825    0.0095    0.0010
## 2         1    0.2340    0.0425    0.0047    0.0005
#--------------

#-----------------------------------------------------------------
# 8.4.4. Adjacent category logit model
#-----------------------------------------------------------------

#--------------
# DIF with adjacent category logit regression model
(fit.ORD2 <- difNLR::difORD(Data = Anxiety_items, 
                            group = Anxiety$gender,
                            focal.name = 1, model = "adjacent"))
## ...
##     Chisq-value P-value 
## ...
## R6   9.8619      0.0072 **
## R7   9.9535      0.0069 **
## R8   1.0119      0.6029
## ...
## R19  9.1928      0.0101 *
## R20 11.1244      0.0038 **
## R21  3.0459      0.2181
## ...
## Items detected as DIF items:
## R6
## R7
## R19
## R20
#--------------

#--------------
# parameters for item R6
coef(fit.ORD2, SE = TRUE)$R6
##              b2     b3     b4     b5      a
## estimate 0.6395 0.9013 2.1545 3.1694 1.3925
## SE       0.1119 0.1248 0.1905 0.3116 0.1205
##           bDIF2  bDIF3  bDIF4  bDIF5   aDIF
## estimate 0.2754 0.2677 0.2307 0.2008 0.0423
## SE       0.0900 0.0969 0.1745 0.2564 0.1274

# plot of category probabilities for item R6
plot(fit.ORD2, item = "R6", group.names = c("Males", "Females"))
#--------------

#--------------
# predicted values (category probabilities) for item R6
predict(fit.ORD2, item = "R6", match = 0, group = c(0, 1))
##   P(Y = 1) P(Y = 2) P(Y = 3) P(Y = 4)  P(Y = 5)
## 1   0.6522   0.2677   0.0763   0.0038   0.00005
## 2   0.7570   0.2037   0.0381   0.0012   0.00001
#--------------

#-----------------------------------------------------------------
# 8.4.5. Multinomial regression model
#-----------------------------------------------------------------

#--------------
# loading data
data(HCItest, HCIkey, package = "ShinyItemAnalysis")
#--------------

#--------------
# DDF with multinomial regression model
(fit.DDF <- difNLR::ddfMLR(Data = HCItest[, 1:20], 
                           group = HCItest$gender, 
                           focal.name = 1, key = unlist(HCIkey)))
##         Chisq-value P-value
## ...
## Item.12 18.5029      0.0178 *
## Item.13  9.1026      0.1679
## ...
## Item.18  6.8674      0.3333
## Item.19 19.9421      0.0106 *
## Item.20 12.0779      0.0603 .
## ...
## Items detected as DDF items:
## Item.12
## Item.19
#--------------

#--------------
# parameters for item 12
coef(fit.DDF, SE = TRUE)[[12]]
##                  b       a    bDIF    aDIF
## A estimate -2.0365 -1.0434  1.0564 -0.3526
## A SE        0.3755  0.2183  0.4323  0.3274
## B estimate -2.2219 -2.3621 -0.6212  1.3403
## B SE        0.2960  0.6244  1.3024  0.7707
## C estimate -2.6307 -1.0247  1.1059 -0.9152
## C SE        0.6225  0.2779  0.6715  0.4874
## E estimate -0.9933 -0.8399  0.4498 -0.3577
## E SE        0.1953  0.1385  0.2633  0.2436

# plot of characteristic curves for item 12
plot(fit.DDF, item = 12, group.names = c("Males", "Females"))
#--------------

#-----------------------------------------------------------------
# 8.5.1 Group-specific IRT models
#-----------------------------------------------------------------

#--------------
(fit.difR.0 <- difR::itemParEst(data = MSATB[MSATB$gender == 0, 1:20], 
                                model = "2PL"))
##             a       b  se(a)   se(b) cov(a,b)
## Item49 1.0404 -1.7197 0.1918  0.2600   0.0432
## Item27 1.1122  1.3154 0.1695  0.1812  -0.0235
## ...
## Item64 0.6661  0.0879 0.1240  0.1514  -0.0021
## ...
fit.difR.1 <- difR::itemParEst(data = MSATB[MSATB$gender == 1, 1:20], 
                               model = "2PL")
(fit.difR.1 <- difR::itemRescale(fit.difR.0, fit.difR.1))
##         new.a   new.b new.se(a) new.se(b) cov(a,b)
## Item49 0.9862 -2.3672    0.1518    0.2905   0.0406
## Item27 1.0462  1.2556    0.1143    0.1332  -0.0115
## ...
## Item64 0.4769  0.1196    0.0815    0.1480  -0.0021
## ...
#--------------

#--------------
plotDIFirt(parameters = rbind(fit.difR.0, fit.difR.1), item = 1)
plotDIFirt(parameters = rbind(fit.difR.0, fit.difR.1), item = 2)
#--------------

#--------------
fit.mirt <- mirt::multipleGroup(data = MSATB[, 1:20], model = 1, 
                                group = as.factor(MSATB$gender),
                                SE = TRUE)

coef(fit.mirt, simplify = TRUE, IRTpars = TRUE)
## $`0`
## $items
##             a       b g u
## Item49 1.0400 -1.7199 0 1
## Item27 1.1122  1.3156 0 1
## ...
## Item64 0.6659  0.0882 0 1
## ...
## 
## $`1`
## $items
##             a       b g u
## Item49 0.9983 -2.3052 0 1
## Item27 1.0586  1.2746 0 1
## ...
## Item64 0.4826  0.1520 0 1
## ...
#--------------

# #--------------
# fit.mirt0 <- mirt(data = MSATB[MSATB$gender == 0, 1:20], model = 1,
#                   SE = TRUE)
# fit.mirt1 <- mirt(data = MSATB[MSATB$gender == 1, 1:20], model = 1,
#                   SE = TRUE)
# logLik(fit.mirt0) + logLik(fit.mirt1)
# logLik(fit.mirt)
# #--------------

#--------------
mirt::itemplot(fit.mirt, item = "Item49")
mirt::itemplot(fit.mirt, item = "Item64")
#--------------

#-----------------------------------------------------------------
# 8.5.2 Lord's test
#-----------------------------------------------------------------

#--------------
difR::difLord(irtParam = rbind(fit.difR.0, fit.difR.1), 
              same.scale = FALSE)
difR::difLord(Data = MSATB, group = "gender", focal.name = 1, 
              model = "2PL")
## ...
##          Stat.  P-value
## Item49 9.5230 0.0086  **
## Item27 0.7242 0.6962
## ...
## Item64 1.6259 0.4436 
## ...
## Detection threshold: 5.992 (significance level: 0.05)
## Items detected as DIF items:
##   Item49
#--------------

#--------------
mirt::DIF(fit.mirt, which.par = c("a1", "d"), Wald = TRUE)
##             W df      p
## Item49 9.3335  2 0.0094
## Item27 0.4094  2 0.8149
## ...
## Item76 7.7364  2 0.0209
## ...
## Item64 1.5297  2 0.4654
## ...
## Item68 6.4863  2 0.0390
## ...
#--------------

#-----------------------------------------------------------------
# 8.5.3 Likelihood ratio test
#-----------------------------------------------------------------

#--------------
mirt::DIF(fit.mirt, which.par = c("a1", "d"), Wald = FALSE)
##        conv.    AIC    AICc   SABIC      HQ    BIC     X2 df      p
## Item49 TRUE -5.2880 -4.7943 -1.1428 -1.3644  5.210 9.2880  2 0.0096
## Item27 TRUE  3.5850  4.0786  7.7301  7.5085 14.083 0.4150  2 0.8126
## ...
## Item76 TRUE -3.5112 -3.0176  0.6339  0.4123  6.987 7.5112  2 0.0233
## ...
## Item64 TRUE  2.4286  2.9222  6.5737  6.3521 12.927 1.5714  2 0.4558
## ...
## Item68 TRUE -2.6954 -2.2018  1.4497  1.2281  7.803 6.6954  2 0.0352
## ...
#--------------

#-----------------------------------------------------------------
# 8.5.4 Raju's test
#-----------------------------------------------------------------

#--------------
itemPar <- list(reference = fit.difR.0[, 1:2],
                focal = fit.difR.1[, 1:2])
SA <- DFIT::SignedArea(itemParameters = itemPar, irtModel = "2pl")
UA <- DFIT::UnsignedArea(itemParameters = itemPar, irtModel = "2pl", 
                   logistic = FALSE)
cbind(SA, UA)
##             SA     UA
## Item49  0.6475 0.6475
## Item27  0.0598 0.0701
## ...
## Item64 -0.0317 0.4858
## ...
#--------------

#--------------
# function to compute signed area
fSA <- function(a, b, c, d) {
  (d - c) * (b[1] - b[2])
}
sapply(1:20, function(i) fSA(a = c(fit.difR.0[i, "a"], 
                                   fit.difR.1[i, "new.a"]),
                             b = c(fit.difR.0[i, "b"], 
                                   fit.difR.1[i, "new.b"]),
                             c = 0, d = 1))
##  [1]  0.6475  0.0598  0.0494 -0.2028  0.0570  0.2744  0.2789  0.5249
##  [9] -0.0177  0.1461 -0.2699 -0.1887 -0.0317 -0.1451 -0.1855 -0.6047
## [17] -0.2844 -0.1283 -0.2648  0.2858

# function to compute unsigned area
fUA <- function(a, b, c, d) {
  H <- 2 * (a[1] - a[2]) / (1.702 * a[1] * a[2]) * 
    log(1 + exp(1.702 * a[1] * a[2] * (b[1] - b[2]) / 
                  (a[1] - a[2]))) - (b[1] - b[2])
  return((d - c) * abs(H))
}
sapply(1:20, function(i) fUA(a = c(fit.difR.0[i, "a"], 
                                   fit.difR.1[i, "new.a"]),
                             b = c(fit.difR.0[i, "b"], 
                                   fit.difR.1[i, "new.b"]),
                             c = 0, d = 1))
##  [1] 0.6475 0.0701 0.0773 0.2050 0.0786 0.2765 0.2841 0.5682 0.0818
## [10] 0.1462 0.3561 0.1970 0.4858 0.1527 0.1855 0.6549 0.2858 0.2549
## [19] 0.2648 0.3141
#--------------

# tab <- data.frame(SA = SA,
#                   UA = UA,
#                   diffA = fit.difR.0[, "a"] - fit.difR.1[, "new.a"],
#                   diffB = fit.difR.0[, "b"] - fit.difR.1[, "new.b"])
# ggplot(tab, aes(x = SA, y = UA, col = abs(diffA))) + 
#   geom_point() + 
#   geom_text(label = rownames(tab)) +
#   geom_abline(intercept = 0, slope = 1) + 
#   geom_abline(intercept = 0, slope = -1)
# ggplot(tab, aes(x = SA, y = UA, col = abs(diffB))) + 
#   geom_point() + 
#   geom_text(label = rownames(tab)) +
#   geom_abline(intercept = 0, slope = 1) + 
#   geom_abline(intercept = 0, slope = -1)
# 
# tab["Item64", ]

#--------------
difR::difRaju(Data = MSATB, group = "gender", focal.name = 1, 
              model = "2PL", signed = TRUE)
## PM: this gave me eeror
## Error in gauher(k) : object 'gh' not found
## I was able to solve the error by loading the ltm package as recommended here:
## https://stackoverflow.com/questions/52256310/error-in-gauherk-object-gh-not-found-using-difgenlord-function
## Maybe add code with loading the package?


##        Stat.   P-value  
## Item49 -1.6608  0.0967 .
## Item27 -0.2659  0.7903 
## ...
## Item64  0.1499  0.8809 
## ...
difR::difRaju(Data = MSATB, group = "gender", focal.name = 1, 
              model = "2PL", signed = FALSE)
##        Stat.   P-value  
## Item49 -1.6607  0.0968 .
## Item27 -0.6981  0.4851  
## ... 
## Item64 -1.3101  0.1902  
## ... 
## Item68 -2.2388  0.0252 *
## ...

SA.Raju <- difR::difRaju(Data = MSATB, group = "gender", 
                         focal.name = 1, model = "1PL", 
                         signed = TRUE)
UA.Raju <- difR::difRaju(Data = MSATB, group = "gender", 
                         focal.name = 1, model = "1PL", 
                         signed = FALSE)
#--------------

#-----------------------------------------------------------------
# Lordif
#-----------------------------------------------------------------

#--------------
lordif::lordif(Anxiety_items, Anxiety$gender, model = "GPCM", 
               control = list(NCYCLES = 2000))
## Number of DIF groups: 2
## 
## Number of items flagged for DIF: 4 of 29
## 
## Items flagged: 6, 7, 10, 19
## 
## Number of iterations for purification: 4 of 10
## 
## Detection criterion: Chisqr
## 
## Threshold: alpha = 0.01
## 
##    item ncat  chi12  chi13  chi23
## 1     1    4 0.9879 0.8743 0.6043
## 2     2    4 0.1136 0.2859 0.9998
## 3     3    4 0.2741 0.5302 0.7876
## 4     4    5 0.3628 0.5381 0.5214
## 5     5    5 0.5722 0.8525 0.9920
## 6     6    5 0.0000 0.0000 0.0232
## 7     7    4 0.0437 0.0043 0.0091
## 8     8    4 0.6384 0.7741 0.5895
## 9     9    4 0.0175 0.0353 0.3084
## 10   10    4 0.0212 0.0032 0.0130
## 11   11    4 0.5036 0.4063 0.2446
## 12   12    4 0.0554 0.1471 0.6865
## 13   13    4 0.0839 0.2191 0.8257
## 14   14    4 0.9036 0.9868 0.9131
## 15   15    4 0.7039 0.6876 0.4368
## 16   16    4 0.2011 0.3922 0.6258
## 17   17    3 0.6071 0.7206 0.5318
## 18   18    5 0.9547 0.6412 0.3467
## 19   19    4 0.0020 0.0071 0.5645
## 20   20    4 0.0181 0.0207 0.1408
## 21   21    4 0.0107 0.0378 0.8625
## 22   22    4 0.1733 0.3955 0.9759
## 23   23    5 0.6220 0.3308 0.1605
## 24   24    5 0.9619 0.9899 0.8932
## 25   25    5 0.8372 0.9694 0.8875
## 26   26    4 0.2829 0.4764 0.5658
## 27   27    4 0.7017 0.7054 0.4579
## 28   28    4 0.2794 0.4649 0.5474
## 29   29    4 0.0120 0.0291 0.3839
#--------------

#-----------------------------------------------------------------
# 8.6. Measurement invariance: Factor analytic approach
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 8.6.1. Configural invariance
#-----------------------------------------------------------------

#--------------
library(lavaan)
data("BFI2", package = "ShinyItemAnalysis")
BFI2O <- BFI2[, 5 * (0:11) + 5]
#--------------

# BFI2all <- BFI2[, 1:60]
# BFI2E <- BFI2[, 5*(0:11) + 1]
# BFI2A <- BFI2[, 5*(0:11) + 2]
# BFI2C <- BFI2[, 5*(0:11) + 3]
# BFI2N <- BFI2[, 5*(0:11) + 4]
# 
# corBFI2E <- psych::polychoric(BFI2E)$rho
# corBFI2A <- psych::polychoric(BFI2A)$rho
# corBFI2C <- psych::polychoric(BFI2C)$rho
# corBFI2N <- psych::polychoric(BFI2N)$rho
# corBFI2O <- psych::polychoric(BFI2O)$rho
# corBFI2all <- psych::polychoric(BFI2all)$rho
# 
# plot_corr(corBFI2E, clust_method = "ward.D", n_clust = 3)
# plot_corr(corBFI2A, clust_method = "ward.D", n_clust = 3)
# plot_corr(corBFI2C, clust_method = "ward.D", n_clust = 3)
# plot_corr(corBFI2N, clust_method = "ward.D", n_clust = 3)
# plot_corr(corBFI2O, clust_method = "ward.D", n_clust = 3)
# plot_corr(corBFI2all, clust_method = "ward.D", n_clust = 5)
# 
# hc <- hclust(as.dist(1 - corBFI2all), method = "ward.D") # hierarchical clustering
# ggdendrogram(hc) # dendrogram

#--------------
modelO <- 'Oint =~ i10 + i25 + i40 + i55
           Oaes =~ i5 + i20 + i35 + i50
           Ocrt =~ i15 + i30 + i45 + i60'
fitO <- lavaan::cfa(modelO, data = BFI2O)
semPlot::semPaths(fitO, what = "std.est", rotation = 4)
lavaan::fitMeasures(fitO, fit.measures = 
                     c("cfi", "tli", "aic", "bic", "rmsea"))
##    cfi        tli        aic        bic      rmsea 
## 0.9244     0.9022 56117.9522 56265.3077     0.0749 
#--------------

#--------------
BFI2Og <- BFI2[, c(5 * (0:11) + 5, 61)]
fitOg <- lavaan::cfa(modelO, data = BFI2Og, group = "Gender")
semPlot::semPaths(fitOg, what = "std.est", rotation = 4)
lavaan::fitMeasures(fitOg, fit.measures = 
                      c("cfi", "tli", "aic", "bic", "rmsea"))
##    cfi        tli        aic        bic      rmsea 
## 0.9234     0.9009 55843.7909 56269.4844     0.0753  
#--------------

#-----------------------------------------------------------------
# 8.6.2. Weak invariance
#-----------------------------------------------------------------

#--------------
fitOgWI <- lavaan::cfa(modelO, data = BFI2Og, 
                       group = "Gender",
                       group.equal = "loadings")
semPlot::semPaths(fitOgWI, what = "std.est", rotation = 4)
lavaan::fitMeasures(fitOgWI, fit.measures = 
                      c("cfi", "tli", "aic", "bic", "rmsea"))
##    cfi        tli        aic        bic      rmsea 
## 0.9227     0.9080 55839.8328 56216.4078     0.0726 

anova(fitOg, fitOgWI)
## PM: this gives Error when not loading lavaan package prior to analysis!
## Error in UseMethod("anova") : 
##  no applicable method for 'anova' applied to an object of class "lavaan"
## I have added loading lavaan here and to the book.

## ... Pr(>Chisq) 
## ...     0.1208
#--------------

#-----------------------------------------------------------------
# 8.6.3. Strong invariance
#-----------------------------------------------------------------

#--------------
fitOgSI <- lavaan::cfa(modelO, data = BFI2Og, 
                       group = "Gender",
                       group.equal = c( "loadings", "intercepts"))
semPlot::semPaths(fitOgSI, what = "std.est", rotation = 4)
lavaan::fitMeasures(fitOgSI, fit.measures = 
                      c("cfi", "tli", "aic", "bic", "rmsea"))
##    cfi        tli        aic        bic      rmsea 
## 0.9046     0.8951 55948.9463 56276.4028     0.0775 

anova(fitOgWI, fitOgSI)
## ...    Pr(>Chisq)
## ... < 2.2e-16 ***
#--------------

#-----------------------------------------------------------------
# 8.6.4. Strict invariance
#-----------------------------------------------------------------

#--------------
fitOgStrictI <- lavaan::cfa(modelO, data = BFI2Og, 
                            group = "Gender",
                            group.equal = 
                              c( "loadings", "intercepts", "residuals"))
# lavaan::fitMeasures(fitOgStrictI, fit.measures = 
#                       c("cfi", "tli", "aic", "bic", "rmsea"))
# ##    cfi        tli        aic        bic      rmsea 
# ## 0.9018     0.9019 55955.7211 56217.6863     0.0750
# 
# anova(fitOgSI, fitOgStrictI)
# # ... Pr(>Chisq)   
# # ...     0.0021 **
#--------------

#-----------------------------------------------------------------
# 8.6.5. Partial invariance
#-----------------------------------------------------------------

#--------------
lavaan::lavTestScore(fitOgSI)
## $test
## 
## total score test:
##   
##   test      X2 df p.value
## 1 score 139.065 21       0
## 
## $uni
## 
## univariate score tests:
##   
##      lhs op   rhs     X2 df p.value
## 1   .p2. == .p47.  1.261  1   0.262
## 2   .p3. == .p48.  0.946  1   0.331
## 3   .p4. == .p49.  1.103  1   0.294
## 4   .p6. == .p51.  4.065  1   0.044
## 5   .p7. == .p52.  1.115  1   0.291
## 6   .p8. == .p53.  5.413  1   0.020
## 7  .p10. == .p55.  0.826  1   0.363
## 8  .p11. == .p56.  0.029  1   0.864
## 9  .p12. == .p57.  0.056  1   0.813
## 10 .p31. == .p76.  2.730  1   0.099
## 11 .p32. == .p77. 26.589  1   0.000
## 12 .p33. == .p78.  3.502  1   0.061
## 13 .p34. == .p79. 26.714  1   0.000
## 14 .p35. == .p80.  1.763  1   0.184
## 15 .p36. == .p81. 10.253  1   0.001
## 16 .p37. == .p82.  2.534  1   0.111
## 17 .p38. == .p83. 28.587  1   0.000
## 18 .p39. == .p84. 32.652  1   0.000
## 19 .p40. == .p85. 37.327  1   0.000
## 20 .p41. == .p86.  0.138  1   0.710
## 21 .p42. == .p87.  0.342  1   0.559
#--------------


#-----------------------------------------------------------------
# 8.7.1 Item purification
#-----------------------------------------------------------------

#--------------
fit_ORD3 <- difNLR::difORD(Data = Anxiety_items, 
                           group = Anxiety$gender,
                           focal.name = 1, model = "cumulative",
                           purify = TRUE)
fit_ORD3$difPur
##       R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16
## Step0  0  0  0  0  0  1  1  0  0   1   0   0   0   0   0   0
## Step1  0  0  0  0  0  1  1  0  0   0   0   0   0   0   0   0
## Step2  0  0  0  0  0  1  1  0  0   0   0   0   0   0   0   0
## Step3  0  0  0  0  0  1  1  0  0   0   0   0   0   0   0   0
##       R17 R18 R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29
## Step0   0   0   1   1   0   0   0   0   0   0   0   0   0
## Step1   0   0   1   1   1   0   0   0   0   0   0   0   0
## Step2   0   0   1   1   0   0   0   0   0   0   0   0   0
## Step3   0   0   1   1   0   0   0   0   0   0   0   0   0
#--------------

#-----------------------------------------------------------------
# 8.7.2 Corrections for multiple comparisons
#-----------------------------------------------------------------

#--------------
# without multiple comparison correction
difR::difLogistic(Data = HCI[, 1:20], group = HCI$gender, 
                  focal.name = 1)$p.value
##  [1] 0.0380 0.9420 0.8183 0.2199 0.5072 0.2608 0.8268 0.4240 0.1889
## [10] 0.2971 0.0823 0.0425 0.1989 0.8801 0.8078 0.9019 0.7614 0.8740
## [19] 0.0078 0.0102
# using Benjamini-Hochberg correction
difR::difLogistic(Data = HCI[, 1:20], group = HCI$gender, 
                  focal.name = 1, p.adjust.method = "BH")$adjusted.p
##  [1] 0.2123 0.9420 0.9420 0.5497 0.8453 0.5796 0.9420 0.7709 0.5497
## [10] 0.5943 0.3290 0.2123 0.5497 0.9420 0.9420 0.9420 0.9420 0.9420
## [19] 0.1017 0.1017
#--------------
