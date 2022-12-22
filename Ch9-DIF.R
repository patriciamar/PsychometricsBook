#-----------------------------------------------------------------
# Chapter 9 - Differential item functioning
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
# 9.3.1 Delta method
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
library(deltaPlotR)
# delta plot using fixed threshold
(DP_fixed <- deltaPlot(data = MSATB, group = "gender", focal.name = 1, thr = 1.5))
## ...
##        Prop.Ref Prop.Foc Delta.Ref Delta.Foc   Dist.
## Item1    0.8161   0.8776    9.3974    8.3482  0.8753
## Item2    0.2335   0.2470   15.9099   15.7356  0.1493
## Item3    0.3843   0.3803   14.1769   14.2190  0.0214
## ...
## Code: '***' if item is flagged as DIF
## Parameters of the major axis:
##       a      b
##  0.3787 0.9784
## ...
#--------------

#--------------
# delta plot using normal approximation threshold
(DP_norm <- deltaPlot(data = MSATB, group = "gender", focal.name = 1, 
                      thr = "norm"))
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
# delta plot method with item purification (code not shown in the book)
deltaPlot(data = MSATB, group = "gender", focal.name = 1, 
          thr = "norm", purify = TRUE)
#--------------

#--------------
# diagonal plots
diagPlot(DP_fixed, thr.draw = TRUE)
diagPlot(DP_norm, thr.draw = TRUE)
#--------------

#-----------------------------------------------------------------
# 9.3.2 Mantel-Haenszel test
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
(MHstat <- (abs(sum(n_item49_01 - n_item49_R * n_item49_1 / n_item49)) - 0.5)^2 /
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
library(difR)
difMH(Data = MSATB, group = "gender", focal.name = 1)
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

#--------------
# MH test with item purification and Benjamini-Hochberg adjustment for multiple comparisons
# (code not shown in the book)
difMH(Data = MSATB, group = "gender", focal.name = 1,
      purify = TRUE, p.adjust.method = "BH")

# purification steps (code not shown in the book)
difMH(Data = MSATB, group = "gender", focal.name = 1,
      purify = TRUE, p.adjust.method = "BH")$difPur
##       Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9 Item10 Item11 Item12 Item13 Item14
## Step0     1     0     0     0     0     0     0     0     0      0      0      0      0      0
## Step1     1     0     0     0     0     0     0     0     0      0      0      0      0      0
##       Item15 Item16 Item17 Item18 Item19 Item20
## Step0      0      0      1      0      0      0
## Step1      0      0      1      0      0      0
#--------------

#-----------------------------------------------------------------
# 9.3.3 SIBTEST
#-----------------------------------------------------------------

#--------------
difSIBTEST(Data = MSATB, group = "gender", focal.name = 1)
## ...
##           Beta      SE X2 Stat. P-value
## Item49 -0.0871  0.0231 14.1465   0.0002 ***
## ...
## Item38 -0.0321  0.0266  1.4620   0.2266
## ...
## Item76  0.0429  0.0276  2.4072   0.1208   
## ...
## Detection threshold: 3.841 (significance level: 0.05)
## Items detected as DIF items:
##   Item49
#--------------

#--------------
difSIBTEST(Data = MSATB, group = "gender", focal.name = 1, type = "nudif")
## ...
##           Beta    SE X2 Stat. P-value
## Item49  0.0871    NA 14.1465   0.0002 ***
## ...
## Item38  0.0591    NA  7.2962   0.0260 *
## ...
## Item76  0.0593    NA  6.0066   0.0496 *
## ...
## Detection threshold: 3.841 (significance level: 0.05)
## ...
## Items detected as DIF items:
##   Item49
##   Item38
##   Item76
#--------------

#--------------
# SIBTEST with item purification and Benjamini-Hochberg adjustment for multiple comparisons
# (code not shown in the book)
difSIBTEST(Data = MSATB, group = "gender", focal.name = 1,
           purify = TRUE, p.adjust.method = "BH")
## ...
## Items detected as DIF items:
##   Item49

# purification steps (code not shown in the book)
difSIBTEST(Data = MSATB, group = "gender", focal.name = 1,
           purify = TRUE, p.adjust.method = "BH")$difPur
#--------------

#--------------
library(mirt)
lapply(1:20, function(i)
    SIBTEST(dat = MSATB[, 1:20], group = MSATB$gender, suspect_set = i))
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
# 9.4.1. Logistic regression
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
library(aod)
wald.test(Sigma = vcov(fit_alt), b = coef(fit_alt), Terms = c(3, 4))
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
    coef(fit_alt)[3]) /  (coef(fit_alt)[2] * (coef(fit_alt)[2] + coef(fit_alt)[4]))
setNames(c(a, b, aDIF, bDIF), c("a", "b", "aDIF", "bDIF"))
##      a       b    aDIF    bDIF 
## 1.2659 -1.4740 -0.0797 -0.6025 
#--------------

#--------------
library(msm)
# delta method to compute standard errors of parameters for item 49
deltamethod(list(~ x2, ~ -x1 / x2, ~ x4, ~ (x1 * x4 - x2 * x3) / (x2 * (x2 + x4))),
            mean = coef(fit_alt), cov = vcov(fit_alt))
## [1] 0.1671 0.1584 0.2142 0.2374
#--------------

#--------------
# DIF detection with logistic regression model
(fit_LR <- difLogistic(Data = MSATB, group = "gender", focal.name = 1))
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
difLogistic(Data = MSATB, group = "gender", focal.name = 1, criterion = "Wald")
## ...
##          Stat. P-value    
## Item49 14.8975  0.0006 ***
## Item27  1.1941  0.5504    
## Item41  0.6420  0.7254
## ...
#--------------

#--------------
# parameters for item 49
fit_LR$logitPar[1, ]
## (Intercept)       SCORE       GROUP SCORE:GROUP 
##     -1.7668      0.3281      0.8259     -0.0206 
#--------------

#--------------
# plot of characteristic curves for item 49
plot(fit_LR, plot = "itemCurve")
library(ShinyItemAnalysis)
plotDIFLogistic(fit_LR, item = 1, Data = MSATB[, 1:20], group = MSATB[, 21])
#--------------

#--------------
# difLogistic() with item purification and BH multiple comparsions adjustment 
# (code not shown in th book) 
(fit_LR_puriBH <- difLogistic(Data = MSATB, group = "gender", focal.name = 1,
                              purify = TRUE, p.adjust.method = "BH"))
# item purification steps
fit_LR_puriBH$difPur
#--------------


#--------------
# DIF detection with logistic regression (2PL model) in the difNLR package
library(difNLR)
(fit_NLR_2PL <- difNLR(Data = MSATB, group = "gender", focal.name = 1, 
                       model = "2PL", method = "irls"))
## ...
##        Chisq-value P-value
## Item49 14.7603      0.0006 ***
## Item27  1.2130      0.5453    
## Item41  0.6366      0.7274 
## ...
## Items detected as DIF items:
## Item49

# parameters for item 49 - IRT parametrization
coef(fit_NLR_2PL)$Item49
##               a       b    aDif    bDif
## estimate 1.2659 -1.4740 -0.0797 -0.6025
## CI2.5    0.9384 -1.7845 -0.4994 -1.0678
## CI97.5   1.5934 -1.1634  0.3401 -0.1371
#--------------

#--------------
# parameters for item 49 - intercept-slope parametrization (code not shown in the book)
coef(fit_NLR_2PL, IRTpars = FALSE)$Item49
##          (Intercept)     x       g     x:g
## estimate      1.8659 1.2659 0.5973 -0.0797
## CI2.5         1.5558 0.9384 0.1765 -0.4994
## CI97.5        2.1761 1.5934 1.0180  0.3401

# parameters for item 49 - IRT parametrization, with SE (code not shown in the book)
coef(fit_NLR_2PL, SE = TRUE, CI = 0)$Item49
##               a       b    aDif    bDif
## estimate 1.2659 -1.4740 -0.0797 -0.6025
## SE       0.1671  0.1584  0.2142  0.2374
#--------------

#--------------
# plot of characteristic curves for item 47
plot(fit_NLR_2PL, item = "Item47")
#--------------

#--------------
# difNLR with item purification and BH correction for multiple comparisons 
# (code not shown in the book)
(fit_NLR_2PL_puriBH <- difNLR(Data = MSATB, group = "gender", focal.name = 1, 
                              model = "2PL", method = "irls", 
                              purify = TRUE, p.adjust.method = "BH"))

# item purification steps
fit_NLR_2PL_puriBH$difPur
#--------------

#-----------------------------------------------------------------
# 9.4.2. Generalized logistic regression models
#-----------------------------------------------------------------

#--------------
# DIF detection with 3PL model with freely estimated guessing parameter
(fit_NLR_3PL <- difNLR(Data = MSATB, group = "gender", focal.name = 1, 
                       model = "3PLc"))
## ...
##        Chisq-value P-value
## Item49 22.3978      0.0001 ***
## ...
## Item47  9.5320      0.0230 *
## ...
## Items detected as DIF items:
## Item49
## Item47

# parameters for item 47
coef(fit_NLR_3PL)$Item47
##               a       b       c    aDif    bDif    cDif
## estimate 3.9562 -1.4195  0.4807 -2.7970 -1.0999 -0.4807
## CI2.5    1.2439 -1.7505  0.2290 -5.6017 -4.7142 -3.0727
## CI97.5   6.6684 -1.0886  0.7325  0.0077  2.5144  2.1112

# plot of characteristic curves for item 47
plot(fit_NLR_3PL, item = "Item47", group.names = c("Males", "Females"))
#--------------

#-----------------------------------------------------------------
# 9.4.3. Group-specific cumulative logit model
#-----------------------------------------------------------------

#--------------
# loading data
data(Anxiety, package = "ShinyItemAnalysis")
Anxiety_items <- Anxiety[, paste0("R", 1:29)]
#--------------

#--------------
# DIF detection with cumulative logit regression model
(fit_ORD1 <- difORD(Data = Anxiety_items, group = Anxiety$gender, focal.name = 1, 
                    model = "cumulative"))
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
coef(fit_ORD1, SE = FALSE, CI = 0)$R6
##     b1     b2     b3     b4      a
## 0.2248 1.1264 2.1714 3.2289 2.1390
##  bDIF1  bDIF2  bDIF3  bDIF4   aDIF
## 0.3112 0.2811 0.2462 0.2109 0.0738
#--------------

#--------------
# plot of cumulative probabilities for item R6
plot(fit_ORD1, item = "R6", plot.type = "cumulative", 
     group.names = c("Males", "Females"))
# plot of category probabilities for item R6
plot(fit_ORD1, item = "R6", plot.type = "category", 
     group.names = c("Males", "Females"))
#--------------

#--------------
# predicted values (category probabilities) for item R6
predict(fit_ORD1, item = "R6", match = 0, group = c(0, 1))
##   P(Y = 1) P(Y = 2) P(Y = 3) P(Y = 4)  P(Y = 5)
## 1   0.6179   0.2996   0.0729   0.0085    0.0010
## 2   0.7660   0.1915   0.0378   0.0042    0.0005
# predicted values (cumulative probabilities) for item R6
predict(fit_ORD1, item = "R6", match = 0, group = c(0, 1), type = "cumulative")
##   P(Y <= 1) P(Y <= 2) P(Y <= 3) P(Y <= 4) P(Y <= 5)
## 1         1    0.3821    0.0825    0.0095    0.0010
## 2         1    0.2340    0.0425    0.0047    0.0005
#--------------

#-----------------------------------------------------------------
# 9.4.4. Group-specific adjacent category logit model
#-----------------------------------------------------------------

#--------------
# DIF with adjacent category logit regression model
(fit_ORD2 <- difORD(Data = Anxiety_items, group = Anxiety$gender,
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
coef(fit_ORD2, SE = TRUE, CI = 0)$R6
##              b2     b3     b4     b5      a
## estimate 0.6395 0.9013 2.1545 3.1694 1.3925
## SE       0.1119 0.1248 0.1905 0.3116 0.1205
##           bDIF2  bDIF3  bDIF4  bDIF5   aDIF
## estimate 0.2754 0.2677 0.2307 0.2008 0.0423
## SE       0.0900 0.0969 0.1745 0.2564 0.1274

# plot of category probabilities for item R6
plot(fit_ORD2, item = "R6", group.names = c("Males", "Females"))
#--------------

#--------------
# predicted values (category probabilities) for item R6
predict(fit_ORD2, item = "R6", match = 0, group = c(0, 1))
##   P(Y = 1) P(Y = 2) P(Y = 3) P(Y = 4)  P(Y = 5)
## 1   0.6522   0.2677   0.0763   0.0038   0.00005
## 2   0.7570   0.2037   0.0381   0.0012   0.00001
#--------------

#-----------------------------------------------------------------
# 9.4.5. Group-specific multinomial regression model
#-----------------------------------------------------------------

#--------------
# loading data
data(HCItest, HCIkey, package = "ShinyItemAnalysis")
#--------------

#--------------
# DDF with multinomial regression model
(fit_DDF <- ddfMLR(Data = HCItest[, 1:20], group = HCItest$gender, 
                   focal.name = 1, key = HCIkey))
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
coef(fit_DDF, SE = TRUE, CI = 0)[[12]]
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
plot(fit_DDF, item = 12, group.names = c("Males", "Females"))
#--------------

#--------------
# predicted values (category probabilities) for item 12
predict(fit_DDF, item = 12, match = -1, group = c(0, 1))
##        D      A       B     C      E
## 1 0.3863 0.1310 0.0216 0.0727 0.3885
## 2 0.2342 0.2408 0.0356 0.0846 0.4047
#--------------

#-----------------------------------------------------------------
# 9.5.1 Group-specific IRT models
#-----------------------------------------------------------------

#--------------
(fit_difR0 <- itemParEst(data = MSATB[MSATB$gender == 0, 1:20], model = "2PL"))
##             a       b  se(a)   se(b) cov(a,b)
## Item49 1.0404 -1.7197 0.1918  0.2600   0.0432
## Item27 1.1122  1.3154 0.1695  0.1812  -0.0235
## ...
## Item64 0.6661  0.0879 0.1240  0.1514  -0.0021
## ...
fit_difR1 <- itemParEst(data = MSATB[MSATB$gender == 1, 1:20], model = "2PL")
(fit_difR1 <- itemRescale(fit_difR0, fit_difR1))
##         new.a   new.b new.se(a) new.se(b) cov(a,b)
## Item49 0.9862 -2.3672    0.1518    0.2905   0.0406
## Item27 1.0462  1.2556    0.1143    0.1332  -0.0115
## ...
## Item64 0.4769  0.1196    0.0815    0.1480  -0.0021
## ...
#--------------

#--------------
plotDIFirt(parameters = rbind(fit_difR0, fit_difR1), item = 1)
plotDIFirt(parameters = rbind(fit_difR0, fit_difR1), item = 2)
#--------------

#--------------
fit_mirt0 <- mirt(MSATB[MSATB$gender == 0, 1:20], model = 1, SE = TRUE)
fit_mirt1 <- mirt(MSATB[MSATB$gender == 1, 1:20], model = 1, SE = TRUE)

fit_mirt <- multipleGroup(data = MSATB[, 1:20], model = 1, 
                          group = as.factor(MSATB$gender), SE = TRUE)

coef(fit_mirt, simplify = TRUE, IRTpars = TRUE)
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

#--------------
itemplot(fit_mirt, item = "Item49")
itemplot(fit_mirt, item = "Item64")
#--------------

#-----------------------------------------------------------------
# 9.5.2 Lord's test
#-----------------------------------------------------------------

#--------------
difLord(irtParam = rbind(fit_difR0, fit_difR1), same.scale = TRUE)
difLord(Data = MSATB, group = "gender", focal.name = 1, model = "2PL")
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
DIF(MGmodel = fit_mirt, which.par = c("a1", "d"), Wald = TRUE)
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

#--------------
library(equateIRT)

# extract the coefficients and the covariance matrix
est0 <- import.mirt(fit_mirt0, display = FALSE)
est1 <- import.mirt(fit_mirt1, display = FALSE)

# create a list of coefficients and covariance matrices
coefs <- list(est0$coef, est1$coef)
vars <- list(est0$var, est1$var)

# perform the test for DIF 
# equating method: mean-mean
dif.test(coef = coefs, var = vars)
##      Test for Differential Item Functioning
## 
## Item parameters tested for DIF: intercept and slope
## Equating method used: mean-mean 
## Reference group: T1    Focal group: T2 
## Item purification not applied
## 
## statistic p.value     
## Item1      2.644  0.2665     
## Item10     0.574  0.7503     
## Item17     1.432  0.4886     
## Item2      0.933  0.6272     
## Item24     2.264  0.3224     
## Item25     2.413  0.2992     
## Item27     0.665  0.7172     
## Item28     0.585  0.7464     
## Item38     1.137  0.5663     
## Item41     0.554  0.7581     
## Item45     2.918  0.2325     
## Item47     3.479  0.1756     
## Item49    12.149  0.0023 **  
## Item61     2.906  0.2338     
## Item64     1.859  0.3948     
## Item68     5.890  0.0526 .   
## Item7      2.714  0.2575     
## Item75     0.213  0.8990     
## Item76     6.228  0.0444 *   
## Item9      2.546  0.2800     
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#--------------

#-----------------------------------------------------------------
# 9.5.3 Likelihood ratio test
#-----------------------------------------------------------------

#--------------
DIF(MGmodel = fit_mirt, which.par = c("a1", "d"), Wald = FALSE)
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
DIF(MGmodel = fit_mirt, which.par = "a1", Wald = FALSE, plotdif = TRUE)
## ...
## Item76      TRUE -2.149 -0.077 -0.188 3.100 4.149  1 0.042
## ...
DIF(MGmodel = fit_mirt, which.par = "d", Wald = FALSE, plotdif = TRUE)
## Item49      TRUE -3.169 -1.096 -1.207 2.080 5.169  1 0.023
## ...
## Item47      TRUE -2.840 -0.768 -0.879 2.409 4.840  1 0.028
## ...
## Item68      TRUE -4.551 -2.478 -2.589 0.698 6.551  1 0.010
## ...
#--------------

#-----------------------------------------------------------------
# 9.5.4 Raju's test
#-----------------------------------------------------------------

#--------------
library(DFIT)
itemPar <- list(reference = fit_difR0[, 1:2],  focal = fit_difR1[, 1:2])
SA <- SignedArea(itemParameters = itemPar, irtModel = "2pl")
UA <- UnsignedArea(itemParameters = itemPar, irtModel = "2pl", 
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
sapply(1:20, function(i) fSA(a = c(fit_difR0[i, "a"], fit_difR1[i, "new.a"]),
                             b = c(fit_difR0[i, "b"], fit_difR1[i, "new.b"]),
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
sapply(1:20, function(i) fUA(a = c(fit_difR0[i, "a"], 
                                   fit_difR1[i, "new.a"]),
                             b = c(fit_difR0[i, "b"], 
                                   fit_difR1[i, "new.b"]),
                             c = 0, d = 1))
##  [1] 0.6475 0.0701 0.0773 0.2050 0.0786 0.2765 0.2841 0.5682 0.0818
## [10] 0.1462 0.3561 0.1970 0.4858 0.1527 0.1855 0.6549 0.2858 0.2549
## [19] 0.2648 0.3141
#--------------

#--------------
difRaju(Data = MSATB, group = "gender", focal.name = 1, model = "2PL", 
        signed = TRUE)

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
#--------------

#--------------
# code not shown in the book
SA.Raju <- difRaju(Data = MSATB, group = "gender", 
                   focal.name = 1, model = "1PL", 
                   signed = TRUE)
UA.Raju <- difRaju(Data = MSATB, group = "gender", 
                   focal.name = 1, model = "1PL", 
                   signed = FALSE)
#--------------

#-----------------------------------------------------------------
# 9.6.1 Iterative hybrid ordinal logistic regression with IRT
#-----------------------------------------------------------------

#--------------
library(lordif)
(fit_lordif <- lordif(resp.data = Anxiety_items, group = Anxiety$gender, 
                      alpha = 0.05))
## ...
## Items flagged: 6, 7, 9, 10, 19, 20, 21, 29 
## ...
##    item ncat  chi12  chi13  chi23
## 6     6    5 0.0000 0.0000 0.0524
## 7     7    4 0.0430 0.0043 0.0091
## 8     8    4 0.6301 0.7511 0.5595
## 9     9    4 0.0176 0.0408 0.3833
## 10   10    4 0.0206 0.0035 0.0145
## ...
## 19   19    4 0.0022 0.0077 0.5309
## 20   20    4 0.0183 0.0211 0.1425
## 21   21    4 0.0108 0.0389 0.9948
## ...
## 29   29    4 0.0120 0.0305 0.4144
#--------------

#--------------
fit_lordif$ipar.sparse
##            a    cb1    cb2    cb3    cb4
## ...
## I6.1  2.3511 0.3619 1.1025 1.8668 2.6918
## I6.2  3.0226 0.6220 1.2403 1.9581 2.6711
## ...
#--------------

#--------------
# code not shown in the book
cor(rowSums(Anxiety_items), fit_lordif$calib.sparse$theta)
## [1] 0.9373
#--------------

#--------------
# code not shown in the book
ggplot(data.frame(theta = fit_lordif$calib.sparse$theta,
                  group = as.factor(Anxiety$gender)),
       aes(x = theta, group = group, col = group, fill = group)) +
  geom_histogram(position = "dodge2", alpha = 0.75, binwidth = 0.25) + 
  xlab(expression("Level of Anxiety"~theta)) + 
  ylab("Count") + 
  scale_colour_manual("", values = c("red3", "goldenrod2"), labels = c("Males", "Females")) +
  scale_fill_manual("", values = c("red3", "goldenrod2"), labels = c("Males", "Females")) +
  theme_fig() + 
  theme(legend.position = c(0.8, 0.8))
#--------------

#--------------
# code not shown in the book
ggplot(data.frame(score = rowSums(Anxiety_items),
                  group = as.factor(Anxiety$gender)),
       aes(x = score, group = group, col = group, fill = group)) +
  geom_histogram(position = "dodge2", alpha = 0.75, binwidth = 5) + 
  xlab("Total score") + 
  ylab("Count") + 
  scale_colour_manual("", values = c("red3", "goldenrod2"), labels = c("Males", "Females")) +
  scale_fill_manual("", values = c("red3", "goldenrod2"), labels = c("Males", "Females")) +
  theme_fig() + 
  theme(legend.position = "none")
#--------------

#-----------------------------------------------------------------
# 9.6.2 Regularization approach for DIF detection
#-----------------------------------------------------------------

#-------------- code not shown in book
library(DIFlasso)
Anxiety_bin_items <- as.data.frame(0 + (Anxiety[, -c(1:3,33:34)] >= 3))
Anxiety_covar_std <- as.data.frame(sapply(Anxiety[, 1:3], scale))
fitRasch_lasso <- DIFlasso(Y = Anxiety_bin_items, X = Anxiety_covar_std)
## Number of (valid) persons: P = 569 
## Number of items: I = 29 
## DIF Items: 25 
## 
## Matrix of estimated item-specific coefficients:
##          R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16 R17
## gender    0  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0
## age       0  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0
## education 0  0  0  0  0  0  0  0  0   0   0   0   0   0   0   0   0
##           R18 R19 R20 R21 R22 R23 R24     R25 R26 R27 R28 R29
## gender      0   0   0   0   0   0   0  0.0773   0   0   0   0
## age         0   0   0   0   0   0   0 -0.2238   0   0   0   0
## education   0   0   0   0   0   0   0  0.0520   0   0   0   0


fit_Rasch_gender <- multipleGroup(data = Anxiety_bin_items, model = 1, 
                                  group = as.factor(Anxiety$gender), 
                                  itemtype = "Rasch")
fit_Rasch_age <- multipleGroup(data = Anxiety_bin_items, model = 1, 
                               group = as.factor(Anxiety$age), 
                               itemtype = "Rasch")
fit_Rasch_education <- multipleGroup(data = Anxiety_bin_items, model = 1, 
                                     group = as.factor(Anxiety$education), 
                                     itemtype = "Rasch")
(test_gender <- DIF(MGmodel = fit_Rasch_gender, which.par = c("a1", "d")))
##  [1] "R2"  "R7"  "R16" "R20" "R22" "R28"
(test_age <- DIF(MGmodel = fit_Rasch_age, which.par = c("a1", "d")))
##  [1] "R1"  "R2"  "R3"  "R4"  "R5"  "R6"  "R7"  "R8"  "R9"  "R10" "R11"
## [12] "R12" "R13" "R14" "R15" "R16" "R17" "R18" "R19" "R20" "R22" "R23"
## [23] "R24" "R26" "R27" "R28" "R29"
(test_education <- DIF(MGmodel = fit_Rasch_education, which.par = c("a1", "d")))
##  [1] "R3"  "R8"  "R10" "R14" "R22" "R26" "R27" "R29"
#--------------

#-----------------------------------------------------------------
# 9.7. DIF detection in interactive application
#-----------------------------------------------------------------

startShinyItemAnalysis()
