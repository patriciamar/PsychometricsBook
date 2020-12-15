#-----------------------------------------------------------------
# Chapter 8 - Differential item functioning
# Introduction to psychometric methods
# in education, psychology, and health.
# With examples in R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(deltaPlotR)
library(difNLR)
library(difR)
library(ggplot2)
library(ltm)
library(mirt)
library(ShinyItemAnalysis)

#-----------------------------------------------------------------
# Functions
#-----------------------------------------------------------------

theme_fig <- function(base_size = 15, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      legend.key = element_rect(fill = "white", colour = NA),
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

#-----------------------------------------------------------------
# 8.2.2  Examples of DIF items in literature
#-----------------------------------------------------------------

data(GMAT, package = "difNLR")
score <- rowSums(GMAT[, 1:20])

df <- data.frame(score, group = as.factor(GMAT$group))

# Histogram of total scores wrt group
ggplot(data = df, aes(x = score, fill = group, col = group)) +
  geom_histogram(binwidth = 1, position = "dodge2", alpha = 0.75) +
  xlab("Total score") +
  ylab("Number of respondents") +
  scale_fill_manual(values = c("dodgerblue2", "goldenrod2"), labels = c("Reference", "Focal")) +
  scale_colour_manual(values = c("dodgerblue2", "goldenrod2"), labels = c("Reference", "Focal")) +
  theme_fig(base_size = 19) +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8)) +
  ggtitle("")

fit <- difLogistic(GMAT[, 1:20], GMAT$group, focal.name = 1)
plotDIFLogistic(fit, Data = GMAT[, 1:20], group = GMAT$group) + theme_fig(base_size = 19) +
  theme(legend.box = "horizontal",
    legend.position = c(0.3, 0.7))


data(HCI, package = "ShinyItemAnalysis")
score <- rowSums(HCI[, 1:20])

df <- data.frame(score, group = as.factor(HCI$gender))

# Histogram of total scores wrt group
ggplot(data = df, aes(x = score, fill = group, col = group)) +
  geom_histogram(binwidth = 1, position = "dodge2", alpha = 0.75) +
  xlab("Total score") +
  ylab("Number of respondents") +
  scale_fill_manual(values = c("dodgerblue2", "goldenrod2"), labels = c("Males", "Females")) +
  scale_colour_manual(values = c("dodgerblue2", "goldenrod2"), labels = c("Males", "Females")) +
  theme_fig(base_size = 19) +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8)) +
  ggtitle("")


fit <- difLogistic(HCI[, 1:20], HCI$gender, focal.name = 1, p.adjust.method = "BH")
plotDIFLogistic(fit, item = 3, Data = HCI[, 1:20], group = HCI$gender) + theme_fig(base_size = 19) +
  theme(legend.box = "horizontal",
        legend.position = c(0.7, 0.3))


#-----------------------------------------------------------------
# 8.3.1 Delta method
#-----------------------------------------------------------------

#--------------
data(MSATB, package = "difNLR")
head(MSATB, n = 2)
##   Item49 Item27 Item41 ...
## 1      1      0      0 ...
## 2      1      0      1 ...
## ...
#--------------

#--------------
# calculating proportions of correct answer per group
(piR <- colMeans(MSATB[MSATB$gender == 0, -21]))[1:3]
## Item49 Item27 Item41
## 0.8161 0.2335 0.3843
(piF <- colMeans(MSATB[MSATB$gender == 1, -21]))[1:3]
## Item49 Item27 Item41
## 0.8776 0.2470 0.3803

#--------------
# transformation into standard normal quantiles
(zR <- qnorm(1 - piR))[1:3]
##  Item49  Item27  Item41
## -0.9007  0.7275  0.2942
(zF <- qnorm(1 - piF))[1:3]
## Item49  Item27  Item41
## -1.1629  0.6839  0.3047

# transformation into delta scores
(deltaR <- 4 * zR + 13)[1:3]
## Item49  Item27  Item41
## 9.3974 15.9099 14.1769
(deltaF <- 4 * zF + 13)[1:3]
## Item49  Item27  Item41
## 8.3482 15.7356 14.2190
#--------------

#--------------
# standard deviations of delta scores
sR <- sd(deltaR)
sF <- sd(deltaF)
# covariance of delta scores
sRF <- cov(deltaR, deltaF)
# mean of delta scores
mR <- mean(deltaR)
mF <- mean(deltaF)

# calculation of parameters a and b of major axis
(b <- (sF^2 - sR^2 + sqrt((sF^2 - sR^2)^2 + 4 * sRF^2))/(2 * sRF))
## [1] 0.9784
(a <- mF - b * mR)
## [1] 0.3787
#--------------

#--------------
# calculation of distances of delta scores from major axis
(D <- (b * deltaR + a - deltaF)/(sqrt(b^2 + 1)))[1:3]
## Item49 Item27 Item41
## 0.8753 0.1493 0.0214
#--------------

#--------------
# delta plot using fixed threshold
(DP_fixed <- deltaPlot(
  data = MSATB, group = "gender",
  focal.name = 1)
)
## ...
##        Prop.Ref Prop.Foc Delta.Ref Delta.Foc Dist.
## Item1   0.8161   0.8776   9.3974    8.3482    0.8753
## Item2   0.2335   0.2470  15.9099   15.7356    0.1493
## Item3   0.3843   0.3803  14.1769   14.2190    0.0214
## ...
## Code: '***' if item is flagged as DIF
## Parameters of the major axis:
##      a     b
##  0.379 0.978
## ...
#--------------

#--------------
# delta plot using normal approximation threshold
(DP_norm <- deltaPlot(
  data = MSATB, group = "gender",
  focal.name = 1, thr = "norm")
)
## ...
##        Prop.Ref Prop.Foc Delta.Ref Delta.Foc Dist.
## Item1   0.8161   0.8776   9.3974    8.3482    0.8753 ***
## Item2   0.2335   0.2470  15.9099   15.7356    0.1493
## Item3   0.3843   0.3803  14.1769   14.2190    0.0214
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
diagPlot(DP_fixed, thr.draw = TRUE)
diagPlot(DP_norm, thr.draw = TRUE)
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

# contingency table for item 49 and score 10
(tab2 <- table(MSATB[score == 10, c("gender", "Item49")]))
##       Item49
## gender  1  0
##      0 30  6
##      1 86  8
# odds ratio in contingency table above
n_item49_01_10 <- tab2[1, 1]
n_item49_00_10 <- tab2[1, 2]
n_item49_11_10 <- tab2[2, 1]
n_item49_10_10 <- tab2[2, 2]
(n_item49_01_10 * n_item49_10_10) / (n_item49_00_10 * n_item49_11_10)
## [1] 0.4651
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

# deltaMH
- 2.35 * log(alphaMH)
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
difMH(Data = MSATB, group = "gender", focal.name = 1)
## ...
##   Stat.   P-value
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
difSIBTEST(MSATB, group = "gender", focal.name = 1)
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
difSIBTEST(MSATB, group = "gender", focal.name = 1, type = "nudif")
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
  SIBTEST(MSATB[, 1:20], group = MSATB$gender,
          suspect_set = i, LiStout1996 = TRUE)
)
## [[1]]
##                     focal_group n_matched_set n_suspect_set  beta
## SIBTEST                       0            19             1 0.087
## CSIBTEST                      0            19             1 0.087
## CSIBTEST_randomized           0            19             1 0.087
##                        SE    X2 df     p
## SIBTEST             0.023 14.15  1 0.000
## CSIBTEST               NA 14.15  1 0.000
## CSIBTEST_randomized 0.023 14.15 NA 0.014
## ...
#--------------

#--------------
lapply(1:20, function(i)
  SIBTEST(MSATB[, 1:20], group = MSATB$gender,
          suspect_set = i, LiStout1996 = FALSE)
)
## [[1]]
##          focal_group n_matched_set n_suspect_set  beta    SE    X2
## SIBTEST            0            19             1 0.087 0.023 14.15
## CSIBTEST           0            19             1 0.087    NA 14.15
##          df p
## SIBTEST   1 0
## CSIBTEST  1 0
## ...
#--------------


#-----------------------------------------------------------------
# 8.3.4 Logistic regression and its extensions
#-----------------------------------------------------------------

#--------------
m1 <- glm(Item49 ~ score * gender, data = MSATB, family = binomial)
m0 <- glm(Item49 ~ score, data = MSATB, family = binomial)
anova(m0, m1, test = "Chisq")
## Analysis of Deviance Table
##
## Model 1: Item49 ~ score
## Model 2: Item49 ~ score * gender
## Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1      1405      982.1
## 2      1403      967.3  2    14.76 0.000623 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#--------------

#--------------
(fit.LR <- difLogistic(MSATB, group = "gender", focal.name = 1))
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
#--------------

#--------------
coef(m1)
## (Intercept)        score     gender score:gender
##     -1.7668       0.3281     0.8259      -0.0206

fit.LR$logitPar[1, ]
## (Intercept)       SCORE       GROUP SCORE:GROUP
##     -1.7668      0.3281      0.8259     -0.0206
#--------------

#--------------
plot(fit.LR, plot = "itemCurve", item = 1)
#--------------

#--------------
(fit.NLR.2PL <- difNLR(
  MSATB, group = "gender", focal.name = 1,
  model = "2PL")
)
## ...
##        Chisq-value P-value
## Item49 22.5070      0.0000 ***
## Item27  0.8534      0.6527
## Item41  0.8120      0.6663
## ...
## Items detected as DIF items:
## Item49
## Item68
coef(fit.NLR.2PL)$Item49
##      a       b    aDif    bDif
## 1.1203 -1.5514 -0.0928 -0.6922
#--------------

#--------------
plot(fit.NLR.2PL, item = "Item49")[[1]] + theme_fig() +
  theme(legend.box = "horizontal",
        legend.position = c(0.75, 0.25))
#--------------

#--------------
(fit.NLR <- difNLR(MSATB, group = "gender", focal.name = 1, model = "3PLc"))
## ...
##        Chisq-value P-value
## Item49 22.3978      0.0001 ***
## Item27  1.0883      0.7799
## Item41  1.4556      0.6925
## ...
## Items detected as DIF items:
## Item49
## Item47
#--------------

#--------------
# parameters for item 47
coef(fit.NLR, SE = TRUE)$Item47
##               a       b       c    aDif    bDif    cDif
## estimate 3.9562 -1.4195  0.4807 -2.7970 -1.0999 -0.4807
## SE       1.3838  0.1689  0.1284  1.4310  1.8441  1.3225

# plot of characteristic curves for item 47
plot(fit.NLR, item = "Item47", group.names = c("Males", "Females"))[[1]] + theme_fig() +
  theme(legend.box = "horizontal",
        legend.position = c(0.75, 0.25))
#--------------

#--------------
# loading data
data(Anxiety, package = "lordif")
#--------------

#--------------
# DIF with cumulative logit regression model
(fit_ORD1 <- difORD(
  Data = Anxiety[, paste0("R", 1:29)], group = Anxiety$gender,
  focal.name = 1, model = "cumulative"
))
## ...
## R6  13.8917      0.0010 ***
## R7   9.3795      0.0092 **
## R8   1.2370      0.5388
## R9   4.3732      0.1123
## R10  6.1645      0.0459 *
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
# coefficients for item R6
coef(fit_ORD1, SE = TRUE)$R6
##              b2     b3     b4     b5     a
## estimate 0.2248 1.1264 2.1714 3.2289 2.1390
## SE       0.0640 0.0899 0.1393 0.2235 0.1490
##           bDIF2  bDIF3  bDIF4  bDIF5   aDIF
## estimate 0.3112 0.2811 0.2462 0.2109 0.0738
## SE       0.0821 0.1049 0.1711 0.2498 0.1791
#--------------

#--------------
# plot of cumulative probabilities
plot(
  fit_ORD1, item = "R6",
  plot.type = "cumulative", group.names = c("Males", "Females")
)[[1]] + theme(text = element_text(size = 19))

# plot of category probabilities
plot(
  fit_ORD1, item = "R6",
  plot.type = "category", group.names = c("Males", "Females")
)[[1]] + theme(text = element_text(size = 19))
#--------------

#--------------
# DIF with adjacent category logit regression model
(fit_ORD2 <- difORD(
  Data = Anxiety[, paste0("R", 1:29)], group = Anxiety$gender,
  focal.name = 1, model = "adjacent"
))
## ...
## R6   9.8619      0.0072 **
## R7   9.9535      0.0069 **
## R8   1.0119      0.6029
## R9   2.8220      0.2439
## R10  5.2412      0.0728
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
# coefficients for item R6
coef(fit_ORD2, SE = TRUE)$R6
##              b2     b3     b4     b5      a
## estimate 0.6395 0.9013 2.1545 3.1694 1.3925
## SE       0.1119 0.1248 0.1905 0.3116 0.1205
##           bDIF2  bDIF3  bDIF4  bDIF5   aDIF
## estimate 0.2754 0.2677 0.2307 0.2008 0.0423
## SE       0.0900 0.0969 0.1745 0.2564 0.1274
#--------------

#--------------
# plot of category probabilities
plot(fit_ORD2, item = "R6")[[1]] +
  theme(text = element_text(size = 15))
#--------------

#--------------
# loading data
data(HCItest, HCIkey, package = "ShinyItemAnalysis")

# DDF with multinomial regression model
(fitDDF <- ddfMLR(
  HCItest[, 1:20], HCItest$gender,
  focal.name = 1,
  unlist(HCIkey)
))
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
# estimated coefficients for item 12
coef(fitDDF, SE = TRUE)[[12]]
##                  b       a    bDIF    aDIF
## A estimate -2.0365 -1.0434  1.0564 -0.3526
## A SE        0.3755  0.2183  0.4323  0.3274
## B estimate -2.2219 -2.3621 -0.6212  1.3403
## B SE        0.2960  0.6244  1.3024  0.7707
## C estimate -2.6307 -1.0247  1.1059 -0.9152
## C SE        0.6225  0.2779  0.6715  0.4874
## E estimate -0.9933 -0.8399  0.4498 -0.3577
## E SE        0.1953  0.1385  0.2633  0.2436
#--------------

#--------------
# plot of ICCs for item 12
plot(fitDDF, item = 12, group.names = c("Males", "Females"))[[1]] + theme_fig() +
  theme(legend.box = "horizontal",
        legend.position = c(0.35, 0.7))
#--------------

#-----------------------------------------------------------------
# 8.3.5.1 Lord's test
#-----------------------------------------------------------------

#--------------
difLord(MSATB, group = "gender", focal.name = 1, model = "2PL")
## ...
##          Stat.  P-value
## Item49 9.5230 0.0086  **
## Item27 0.7242 0.6962
## Item41 0.6427 0.7252
## ...
## Detection threshold: 5.992 (significance level: 0.05)
## Items detected as DIF items:
##   Item49
#--------------

#--------------
modelMG <- multipleGroup(
  MSATB[, 1:20], 1, as.factor(MSATB$gender), SE = TRUE
)
DIF(modelMG, which.par = c("a1", "d"), Wald = TRUE)
##            W df     p
## Item49 9.334  2 0.009
## Item27 0.409  2 0.815
## Item41 0.764  2 0.683
## ...
#--------------

#--------------
coef(modelMG)
## $`0`
## $Item49
##            a1     d  g  u
## par     1.040 1.789  0  1
## CI_2.5  0.664 1.465 NA NA
## CI_97.5 1.416 2.113 NA NA
## ...
## $`1`
## $Item49
##            a1     d  g  u
## par     0.998 2.301  0  1
## CI_2.5  0.697 2.021 NA NA
## CI_97.5 1.299 2.582 NA NA
## ...
#--------------


#--------------
itemplot(modelMG, item = "Item49")
#--------------

#-----------------------------------------------------------------
# 8.3.5.2 Likelihood ratio test
#-----------------------------------------------------------------

#--------------
difLRT(MSATB, group = "gender", focal.name = 1)
#--------------

#--------------
DIF(modelMG, which.par = c("a1", "d"), Wald = FALSE)
##           AIC   AICc  SABIC     HQ    BIC    X2 df     p
## Item49 -5.288 -4.794 -1.143 -1.364  5.210 9.288  2 0.010
## Item27  3.585  4.079  7.730  7.509 14.083 0.415  2 0.813
## Item41  3.255  3.749  7.400  7.179 13.753 0.745  2 0.689
## ...
#--------------

#-----------------------------------------------------------------
# 8.3.5.3 Raju's test
#-----------------------------------------------------------------

#--------------
difRaju(MSATB, group = "gender", focal.name = 1, model = "1PL")
## ...
##           Stat.   P-value
## Item49 -3.4433  0.0006 ***
## Item27 -0.8871  0.3750
## Item41 -0.2233  0.8233
## ...
## Detection thresholds: -1.96 and 1.96 (significance level: 0.05)
## Items detected as DIF items:
##   Item49
##   Item68
##
## Effect size (ETS Delta scale):
## Effect size code:
##   'A': negligible effect
##   'B': moderate effect
##   'C': large effect
##
##          mF-mR   deltaRaju
## Item49 -0.5967  1.4022   B
## Item27 -0.1369  0.3217   A
## Item41 -0.0310  0.0728   A
## ...
## Effect size codes: 0 'A' 1.0 'B' 1.5 'C'
## (for absolute values of 'deltaRaju')
#--------------

#-----------------------------------------------------------------
# 8.4.1 Item purification
#-----------------------------------------------------------------

#--------------
fit_ORD3 <- difORD(
  Data = Anxiety[, paste0("R", 1:29)], group = Anxiety$gender,
  focal.name = 1, model = "cumulative",
  purify = TRUE
)
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
# 8.4.1 Corrections for multiple comparisons
#-----------------------------------------------------------------

#--------------
# without multiple comparison correction
difLogistic(
  HCI[, 1:20], HCI$gender, focal.name = 1
)$p.value
##  [1] 0.0380 0.9420 0.8183 0.2199 0.5072 0.2608 0.8268 0.4240 0.1889
## [10] 0.2971 0.0823 0.0425 0.1989 0.8801 0.8078 0.9019 0.7614 0.8740
## [19] 0.0078 0.0102
# using Benjamini-Hochberg correction
difLogistic(
  HCI[, 1:20], HCI$gender, focal.name = 1,
  p.adjust.method = "BH"
)$adjusted.p
##  [1] 0.2123 0.9420 0.9420 0.5497 0.8453 0.5796 0.9420 0.7709 0.5497
## [10] 0.5943 0.3290 0.2123 0.5497 0.9420 0.9420 0.9420 0.9420 0.9420
## [19] 0.1017 0.1017
#--------------
