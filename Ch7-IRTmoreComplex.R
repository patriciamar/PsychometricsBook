#-----------------------------------------------------------------
# Chapter 7 - More complex IRT models
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(ggplot2)
library(ltm)
library(mirt)
library(ShinyItemAnalysis)
library(Cairo)

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
# 7.2  Cumulative logit IRT models
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 7.2.1  Graded response model
#-----------------------------------------------------------------

#--------------
data("Anxiety", package = "lordif")
head(Anxiety_items <- Anxiety[, paste0("R", 1:29)], n = 2)
##   R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16 R17
## 1  1  1  1  1  1  1  1  2  2   1   2   2   1   1   1   1   1
## 2  1  1  1  1  1  1  1  1  1   1   1   1   1   1   1   1   1
##   R18 R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29
## 1   2   1   2   2   1   1   2   3   2   1   1   2
## 2   1   1   1   1   1   1   1   2   1   1   1   1
#--------------

#--------------
# GRM with the mirt package
fit_GRM_mirt <- mirt::mirt(Anxiety_items, model = 1, 
                           itemtype = "graded", SE = TRUE)
#--------------

#--------------
# estimated coefficients with CI (not displayed in the book)
coef(fit_GRM_mirt, IRTpars = TRUE, SE = TRUE)
## $R1
##             a    b1    b2    b3    b4
## par     3.449 0.494 1.251 2.031 2.814
## CI_2.5  2.983 0.394 1.116 1.824 2.448
## CI_97.5 3.915 0.594 1.387 2.238 3.179
## ...
#--------------

#--------------
coef(fit_GRM_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##         a     b1    b2    b3    b4
## R1  3.449  0.494 1.251 2.031 2.814
## R2  3.168  0.568 1.426 2.253 3.253
## ...
## R25 1.372 -0.762 0.154 1.377 2.592
## ...
#--------------

#--------------
mirt::itemplot(fit_GRM_mirt, item = 1, type = "infotrace")
#--------------

#--------------
mirt::itemplot(fit_GRM_mirt, item = 25, type = "infotrace")
#--------------

#--------------
# Further item plots (not displayed in the book):
# item response curves (IRC) for item 1
mirt::itemplot(fit_GRM_mirt, item = 1, type = "trace")

# item score function for item 1
mirt::itemplot(fit_GRM_mirt, item = 1, type = "score")

# item information curve (IIC) and SE for item 1
mirt::itemplot(fit_GRM_mirt, item = 1, type = "infoSE")
#--------------

#--------------
# Further test plots (not displayed in the book):
# test score curve
plot(fit_GRM_mirt)
# test score curve with 95% CI
plot(fit_GRM_mirt, MI = 200)

# test information curve (TIC)
plot(fit_GRM_mirt, type = "info") # test information
plot(fit_GRM_mirt, type = "infoSE") # test information and SE
#--------------

#--------------
# factor scores
fs <- as.vector(fscores(fit_GRM_mirt))
head(fs)
## [1] -0.14263 -1.41719 -0.12252 -0.25884 -1.70984 -0.04914

fsSE <- fscores(fit_GRM_mirt, full.scores.SE = TRUE)
head(fsSE)
##            F1  SE_F1
## [1,] -0.14263 0.1618
## [2,] -1.41719 0.4356
## ...
#--------------

#--------------
# Factor scores vs standardized total scores (not displayed in the book)
sts <- as.vector(scale(rowSums(Anxiety_items)))
plot(fs ~ sts)
#--------------

#--------------
# GRM in the ltm package
fit_GRM_ltm <- ltm::grm(Anxiety_items)

# coefficients
coef(fit_GRM_ltm)
##     Extrmt1 Extrmt2 Extrmt3 Extrmt4 Dscrmn
## R1    0.560   1.461   2.366   3.325  2.773
## R2    0.657   1.667   2.604   3.741  2.605
## ...
## R25  -0.918   0.150   1.612   3.056  1.153
## ...
#--------------

#--------------
ltm::factor.scores(fit_GRM_ltm)
## ...
## Factor-Scores for observed response patterns:
##    R1 ... R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29 Obs    Exp     z1 se.z1
## 1   1 ...   1   1   1   1   1   1   1   1   1   1   1  60 31.463 -1.693 0.516
## 2   1 ...   1   1   1   1   1   1   1   1   1   2   1   1  1.442 -1.242 0.376
## 3   1 ...   1   1   1   1   1   1   1   3   1   1   1   1  0.391 -1.262 0.394
## ...
#--------------

#--------------
ltm::factor.scores(fit_GRM_ltm, resp.patterns = Anxiety_items)
## Factor-Scores for specified response patterns:
##    R1 ... R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29 Obs   Exp     z1 se.z1
## 1   2 ...   1   1   2   1   2   2   1   1   2   3   2   0 0.000 -0.012 0.183
## 2   2 ...   1   1   1   1   1   1   1   1   1   2   1   0 0.011 -0.933 0.300
## 3   2 ...   2   1   2   1   1   1   1   1   1   3   2   0 0.000  0.025 0.186
## ...
#--------------

#--------------
ltm::factor.scores(fit_GRM_ltm, resp.patterns = t(as.matrix(rep(3, 29))))
## Factor-Scores for specified response patterns:
##     R1 ... R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29 Obs Exp    z1 se.z1
## Exp  3 ...   3   3   3   3   3   3   3   3   3   3   3   0   0 1.751 0.139
#--------------

#--------------
# Factor scores in mirt vs. ltm package
fs_ltm <- as.vector(ltm::factor.scores(fit_GRM_ltm, 
                                       resp.patterns = Anxiety_items)$score.dat[, "z1"])
head(fs_ltm) # First five factor scores from ltm
## [1] -0.01162 -0.93264  0.02471 -0.36256 -1.18225 -0.11107

cor(fs, fs_ltm)
## [1] 0.9749

df <- data.frame(fs, fs_ltm)
ggplot(data = df, aes(x = fs, y = fs_ltm)) +
  geom_point() +
  theme_fig() +
  xlab("Factor scores by mirt") + ylab("Factor scores by ltm") + 
  geom_smooth(method = "lm", se = FALSE)
#--------------

#-----------------------------------------------------------------
# 7.2.2  Graded ratings scale model
#-----------------------------------------------------------------

#--------------
# GRSM with the IRT parametrization
fit_GRSMirt_mirt <- mirt::mirt(Anxiety_items, model = 1, 
                               itemtype = "grsmIRT")
# coefficients
coef(fit_GRSMirt_mirt, simplify = TRUE)
##  $items
##        a1     b1     b2     b3     b4      c
## R1  3.200 -0.454 -1.256 -2.214 -3.202  0.000
## R2  3.109 -0.454 -1.256 -2.214 -3.202 -0.110
## ...
## R25 1.613 -0.454 -1.256 -2.214 -3.202  1.081
## ...
#--------------

#--------------
anova(fit_GRSMirt_mirt, fit_GRM_mirt)
##        AIC     AICc    SABIC       HQ      BIC    logLik
## 1 35268.78 35279.52 35358.19 35377.76 35551.89 -17573.39
## 2 35130.82 35199.11 35343.35 35389.87 35803.79 -17420.41
##        X2  df   p
## 1     NaN NaN NaN
## 2 305.959  84   0
#--------------

#-----------------------------------------------------------------
# 7.3  Adjacent-categories logit IRT models
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 7.3.1  Generalized partial credit model
#-----------------------------------------------------------------

#--------------
fit_GPCM_mirt <- mirt(Anxiety_items, model = 1, 
                      itemtype = "gpcm")
# coefficients
coef(fit_GPCM_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##         a     b1     b2    b3    b4
## R1  2.935  0.614  1.204 1.859 2.439
## R2  2.823  0.680  1.355 2.012 2.829
## ...
## R25 0.741  0.013 -0.182 1.468 2.119
## ...
#--------------

#--------------
mirt::itemplot(fit_GPCM_mirt, item = 1, type = "infotrace")
#--------------

#--------------
mirt::itemplot(fit_GPCM_mirt, item = 25, type = "infotrace")
#--------------

#-----------------------------------------------------------------
# 7.3.2  Partial credit model
#-----------------------------------------------------------------

#--------------
model_PCM <- "F = 1-29
              FIXED = (1-29, a1),
              START = (1-29, a1, 1)"
fit_PCM_mirt <- mirt::mirt(Anxiety_items, model = model_PCM, 
                           itemtype = "gpcm")
# coefficients
coef(fit_PCM_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##     a     b1     b2    b3    b4
## R1  1  1.305  1.948 3.184 4.106
## R2  1  1.390  2.243 3.434 4.814
## ...
## R25 1 -0.462 -0.106 1.639 2.670
## ...
#--------------

#--------------
anova(fit_PCM_mirt, fit_GPCM_mirt)
##        AIC     AICc    SABIC       HQ      BIC    logLik
## 1 36632.50 36674.32 36802.53 36839.74 37170.88 -18200.25
## 2 35326.84 35395.13 35539.37 35585.89 35999.81 -17518.42
##         X2  df   p
## 1      NaN NaN NaN
## 2 1363.662  29   0
#--------------

#-----------------------------------------------------------------
# 7.3.3  Rating scale model
#-----------------------------------------------------------------

#--------------
fit_RSM_mirt <- mirt::mirt(Anxiety_items, model = 1, itemtype = "rsm")

# coefficients
coef(fit_RSM_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##     a1    b1   b2    b3    b4      c
## R1   1 1.347 2.09 3.797 5.101  0.000
## R2   1 1.347 2.09 3.797 5.101 -0.205
## ...
## R25  1 1.347 2.09 3.797 5.101  2.030
## ...
#--------------

#--------------
mirt::itemplot(fit_RSM_mirt, item = 1, type = "infotrace")
#--------------

#--------------
mirt::itemplot(fit_RSM_mirt, item = 25, type = "infotrace")
#--------------

#--------------
anova(fit_RSM_mirt, fit_PCM_mirt)
##        AIC     AICc    SABIC       HQ      BIC    logLik
## 1 36446.99 36450.05 36495.36 36505.94 36600.15 -18190.49
## 2 35326.84 35395.13 35539.37 35585.89 35999.81 -17518.42
##
## X2  df   p
## 1      NaN NaN NaN
## 2 1344.149 112   0
#--------------

#--------------
anova(fit_RSM_mirt, fit_GPCM_mirt)
##        AIC     AICc    SABIC       HQ      BIC    logLik
## 1 36446.99 36450.05 36495.36 36505.94 36600.15 -18190.49
## 2 35326.84 35395.13 35539.37 35585.89 35999.81 -17518.42
##
## X2  df   p
## 1      NaN NaN NaN
## 2 1344.149 112   0
#--------------

#-----------------------------------------------------------------
# 7.4  Baseline-category logit IRT models
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 7.4.1  Nominal response model
#-----------------------------------------------------------------

#--------------
HCI.numeric <- HCItest[, 1:20]
HCI.numeric[] <- sapply(HCI.numeric, as.numeric)

fit_NRM_mirt <- mirt::mirt(HCI.numeric, model = 1, 
                           itemtype = "nominal")
# coefficients in default model parametrization as in Thissen et al. (2010)
coef(fit_NRM_mirt, simplify = TRUE)
##             a1 ak0     ak1     ak2     ak3 d0     d1     d2     d3 ak4     d4
## Item 1   0.449   0   2.158   0.591   3.000  0  1.261  1.608  3.282  NA     NA
## Item 2   0.282   0   3.376   2.000      NA  0  1.886 -0.143     NA  NA     NA
## ...
## Item 19  0.086   0  -2.096  17.969   3.758  0 -0.104  3.081 -0.059   4 -0.227
## Item 20  0.473   0  -0.607   1.369   3.000  0  0.983  3.024  4.356  NA     NA
#--------------

#--------------
# coefficients in (intercept-slope) model parametrization as proposed by Bock (1972)
coef(fit_NRM_mirt, IRTpars = TRUE, simplify = TRUE)
##             a1     a2     a3     a4     c1     c2     c3     c4     a5     c5
## Item 1  -0.645  0.323 -0.379  0.701 -1.538 -0.277  0.070  1.744     NA     NA
## Item 2  -0.505  0.447  0.059     NA -0.581  1.305 -0.724     NA     NA     NA
## ...
## Item 19 -0.407 -0.587  1.139 -0.083 -0.538 -0.642  2.542 -0.597 -0.062 -0.766
## Item 20 -0.445 -0.732  0.203  0.974 -2.091 -1.108  0.934  2.265     NA     NA
#--------------

#--------------
# item characteristic curve for item 1
mirt::itemplot(fit_NRM_mirt, item = 1, type = "infotrace")
# item information curves
plot(fit_NRM_mirt, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit_NRM_mirt, type = "infoSE")
# factor scores vs standardized total scores
fs <- as.vector(fscores(fit_NRM_mirt))
sts <- as.vector(scale(rowSums(HCI[, 1:20])))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)
#--------------

#-----------------------------------------------------------------
# 7.5  Item-specific IRT models
#-----------------------------------------------------------------

#--------------
data("CZmaturaS", package = "ShinyItemAnalysis")
CZmathS <- CZmaturaS[,grepl("^b", names(CZmatura))]
# rescoring items 17-24 (multiple-choice, originally scored as 0 or 2 pts)
CZmathS[, paste0("b", 17:24)] <- 
  as.numeric(CZmathS[, paste0("b", 17:24)] == 2)
head(CZmathS, n = 2)
#--------------

#--------------
fit_gpcm <- mirt::mirt(CZmathS, model = 1, itemtype = "gpcm")
#--------------

#--------------
itemtype <- rep("gpcm", length(CZmathS))
names(itemtype) <- names(CZmathS)
# 3PL model for multiple-choice items where guessing is expected: 
itemtype[paste0("b", 17:24)] <- "3PL"
fit_mixed <- mirt::mirt(CZmathS, model = 1, itemtype = itemtype)
#--------------

#--------------
fs_gpcm = as.vector(mirt::fscores(fit_gpcm))
head(fs_gpcm)
## [1]  0.3757  0.3327  0.0210 -0.1369 -0.0625 -0.7373
fs_mixed = as.vector(mirt::fscores(fit_mixed))
head(fs_mixed)
## [1]  0.3937  0.3276  0.0328 -0.1248 -0.0449 -0.7749
cor(fs_gpcm, fs_mixed)
## [1] 0.999
#--------------

#--------------
plot(fs_gpcm, fs_mixed)
plot(fit_gpcm, type = 'trace')
plot(fit_mixed, type = 'trace') # guessing in items b17 - b24
#--------------

#-----------------------------------------------------------------
# 7.6 Multidimensional IRT models
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 7.6.1 Multidimensional 2PL model
#-----------------------------------------------------------------

#--------------
BFI2en <- BFI2[, c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 
                   4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59)]
BFI2en01 <- 1 * (BFI2en > 2)
#--------------

#--------------
m2PL <- mirt::mirt(BFI2en01, model = 2, itemtype = "2PL")
#--------------

#--------------
coef(m2PL, simplify = TRUE)
## $items
##         a1     a2      d g u
## i1  -0.037  1.854  2.312 0 1
## i6  -0.121  1.886  2.325 0 1
## ...
## i4   1.531 -0.556  0.903 0 1
## ...
## i54  2.008 -1.219  0.150 0 1
## i59  2.154  0.000  0.414 0 1
#--------------

#--------------
mirt::itemplot(m2PL, item = 1) # item loading strongly on one factor
mirt::itemplot(m2PL, item = 13) # item loading strongly on other factor
# itemplot(m2PL, item = 1, rotate = "oblimin")
#--------------

#--------------
mirt::MDIFF(m2PL)
##     MDIFF_1
## i1  -1.2465
## i6  -1.2305 
## ...

mirt::MDISC(m2PL)
##     i1     i6    i11    i16    i21    i26    i31    i36    i41 
## 1.8548 1.8896 0.5401 2.3346 1.7972 0.9268 2.2508 0.8626 1.4868 
##    i46    i51    i56     i4     i9    i14    i19    i24    i29 
## 2.1646 1.7246 1.2201 1.6293 1.2734 1.9687 1.3930 1.2216 2.5930 
##    i34    i39    i44    i49    i54    i59 
## 1.4385 2.3060 1.5733 1.3502 2.3491 2.1542 
#--------------

#--------------
# model fit indices
mirt::M2(m2PL)
##         M2  df p  RMSEA RMSEA_5 RMSEA_95  SRMSR    TLI    CFI
## stats 1660 229 0 0.0601  0.0574   0.0628 0.0447 0.9170 0.9310
#--------------

#--------------
head(mirt::fscores(m2PL), n = 3)
##           F1      F2
## [1,]  0.0525  1.3830
## [2,] -0.9720  1.2715
## [3,]  1.1962 -2.1297
#--------------

#-----------------------------------------------------------------
# 7.6.2 Multidimensional Graded Response Model
#-----------------------------------------------------------------

#--------------
mGRM <- mirt::mirt(BFI2en, model = 2, itemtype = "graded")
#--------------

#--------------
coef(mGRM, simplify = TRUE)
## $items
##         a1     a2    d1     d2     d3     d4
## i1  -0.125  1.863 4.925  2.293  0.864 -2.174
## i6  -0.187  1.594 4.611  2.090  0.608 -2.067
## ...
## i4   1.747 -0.581 3.453  0.963 -0.578 -3.228
## ...
## i54  1.938 -1.066 2.539  0.179 -1.261 -3.919
## i59  1.973  0.000 3.153  0.435 -0.992 -3.636
## 
## $means
## F1 F2 
## 0  0 
## 
## $cov
## F1 F2
## F1  1  0
## F2  0  1

#--------------
itemplot(mGRM, item = 1) # item loading strongly on one factor
itemplot(mGRM, item = 13) # item loading strongly on other factor
#--------------

#--------------
mirt::MDIFF(mGRM)
##     MDIFF_1 MDIFF_2 MDIFF_3 MDIFF_4
## i1   -2.638  -1.228  -0.462   1.164
## i6   -2.873  -1.302  -0.379   1.288

mirt::MDISC(mGRM)
##     i1     i6    i11    i16    i21    i26    i31    i36    i41    i46 
## 1.8675 1.6050 0.6456 2.3866 1.7110 0.7971 1.9245 0.7359 1.5194 2.0703 
##    i51    i56     i4     i9    i14    i19    i24    i29    i34    i39 
## 1.5477 1.1477 1.8414 1.3404 1.7667 1.4188 1.4438 2.6689 1.4735 2.1694 
##    i44    i49    i54    i59 
## 1.4927 1.4560 2.2120 1.9733 
#--------------

#--------------
# model fit indices
mirt::M2(mGRM)
##         M2  df p   RMSEA RMSEA_5 RMSEA_95   SRMSR    TLI   CFI
## stats 1470 157 0 0.06948 0.06623  0.07273 0.05664 0.9491 0.9609
#--------------

#--------------
head(mirt::fscores(mGRM), n = 3)
##           F1     F2
## [1,]  0.4821  1.274
## [2,] -1.1366  1.160
## [3,]  3.4887 -3.497
#--------------

#-----------------------------------------------------------------
# 7.4.3 Confirmatory multidimensional PCM
#-----------------------------------------------------------------

#--------------
model_ENirt <- 'N = 13-24
                E = 1-12
                COV = N*E'
#fit_ENirtRasch <- mirt::mirt(BFI2en, model = model_ENirt, itemtype = "Rasch")
fit_ENirtGRM <- mirt::mirt(BFI2en, model = model_ENirt, itemtype = "graded")
#--------------

#--------------
coef(fit_ENirtGRM, simplify = TRUE)
## $items
##        a1    a2    d1     d2     d3     d4
## i1  0.000 1.815 4.847  2.264  0.855 -2.141
## i6  0.000 1.668 4.685  2.136  0.626 -2.104
## ...
## i4  1.855 0.000 3.468  0.969 -0.579 -3.242
## ...
## i54 2.164 0.000 2.511  0.187 -1.237 -3.857
## i59 1.679 0.000 2.900  0.405 -0.902 -3.341
## 
## $means
## N E 
## 0 0 
## 
## $cov
##        N      E
## N  1.000 -0.366
## E -0.366  1.000
#--------------

#--------------
itemplot(fit_ENirtGRM, item = 1) # item loading on 1st factor
itemplot(fit_ENirtGRM, item = 13) # item loading on 2nd factor
#--------------

#--------------
MDIFF(fit_ENirtGRM)
##     MDIFF_1   MDIFF_2  MDIFF_3 MDIFF_4
## i1   -2.670 -1.247133 -0.47093   1.179
## i6   -2.809 -1.280900 -0.37535   1.262

MDISC(fit_ENirtGRM)
##     i1     i6    i11    i16    i21    i26    i31    i36    i41    i46    i51    i56 
## 1.8153 1.6678 0.6313 2.3159 1.8016 0.8033 1.9004 0.7631 1.3134 1.9334 1.6300 1.1060 
##     i4     i9    i14    i19    i24    i29    i34    i39    i44    i49    i54    i59 
## 1.8549 1.3335 1.6935 1.4488 1.2662 2.4583 1.4914 2.1672 1.1882 1.4682 2.1639 1.6792 
#--------------

#--------------
# model fit indices
M2(fit_ENirtGRM)
##         M2  df p   RMSEA RMSEA_5 RMSEA_95   SRMSR    TLI   CFI
## stats 2248 180 0 0.08144 0.07843  0.08444 0.08602 0.9301 0.9383
#--------------

#--------------
head(fscores(fit_ENirtGRM), n = 3)
##            E       N
## [1,]  0.7727  0.2232
## [2,]  0.7267 -0.7867
## [3,] -2.5039  2.8328
#--------------

#-----------------------------------------------------------------
# 7.7. More complex IRT models in interactive application
#-----------------------------------------------------------------

startShinyItemAnalysis()
