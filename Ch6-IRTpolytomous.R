#-----------------------------------------------------------------
# Chapter 6 - Polytomous IRT models
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
# 6.2  Cumulative logit
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 6.2.1  Graded response model
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
fit_GRM_mirt <- mirt::mirt(Anxiety_items, model = 1, itemtype = "graded")
#--------------

#--------------
# coefficients
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
# test score curve
plot(fit_GRM_mirt)
# test score curve with 95% CI
# plot(fit_GRM_mirt, MI = 200)

# TIC
plot(fit_GRM_mirt, type = "info") # test information
plot(fit_GRM_mirt, type = "infoSE") # test information and SE
#--------------

#--------------
# Factor scores vs standardized total scores
fs <- as.vector(fscores(fit_GRM_mirt))
sts <- as.vector(scale(rowSums(Anxiety[, paste0("R", 1:29)])))
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
fs_GRM_mirt <- unique(sort(as.vector(mirt::fscores(fit_GRM_mirt))))
fs_GRM_ltm <- ltm::factor.scores(fit_GRM_ltm)$score.dat[, "z1"]

cor(fs_GRM_mirt, fs_GRM_ltm)
## [1] 0.9066

df <- data.frame(fs_GRM_mirt, fs_GRM_ltm)
ggplot(data = df, aes(x = fs_GRM_mirt, y = fs_GRM_ltm)) +
  geom_point() +
  theme_fig() +
  xlab("Factor scores by mirt") + ylab("Factor scores by ltm") + 
  geom_smooth(method = "lm", se = FALSE)
#--------------

#-----------------------------------------------------------------
# 6.2.2  Graded ratings scale model
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
# 6.3  Adjacent-categories logit
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 6.3.1  Generalized partial credit model
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
# 6.3.2  Partial credit model
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
# 6.3.3  Rating scale model
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
# 6.4  Baseline-category logit
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 6.4.1  Nominal response model
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
