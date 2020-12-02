#-----------------------------------------------------------------
# Chapter 7 - Item response theory models
# Introduction to psychometric methods
# in education, psychology, and health.
# With examples in R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(eRm)
library(ggplot2)
library(lme4)
library(ltm)
library(mirt)
library(reshape)
library(ShinyItemAnalysis)

#-----------------------------------------------------------------
# 7.3.1  Rasch model and 1PL IRT model
#-----------------------------------------------------------------

#--------------
data(HCI, package = "ShinyItemAnalysis")
head(HCI)
summary(HCI)
#--------------

#--------------
fit_rasch_mirt <- mirt(
  data = HCI[, 1:20], model = 1, itemtype = "Rasch",
  SE = TRUE
)
#--------------

#--------------
# coefficients - intercept/slope parametrization d + a1 * x with SE added
coef(fit_rasch_mirt, SE = TRUE)
## $`Item 1`
##         a1     d  g  u
## par      1 0.963  0  1
## CI_2.5  NA 0.774 NA NA
## CI_97.5 NA 1.152 NA NA
##
## $`Item 2`
##  ...
## $GroupPars
##         MEAN_1 COV_11
## par          0  0.669
## CI_2.5      NA  0.558
## CI_97.5     NA  0.780
#--------------

#--------------
# coefficients - IRT parametrization a * (x - b)
coef(fit_rasch_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
## a      b g u
## Item 1  1 -0.963 0 1
## Item 2  1 -1.267 0 1
## Item 3  1 -1.934 0 1
## ...
## Item 20 1 -1.089 0 1
#--------------

#--------------
# ICC
plot(fit_rasch_mirt, type = "trace", facet_items = FALSE)
#--------------

#--------------
# test score curve
plot(fit_rasch_mirt)
#--------------

#--------------
# latent abilities (factor scores)
fs <- as.vector(fscores(fit_rasch_mirt))
head(fs)
## [1] 0.7096 1.4169 0.9286 1.6951 1.4169 1.6951
summary(fs)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## -1.7051 -0.4252 -0.0680 -0.0001  0.5028  1.6951
sd(fs)
## [1] 0.6899
#--------------

#--------------
# latent abilities (factor scores) with SE
fs_SE <- fscores(fit_rasch_mirt, full.scores.SE = TRUE)
head(fs_SE, n = 3)
##          F1  SE_F1
## [1,] 0.7096 0.4609
## [2,] 1.4169 0.5149
## [3,] 0.9286 0.4756
#--------------

#--------------
# comparison with total test score
total_score <- rowSums(HCI[, 1:20])
cor(total_score, fs)
# [1] 0.9985

ggplot(
  data.frame(total_score, fs),
  aes(x = total_score, y = fs)
) +
  geom_point() +
  theme_app() +
  xlab("Total score") +
  ylab("Factor score")
#--------------

#--------------
# ltm package
# Rasch model, discrimination fixed at value of 1
fit_rasch_ltm <- rasch(
  HCI[, 1:20],
  constraint = cbind(20 + 1, 1)
)

# coefficients - IRT parametrization
coef(fit_rasch_ltm)
##          Dffclt Dscrmn
## Item 1  -0.9892      1
## Item 2  -1.2996      1
## Item 3  -1.9787      1
## ...
## Item 20 -1.1181      1

# ICC
plot(fit_rasch_ltm)

# factor scores
head(factor.scores(fit_rasch_ltm)$score.dat[, c("z1", "se.z1")], n = 3)
##        z1  se.z1
## 1 -1.4503 0.4540
## 2 -1.8849 0.4807
## 3 -1.2482 0.4456
#--------------

#--------------
# eRm package
# Rasch model with sum-0 beta restriction
fit_rasch_eRm1 <- RM(X = HCI[, 1:20])
coef(fit_rasch_eRm1)
## beta Item 1  beta Item 2  beta Item 3  beta Item 4  beta Item 5
##      0.3963       0.6994       1.3619      -1.0137      -0.8327
## beta Item 6  beta Item 7  beta Item 8  beta Item 9 beta Item 10
##     -1.2151      -0.3496       0.4294      -0.8758       0.1350
## beta Item 11 beta Item 12 beta Item 13 beta Item 14 beta Item 15
##      0.8608      -0.2278      -0.0602       0.7364      -0.8255
## beta Item 16 beta Item 17 beta Item 18 beta Item 19 beta Item 20
##     -0.0676      -1.5564       0.9823       0.9005       0.5222
#--------------

#--------------
# Rasch model with beta.1 restricted to 0
fit_rasch_eRm2 <- RM(X = HCI[, 1:20], sum0 = FALSE)
coef(fit_rasch_eRm2)
## beta Item 1  beta Item 2  beta Item 3  beta Item 4  beta Item 5
##      0.0000       0.3031       0.9656      -1.4101       -1.229
## beta Item 6  beta Item 7  beta Item 8  beta Item 9 beta Item 10
##     -1.6114      -0.7459       0.0330      -1.2722      -0.2614
## beta Item 11 beta Item 12 beta Item 13 beta Item 14 beta Item 15
##      0.4645      -0.6241      -0.4566       0.3401      -1.2219
## beta Item 16 beta Item 17 beta Item 18 beta Item 19 beta Item 20
##     -0.4640      -1.9528       0.5859       0.5042       0.1259
#--------------

#--------------
# lme4 package
# data long format:
HCI$ID <- as.factor(1:dim(HCI)[1])
HCIlong <- melt(HCI,
  id.vars = c("ID", "gender", "major"),
  measure.vars = c(1:20),
  variable_name = "Item"
)
levels(HCIlong$Item) <- 1:20
summary(HCIlong)
head(HCIlong)
#--------------

#--------------
# fit Rasch model with lme4 (TAKES FEW MINUTES!)
fit_rasch_glmer <- glmer(value ~ -1 + Item + (1 | ID),
  data = HCIlong,
  family = binomial
)
coef(fit_rasch_glmer)$ID[1, -1]
##    Item1 Item2 Item3   Item4   Item5  Item6  Item7  Item8   Item9
## 1 0.9494 1.266 1.925 -0.4515 -0.2701 -0.653 0.2068 0.9911 -0.3128
##   Item10 Item11 Item12 Item13 Item14  Item15 Item16  Item17 Item18
## 1 0.6961  1.424 0.3299  0.502  1.299 -0.2613 0.4977 -0.9936  1.549
##   Item19 Item20
## 1 1.4670 1.0880
#--------------

# #--------------
# # traditional difficulty estimates
# (dif_trad <- colMeans(HCI[, 1:20])) # traditional difficulty estimates
# ##  Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8  Item 9
# ##  0.6989  0.7527  0.8479  0.4040  0.4424  0.3625  0.5469  0.7051  0.4332
# ## Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 Item 17 Item 18
# ##  0.6482  0.7788  0.5730  0.6083  0.7588  0.4439  0.6068  0.2965  0.7972
# ## Item 19 Item 20
# ##  0.7849  0.7220
#
# as.vector(scale(dif_trad))
# #--------------

#--------------
# Wright map
b <- coef(fit_rasch_mirt, simplify = TRUE)$items[, "d"]
ggWrightMap(fs, b)
#--------------

#-----------------------------------------------------------------
# 7.3.2 2PL IRT model
#-----------------------------------------------------------------

#--------------
# mirt() of mirt package
fit_2PL_mirt <- mirt(
  HCI[, 1:20],
  model = 1, itemtype = "2PL", SE = TRUE
)
# coefficients
coef(fit_2PL_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##             a      b g u
## Item 1  0.851 -1.146 0 1
## Item 2  0.717 -1.723 0 1
## Item 3  1.574 -1.526 0 1
## ..
#--------------

#--------------
# ICC
plot(fit_2PL_mirt, type = "trace", facet_items = FALSE)
# IIC
plot(fit_2PL_mirt, type = "infotrace", facet_items = FALSE)
# TIC
plot(fit_2PL_mirt, type = "infoSE")
# test score curve
plot(fit_2PL_mirt)
#--------------

#--------------
# latent abilities (a.k.a factor scores)
fs <- as.vector(fscores(fit_2PL_mirt))
summary(fs)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## -2.0438 -0.6429 -0.0262  0.0000  0.6541  1.8618
#--------------

#--------------
# ltm package
fit_2PL_ltm <- ltm(HCI[, 1:20] ~ z1)
coef(fit_2PL_ltm)
##          Dffclt Dscrmn
## Item 1  -1.1459 0.8511
## Item 2  -1.7226 0.7167
## Item 3  -1.5259 1.5746
## ...
## Item 20 -1.1617 0.9886
#--------------

#-----------------------------------------------------------------
# 7.3.3 Normal ogive model
#-----------------------------------------------------------------

# TO BE ADDED

#-----------------------------------------------------------------
# 7.3.4 3PL IRT model
#-----------------------------------------------------------------

#--------------
fit_3PL_mirt <- mirt(HCI[, 1:20],
  model = 1, itemtype = "3PL", SE = TRUE,
  technical = list(NCYCLES = 2000)
)
# coefficients
coef(fit_3PL_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##             a      b     g u
## Item 1  1.080 -0.331 0.296 1
## Item 2  0.759 -1.353 0.135 1
## Item 3  2.408 -0.720 0.453 1
## Item 4  0.352  1.268 0.018 1
## Item 5  0.721  0.393 0.009 1
## ..
#--------------

#--------------
# test score function
plot(fit_3PL_mirt)
# ICC
plot(fit_3PL_mirt, type = "trace", facet_items = FALSE)
# IIC
plot(fit_3PL_mirt, type = "infotrace", facet_items = FALSE)
# TIC
plot(fit_3PL_mirt, type = "infoSE")
#--------------

#--------------
# latent abilities (a.k.a factor scores)
fs.se <- as.vector(fscores(fit_3PL_mirt))
summary(fs)
#--------------

#--------------
# ltm package
fit_3PL_ltm <- tpm(HCI[, 1:20])
coef(fit_3PL_ltm)
##         Gussng  Dffclt Dscrmn
## Item 1  0.2983 -0.3177 1.0864
## Item 2  0.1785 -1.2092 0.7837
## Item 3  0.4571 -0.7067 2.4263
## ...
## Item 20 0.0678 -1.0653 0.9437
#--------------

#-----------------------------------------------------------------
# 7.3.5 4PL IRT model
#-----------------------------------------------------------------

#--------------
fit_4PL_mirt <- mirt(
  HCI[, 1:20],
  model = 1, itemtype = "4PL", SE = TRUE
)
# Warning message:
# Could not invert information matrix; model likely is not empirically identified.
#--------------

#--------------
# ICC
plot(fit_4PL_mirt, type = "trace", facet_items = FALSE)
# IIC
plot(fit_4PL_mirt, type = "infotrace", facet_items = FALSE)
# TIC
plot(fit_4PL_mirt, type = "infoSE")
# test score function
plot(fit_4PL_mirt)
#--------------

#--------------
# coefficients
coef(fit_4PL_mirt, simplify = TRUE)
coef(fit_4PL_mirt, IRTpars = TRUE, simplify = TRUE)
#--------------

#--------------
# latent abilities (a.k.a factor scores)
fs <- as.vector(fscores(fit_4PL_mirt))
summary(fs)
#--------------

#-----------------------------------------------------------------
# 7.3.6 Item and test information
#-----------------------------------------------------------------

#--------------
# ICC
plot(fit_2PL_mirt, type = "trace", facet_items = FALSE)
# IIC
plot(fit_2PL_mirt, type = "infotrace", facet_items = FALSE)
# TIC
plot(fit_2PL_mirt, type = "infoSE")
#--------------

#-----------------------------------------------------------------
# 7.4  IRT models for polytomous items
#-----------------------------------------------------------------

#--------------
data("Anxiety", package = "lordif")
head(Anxiety[, paste0("R", 1:29)], n = 2)
##   R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16 R17
## 1  1  1  1  1  1  1  1  2  2   1   2   2   1   1   1   1   1
## 2  1  1  1  1  1  1  1  1  1   1   1   1   1   1   1   1   1
##   R18 R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29
## 1   2   1   2   2   1   1   2   3   2   1   1   2
## 2   1   1   1   1   1   1   1   2   1   1   1   1
#--------------

#-----------------------------------------------------------------
# 7.4.1  Graded response model
#-----------------------------------------------------------------

#--------------
# GRM with mirt package
fit_GRM_mirt <- mirt(
  Anxiety[, paste0("R", 1:29)],
  model = 1, itemtype = "graded", SE = TRUE
)

# coefficients with CI
coef(fit_GRM_mirt, IRTpars = TRUE)
## $R1
##             a    b1    b2    b3    b4
## par     3.449 0.494 1.251 2.031 2.814
## CI_2.5  2.983 0.394 1.116 1.824 2.448
## CI_97.5 3.915 0.594 1.387 2.238 3.179
## ...
#--------------

# coefficients in IRT parametrization (matrix form)
coef(fit_GRM_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##         a     b1    b2    b3    b4
## R1  3.449  0.494 1.251 2.031 2.814
## R2  3.168  0.568 1.426 2.253 3.253
##  ...

# coefficients in intercept-slope parametrization (matrix form)
coef(fit_GRM_mirt, simplify = TRUE)
## $items
##        a1     d1     d2     d3      d4
## R1  3.449 -1.703 -4.316 -7.005  -9.704
## R2  3.168 -1.800 -4.518 -7.138 -10.305
## ...

# IRC
plot(fit_GRM_mirt, type = "trace")
plot(fit_GRM_mirt, type = "trace", facet_items = FALSE)
# ICC
plot(fit_GRM_mirt, type = "itemscore")
plot(fit_GRM_mirt, type = "itemscore", facet_items = FALSE)
itemplot(fit_GRM_mirt, item = 1, type = "score")
# IIC
plot(fit_GRM_mirt, type = "infotrace")
plot(fit_GRM_mirt, type = "infotrace", facet_items = FALSE)
itemplot(fit_GRM_mirt, item = 1, type = "info")

itemplot(fit_GRM_mirt, item = 1, type = "infotrace")
itemplot(fit_GRM_mirt, item = 25, type = "infotrace")

itemplot(fit_GRM_mirt, item = 1)

# test score curve
plot(fit_GRM_mirt)
# test score curve with 95% CI
plot(fit_GRM_mirt, MI = 200)

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
# GRM in ltm package
fit_GRM_ltm <- ltm::grm(Anxiety[, paste0("R", 1:29)])
coef(fit_GRM_ltm)
##     Extrmt1 Extrmt2 Extrmt3 Extrmt4 Dscrmn
## R1    0.560   1.461   2.366   3.325  2.773
## R2    0.657   1.667   2.604   3.741  2.605
## ...
#--------------

#--------------
fs_GRM_mirt <- unique(sort(as.vector(fscores(fit_GRM_mirt))))
fs_GRM_ltm <- factor.scores(fit_GRM_ltm)$score.dat[, "z1"]
summary(fs_GRM_ltm)
plot(fs_GRM_mirt ~ fs_GRM_ltm)
cor(fs_GRM_mirt, fs_GRM_ltm)
## [1] 0.907
#--------------

#-----------------------------------------------------------------
# 7.4.2  Graded rating scale model
#-----------------------------------------------------------------

#--------------
# GRSM with IRT parametrization
fit_GRSMirt_mirt <- mirt(
  Anxiety[, paste0("R", 1:29)],
  model = 1, itemtype = "grsmIRT", SE = TRUE
)

# coefficients
coef(fit_GRSMirt_mirt, simplify = TRUE)
##  $items
##        a1     d1     d2     d3     d4      c
## R1  3.200 -0.454 -1.256 -2.214 -3.202  0.000
## R2  3.109 -0.454 -1.256 -2.214 -3.202 -0.110
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
# 7.4.3  Generalized partial credit model
#-----------------------------------------------------------------

#--------------
fit_GPCM_mirt <- mirt(
  Anxiety[, paste0("R", 1:29)],
  model = 1, itemtype = "gpcm", SE = TRUE
)
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
itemplot(fit_GPCM_mirt, item = 1, type = "infotrace") 
itemplot(fit_GPCM_mirt, item = 25, type = "infotrace") 
#--------------


#-----------------------------------------------------------------
# 7.4.4  Partial credit model
#-----------------------------------------------------------------

# NEEDS TO BE ADDED

#-----------------------------------------------------------------
# 7.4.5  Rating scale model
#-----------------------------------------------------------------

#--------------
fit_RSM_mirt <- mirt(
  Anxiety[, paste0("R", 1:29)],
  model = 1, itemtype = "rsm", SE = TRUE
)
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
itemplot(fit_RSM_mirt, item = 1, type = "infotrace")
itemplot(fit_RSM_mirt, item = 25, type = "infotrace") 
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
# 7.4.6  Nominal response model
#-----------------------------------------------------------------

# NEEDS TO BE ADDED, on HCI data

#-----------------------------------------------------------------
# 7.4.7  Item-specific models
#-----------------------------------------------------------------

#--------------
load("datasets/CZmatura/CZmathS_sample.RData")
names(CZmathS)
summary(CZmathS)

CZmathS$b7 <- CZmathS$b7.1 + CZmathS$b7.2
CZmathS$b8 <- CZmathS$b8.1 + CZmathS$b8.2
CZmathS$b9 <- CZmathS$b9.1 + CZmathS$b9.2
CZmathS <- CZmathS[, paste0("b", 1:26)]
head(CZmathS, n = 2)
##      b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18
## 2463  1  0  0  0  0  0  0  0  1   0   1   0   0   0   0   1   0   0
## 2511  1  1  1  2  1  2  3  1  1   1   1   1   1   3   1   2   2   2
##      b19 b20 b21 b22 b23 b24 b25 b26
## 2463   0   0   0   0   0   0   0   0
## 2511   2   2   2   2   2   2   4   3
summary(CZmathS)

# 17-24 are scored multiple-choice items (2 points awarded for correct answer)
CZmathS[, 17:24] <- as.numeric(CZmathS[, 17:24] == 2)
summary(CZmathS)

maxscore <- sapply(CZmathS, max) # maximal item scores
maxscore
# https://maturita.cermat.cz/files/files/Matematika/intaktni-zaci/MZ2020/MA_jaro_2020_klic.pdf
## b1  b2  b3  b4  b5  b6  b7  b8  b9 b10 b11 b12 b13 b14 b15 b16 b17
##  1   1   1   2   1   2   3   2   2   1   1   1   1   3   3   2   1
## b18 b19 b20 b21 b22 b23 b24 b25 b26
##   1   1   1   1   1   1   1   4   3
itemtype <- ifelse(maxscore == 1, "2PL", "gpcm")
itemtype[17:24] <- "3PL"
#--------------

#--------------
CZmathS_binary <- as.data.frame(key2binary(CZmathS, maxscore))
summary(CZmathS_binary)

fit_binary <- mirt(CZmathS_binary, model = 1, itemtype = "2PL")
fit_mixed <- mirt(CZmathS, model = 1, itemtype = itemtype)

# seems not appropriate, different data
# anova(fit_mixed, fit_binary)
#--------------

#--------------
head(df_fs_CERMAT <- data.frame(
  fs_mixed = as.vector(fscores(fit_mixed)),
  fs_binary = as.vector(fscores(fit_binary))
), n = 3)
##   fs_mixed fs_binary
## 1  -1.5180   -1.8272
## 2   1.3271    1.2489
## 3   1.4002    1.3361

ggplot(df_fs_CERMAT,
       aes(x = fs_binary, y = fs_mixed)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Factor scores - 2PL IRT model") + ylab("Factor scores - item-specific model") +
  theme_app()
# ggsave("figures/irt_factorscores_CERMAT.png",
#       width = 6, height = 4, dpi = 300, bg = "transparent")
cor(df_fs_CERMAT$fs_mixed, df_fs_CERMAT$fs_binary)
## [1] 0.9908
#--------------

#--------------
# TSC
plot(fit_mixed)
plot(fit_binary) 

# ICC selected items 
plot(fit_binary, type = "trace")
plot(fit_mixed, type = "trace")

# ICC selected items 2, 22 and 26
itemplot(fit_binary, item = 2, type = "infotrace")
itemplot(fit_mixed, item = 2, type = "infotrace")
itemplot(fit_binary, item = 22, type = "infotrace")
itemplot(fit_mixed, item = 22, type = "infotrace") # 3pl
itemplot(fit_binary, item = 26, type = "infotrace")
itemplot(fit_mixed, item = 26, type = "infotrace") # gpcm

# IIC
plot(fit_mixed, type = "infotrace", facet_items = FALSE)
plot(fit_binary, type = "infotrace", facet_items = FALSE)

# TIC
plot(fit_mixed, type = "infoSE")
plot(fit_binary, type = "infoSE")
#--------------

