#-----------------------------------------------------------------
# Chapter 7 - Item response theory models
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

#-----------------------------------------------------------------
# 7.4.1  The mirt package
#-----------------------------------------------------------------

#--------------
library(mirt)
?mirt
#--------------

#--------------
# load the data
data(HCI, package = "ShinyItemAnalysis")
#--------------

#--------------
# explore the data (code not shown in the book)
head(HCI)
summary(HCI)
#--------------

#--------------
# Rasch model
fit_rasch_mirt <- mirt(data = HCI[, 1:20], model = 1, itemtype = "Rasch", 
                       SE = TRUE)
#--------------

#--------------
# coefficients - intercept/slope parametrization d + a1 * x
coef(fit_rasch_mirt)
## $`Item 1`
##         a1     d  g  u
## par      1 0.963  0  1
## CI_2.5  NA 0.774 NA NA
## CI_97.5 NA 1.152 NA NA
##  ...
## $GroupPars
##         MEAN_1 COV_11
## par          0  0.669
## CI_2.5      NA  0.558
## CI_97.5     NA  0.780
#--------------

#--------------
# coefficients - intercept/slope parametrization, SE instead of CI (not shown in the book)
coef(fit_rasch_mirt, printSE = TRUE)
## $`Item 1`
##     a1     d logit(g) logit(u)
## par  1 0.963     -999      999
## SE  NA 0.097       NA       NA
##  ...
#--------------

#--------------
# coefficients - IRT parametrization a * (x - b)
coef(fit_rasch_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##         a      b g u
## Item 1  1 -0.963 0 1
## Item 2  1 -1.267 0 1
## ...
## Item 20 1 -1.089 0 1
#--------------

#--------------
# code not shown in the book
sqrt(0.669) # 0.818 is the SD of the latent trait
0.963 / sqrt(0.669) # latent trait 1.177 below average is needed
                    # to answer Item 1 correctly with probability 0.5
#--------------

#--------------
# TSC (test score curve)
plot(fit_rasch_mirt)
#--------------

#--------------
# ICC
plot(fit_rasch_mirt, type = "trace", facet_items = FALSE)
#--------------

#--------------
# further plots (not displayed in the book)
plot(fit_rasch_mirt, type = "trace", facet_items = TRUE) # ICC separately
plot(fit_rasch_mirt, type = "infotrace", facet_items = FALSE) # Item information curves (IIC)
plot(fit_rasch_mirt, type = "infotrace", facet_items = TRUE) # IICs separately
plot(fit_rasch_mirt, type = "info", facet_items = FALSE) # Test information curve (TIC)
plot(fit_rasch_mirt, type = "infoSE", facet_items = FALSE) # TIC and SE
#--------------

#--------------
# latent abilities (factor scores) with SE
fs_rasch_mirt_SE <- fscores(fit_rasch_mirt, full.scores.SE = TRUE)
head(fs_rasch_mirt_SE, n = 3)
##          F1  SE_F1
## [1,] 0.7096 0.4609
## [2,] 1.4169 0.5149
## [3,] 0.9286 0.4756
#--------------

#--------------
# mean of ability estimates is about 0
summary(fs_rasch_mirt_SE[, 1])
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## -1.7051 -0.4252 -0.0680 -0.0001  0.5028  1.6951
#--------------

#--------------
# SD of ability estimates is close to 0.669 (code not shown in the book)
sd(fs_rasch_mirt_SE[, 1])
## [1] 0.6899
#--------------

#--------------
# comparison with total test score (code not shown in the book)
cor(HCI$total, fs_rasch_mirt_SE[, 1])
# [1] 0.9985

library(ggplot2)
ggplot(
  data.frame(HCI$total, fs = fs_rasch_mirt_SE[, 1]),
  aes(x = HCI$total, y = fs)
) +
  geom_point(size = 1.8) +
  xlab("Total score") +
  ylab("Factor score") + 
  theme_fig()
#--------------

#--------------
# histogram of Rasch ability estimates(code not shown in the book)
ggplot(
  data.frame(fs = fs_rasch_mirt_SE[, 1]),
  aes(x = fs)
) +
  geom_histogram(bins = length(unique(HCI$total)), col = "black", fill = "gold") +
  xlab(expression(Ability~theta)) +
  ylab("Number of respondents") + 
  theme_fig()
#--------------

#--------------
# 2PL IRT model
fit_2PL_mirt <- mirt(HCI[, 1:20], model = 1, itemtype = "2PL")
# coefficients
coef(fit_2PL_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##             a      b g u
## Item 1  0.851 -1.146 0 1
## Item 2  0.717 -1.723 0 1
## ...
## Item 20 0.988 -1.162 0 1
#--------------

#--------------
# ICC
plot(fit_2PL_mirt, type = "trace", facet_items = FALSE)
#--------------

#--------------
# IIC
plot(fit_2PL_mirt, type = "infotrace", facet_items = FALSE)
#--------------

#--------------
# TIC
plot(fit_2PL_mirt, type = "infoSE")
#--------------

#--------------
# estimated latent abilities (code not shown in the book)
fs_2PL_mirt <- as.vector(fscores(fit_2PL_mirt))
summary(fs_2PL_mirt)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## -2.0438 -0.6429 -0.0262  0.0000  0.6541  1.8618
#--------------

#--------------
# comparison with total test score (code not shown in the book)
cor(HCI$total, fs_2PL_mirt)
# [1] 0.9695

ggplot(
  data.frame(HCI$total, fs = fs_2PL_mirt),
  aes(x = HCI$total, y = fs)
) +
  geom_point(size = 1.8) +
  xlab("Total score") +
  ylab("Factor score") + 
  theme_fig()
#--------------

#--------------
# obtaining details on values of model parameters
mirt(HCI[, 1:20], model = 1, itemtype = "2PL", pars = "values") # a1 parnum are 1, 5, 9,...
##  group   item class name parnum   value lbound ubound   est prior.type prior_1 prior_2
## 1  all Item 1  dich   a1      1  0.8510   -Inf    Inf  TRUE       none     NaN     NaN
## 2  all Item 1  dich    d      2  1.0245   -Inf    Inf  TRUE       none     NaN     NaN
## 3  all Item 1  dich    g      3  0.0000  0e+00      1 FALSE       none     NaN     NaN
## 4  all Item 1  dich    u      4  1.0000  0e+00      1 FALSE       none     NaN     NaN
## 5  all Item 2  dich   a1      5  0.8510   -Inf    Inf  TRUE       none     NaN     NaN
## 6  all Item 2  dich    d      6  1.3422   -Inf    Inf  TRUE       none     NaN     NaN
## ...
#--------------

#--------------
# 1PL model fitted as 2PL with slope a1 parameters constrained to be equal
fit_1PL_mirt <- mirt(HCI[, 1:20], model = 1, itemtype = "2PL",
                     constrain = list((1:20) * 4 - 3))
coef(fit_1PL_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##             a      b g u
## Item 1  0.818 -1.178 0 1
## Item 2  0.818 -1.550 0 1
## ...
#--------------

#--------------
# 3PL IRT model with default setting (code not shown in the book)
fit_3PL_mirt <- mirt(HCI[, 1:20], model = 1, itemtype = "3PL")
## EM cycles terminated after 500 iterations.
#--------------

#--------------
# 3PL IRT model with NCYCLES increased to 2000
fit_3PL_mirt <- mirt(HCI[, 1:20], model = 1, itemtype = "3PL", 
                     technical = list(NCYCLES = 2000))
# coefficients
coef(fit_3PL_mirt, IRTpars = TRUE, simplify = TRUE)
## $items
##             a      b     g u
## Item 1  1.080 -0.331 0.296 1
## Item 2  0.759 -1.353 0.135 1
## ...
## Item 20 0.952 -1.041 0.079 1
#--------------

#--------------
# Plots (not shown in the book)
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
# latent abilities (not shown in the book)
fs_3PL_mirt <- as.vector(fscores(fit_3PL_mirt))
summary(fs_3PL_mirt)
#--------------

#--------------
# 4PL IRT model (not shown in the book)
fit_4PL_mirt <- mirt(HCI[, 1:20], model = 1, itemtype = "4PL", SE = TRUE)
summary(fit_4PL_mirt)
coef(fit_4PL_mirt)
## $`Item 1`
##       a1     d     g     u
## par 1.22 0.087 0.378 0.999
## ...
## $GroupPars
## MEAN_1 COV_11
## par      0      1

## Warning message:
## Could not invert information matrix; model likely is not 
## empirically identified.
#--------------

#--------------
# coefficients
coef(fit_4PL_mirt, printSE = TRUE) # SE not calculated, thus not printed
coef(fit_4PL_mirt, IRTpars = TRUE, simplify = TRUE)
#--------------

#--------------
# model selection (X2 test not appropriate for selection btw. 2PL, 3PL, and 4PL)
anova(fit_1PL_mirt, fit_2PL_mirt, fit_3PL_mirt, fit_4PL_mirt)
##                AIC SABIC    HQ   BIC logLik      X2 df     p
## fit_1PL_mirt 15278 15306 15315 15372  -7618             
## fit_2PL_mirt 15133 15186 15203 15313  -7527 182.928 19     0
## fit_3PL_mirt 15104 15182 15208 15373  -7492  69.468 20     0
#--------------

#--------------
# model fit (code not shown in the book)
M2(fit_1PL_mirt)
M2(fit_2PL_mirt)
M2(fit_3PL_mirt)
#--------------

#--------------
# item fit (code not shown in the book)
itemfit(fit_1PL_mirt)
itemfit(fit_2PL_mirt)
itemfit(fit_3PL_mirt)
#--------------

#--------------
# person fit (code not shown in the book)
personfit(fit_1PL_mirt)
personfit(fit_2PL_mirt)
personfit(fit_3PL_mirt)
#--------------

#-----------------------------------------------------------------
# 7.4.2  The ltm package
#-----------------------------------------------------------------

#--------------
library(ltm)
#--------------

#--------------
# 1PL model, discrimination not fixed
fit_1PL_ltm <- rasch(HCI[, 1:20])
#--------------

#--------------
# model summary (code not shown in the book)
summary(fit_1PL_ltm)  
#--------------

#--------------
# coefficients - IRT parametrization
coef(fit_1PL_ltm)
##           Dffclt Dscrmn
##  Item 1  -1.1774 0.8179
##  Item 2  -1.5490 0.8179
## ...
##  Item 20 -1.3314 0.8179
#--------------

#--------------
# Rasch model, discrimination fixed at value of 1
fit_rasch_ltm <- rasch(HCI[, 1:20], constraint = cbind(20 + 1, 1))
#--------------

#--------------
# model summary (code not shown in the book)
summary(fit_rasch_ltm)
#--------------

#--------------
# coefficients - IRT parametrization
coef(fit_rasch_ltm)
##          Dffclt Dscrmn
## Item 1  -0.9892      1
## Item 2  -1.2996      1
## ...
## Item 20 -1.1181      1
#--------------

#--------------
# ICC
plot(fit_rasch_ltm)
#--------------

#--------------
# 2PL IRT model in the ltm package
fit_2PL_ltm <- ltm(HCI[, 1:20] ~ z1)
coef(fit_2PL_ltm)
##          Dffclt Dscrmn
## Item 1  -1.1459 0.8511
## Item 2  -1.7226 0.7167
## ...
## Item 20 -1.1617 0.9886
#--------------

#--------------
# 3PL IRT model in ltm package
fit_3PL_ltm <- tpm(HCI[, 1:20])
coef(fit_3PL_ltm)
##         Gussng  Dffclt Dscrmn
## Item 1  0.2983 -0.3177 1.0864
## Item 2  0.1785 -1.2092 0.7837
## ...
## Item 20 0.0678 -1.0653 0.9437
#--------------

#-------------- 
# latent abilities (factor scores)
ltm::factor.scores(fit_rasch_ltm)
## Factor-Scores for observed response patterns:
##    Item 1 Item 2 ... Item 18 Item 19 Item 20 Obs   Exp     z1 se.z1
## 1       0      0 ...       1       1       0   1 0.002 -1.450 0.454
## 2       0      0 ...       0       0       0   1 0.017 -1.885 0.481
## 3       0      0 ...       1       1       0   1 0.000 -1.248 0.446
#--------------

#-------------- 
ltm::factor.scores(fit_rasch_ltm, resp.patterns = HCI[, 1:20])
## Factor-Scores for specified response patterns:
##    Item 1 Item 2 ... Item 18 Item 19 Item 20 Obs   Exp     z1 se.z1
## 1       1      1 ...       1       1       1   2 0.188  0.754 0.488
## 2       1      1 ...       1       1       1   3 0.991  1.578 0.570
## 3       1      1 ...       1       1       1   1 0.116  1.002 0.509
#--------------

#-------------- 
ltm::factor.scores(fit_rasch_ltm, 
                   resp.patterns = matrix(rep(c(1, 0), each = 10), nrow = 1))
## Factor-Scores for specified response patterns:
##    Item 1 Item 2 ... Item 18 Item 19 Item 20 Obs Exp     z1 se.z1
## 1       1      1 ...       0       0       0   0   0 -0.482 0.436
#--------------

#-----------------------------------------------------------------
# 7.4.3  The eRm package
#-----------------------------------------------------------------

#--------------
library(eRm)
#--------------

#--------------
# Rasch model with sum = 0 beta restriction
fit_rasch_eRm1 <- RM(X = HCI[, 1:20])
#--------------

#--------------
# model summary (code not shown in the book)
fit_rasch_eRm1
summary(fit_rasch_eRm1)
#--------------

#--------------
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
# shifted by mean ability estimate
lat_var <- person.parameter(fit_rasch_eRm1)
lat_var$thetapar[1] # not shown in the book
mean(as.numeric(unlist(lat_var$thetapar)))
## [1] 0.5789
coef(fit_rasch_eRm1) + mean(as.numeric(unlist(lat_var$thetapar)))
## beta Item 1  beta Item 2  beta Item 3  beta Item 4  beta Item 5 
##      0.9753       1.2784       1.9409      -0.4348      -0.2537 
## beta Item 6  beta Item 7  beta Item 8  beta Item 9 beta Item 10 
##     -0.6361       0.2294       1.0084      -0.2968       0.7139 
## beta Item 11 beta Item 12 beta Item 13 beta Item 14 beta Item 15 
##       1.4398       0.3512       0.5187       1.3154      -0.2466 
## beta Item 16 beta Item 17 beta Item 18 beta Item 19 beta Item 20 
##       0.5113      -0.9775       1.5612       1.4795       1.1012 
#--------------

#--------------
# Rasch model with beta.1 restricted to 0 (not shown in the book)
fit_rasch_eRm2 <- RM(X = HCI[, 1:20], sum0 = FALSE)
fit_rasch_eRm2
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

#-----------------------------------------------------------------
# 7.4.4  Other IRT packages
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 7.4.4.1  The TAM package
#-----------------------------------------------------------------

#--------------
library(TAM)
#--------------

#--------------
# JML estimation with the TAM package
fit_rasch_TAM1 <- tam.jml(resp = HCI[, 1:20])
fit_rasch_TAM1$xsi
##  [1] -0.9855 -1.2883 -1.9470  0.4327  0.2496  0.6366 -0.2373 -1.0186
##  [9]  0.2932 -0.7238 -1.4491 -0.3598 -0.5281 -1.3251  0.2424 -0.5207
## [17]  0.9830 -1.5700 -1.4887 -1.1113
#--------------

#--------------
# MML estimation with the TAM package
fit_rasch_TAM2 <- tam.mml(resp = HCI[, 1:20], irtmodel = "Rasch") 
fit_rasch_TAM2$xsi[, 1]
##  [1] -0.9629 -1.2670 -1.9336  0.4445  0.2641  0.6453 -0.2175 -0.9961
##  [9]  0.3070 -0.7013 -1.4291 -0.3390 -0.5062 -1.3041  0.2569 -0.4989
## [17]  0.9858 -1.5512 -1.4690 -1.0891
#--------------

#-----------------------------------------------------------------
# 7.4.4.2  The ShinyItemAnalysis package
#-----------------------------------------------------------------

#--------------
library(ShinyItemAnalysis)
#--------------

#--------------
# Wright map with the ShinyItemAnalysis package
b <- coef(fit_1PL_mirt, simplify = TRUE, IRTpars = TRUE)$items[, "b"]
fs_1PL_mirt <- as.vector(fscores(fit_1PL_mirt))
ggWrightMap(theta = fs_1PL_mirt, b = b)
#--------------

#-----------------------------------------------------------------
# 7.4.5  IRT models in the lme4 and nlme package
#-----------------------------------------------------------------

#--------------
library(lme4)
#--------------

#--------------
# HCI data long format:
data("HCIlong", package = "ShinyItemAnalysis")
#--------------

#--------------
# explore the data (code not shown in the book)
head(HCIlong)
HCIlong$zscore <- (HCIlong$total - mean(HCI$total))/sd(HCI$total)
HCIlong$id <- factor(HCIlong$id)
HCIlong$item <- factor(gsub("Item ", "", HCIlong$item), levels = 1:20)
summary(HCIlong)
#--------------

#--------------
# fit Rasch model with lme4 (TAKES FEW MINUTES!)
fit_rasch_lme4 <- glmer(rating ~ -1 + item + (1 | id), data = HCIlong, 
                        family = binomial)
coef(fit_rasch_lme4)$id[1, -1]
##    Item1 Item2 Item3   Item4   Item5  Item6  Item7  Item8   Item9
## 1 0.9494 1.266 1.925 -0.4515 -0.2701 -0.653 0.2068 0.9911 -0.3128
##   Item10 Item11 Item12 Item13 Item14  Item15 Item16  Item17 Item18
## 1 0.6961  1.424 0.3299  0.502  1.299 -0.2613 0.4977 -0.9936  1.549
##   Item19 Item20
## 1 1.4670 1.0880
#--------------

#-----------------------------------------------------------------
# 7.4.6  Bayesian IRT with the brms package
#-----------------------------------------------------------------

#--------------
library(brms)
#--------------

#--------------
# brms package (Bayesian 1PL IRT). NOTE: this analysis takes several minutes
formula_1PL <- bf(rating ~ 1 + (1 | item) + (1 | id))
# formula_1PL <- bf(rating ~ 0 + item + (1 | id))
prior_1PL <- prior("normal(0, 3)", class = "sd", group = "id") +
             prior("normal(0, 3)", class = "sd", group = "item")
# prior_1PL <- prior("normal(0, 3)", class = "sd", group = "id")

fit_1PL_brms <- brm(formula = formula_1PL, data = HLIlong, prior = prior_1PL,
                    family = brmsfamily("bernoulli", "logit"), seed = 123)
coef(fit_1PL_brms)$item[, , "Intercept"]
##     Estimate Est.Error     Q2.5    Q97.5
##  1    0.9602   0.09574  0.76907  1.15350
##  2    1.2570   0.09977  1.06261  1.45843
##  3    1.9112   0.11642  1.68002  2.14513
## ...
## 20    1.0797   0.09478  0.89838  1.26513
#--------------

#--------------
plot(fit_1PL_brms)
#--------------

#-----------------------------------------------------------------
# Comparison of R packages for IRT (not shown in the book)
#-----------------------------------------------------------------

#--------------
b_mirt <- coef(fit_rasch_mirt, IRTpars = TRUE, 
               simplify = TRUE)$items[, "b"]
b_ltm <- coef(fit_rasch_ltm)[, 1]
b_eRm1 <- -coef(fit_rasch_eRm1) - mean(as.numeric(unlist(lat_var$thetapar)))
b_lme4 <- -unlist(coef(fit_rasch_lme4)$id[1, -1])
b_TAMj <- fit_rasch_TAM1$xsi
b_TAMm <- fit_rasch_TAM2$xsi[, 1]
b_brms <- -coef(fit_1PL_brms)$item[, "Estimate", "Intercept"]

cbind(b_mirt, b_ltm, b_eRm1, b_lme4, b_TAMj, b_TAMm, b_brms)
rbind(b_mirt, b_ltm, b_eRm1, b_lme4, b_TAMj, b_TAMm, b_brms)

##         Item 1 Item 2 Item 3 Item 4 Item 5 Item 6  Item 7  Item 8 Item 9
## b_mirt -0.9632 -1.267 -1.934 0.4443 0.2638 0.6450 -0.2177 -0.9963 0.3068
## b_ltm  -0.9892 -1.300 -1.979 0.4531 0.2680 0.6591 -0.2257 -1.0230 0.3120
## b_eRm1 -0.9753 -1.278 -1.941 0.4348 0.2537 0.6361 -0.2294 -1.0084 0.2968
## b_lme4 -0.9494 -1.266 -1.925 0.4515 0.2701 0.6530 -0.2068 -0.9911 0.3128
## b_TAMj -0.9855 -1.288 -1.947 0.4327 0.2496 0.6366 -0.2373 -1.0186 0.2932
## b_TAMm -0.9660 -1.270 -1.936 0.4412 0.2608 0.6420 -0.2208 -0.9992 0.3037
## b_brms -0.9608 -1.261 -1.914 0.4348 0.2560 0.6357 -0.2220 -0.9906 0.2961
## 
##        Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 Item 17
## b_mirt -0.7016  -1.429 -0.3392 -0.5065  -1.304  0.2567 -0.4991  0.9856
## b_ltm  -0.7215  -1.465 -0.3504 -0.5218  -1.338  0.2607 -0.5143  1.0084
## b_eRm1 -0.7139  -1.440 -0.3512 -0.5187  -1.315  0.2466 -0.5113  0.9775
## b_lme4 -0.6961  -1.424 -0.3299 -0.5020  -1.299  0.2613 -0.4977  0.9936
## b_TAMj -0.7238  -1.449 -0.3598 -0.5281  -1.325  0.2424 -0.5207  0.9830
## b_TAMm -0.7046  -1.432 -0.3423 -0.5095  -1.307  0.2536 -0.5022  0.9825
## b_brms -0.7013  -1.424 -0.3428 -0.5102  -1.295  0.2471 -0.5001  0.9712
## 
##        Item 18 Item 19 Item 20
## b_mirt  -1.551  -1.469  -1.089
## b_ltm   -1.589  -1.506  -1.118
## b_eRm1  -1.561  -1.479  -1.101
## b_lme4  -1.549  -1.467  -1.088
## b_TAMj  -1.570  -1.489  -1.111
## b_TAMm  -1.554  -1.472  -1.093
## b_brms  -1.537  -1.459  -1.089
#--------------

#--------------
# traditional difficulty estimates
(dif_trad <- colMeans(HCI[, 1:20])) # traditional difficulty estimates
##  Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8  Item 9
##  0.6989  0.7527  0.8479  0.4040  0.4424  0.3625  0.5469  0.7051  0.4332
## Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 Item 17 Item 18
##  0.6482  0.7788  0.5730  0.6083  0.7588  0.4439  0.6068  0.2965  0.7972
## Item 19 Item 20
##  0.7849  0.7220

# as.vector(scale(dif_trad))
cor(b_mirt, dif_trad)
## [1] -0.998
plot(b_mirt, dif_trad)
#--------------

#-----------------------------------------------------------------
# 7.5.1 Relationship between IRT and factor analysis
#-----------------------------------------------------------------

#--------------
library(psych)
#--------------

#--------------
corHCI <- tetrachoric(HCI[, 1:20])$rho
(tau <- tetrachoric(HCI[, 1:20])$tau) # thresholds
##  Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8 
## -0.5213 -0.6830 -1.0276  0.2430  0.1449  0.3517 -0.1177 -0.5390 
##  Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 
##  0.1683 -0.3806 -0.7682 -0.1839 -0.2749 -0.7026  0.1410 -0.2709 
## Item 17 Item 18 Item 19 Item 20 
##  0.5346 -0.8318 -0.7890 -0.5887 
#--------------

#--------------
FA1 <- fa(HCI[, 1:20], nfactors = 1, fm = "mle", cor = "poly")
(alpha <- c(FA1$loadings)) # factor loadings
##  [1] 0.4441 0.3863 0.6148 0.1987 0.4161 0.5133 0.1522 0.5296 0.2832
## [10] 0.4189 0.4763 0.4477 0.5652 0.5389 0.4602 0.5521 0.0448 0.6966
## [19] 0.6066 0.4989

q <- sqrt(c(FA1$uniquenesses)) # q = sqrt(1 - alpha^2), uniqueness

D <- 1.702 # scaling parameter
beta1FA <- D * alpha / q # slope
beta0FA <- -D * tau / q  # intercept
#--------------

#--------------
# fit_2PL_mirt <- mirt(HCI[, 1:20], model = 1, itemtype = "2PL")
beta1IRT <- coef(fit_2PL_mirt, simplify = TRUE)$items[, "a1"]
beta0IRT <- coef(fit_2PL_mirt, simplify = TRUE)$items[, "d"]
#--------------

#--------------
rbind(beta1FA, beta1IRT)
##          Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 ...
## beta1FA  0.8437 0.7128 1.3269 0.3450 0.7788 1.0180 0.2621 ...
## beta1IRT 0.8509 0.7167 1.5745 0.3467 0.7275 1.0115 0.2257 ...
rbind(beta0FA, beta0IRT)
##          Item 1 Item 2 Item 3  Item 4  Item 5  Item 6 Item 7 ...
## beta0FA  0.9903 1.2602 2.2176 -0.4220 -0.2712 -0.6976 0.2027 ...
## beta0IRT 0.9754 1.2348 2.4033 -0.3995 -0.2545 -0.6761 0.1905 ...

cor(beta1FA, beta1IRT)
## [1] 0.9908
cor(beta0FA, beta0IRT)
## [1] 0.9987

plot(beta1FA, beta1IRT, xlim = c(0, 2), ylim = c(0, 2))
abline(coef = c(0, 1))
plot(beta0FA, beta0IRT, xlim = c(-1, 3), ylim = c(-1, 3))
abline(coef = c(0, 1))
#--------------

#--------------
# extracting FA parameters from mirt()
summary(fit_2PL_mirt)
##             F1      h2
## Item 1  0.4472 0.19997
## Item 2  0.3881 0.15060
## Item 3  0.6791 0.46114
## ...

# F1 from mirt() corresponds to factor loadings from fa()
unclass(FA1$loadings)
##            ML1
## Item 1  0.4441
## Item 2  0.3863
## Item 3  0.6148
## ...

# h2 from mirt() corresponds to communalities from fa()
FA1$communalities
##  Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8 
##  0.1973  0.1492  0.3780  0.0395  0.1731  0.2635  0.0232  0.2804 
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 
##  0.0802  0.1755  0.2268  0.2004  0.3195  0.2904  0.2118  0.3048 
## Item 17 Item 18 Item 19 Item 20 
##  0.0020  0.4853  0.3679  0.2489 
#--------------

#--------------
# psych::fit_irtfa() function (code not shown in the book)
fit_irtfa <- irt.fa(HCI[, 1:20], nfactors = 1, fm = "ml", cor = "poly")
fit_irtfa$tau
b_irtfa <- c(fit_irtfa$irt$difficulty[[1]])
a_irtfa <- c(fit_irtfa$irt$discrimination)

cor(beta1FA, a_irtfa) # 1
cor(beta0FA, b_irtfa) # -1

-D * b_irtfa # equals beta0FA
D * a_irtfa # equals beta1FA
#--------------

#--------------
# ICC, IIC, and TIC plots with irtfa()
plot(fit_irtfa, type = "ICC")
plot(fit_irtfa, type = "IIC")
plot(fit_irtfa, type = "test")
#--------------

#-----------------------------------------------------------------
# 7.7. IRT models in interactive application
#-----------------------------------------------------------------

startShinyItemAnalysis()
