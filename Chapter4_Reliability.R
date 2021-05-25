#-----------------------------------------------------------------
# Chapter 4 - Reliability in complex designs
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(brms)
library(ggplot2)
library(gtheory)
library(hemp)
library(lme4)
library(psych)
library(psychometric)
library(ShinyItemAnalysis)
library(sirt)
library(plotrix)
library(ggforce)
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

# margins for mirt plots
lw <- list(left.padding = list(x = 0.1, units = "inches"))
lw$right.padding <- list(x = -0.1, units = "inches")
lh <- list(bottom.padding = list(x = 0, units = "inches"))
lh$top.padding <- list(x = -0.2, units = "inches")

lattice.options(layout.widths = lw, layout.heights = lh)


#-----------------------------------------------------------------
# 4.2.5.1  Spearman-Brown prophecy formula
#-----------------------------------------------------------------

#--------------
rho.original <- 0.75 # reliability of original data
items.original <- 20 # number of items in original data
items.new <- 30 # number of items in new data
(m <- items.new / items.original) # ratio of tests lengths
## [1] 1.5

# new reliability
m * rho.original / (1 + (m - 1) * rho.original)
## [1] 0.8182
#--------------

#--------------
SBrel(Nlength = m, rxx = rho.original)
## [1] 0.8182
#--------------

#--------------
rho.new <- 0.85 # desired reliability
# determining test length
(m <- rho.new * (1 - rho.original) / (rho.original * (1 - rho.new)))
## [1] 1.8889
ceiling(m * items.original) # new test length
## [1] 38
#--------------

#--------------
(m <- SBlength(rxxp = rho.new, rxx = rho.original))
## [1] 1.8889
ceiling(m * items.original) # new test length
## [1] 38
#--------------

#--------------
rho.new <- 0.7 # desired reliability
(m <- rho.new * (1 - rho.original) / (rho.original * (1 - rho.new)))
## [1] 0.7778
ceiling(m * items.original) # new test length
## [1] 16
#--------------

#--------------
(m <- SBlength(rxxp = rho.new, rxx = rho.original))
## [1] 0.7778
ceiling(m * items.original) # new test length
## [1] 16
#--------------

#-----------------------------------------------------------------
# 4.3.1  Correlation coefficients
#-----------------------------------------------------------------

data(HCItestretest, package = "ShinyItemAnalysis")


# divide dataset by "test"
HCI_test    <- HCItestretest[HCItestretest$test == "test", ]
HCI_retest  <- HCItestretest[HCItestretest$test == "retest", ]
#--------------

#--------------
ggplot(
  data.frame(Test = HCI_test$total, Retest = HCI_retest$total),
  aes(x = Test, y = Retest)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_fig()
#--------------

#--------------
cor.test(HCI_test$total, HCI_retest$total)
## Pearson's product-moment correlation
##
## data:  HCI_test$total and HCI_retest$total
## t = 7.914, df = 43, p-value = 6.e-10
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.6156 0.8674
## sample estimates:
##       cor
## 0.7700
#--------------

#-----------------------------------------------------------------
# 4.3.2.1  Split-half coefficient
#-----------------------------------------------------------------

#--------------
# loading dataset, taking item data
data(HCI, package = "ShinyItemAnalysis")
data <- HCI[, 1:20]
#--------------

#--------------
# first-second split
df1 <- data[, 1:10]
df2 <- data[, 11:20]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearmann-Brown formula to estimate reliability
2 * cor.x / (1 + cor.x)
## [1] 0.6966
#--------------

#--------------
# even-odd split
df1 <- data[, seq(1, 20, 2)]
df2 <- data[, seq(2, 20, 2)]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearmann-Brown formula to estimate reliability
2 * cor.x / (1 + cor.x)
## [1] 0.7415
#--------------

#--------------
# random split
set.seed(123) # setting seed for reproducibility
samp <- sample(1:20, 10) # 10 random items
df1 <- data[, samp]
df2 <- data[, setdiff(1:20, samp)]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearmann-Brown formula to estimate reliability
2 * cor.x / (1 + cor.x)
## [1] 0.7386
#--------------

#--------------
# minimum of all possible split-halves
split <- splitHalf(data, raw = TRUE, brute = TRUE)
items1 <- which(split$minAB[, "A"] == 1)
items2 <- which(split$minAB[, "B"] == 1)
df1 <- data[, items1]
df2 <- data[, items2]
# total score calculation
ts1 <- rowSums(df1)
ts2 <- rowSums(df2)
# correlation
cor.x <- cor(ts1, ts2)
# apply Spearmann-Brown formula to estimate reliability
2 * cor.x / (1 + cor.x)
## [1] 0.6114
#--------------

#--------------
# average of 10,000 split-halves
split <- splitHalf(data, raw = TRUE)
mean(split$raw)
## [1] 0.7209
#--------------

#--------------
# average of all split-halves
split <- splitHalf(data, raw = TRUE, brute = TRUE)
mean(split$raw)
## [1] 0.7213
#--------------

#-----------------------------------------------------------------
# 4.3.2  Cronbach's alpha
#-----------------------------------------------------------------

#--------------
score <- rowSums(data) # total scores
m <- ncol(data) # number of items
var(score) # variance of total scores
## [1] 13.2473
item_vars <- sapply(data, var) # item variances

# Cronbach's alpha (3.11)
m / (m - 1) * (1 - (sum(item_vars)) / var(score))
## [1] 0.7155
#--------------

#--------------
VC <- var(data) # covariance matrix of data
# sum(diag(VC)) is trace of covariance matrix

# Cronbach's alpha (3.12)
(m / (m - 1)) * (1 - sum(diag(VC)) / sum(VC))
## [1] 0.7155
#--------------

#--------------
# Cronbach's alpha in the psych and the psychometric packages
psych::alpha(data)$total[1]
## raw_alpha
## 0.7155

psychometric::alpha(data)
## [1] 0.7155
#--------------

#-----------------------------------------------------------------
# 4.3.2.1  Cronbach's alpha and inter-item correlations
#-----------------------------------------------------------------

#--------------
# Cronbach's alpha  as function of average inter-item correlation
(c_bar <- sum(sapply(1:m, function(i) {
  sum(cov(data[, i], data[, -i]))
})) / (m * (m - 1)))
## [1] 0.0237
(v_bar <- sum(item_vars) / m)
## [1] 0.2122

# Cronbach's alpha (3.13)
m * c_bar / (v_bar + (m - 1) * c_bar)
## [1] 0.7155
#--------------

#-----------------------------------------------------------------
# 4.3.2.2  Cronbach's alpha in ANOVA framework
#-----------------------------------------------------------------

#--------------
# Cronbach's alpha in ANOVA framework
m <- ncol(data) # number of items
n <- nrow(data) # number of persons

# converting data to the long format
data.long <- reshape(
  data, varying = list(colnames(data)),
  timevar = "item", idvar = "person", v.names = "rating",
  direction = "long", new.row.names = 1:13020
)

head(data.long, n = 2)
## item rating person
## 1    1      1      1
## 2    1      1      2
#--------------

#--------------
# mean ratings
mean.overall <- mean(data.long$rating)
mean.items <- tapply(data.long$rating, data.long$item, mean)
mean.persons <- tapply(data.long$rating, data.long$person, mean)
#--------------

#--------------
# sum of squares, mean sum of squares
# total sum of squares total, df = n * m - 1
SStotal <- sum((data.long$rating - mean.overall)^2)
MStotal <- SStotal / (n * m - 1)
# sum of squares persons, df = n - 1
SSP <- m * sum((mean.persons - mean.overall)^2)
MSP <- SSP / (n - 1)
# sum of squares items, df = m - 1
SSI <- n * sum((mean.items - mean.overall)^2)
MSI <- SSI / (m - 1)
# sum of squares residual, df = (n - 1) * (m - 1)
SSe <- SStotal - SSP - SSI
MSe <- SSe / ((n - 1) * (m - 1))
#--------------

#--------------
# estimates of variance components
(sigma2P <- (MSP - MSe) / m)
## [1] 0.0237
(sigma2I <- (MSI - MSe) / n)
## [1] 0.0270
(sigma2e <- MSe)
## [1] 0.1885
(sigma2total <- sigma2P + sigma2I + sigma2e)
## [1] 0.2392

# Cronbach's alpha
sigma2P / (sigma2P + 1 / m * sigma2e)
## [1] 0.7155
#--------------

#--------------
# Cronbach's alpha as ratio of mean squares
(MSP - MSe) / MSP
## [1] 0.7155
#--------------

#--------------
(FA <- MSP / MSe) # F statistic
## [1] 3.5144
1 - 1 / FA # Cronbach's alpha
## [1] 0.7155

# confidence interval
gamma <- 0.05 # significance level
# lower bound in 95% two-sided confidence interval
1 - qf(1 - gamma / 2, n - 1, (m - 1) * (n - 1)) / FA
## [1] 0.6828
# upper bound in 95% two-sided confidence interval
1 - qf(gamma / 2, n - 1, (m - 1) * (n - 1)) / FA
## [1] 0.7462
#--------------

#--------------
a <- psych::alpha(data)$total[1]
psych::alpha.ci(
  a, n.obs = nrow(data), n.var = ncol(data),
  p.val = 0.05, digits = 4
)
## $lower.ci
## raw_alpha
## 0.6837
##
## $alpha
## raw_alpha
## 0.7155
##
## $upper.ci
## raw_alpha
## 0.7456
##
## $r.bar
## raw_alpha
## 0.1117
#--------------

#--------------
a <- psychometric::alpha(data)
psychometric::alpha.CI(
  a, N = nrow(data), k = ncol(data), level = 0.95
)
##      LCL  ALPHA    UCL
## 1 0.6828 0.7155 0.7462
#--------------

#-----------------------------------------------------------------
# 3.3.3 Variance decomposition
#-----------------------------------------------------------------

#--------------
ICC(data)
## Call: ICC(x = data)
##
## Intraclass correlation coefficients
##                    type   ICC   F df1   df2        p   LCI   UCI
## Sing_raters_abs    ICC1 0.094 3.1 650 12369 1.1e-122 0.083  0.11
## Sing_rand_raters   ICC2 0.099 3.5 650 12350 1.3e-159 0.087  0.11
## Sing_fix_raters    ICC3 0.112 3.5 650 12350 1.3e-159 0.099  0.13
## Aver_raters_abs   ICC1k 0.675 3.1 650 12369 1.1e-122 0.644  0.70
## Aver_rand_raters  ICC2k 0.687 3.5 650 12350 1.3e-159 0.655  0.72
## Aver_fix_raters   ICC3k 0.715 3.5 650 12350 1.3e-159 0.688  0.74
##
## Number of subjects = 651     Number of Judges =  20
#--------------

#-----------------------------------------------------------------
# 4.4. More sources of error and G-theory
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 4.4.1 A one facet study
#-----------------------------------------------------------------

#--------------
fit_2wayr <- lmer(rating ~ (1 | person) + (1 | item), data = data.long)
(fitVar_2wayr <- as.data.frame(VarCorr(fit_2wayr)))
##        grp        var1 var2   vcov  sdcor
## 1   person (Intercept) <NA> 0.0237 0.1539
## 2     item (Intercept) <NA> 0.0270 0.1643
## 3 Residual        <NA> <NA> 0.1885 0.4341
#--------------

#--------------
(varP <- fitVar_2wayr[fitVar_2wayr$grp == "person", "vcov"])
## [1] 0.0237
(varI <- fitVar_2wayr[fitVar_2wayr$grp == "item", "vcov"])
## [1] 0.0270
(vare <- fitVar_2wayr[fitVar_2wayr$grp == "Residual", "vcov"])
## [1] 0.1885

(varT <- varP + varI + vare)
## [1] 0.2392
c(varP, varI, vare) / varT
## [1] 0.0991 0.1129 0.7880
#--------------

#--------------
varT20 <- varP + varI / m + vare / m
c(varP, varI / m, vare / m) / varT20
# [1] 0.6874 0.0392 0.2734
#--------------

#--------------
# absolute error variance
(varDeltaAbs <- varI / m + vare / m)
## [1] 0.0108
#--------------

#--------------
# absolute standard error of measurement
sqrt(varDeltaAbs)
## [1] 0.1038
#--------------

#--------------
# relative error variance
(varDeltaRel <- vare / m)
## [1] 0.0094
#--------------

#--------------
# relative standard error of measurement
sqrt(varDeltaRel)
## [1] 0.0971
#--------------

#--------------
# generalizability coefficient
(varP / (varP + varDeltaRel))
## [1] 0.7155
#--------------

#--------------
model <- rating ~ (1 | person) + (1 | item)
(gfit <- gtheory::gstudy(formula = model, data = data.long))
## $components
## source        var percent n
## 1   person 0.0237     9.9 1
## 2     item 0.0270    11.3 1
## 3 Residual 0.1885    78.8 1
#--------------

#--------------
gtheory::dstudy(
  gfit,
  colname.objects = "person",
  colname.scores = "rating",
  data = data.long
)
## $components
##     source    var percent  n
## 1   person 0.0237    68.7  1
## 2     item 0.0014     3.9 20
## 3 Residual 0.0094    27.3 20
##
## $var.universe
## [1] 0.0237
##
## $generalizability
## [1] 0.7155
##
## $var.error.rel
## [1] 0.0094
##
## $sem.rel
## [1] 0.0971
##
## $see.rel
## [1] 0.0821
##
## $dependability
## [1] 0.6874
##
## $var.error.abs
## [1] 0.0108
##
## $sem.abs
## [1] 0.1038
##
## $see.abs
## [1] 0.0861
#--------------

#--------------
(G_pxi <- hemp::gstudy(fit_2wayr))
##     Source Est.Variance Percent.Variance
## 1   person       0.0237             9.9%
## 2     item       0.0270            11.3%
## 3 Residual       0.1885            78.8%
#--------------

#--------------
hemp::dstudy(G_pxi, n = c("item" = 20), unit = "person")
##     Source Est.Variance     N Ratio of Var:N
## 1   person       0.0237 13020         0.0237
## 2     item       0.0270    20         0.0014
## 3 Residual       0.1885    20         0.0094
##
## The generalizability coefficient is: 0.7155.
## The dependability coefficient is: 0.6875.
#--------------

#--------------
dstudy_plot(
  G_pxi, unit = "person",
  facets = list("Item" = seq(from = 0, to = 30, by = 5)),
  g_coef = TRUE
)
dstudy_plot(
  G_pxi, unit = "person",
  facets = list("Item" = seq(from = 0, to = 30, by = 5)),
  g_coef = FALSE
)
#--------------

#-----------------------------------------------------------------
# 3.4.2 A two facet study
#-----------------------------------------------------------------

#--------------
data(data.ratings2, package = "sirt")
head(data.ratings2)
##   idstud rater k1 k2 k3 k4 k5
## 1   1001   R03  1  1  1  1  2
## 2   1001   R15  1  1  1  1  2
## 3   1002   R05  0  1  1  1  1
## 4   1002   R10  1  1  1  2  2
## 5   1003   R02  2  1  2  1  0
## 6   1004   R01  0  0  0  0  1
#--------------

#--------------
m <- 5 # number of items
data.long2 <- reshape(
  data.ratings2, direction = "long",
  varying = list(3:7), v.names = "ratings",
  timevar = "items", new.row.names = 1:3075
)
data.long2 <- data.long2[,-5]
data.long2$rater <- as.factor(data.long2$rater)
data.long2$items <- as.factor(data.long2$items)
data.long2$idstud <- as.factor(data.long2$idstud)
head(data.long2, n = 3)
##   idstud rater items ratings
## 1   1001   R03     1       1
## 2   1001   R15     1       1
## 3   1002   R05     1       0
#--------------

#--------------
# G-study with gtheory package
formula <- ratings ~ (1 | items) + (1 | rater) + (1 | idstud) +
  (1 | items:rater) + (1 | idstud:rater) + (1 | idstud:items)
gfit <- gtheory::gstudy(formula = formula, data = data.long2)
gfit
##         source    var percent n
## 1 idstud:items 0.1037    13.1 1
## 2 idstud:rater 0.0690     8.7 1
## 3       idstud 0.3682    46.5 1
## 4  items:rater 0.0180     2.3 1
## 5        rater 0.0418     5.3 1
## 6        items 0.0029     0.4 1
## 7     Residual 0.1880    23.7 1
##
## attr(,"class")
## [1] "gstudy" "list"
#--------------

#--------------
# D-study with gtheory package
dfit <- gtheory::dstudy(
  gfit, colname.objects = "idstud",
  colname.scores = "ratings", data = data.long2
)
dfit$components
##         source    var percent  n
## 1 idstud:items 0.0207     4.5  5
## 2 idstud:rater 0.0345     7.4  2
## 3       idstud 0.3682    79.1  1
## 4  items:rater 0.0018     0.4 10
## 5        rater 0.0209     4.5  2
## 6        items 0.0006     0.1  5
## 7     Residual 0.0188     4.0 10
#--------------

#--------------
dfit$var.universe
## [1] 0.3682
#--------------

#--------------
dfit$var.error.abs
## [1] 0.0973
#--------------

#--------------
dfit$dependability
## [1] 0.7910
#--------------

#--------------
dfit$var.error.rel
## [1] 0.0740
#--------------

#--------------
dfit$generalizability
## [1] 0.8326
#--------------

#--------------
# G-study with hemp package
fit <- lmer(ratings ~ (1 | items) + (1 | rater) + (1 | idstud) +
              (1 | items:rater) + (1 | idstud:rater) +
              (1 | idstud:items), data = data.long2)
(G_pxixr <- hemp::gstudy(fit))
##         Source Est.Variance Percent.Variance
## 1 idstud:items       0.1037            13.1%
## 2 idstud:rater       0.0690             8.7%
## 3       idstud       0.3682            46.5%
## 4  items:rater       0.0180             2.3%
## 5        rater       0.0418             5.3%
## 6        items       0.0029             0.4%
## 7     Residual       0.1880            23.7%

#--------------
# D-study with gtheory package
hemp::dstudy(G_pxixr, n = c("items" = 20, "rater" = 2), unit = "idstud")
##
##         Source Est.Variance    N Ratio of Var:N
## 1 idstud:items       0.1037   20       0.005185
## 2 idstud:rater       0.0690    2       0.034500
## 3       idstud       0.3682 3075       0.368200
## 4  items:rater       0.0180   40       0.000450
## 5        rater       0.0418    2       0.020900
## 6        items       0.0029   20       0.000145
## 7     Residual       0.1880   40       0.004700
##
## The generalizability coefficient is: 0.8924222.
## The dependability coefficient is: 0.8482307.

dstudy_plot(
  G_pxixr, unit = "idstud",
  facets = list("items" = seq(from = 0, to = 5),
                "rater" = c(1, 2, 3)),
  g_coef = TRUE
)
dstudy_plot(
  G_pxixr, unit = "idstud",
  facets = list("items" = seq(from = 0, to = 5),
                "rater" = c(1, 2, 3)),
  g_coef = FALSE
)
#--------------

#-----------------------------------------------------------------
# 4.5.1.1 ANOVA method of estimation
#-----------------------------------------------------------------

#--------------
# mean ratings
mean.overall <- mean(data.long$rating)
mean.persons <- tapply(data.long$rating, data.long$person, mean)

m <- length(unique(data.long$item))
n <- length(unique(data.long$person))
#--------------

#--------------
# sum of squares persons, df = n - 1
SSP <- m * sum((mean.persons - mean.overall)^2)
MSP <- SSP / (n - 1)

# sum of squares residual, df = (n - 1) * (m - 1)
SSe1way <- sum((data.long$rating - rep(mean.persons, 20))^2)
MSe1way <- SSe1way / ((m - 1) * n)

# ANOVA estimates of variance components
(sigma2E_ANOVA <- MSe1way)
## [1] 0.2155
(sigma2P_ANOVA <- ((MSP - MSe1way) / m))
## [1] 0.0223


#-----------------------------------------------------------------
# 4.5.1.2 Maximum likelihood
#-----------------------------------------------------------------

#--------------
fit <- lmer(rating ~ (1 | person), data = data.long, REML = FALSE)
as.data.frame(VarCorr(fit))
##        grp        var1 var2   vcov  sdcor
## 1   person (Intercept) <NA> 0.0223 0.1493
## 2 Residual        <NA> <NA> 0.2155 0.4642

#--------------
# ML estimates of variance components
(sigma2E_ML <- MSe1way)
## [1] 0.2155
(sigma2P_ML <- ((1 - 1 / n) * MSP - MSe1way) / m)
## [1] 0.0223


#-----------------------------------------------------------------
# 4.5.1.3 Restricted maximum likelihood (REML)
#-----------------------------------------------------------------

#--------------
fit_1wayr <- lmer(rating ~ (1 | person), data = data.long)
as.data.frame(VarCorr(fit_1wayr))
##        grp        var1 var2   vcov  sdcor
## 1   person (Intercept) <NA> 0.0223 0.1495
## 2 Residual        <NA> <NA> 0.2155 0.4642
#--------------

#-----------------------------------------------------------------
# 4.5.1.4 Bootstrap
#-----------------------------------------------------------------

#--------------
boot <- bootMer(
  fit_2wayr,
  FUN = function(mm) unlist(as.data.frame(VarCorr(mm))["vcov"]),
  nsim = 100, seed = 123
)
bootVars <- as.data.frame(boot$t)
colnames(bootVars) <- fitVar_2wayr[, "grp"]
head(bootVars)
##    person    item Residual
## 1 0.01990 0.02665   0.1899
## 2 0.02637 0.04694   0.1876
## 3 0.02410 0.02064   0.1886
## 4 0.02286 0.02237   0.1859
## 5 0.02496 0.01048   0.1884
## 6 0.02413 0.04510   0.1925
#--------------

#--------------
bootAlphaCR <- bootVars[, "person"] /
  (bootVars[, "person"] + bootVars[, "Residual"] / m)

# median of bootstrapped samples
median(bootAlphaCR)
## [1] 0.7162

# lower bound of 95% bootstrapped confidence interval
quantile(bootAlphaCR, 0.025)
##      2.5%
##    0.6864
# upper bound of 95% bootstrapped confidence interval
quantile(bootAlphaCR, 0.975)
##     97.5%
##    0.7446
#--------------

#--------------
# histogram
ggplot(
  data = data.frame(alpha = bootAlphaCR),
  aes(x = alpha)
) +
  geom_histogram(binwidth = 0.005, alpha = 0.5, col = "black") +
  geom_vline(xintercept = median(bootAlphaCR), col = "red") +
  geom_vline(xintercept = quantile(bootAlphaCR, 0.025), col = "red") +
  geom_vline(xintercept = quantile(bootAlphaCR, 0.975), col = "red") +
  xlab("Cronbach's alpha") + ylab("Count") +
  theme_fig()
#--------------

#-----------------------------------------------------------------
# 4.5.1.5 Bayesian estimation
#-----------------------------------------------------------------

#--------------
set.seed(1234) # TODO: resave results with seed
fitB <- brm(
  rating ~ (1 | person) + (1 | item),
  data = data.long
)
results <- as.data.frame(fitB)
head(results[, 1:4], n = 3)
##   b_Intercept sd_item__Intercept sd_person__Intercept  sigma
## 1      0.5845             0.1810               0.1474 0.4352
## 2      0.5730             0.1839               0.1549 0.4330
## 3      0.5811             0.1553               0.1582 0.4322

# Cronbach's alpha for each generated sample
alphaCR <- results[, "sd_person__Intercept"]^2 /
  (results[, "sd_person__Intercept"]^2 + results[, "sigma"]^2 / m)

quantile(alphaCR, 0.5) # median of generated samples
##    50%
## 0.7156
quantile(alphaCR, 0.025) # lower bound of 95% credible interval
##   2.5%
## 0.6814
quantile(alphaCR, 0.975) # upper bound of 95% credible interval
##  97.5%
## 0.7455
#--------------

#--------------
# histogram
ggplot(
  data = data.frame(alphaCR),
  aes(x = alphaCR)
) +
  geom_histogram(binwidth = 0.005, alpha = 0.5, col = "black") +
  geom_vline(xintercept = median(bootAlphaCR), col = "red") +
  geom_vline(xintercept = quantile(bootAlphaCR, 0.025), col = "red") +
  geom_vline(xintercept = quantile(bootAlphaCR, 0.975), col = "red") +
  xlab("Cronbach's alpha") + ylab("Count") +
  theme_fig()
#--------------

