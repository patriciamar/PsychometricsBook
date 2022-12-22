#-----------------------------------------------------------------
# Chapter 4 - Reliability
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
# 4.2.5.1  Spearman-Brown prophecy formula
#-----------------------------------------------------------------

#--------------
rho_original <- 0.75 # reliability of original data
items_original <- 20 # number of items in original data
items_new <- 30 # number of items in new data
(m <- items_new / items_original) # ratio of tests lengths
## [1] 1.5

# new reliability
m * rho_original / (1 + (m - 1) * rho_original)
## [1] 0.8182
#--------------

#--------------
library(psychometric)
SBrel(Nlength = m, rxx = rho_original)
## [1] 0.8182
#--------------

#--------------
rho_new <- 0.85 # desired reliability
# determining test length
(m <- rho_new * (1 - rho_original) / (rho_original * (1 - rho_new)))
## [1] 1.8889
ceiling(m * items_original) # new test length
## [1] 38
#--------------

#--------------
(m <- SBlength(rxxp = rho_new, rxx = rho_original))
## [1] 1.8889
ceiling(m * items_original) # new test length
## [1] 38
#--------------

#--------------
rho_new <- 0.7 # desired reliability
(m <- rho_new * (1 - rho_original) / (rho_original * (1 - rho_new)))
## [1] 0.7778
ceiling(m * items_original) # new test length
## [1] 16
#--------------

#--------------
(m <- SBlength(rxxp = rho_new, rxx = rho_original))
## [1] 0.7778
ceiling(m * items_original) # new test length
## [1] 16
#--------------

#-----------------------------------------------------------------
# 4.3.1  Reliability estimation with correlation coefficients
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 4.3.1.1 Test-retest reliability
#-----------------------------------------------------------------

#--------------
data(HCItestretest, package = "ShinyItemAnalysis")

# divide dataset by "test"
HCI_test    <- HCItestretest[HCItestretest$test == "test", ]
HCI_retest  <- HCItestretest[HCItestretest$test == "retest", ]
#--------------

#--------------
# code not shown in the book:
library(ggplot2)
ggplot(data.frame(Test = HCI_test$total, Retest = HCI_retest$total),
       aes(x = Test, y = Retest)) + geom_point() + theme_fig()
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
# 4.3.1.3 Split-half coefficient
#-----------------------------------------------------------------

#--------------
# loading dataset, taking item data
data(HCI, package = "ShinyItemAnalysis")
HCI_items <- HCI[, 1:20]
#--------------

#--------------
# first-second split
df1 <- HCI_items[, 1:10]; df2 <- HCI_items[, 11:20]
ts1 <- rowSums(df1); ts2 <- rowSums(df2)
cor_x <- cor(ts1, ts2)
2 * cor_x / (1 + cor_x) # Spearman-Brown formula
## [1] 0.6966
#--------------

#--------------
# even-odd split
df1 <- HCI_items[, seq(1, 20, 2)]; df2 <- HCI_items[, seq(2, 20, 2)]
ts1 <- rowSums(df1); ts2 <- rowSums(df2)
cor_x <- cor(ts1, ts2)
2 * cor_x / (1 + cor_x) # Spearman-Brown formula
## [1] 0.7415
#--------------

#--------------
# random split
set.seed(123) # setting seed for reproducibility
samp <- sample(1:20, 10) # 10 random items
df1 <- HCI_items[, samp]; df2 <- HCI_items[, setdiff(1:20, samp)]
ts1 <- rowSums(df1); ts2 <- rowSums(df2)
cor_x <- cor(ts1, ts2)
2 * cor_x / (1 + cor_x) # Spearman-Brown formula
## [1] 0.7386
#--------------

#-----------------------------------------------------------------
# 4.3.2  Cronbach's alpha
#-----------------------------------------------------------------

#--------------
m <- ncol(HCI_items) # number of items
var(HCI$total) # sample variance of total scores
## [1] 13.2473
item_vars <- sapply(HCI_items, var) # item sample variances
m / (m - 1) * (1 - (sum(item_vars)) / var(HCI$total))
## [1] 0.7155
#--------------

#--------------
VC <- var(HCI_items)
(m / (m - 1)) * (1 - sum(diag(VC)) / sum(VC))
## [1] 0.7155
#--------------

#--------------
# Cronbach's alpha in the psych and the psychometric packages
psych::alpha(HCI_items)$total[1]
## raw_alpha
## 0.7155

psychometric::alpha(HCI_items)
## [1] 0.7155
#--------------

#--------------
# Cronbach's alpha in the psych package (code not shown in the book)
psych::alpha(HCI_items)
#--------------

#-----------------------------------------------------------------
# 4.3.2.1 Cronbach's alpha and inter-item correlations
#-----------------------------------------------------------------

#--------------
# Cronbach's alpha  as function of average inter-item correlation
(c_bar <- sum(sapply(1:m, function(i) {
  sum(cov(HCI_items[, i], HCI_items[, -i]))
})) / (m * (m - 1)))
## [1] 0.0237
(v_bar <- sum(item_vars) / m)
## [1] 0.2122
m * c_bar / (v_bar + (m - 1) * c_bar)
## [1] 0.7155
#--------------

#-----------------------------------------------------------------
# 4.4 Estimation of reliability with variance components
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 4.4.1 ANOVA method of estimation
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 4.4.1.1 One-way ANOVA
#-----------------------------------------------------------------

#--------------
data(AIBS, package = "ShinyItemAnalysis")
head(AIBS, n = 4)
##    ID ...  ScoreRankAdj Score  ...
##  1 10 ...            48   2.0  ...
##  2 10 ...            48   3.5  ...
##  3 10 ...            48   2.0  ...
##  4 12 ...            38   2.0  ...
#--------------

#--------------
# AIBS summary (code not shown in the book)
summary(AIBS[, c("ID", "Score", "ScoreAvg", "ScoreRankAdj")]) # code not shown in the book
#--------------

#--------------
# Caterpillar plot of AIBS overall scientific merit scores
# code not shown in the book
ggplot(data = AIBS, aes(x = ScoreRankAdj, y = Score, group = ID)) + 
  geom_line(col = "gray") + geom_point(shape = 1, size = 1.5) +
  stat_summary(fun = mean, fun.args = list(na.rm = TRUE), 
               geom = "point", col = "blue") + 
  labs(x = "AIBS application rank", y = "Rating") +
  coord_cartesian(ylim = c(1, 5)) + theme_fig()
#--------------

#--------------
n = 72; m = 3
mean_proposals <- tapply(AIBS$Score, AIBS$ID, mean)
mean_overall <- mean(AIBS$Score)

(SSP <- m * sum((mean_proposals - mean_overall)^2)) # observed SSb
## [1] 78.7398
(SSe <- sum((AIBS$Score - AIBS$ScoreAvg)^2)) # observed SSw
## [1] 57.6400
#--------------

#--------------
(MSP <- SSP / (n - 1))
## [1] 1.1090
(MSe <- SSe / (n * (m - 1)))  # estimate of residual variance
## [1] 0.4003
(MSP - MSe) / m # proposal (true-score) variance
## [1] 0.2362
#--------------

#--------------
((MSP - MSe) / m) / ((MSP - MSe) / m + MSe) # IRR
## [1] 0.3711
((MSP - MSe) / m) / ((MSP - MSe) / m + MSe / m) # IRR
## [1] 0.6391
#--------------

#--------------
# With aov() (code not shown in the book)
aov(Score ~ ID, data = AIBS)
#--------------

#-----------------------------------------------------------------
# 4.4.1.2 Two-way ANOVA and Cronbach's alpha
#-----------------------------------------------------------------

#--------------
data(HCIlong, package = "ShinyItemAnalysis")
head(HCIlong, n = 2)
##   id   item rating gender major total
## 1  1 Item 1      1      0     1    16
## 2  1 Item 2      1      0     1    16
#--------------

#--------------
m <- nlevels(as.factor(HCIlong$item)) # number of items
n <- nlevels(as.factor(HCIlong$id)) # number of persons
#--------------

#--------------
# mean ratings
mean_overall <- mean(HCIlong$rating)
mean_items <- tapply(HCIlong$rating, HCIlong$item, mean)
mean_persons <- tapply(HCIlong$rating, HCIlong$id, mean)
#--------------

#--------------
# sum of squares
SStotal <- sum((HCIlong$rating - mean_overall)^2)
SSP <- m * sum((mean_persons - mean_overall)^2)
SSI <- n * sum((mean_items - mean_overall)^2)
SSe <- SStotal - SSP - SSI
#--------------

#--------------
# mean sum of squares
MStotal <- SStotal / (n * m - 1)
MSP <- SSP / (n - 1)
MSI <- SSI / (m - 1)
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
#--------------

#--------------
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
#--------------

#--------------
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
# confidence interval in psychometric package
a <- psychometric::alpha(HCI_items)
psychometric::alpha.CI(a, N = nrow(HCI_items), k = ncol(HCI_items), level = 0.95)
##      LCL  ALPHA    UCL
## 1 0.6828 0.7155 0.7462
#--------------

#--------------
# confidence interval in psych package
psych::alpha(HCI_items)$feldt
## 95% confidence boundaries (Feldt)
## lower alpha upper
##  0.68  0.72  0.75
#--------------

#--------------
# confidence interval in psych package
# not shown in the book
# NOTE: for some reason does not work now (no output)
a <- psych::alpha(HCI_items)$total[1]
psych::alpha.ci(a, n.obs = nrow(HCI_items), n.var = ncol(HCI_items), p.val = 0.05, 
                digits = 4)
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
# CI calculated as in psych package (not shown in the book)
# lower bound in 95% two-sided confidence interval
1 - qf(1 - gamma / 2, n - 1, Inf) / FA
## [1] 0.6837
# upper bound in 95% two-sided confidence interval
1 - qf(gamma / 2, n - 1, Inf) / FA
## [1] 0.7456
#--------------

#-----------------------------------------------------------------
# 4.4.2 Maximum likelihood
#-----------------------------------------------------------------

#--------------
library(lme4)
model1_ML <- lmer(Score ~ 1 + (1|ID), data = AIBS, REML = FALSE)
#--------------

#--------------
# model summary (code not shown in the book)
summary(model1_ML)
#--------------

#--------------
as.data.frame(VarCorr(model1_ML))
##        grp        var1 var2   vcov  sdcor
## 1       ID (Intercept) <NA> 0.2311 0.4807
## 2 Residual        <NA> <NA> 0.4003 0.6327
#--------------

#--------------
(sigma2P_ML <- as.numeric(VarCorr(model1_ML)))  # Proposal (true score) variance
## [1] 0.2311
(sigma2e_ML <- sigma(model1_ML)^2)  # Residual variance
## [1] 0.4003
#--------------

#--------------
# single-rating IRR
sigma2P_ML / (sigma2P_ML + sigma2e_ML)
## [1] 0.3660
# multiple-rating IRR
sigma2P_ML / (sigma2P_ML + sigma2e_ML / m)
## [1] 0.6340
#--------------

#-----------------------------------------------------------------
# 4.4.3 Restricted maximum likelihood (REML)
#-----------------------------------------------------------------

#--------------
# AIBS with one-way random-effect model
model1_REML <- lmer(Score ~ 1 + (1|ID), data = AIBS, REML = TRUE)
as.data.frame(VarCorr(model1_REML))
#        grp        var1 var2   vcov  sdcor
# 1       ID (Intercept) <NA> 0.2362 0.4860
# 2 Residual        <NA> <NA> 0.4003 0.6327
#--------------

#--------------
# variance components (code not shown in the book)
(sigma2P_REML <- as.numeric(VarCorr(model1_REML)))
## [1] 0.2362
(sigma2e_REML <- sigma(model1_REML)^2)
## [1] 0.4003
#--------------

#--------------
# single-rating IRR (code not shown in the book)
sigma2P_REML / (sigma2P_REML + sigma2e_REML)
## [1] 0.3711
# multiple-rating IRR
sigma2P_REML / (sigma2P_REML + sigma2e_REML / m)
## [1] 0.6391
#--------------

#--------------
# HCI with two-way random-effect model
model2_REML <- lmer(rating ~ (1 | id) + (1 | item), data = HCIlong)
(VC <- as.data.frame(VarCorr(model2_REML)))
##        grp        var1 var2   vcov  sdcor
## 1       id (Intercept) <NA> 0.0237 0.1539
## 2     item (Intercept) <NA> 0.0270 0.1643
## 3 Residual        <NA> <NA> 0.1885 0.4341
#--------------

#--------------
(sigma2P_HCI <- VC[VC$grp == "id", "vcov"])
## [1] 0.0237
(sigma2e_HCI <- VC[VC$grp == "Residual", "vcov"])
## [1] 0.1885
sigma2P_HCI / (sigma2P_HCI + sigma2e_HCI / m)
## [1] 0.7155
#--------------

#-----------------------------------------------------------------
## 4.3.3.5 Bootstrap
#-----------------------------------------------------------------

#--------------
boot <- bootMer(model2_REML, nsim = 100, seed = 543,
                FUN = function(mm) unlist(as.data.frame(VarCorr(mm))["vcov"]))
#--------------

#--------------
bootVars <- as.data.frame(boot$t)
colnames(bootVars) <- VC[, "grp"]
head(bootVars, n = 2)
##        id    item Residual
## 1 0.02382 0.01229   0.1883
## 2 0.02069 0.01420   0.1893
#--------------

#--------------
bootAlphaCR <- bootVars[, "id"] / (bootVars[, "id"] + bootVars[, "Residual"] / m)
#--------------

#--------------
# median (code not shown in the book)
median(bootAlphaCR)
## [1] 0.7138
#--------------

#--------------
# lower and upper bound of bootstrapped CI
quantile(bootAlphaCR, 0.025)
##      2.5%
##    0.6819
quantile(bootAlphaCR, 0.975)
##     97.5%
##    0.7406
#--------------

#--------------
# histogram (code not shown in the book)
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
# 4.4.5 Bayesian estimation
#-----------------------------------------------------------------

#--------------
library(brms)
set.seed(1234) 
fitB <- brm(rating ~ (1 | id) + (1 | item), data = HCIlong)
#--------------

#--------------
results <- as.data.frame(fitB)
head(results[, 1:4], n = 3)
##   b_Intercept sd_item__Intercept sd_id__Intercept  sigma
## 1      0.5845             0.1810           0.1474 0.4352
## 2      0.5730             0.1839           0.1549 0.4330
## 3      0.5811             0.1553           0.1582 0.4322
#--------------

#--------------
# Cronbach's alpha for each generated sample
alphaCR <- results[, "sd_id__Intercept"]^2 /
  (results[, "sd_id__Intercept"]^2 + results[, "sigma"]^2 / m)
#--------------

#--------------
# median, lower and upper bound of credible interval
quantile(alphaCR, 0.5)
##    50%
## 0.7156
quantile(alphaCR, 0.025)
##   2.5%
## 0.6814
quantile(alphaCR, 0.975)
##  97.5%
## 0.7455
#--------------

#--------------
# histogram (code not shown in the book)
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

#-----------------------------------------------------------------
# 4.5. More sources of error and G-theory
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 4.5.1 A one facet study
#-----------------------------------------------------------------

#--------------
# 2-way random/effect model from Section on REML
# (code not shown again in the book)
model2_REML <- lmer(rating ~ (1 | id) + (1 | item), data = HCIlong)
(VC <- as.data.frame(VarCorr(model2_REML)))
##        grp        var1 var2   vcov  sdcor
## 1      id  (Intercept) <NA> 0.0237 0.1539
## 2     item (Intercept) <NA> 0.0270 0.1643
## 3 Residual        <NA> <NA> 0.1885 0.4341
#--------------

#--------------
# variance components, total variance, ratios
(varP <- VC[VC$grp == "id", "vcov"])
## [1] 0.0237
(varI <- VC[VC$grp == "item", "vcov"])
## [1] 0.0270
(vare <- VC[VC$grp == "Residual", "vcov"])
## [1] 0.1885

(varT <- varP + varI + vare)
## [1] 0.2392
c(varP, varI, vare) / varT
## [1] 0.0991 0.1129 0.7880
#--------------

#--------------
# total variance and variance ratios for composite measurement
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
# dependability coefficient
varP / (varP + varDeltaAbs)
## [1] 0.6874
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
varP / (varP + varDeltaRel)
## [1] 0.7155
#--------------

#--------------
model <- rating ~ (1 | id) + (1 | item)
(gfit <- gtheory::gstudy(formula = model, data = HCIlong))
## $components
##     source    var percent n
## 1       id 0.0237     9.9 1
## 2     item 0.0270    11.3 1
## 3 Residual 0.1885    78.8 1
#--------------

#--------------
gtheory::dstudy(gfit, colname.objects = "id", colname.scores = "rating", 
                data = HCIlong)
## $components
##     source    var percent  n
## 1       id 0.0237    68.7  1
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
(G_pxi <- hemp::gstudy(model2_REML))
##     Source Est.Variance Percent.Variance
## 1       id       0.0237             9.9%
## 2     item       0.0270            11.3%
## 3 Residual       0.1885            78.8%
#--------------

#--------------
hemp::dstudy(G_pxi, n = c(item = 20), unit = "id")
##     Source Est.Variance     N Ratio of Var:N
## 1       id       0.0237 13020         0.0237
## 2     item       0.0270    20         0.0014
## 3 Residual       0.1885    20         0.0094
##
## The generalizability coefficient is: 0.7155.
## The dependability coefficient is: 0.6875.
#--------------

#--------------
hemp::dstudy_plot(G_pxi, unit = "id", facets = list(item = seq(0, 30, 5)), 
                  g_coef = TRUE)
hemp::dstudy_plot(G_pxi, unit = "id", facets = list(item = seq(0, 30, 5)),
                  g_coef = FALSE)
#--------------

#-----------------------------------------------------------------
# 4.5.2 A two facet study
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
data.long2 <- reshape(data.ratings2, direction = "long", varying = list(3:7), 
      v.names = "ratings", timevar = "items", new.row.names = 1:3075)
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
dfit <- gtheory::dstudy(gfit, colname.objects = "idstud", 
                        colname.scores = "ratings", data = data.long2)
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
              (1 | items:rater) + (1 | idstud:rater) + (1 | idstud:items), 
            data = data.long2)
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

hemp::dstudy_plot(G_pxixr, unit = "idstud",
                  facets = list("items" = seq(from = 0, to = 5),
                                "rater" = c(1, 2, 3)),
                  g_coef = TRUE)
hemp::dstudy_plot(G_pxixr, unit = "idstud",
                  facets = list("items" = seq(from = 0, to = 5),
                                "rater" = c(1, 2, 3)),
                  g_coef = FALSE)
#--------------

#-----------------------------------------------------------------
# 4.7 Reliability in interactive application
#-----------------------------------------------------------------

ShinyItemAnalysis::run_app()
