#-----------------------------------------------------------------
# Chapter 6 - Regression models for description of item properties
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(ggplot2)
library(msm)
library(nnet)
library(ShinyItemAnalysis)
library(VGAM)


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
# 6.3.1  Linear regression
#-----------------------------------------------------------------

#--------------
data(EPIA, package = "EstCRM")
head(EPIA, n = 2)
##   Item 1 Item 2 Item 3 Item 4 Item 5
## 1     96     36     80     78     79
## 2     42      2      1      1      1

EPIA$score <- rowSums(EPIA) # total scores
#--------------

#--------------
# linear model for Item 1 on total scores
summary(lm(`Item 1` ~ score, data = EPIA))
## Call:
## lm(formula = `Item 1` ~ score, data = EPIA)
##
## Residuals:
##     Min      1Q  Median      3Q     Max
## -95.461 -15.981  -0.639  17.072  69.689
##
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)   15.5293     2.1678   7.164  1.5e-12 ***
## score          0.1878     0.0081  23.246  < 2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 24.03 on 1031 degrees of freedom
## Multiple R-squared:  0.3439,	Adjusted R-squared:  0.3433
## F-statistic: 540.4 on 1 and 1031 DF,  p-value: < 2.2e-16
#--------------

#--------------
# plot of estimated curve
ggplot(data = EPIA, aes(x = `score`, y = `Item 1`)) +
  geom_point(
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5, size = 1.8
  ) +
  geom_smooth(
    method = "lm",
    size = 0.8, color = "darkblue"
  ) +
  xlab("Total score") +
  ylab("Score of item 1") +
  ggtitle("Item 1") + 
  theme_fig()
#--------------

#--------------
coef(lm(`Item 1` ~ score, data = EPIA))
## (Intercept)   score
##     15.5292  0.1876
sqrt(diag(vcov(lm(`Item 1` ~ score, data = EPIA))))
## (Intercept)    score
##      2.1676   0.0081
#--------------

#--------------
mean(EPIA$score) # average total score
## [1] 251.8964
sd(EPIA$score) # standard deviation of total score
## [1] 92.5968

EPIA$zscore <- scale(EPIA$score) # Z-scores

# linear model for Item 1 on Z-scores
coef(lm(`Item 1` ~ zscore, data = EPIA))
## (Intercept)      zscore
##     62.8296     17.3876

sqrt(diag(vcov(lm(`Item 1` ~ zscore, data = EPIA))))
## (Intercept)      zscore
##   0.7476159   0.7479780

# Correspondence to classical item parameters:
mean(EPIA$`Item 1`)
## [1] 62.8296

cor(EPIA$`Item 1`, EPIA$zscore) * sd(EPIA$`Item 1`)
## [1,] 17.3876
#--------------

#--------------
# plot of estimated curve
ggplot(data = EPIA, aes(x = `zscore`, y = `Item 1`)) +
  geom_point(
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5, size = 1.8
  ) +
  geom_smooth(
    method = "lm",
    size = 0.8, color = "darkblue"
  ) +
  xlab("Z-score") +
  ylab("Score of item 1") +
  ggtitle("Item 1") +
  theme_fig()
#--------------

#-----------------------------------------------------------------
# 6.4.1  Logistic regression
#-----------------------------------------------------------------

#--------------
data(HCI, package = "ShinyItemAnalysis")
zscore <- scale(rowSums(HCI[, 1:20])) # Z-score

# logistic model for item 13
fit1 <- glm(HCI[, 13] ~ zscore, family = binomial)

# coefficients
coef(fit1)
## (Intercept)   zscore
##      0.5552   1.1824 

# standard errors
sqrt(diag(vcov(fit1)))
## (Intercept)   zscore
##      0.0932   0.1084
#--------------

#--------------
# probability of answering item 13 with Z-score 0
exp(coef(fit1)[1]) / (1 + exp(coef(fit1)[1]))
## (Intercept)
##      0.6353

# log-odds of answering item 13 correctly with Z-score 1
coef(fit1)[1] + coef(fit1)[2]
## (Intercept)
##      1.7377

# probability of answering item 13 with Z-score 1
exp(coef(fit1)[1] + coef(fit1)[2]) / (1 + exp(coef(fit1)[1] + coef(fit1)[2]))
## (Intercept)
##      0.8504
#--------------

#--------------
# probit link
fit2 <- glm(HCI[, 13] ~ zscore, family = binomial(link = "probit"))

# coefficients
coef(fit2)
## (Intercept)   zscore
##      0.3440   0.7122
# standard errors
sqrt(diag(vcov(fit2)))
## (Intercept)   zscore
##      0.0549   0.0608
#--------------

#--------------
# probability of correct answer for respondent with Z-score 0
pnorm(coef(fit2)[1])
## (Intercept)
##      0.6346

# probability of correct answer for respondent with Z-score 1
pnorm(coef(fit2)[1] + coef(fit2)[2])
## (Intercept)
##      0.8546
#--------------

#--------------
coef(fit2)[2] * 1.7
##  zscore
##  1.2108

coef(fit1)[2] / coef(fit2)[2]
##  zscore
##  1.6601
#--------------

#--------------
# function for plot (logit)
mod_logit <- function(x, b0, b1) {
  exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
}

# function for plot (probit)
mod_probit <- function(x, b0, b1) {
  pnorm(b0 + b1 * x)
}

df <- data.frame(
  x = sort(unique(zscore)),
  y = tapply(HCI[, 13], zscore, mean),
  Count = as.numeric(table(zscore))
)
#--------------

#--------------
# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = Count),
             color = "darkblue",
             fill = "darkblue",
             shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = mod_logit, geom = "line",
    args = list(
      b0 = coef(fit1)[1],
      b1 = coef(fit1)[2]
    ),
    size = 0.8,
    aes(color = "logit", linetype = "logit")
  ) +
  stat_function(
    fun = mod_probit, geom = "line",
    args = list(
      b0 = coef(fit2)[1],
      b1 = coef(fit2)[2]
    ),
    size = 0.8,
    aes(color = "probit", linetype = "probit")
  ) +
  xlab("Z-score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  scale_colour_manual("Link", values = c("darkblue", "darkgoldenrod2")) + 
  scale_linetype_manual("Link", values = c("solid", "dashed")) + 
  ggtitle("Item 13") + 
  theme_fig()
#--------------

#--------------
# coefficients in IRT parametrization
IRTpars13 <- c(coef(fit1)[2], -coef(fit1)[1] / coef(fit1)[2])
setNames(IRTpars13, c("a", "b"))
##      a       b
## 1.1824 -0.4696

# delta method to calculate SE (with the deltamethod() function)
msm::deltamethod(
  list(~x2, ~ -x1 / x2),
  mean = coef(fit1),
  cov = vcov(fit1)
)
## [1] 0.1084 0.0823

# delta method to calculate SE (by hand)
g <- list(~ beta1, ~ -beta0 / beta1) # formula
# covariance matrix of original item parameters
Sigma <- matrix(vcov(fit1), ncol = 2, nrow = 2,
                dimnames = list(c("beta0", "beta1"), 
                                c("beta0", "beta1")))
syms <- colnames(Sigma)
coefs_fit1 <- coef(fit1)
for (i in 1:2) assign(syms[i], coef(fit1)[i])
# calculation of gradient
nabla <- t(sapply(g, function(form) {
  as.numeric(attr(eval(deriv(form, syms)), "gradient"))
}))
new.covar <- nabla %*% Sigma %*% t(nabla)

# new standard errors
sqrt(diag(new.covar))
## [1] 0.1084 0.0823
#--------------

#-----------------------------------------------------------------
# 6.4.2  Nonlinear regression models
#-----------------------------------------------------------------

#--------------
# NLR 3P model for item 13
mod_3PL <- function(x, a, b, c) {
  c + (1 - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

fit3 <- nls(
  HCI[, 13] ~ mod_3PL(zscore, a, b, c),
  algorithm = "port",
  start = c(a = 0.7, b = -0.9, c = 0),
  lower = c(-Inf, -Inf, 0),
  upper = c(Inf, Inf, 1)
)

# coefficients
coef(fit3)
##      a      b      c
## 2.6692 0.2708 0.3214 
#--------------

#--------------
# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = Count),
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = mod_3PL, geom = "line",
    args = list(
      a = coef(fit3)[1],
      b = coef(fit3)[2],
      c = coef(fit3)[3]
    ),
    size = 0.8,
    color = "darkblue"
  ) +
  xlab("Z-score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 13")  +
  theme_fig() + 
  theme(legend.position = c(0.88, 0.23))
#--------------

#--------------
# NLR 4P model for item 13
mod_4PL <- function(x, a, b, c, d) {
  c + (d - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

fit4 <- nls(
  HCI[, 13] ~ mod_4PL(zscore, a, b, c, d),
  algorithm = "port",
  start = c(a = 0.7, b = -0.9, c = 0, d = 1),
  lower = c(-Inf, -Inf, 0, 0),
  upper = c(Inf, Inf, 1, 1)
)

# coefficients
coef(fit4)
##      a      b      c      d
## 3.3405 0.2326 0.3379 0.9569 
#--------------

#-----------------------------------------------------------------
# 6.4.2.2  Model selection and model fit
#-----------------------------------------------------------------

#--------------
# AIC
AIC(fit1, fit2, fit3, fit4)
##      df      AIC
## fit1  2 715.1369
## fit2  2 713.7691
## fit3  4 740.8746
## fit4  5 742.4880

# BIC
BIC(fit1, fit2, fit3, fit4)
##      df      BIC
## fit1  2 724.0939
## fit2  2 722.7261
## fit3  4 758.7886
## fit4  5 764.8806
#--------------

#-----------------------------------------------------------------
# 6.5.1  Ordinal regression models
#-----------------------------------------------------------------

#--------------
data("Anxiety", package = "lordif")
data <- Anxiety[, paste0("R", 1:29)] - 1

zscore <- scale(rowSums(data)) # Z-scores
maxval <- max(data[, 18]) # maximal number of points for item 18
# reordering item 18
data[, 18] <- ordered(factor(data[, 18], levels = 0:maxval))

# cumulative logit model for item 18
fit.cum <- vglm(
  data[, 18] ~ zscore,
  family = cumulative(reverse = TRUE, parallel = TRUE)
)

# coefficients for item 18
coef(fit.cum)
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##        0.4155       -1.2807       -3.4374       -5.1978    1.7325
# SE
sqrt(diag(vcov(fit.cum)))
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##        0.0865        0.1031        0.1825        0.2880    0.0955

# IRT parametrization
c(-coef(fit.cum)[1:4] / coef(fit.cum)[5], coef(fit.cum)[5])
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##       -0.2398        0.7392        1.9841        3.0002    1.7325
# SE using delta method
msm::deltamethod(
  list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
  mean = coef(fit.cum),
  cov = vcov(fit.cum)
)
## [1] 0.0486 0.0643 0.1033 0.1610 0.0955
#--------------

#--------------
# plotting cumulative probabilities
plotCumulative(
  fit.cum,
  type = "cumulative", matching.name = "Z-score"
)  +
  theme_fig() + 
  xlim(-1.1, 5.2) + 
  theme(legend.position = c(0.79, 0.23),
        legend.box = "horizontal",
        legend.margin = margin(0, -5, 0, 0),
        legend.background = element_blank())  
#--------------

#--------------
# plotting category probabilities
plotCumulative(
  fit.cum,
  type = "category", matching.name = "Z-score"
)   +
  theme_fig() + 
  theme(legend.position = c(0.87, 0.5),
        legend.box = "horizontal",
        legend.margin = margin(0, -5, 0, 0),
        legend.background = element_blank()) +
  scale_size_continuous(breaks = c(0, 5, 10, 15)) + 
  guides(size = FALSE)
#--------------

#--------------
# adjacent category logit model for item 18
fit.adj <- vglm(
  data[, 18] ~ zscore,
  family = acat(reverse = FALSE, parallel = TRUE)
)

# coefficients for item 18
coef(fit.adj)
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##       -0.2322       -0.5296       -2.2146       -2.7410    1.0109
# SE
sqrt(diag(vcov(fit.adj)))
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
## 0.0976        0.1183        0.2111        0.3347           0.0722
#--------------

#--------------
# IRT parametrization
c(-coef(fit.adj)[1:4] / coef(fit.adj)[5], coef(fit.adj)[5])
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##        0.2296        0.5239        2.1906        2.7113    1.0109
# SE using delta method
msm::deltamethod(
  list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
  mean = coef(fit.adj),
  cov = vcov(fit.adj)
)
## [1] 0.1035 0.1163 0.1874 0.2769 0.0722
#--------------

#--------------
# plotting category probabilities
plotAdjacent(fit.adj, matching.name = "Z-score")   +
  theme_fig() + 
  scale_size_continuous(breaks = c(5, 10, 30, 50)) + 
  theme(legend.position = c(0.8, 0.7),
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0),
        legend.background = element_blank())
#--------------

#-----------------------------------------------------------------
# 6.5.2  Nominal response models
#-----------------------------------------------------------------

#--------------
# multinominal model
data(HCItest, HCIkey, package = "ShinyItemAnalysis")
key <- unlist(HCIkey)
zscore <- scale(rowSums(HCI[, 1:20])) # Z-score

# re-leveling item 13
HCItest[, 13] <- relevel(HCItest[, 13], ref = paste(key[13]))

# multinomial model for item 13
fit.mult <- multinom(HCItest[, 13] ~ zscore)

# coefficients for item 13
coef(fit.mult)
##   (Intercept)  zscore
## B     -2.3463 -1.4044
## C     -1.6241 -1.1924
## D     -1.2820 -1.0514
# SE
matrix(sqrt(diag(vcov(fit.mult))), ncol = 2, byrow = TRUE)
##        [,1]   [,2]
## [1,] 0.1976 0.1860
## [2,] 0.1410 0.1458
## [3,] 0.1187 0.1289

# IRT parametrization - difficulties
cbind(-coef(fit.mult)[, 1] / coef(fit.mult)[, 2], coef(fit.mult)[, 2])
##      [,1]    [,2]
## B -1.6484 -1.4234
## C -1.3253 -1.2354
## D -1.2052 -1.0560
# SE using delta method
subst_vcov <- function(vcov, cat) {
  ind <- grep(cat, colnames(vcov))
  vcov[ind, ind]
}
t(sapply(
  rownames(coef(fit.mult)),
  function(.x) {
    vcov_subset <- subst_vcov(vcov(fit.mult), .x)
    msm::deltamethod(
      list(~ -x1 / x2, ~x2),
      mean = coef(fit.mult)[.x, ],
      cov = vcov_subset,
      ses = TRUE
    )
  }
))
##     [,1]   [,2]
## B 0.1819 0.1860
## C 0.1537 0.1458
## D 0.1611 0.1289
#--------------

#--------------
plotMultinomial(
  fit.mult,
  matching = zscore, matching.name = "Standardized total score"
)  +
  theme_fig() + 
  scale_size_continuous(breaks = c(5, 10, 30, 50)) + 
  theme(legend.position = c(0.22, 0.75),
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0),
        legend.background = element_blank())
#--------------

#-----------------------------------------------------------------
# 6.6.1  Joint model
#-----------------------------------------------------------------

#--------------
# Joint model for all items: data preparation
HCI$zscore <- scale(rowSums(HCI[, 1:20])) # Z-score
HCI$person <- 1:nrow(HCI)

# converting data to the long format
HCI.long <- reshape(
  data = HCI,
  varying = list(paste("Item", 1:20)), timevar = "item", v.names = "rating",
  idvar = c("person", "gender", "major", "zscore"),
  direction = "long", new.row.names = 1:13020
)

head(HCI.long, n = 2)
##   gender major   zscore person item rating
## 1      0     1 1.040755      1    1      1
## 2      0     1 1.865002      2    1      1

summary(HCI.long)
# person ID and item ID as factors:
HCI.long$person <- as.factor(HCI.long$person)
HCI.long$item <- as.factor(HCI.long$item)
#--------------

#--------------
# Joint model for all items: data preparation
# 1PL model for all items
fit_glm1PL <- glm(rating ~ -1 + item,
  data = HCI.long,
  family = binomial
)
coef(fit_glm1PL)
##   item1   item2   item3   item4   item5   item6   item7   item8
##  0.8422  1.1130  1.7184 -0.3889 -0.2314 -0.5644  0.1880  0.8716
##   item9  item10  item11  item12  item13  item14  item15  item16
## -0.2689  0.6113  1.2587  0.2940  0.4402  1.1463 -0.2252  0.4337
##  item17  item18  item19  item20
## -0.8642  1.3691  1.2947  0.9542

# 2PL model for all items
fit_glm2PL <- glm(rating ~ -1 + item + zscore:item,
  data = HCI.long,
  family = binomial
)
coef(fit_glm2PL)
##         item1         item2         item3         item4         item5
##        1.0118        1.2723        2.3746       -0.4340       -0.2873
##         item6         item7         item8         item9        item10
##       -0.7498        0.1972        1.0867       -0.3114        0.7139
##        item11        item12        item13        item14        item15
##        1.5345        0.3508        0.5552        1.4739       -0.2879
##        item16        item17        item18        item19        item20
##        0.5587       -0.8941        2.0232        1.7478        1.2137
##  item1:zscore  item2:zscore  item3:zscore  item4:zscore  item5:zscore
##  0.9925        0.8294        1.4842        0.6751        0.9399
##  item6:zscore  item7:zscore  item8:zscore  item9:zscore item10:zscore
##  1.1988        0.4931        1.1130        0.7690        0.9007
## item11:zscore item12:zscore item13:zscore item14:zscore item15:zscore
##  1.0535        1.0329        1.1824        1.2166        1.0093
## item16:zscore item17:zscore item18:zscore item19:zscore item20:zscore
##  1.2543        0.3844        1.6575        1.3748        1.1766
#--------------

#--------------
anova(fit1, fit2, test = "Chisq")
BIC(fit1, fit2)
AIC(fit1, fit2)
#--------------
