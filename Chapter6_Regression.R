#-----------------------------------------------------------------
# Chapter 6 - Regression models for description of item properties
# Introduction to psychometric methods
# in educational, psychological, and health-related measurements.
# Using R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(ggplot2)
library(nnet)
library(ShinyItemAnalysis)
library(VGAM)

theme_side_by_side <- theme_app(base_size = 19)

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
    shape = 21, alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    size = 1, color = "darkblue"
  ) +
  xlab("Total score") +
  ylab("Score of item 1") +
  ggtitle("Item 1") +
  theme_side_by_side
#--------------

#ggsave("figures/regression_linear_EPIA_item1_total.png",
#       width = 6, height = 5, dpi = 300, bg = "transparent")

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
#--------------

#--------------
# plot of estimated curve
ggplot(data = EPIA, aes(x = `zscore`, y = `Item 1`)) +
  geom_point(
    color = "darkblue",
    fill = "darkblue",
    shape = 21, alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    size = 1, color = "darkblue"
  ) +
  xlab("Z-score") +
  ylab("Score of item 1") +
  ggtitle("Item 1") +
  theme_side_by_side
#--------------

#ggsave("figures/regression_linear_EPIA_item1_zscore.png",
#       width = 6, height = 5, dpi = 300, bg = "transparent")

#-----------------------------------------------------------------
# 6.4.1  Logistic regression
#-----------------------------------------------------------------

#--------------
data(HCI, package = "ShinyItemAnalysis")
data <- HCI[, 1:20] # item data
zscore <- scale(rowSums(HCI)) # Z-score

# logistic model for item 13
fit1 <- glm(data[, 13] ~ zscore, family = binomial)

# coefficients
coef(fit1)
## (Intercept)   zscore
##      0.5560   1.1625
#--------------

#--------------
# probability of answering item 13 with Z-score 0
exp(coef(fit1)[1]) / (1 + exp(coef(fit1)[1]))
## (Intercept)
##      0.6355

# log-odds of answering item 13 correctly with Z-score 1
coef(fit1)[1] + coef(fit1)[2]
## (Intercept)
##      1.719

# probability of answering item 13 with Z-score 1
exp(coef(fit1)[1] + coef(fit1)[2]) / (1 + exp(coef(fit1)[1] + coef(fit1)[2]))
## (Intercept)
##      0.8479
#--------------

#--------------
# probit link
fit2 <- glm(data[, 13] ~ zscore, family = binomial(link = "probit"))

# coefficients
coef(fit2)
## (Intercept)   zscore
##      0.3440   0.7038
#--------------

#--------------
# probability of correct answer for respondent with Z-score 0
pnorm(coef(fit2)[1])
## (Intercept)
##      0.6346

# probability of correct answer for respondent with Z-score 1
pnorm(coef(fit2)[1] + coef(fit2)[2])
## (Intercept)
##      0.8527

#--------------

#--------------
coef(fit2)[2] * 1.7
## zscore
##  1.197

coef(fit1)[2] / coef(fit2)[2]
## zscore
##  1.652
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
  y = tapply(data[, 13], zscore, mean),
  size = as.numeric(table(zscore))
)

# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = size),
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
    size = 1,
    color = "darkblue"
  ) +
  stat_function(
    fun = mod_probit, geom = "line",
    args = list(
      b0 = coef(fit2)[1],
      b1 = coef(fit2)[2]
    ),
    size = 1,
    color = "gold3", linetype = "dashed"
  ) +
  xlab("Z-score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 13") +
  theme_app()
#--------------

# ggsave("figures/regression_logistic_HCI_probit_logit_item13.png",
#        width = 6, height = 4, dpi = 300, bg = "transparent")

#--------------
# coefficients in IRT parametrization
setNames(
  c(coef(fit1)[2], -coef(fit1)[1] / coef(fit1)[2]),
  c("a", "b")
)
##      a       b
## 1.1625 -0.4782
#--------------

#--------------
# NLR 3P model for item 13
mod_3PL <- function(x, a, b, c) {
  c + (1 - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

fit3 <- nls(
  data[, 13] ~ mod_3PL(zscore, a, b, c),
  algorithm = "port",
  start = c(a = 0.7, b = -0.9, c = 0),
  lower = c(-Inf, -Inf, 0),
  upper = c(Inf, Inf, 1)
)

# coefficients
coef(fit3)
##      a      b      c
## 2.3046 0.2559 0.3115
#--------------

#--------------
# plot of estimated curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = size),
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
    size = 1,
    color = "darkblue"
  ) +
  xlab("Z-score") +
  ylab("Probability of correct answer") +
  ylim(0, 1) +
  ggtitle("Item 13") +
  theme_app()
#--------------

# ggsave("regression_3pl_HCI_item13.png",
#        width = 6, height = 4, dpi = 300, bg = "transparent")

#-----------------------------------------------------------------
# 6.4.2  Nonlinear regression models
#-----------------------------------------------------------------

#--------------
# NLR 4P model for item 13
mod_4PL <- function(x, a, b, c, d) {
  c + (d - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}

fit4 <- nls(
  data[, 13] ~ mod_4PL(zscore, a, b, c, d),
  algorithm = "port",
  start = c(a = 0.7, b = -0.9, c = 0, d = 1),
  lower = c(-Inf, -Inf, 0, 0),
  upper = c(Inf, Inf, 1, 1)
)

# coefficients
coef(fit4)
##      a      b      c      d
## 2.3046 0.2559 0.3115 1.0000
#--------------

#-----------------------------------------------------------------
# 6.5.1  Ordinal regression models
#-----------------------------------------------------------------

#--------------
data("Anxiety", package = "lordif")
data <- Anxiety[, paste0("R", 1:29)] - 1
head(data, n = 4)
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

# IRT parametrization
-coef(fit.cum)[1:4] / coef(fit.cum)[5]
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4
##       -0.2398        0.7392        1.9841        3.0002
#--------------

#--------------
# plotting cumulative probabilities
plotCumulative(
  fit.cum,
  type = "cumulative", matching.name = "Z-score"
) + theme_side_by_side + ggtitle("Item 18")

# ggsave("figures/regression_cumulative_Anxiety_cumulative_item18.png",
#        width = 6, height = 5, dpi = 300, bg = "transparent")

# plotting category probabilities
plotCumulative(
  fit.cum,
  type = "category", matching.name = "Z-score"
) + theme_side_by_side + ggtitle("Item 18")

# ggsave("figures/regression_cumulative_Anxiety_category_item18.png",
#        width = 6, height = 5, dpi = 300, bg = "transparent")
#--------------

#--------------
# adjacent category logit model for item 18
fit.adj <- vglm(
  data[, 18] ~ zscore,
  family = acat(reverse = FALSE, parallel = TRUE)
)

# coefficients for item 18
coef(fit.adj)
# (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4   zscore
#       -0.2322       -0.5296       -2.2146       -2.7410   1.0109

# IRT parametrization
-coef(fit.adj)[1:4] / coef(fit.adj)[5]
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4
##        0.2296        0.5239        2.1906        2.7113
#--------------

#--------------
# plotting category probabilities
plotAdjacent(fit.adj, matching.name = "Z-score") + ggtitle("Item 18")
#--------------

# ggsave("figures/regression_adjacent_Anxiety_item18.png",
#        width = 6, height = 4, dpi = 300, bg = "transparent")

#-----------------------------------------------------------------
# 6.5.1  Nominal response models
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

# IRT parametrization - difficulties
-coef(fit.mult)[, 1] / coef(fit.mult)[, 2]
##       B       C       D
## -1.6484 -1.3253 -1.2052
#--------------

#--------------
plotMultinomial(
  fit.mult,
  matching = zscore, matching.name = "Standardized total score"
)
#--------------

# ggsave("regression_multinomial_HCI_item13.png",
#        width = 6, height = 4, dpi = 300, bg = "transparent")
