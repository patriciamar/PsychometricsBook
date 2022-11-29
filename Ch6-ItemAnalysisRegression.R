#-----------------------------------------------------------------
# Chapter 6 - Item analysis with regression models
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
# 6.3  Models for continuous items
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 6.3.1  Linear regression model
#-----------------------------------------------------------------

#--------------
data(EPIA, package = "ShinyItemAnalysis")
head(EPIA, n = 2)
##   Item 1 Item 2 Item 3 Item 4 Item 5 score
## 1     96     36     80     78     79   369
## 2     42      2      1      1      1    47
#--------------

#--------------
# Z-scores
mean(EPIA$score)
## [1] 251.8964
sd(EPIA$score)
## [1] 92.5968
((EPIA$score - mean(EPIA$score))/sd(EPIA$score))
## [1]  1.2647 -2.2128 -0.8412  1.7506 -1.7592  0.3359  0.8543 ...
(EPIA$zscore <- scale(EPIA$score))[1:10]
## [1]  1.2647 -2.2128 -0.8412  1.7506 -1.7592  0.3359  0.8543 ...
#--------------

#--------------
# linear model for Item 1 on total scores
summary(fitLM <- lm(`Item 1` ~ score, data = EPIA))
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
coef(fitLM)
## (Intercept)   score
##     15.5292  0.1876
sqrt(diag(vcov(fitLM)))
## (Intercept)    score
##      2.1676   0.0081
#--------------

#--------------
# linear model for Item 1 on Z-scores (code for summary not shown in the book)
summary(lm(`Item 1` ~ zscore, data = EPIA))
#--------------

#--------------
# linear model for Item 1 on Z-scores: item parameters
coef(lm(`Item 1` ~ zscore, data = EPIA))
## (Intercept)      zscore
##     62.8296     17.3876
sqrt(diag(vcov(lm(`Item 1` ~ zscore, data = EPIA))))
## (Intercept)      zscore
##   0.7476159   0.7479780
#--------------

#--------------
# relationship with traditional item parameters:
mean(EPIA$`Item 1`)
## [1] 62.8296
cor(EPIA$`Item 1`, EPIA$zscore) * sd(EPIA$`Item 1`)
## [1,] 17.3876
#--------------

#--------------
# plot of estimated regression line for total scores
# (code not shown in the book)
library(ggplot2)
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
# plot of estimated regression line for Z-scores
# (code not shown in the book)
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
# 6.4  Models for binary items
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 6.4.1  Logistic regression model
#-----------------------------------------------------------------

#--------------
data(HCI, package = "ShinyItemAnalysis")
zscore <- scale(HCI$total)
#--------------

#--------------
# logistic model for item 13
fit2PL <- glm(HCI[, 13] ~ zscore, family = binomial)
#--------------

#--------------
#model summary (code not shown in the book)
summary(fit2PL)
#--------------

#--------------
# estimated model parameters and standard errors
coef(fit2PL)
## (Intercept)   zscore
##      0.5552   1.1824 
sqrt(diag(vcov(fit2PL)))
## (Intercept)   zscore
##      0.0932   0.1084
#--------------

#--------------
# model predictions (code not shown in the book)
# probability of answering item 13 with Z-score 0
exp(coef(fit2PL)[1]) / (1 + exp(coef(fit2PL)[1]))
## (Intercept)
##      0.6353

# log-odds of answering item 13 correctly with Z-score 1
coef(fit2PL)[1] + coef(fit2PL)[2]
## (Intercept)
##      1.7377

# probability of answering item 13 with Z-score 1
exp(coef(fit2PL)[1] + coef(fit2PL)[2]) / (1 + exp(coef(fit2PL)[1] + coef(fit2PL)[2]))
## (Intercept)
##      0.8504
#--------------

#-----------------------------------------------------------------
# 6.4.2  Other link functions, probit regression model
#-----------------------------------------------------------------

#--------------
# probit model
fit2PLprob <- glm(HCI[, 13] ~ zscore, 
                  family = binomial(link = "probit"))
#--------------

#--------------
#model summary (code not shown in the book)
summary(fit2PLprob)
#--------------

#--------------
# estimated model parameters and standard errors
coef(fit2PLprob)
## (Intercept)   zscore
##      0.3440   0.7122
sqrt(diag(vcov(fit2PLprob)))
## (Intercept)   zscore
##      0.0549   0.0608
#--------------

#--------------
# model predictions (code not shown in the book)
# probability of correct answer for respondent with Z-score 0
pnorm(coef(fit2PLprob)[1])
## (Intercept)
##      0.6346

# probability of correct answer for respondent with Z-score 1
pnorm(coef(fit2PLprob)[1] + coef(fit2PLprob)[2])
## (Intercept)
##      0.8546
#--------------

#--------------
# ratio between slopes estimated from logit and probit regression
# (code not shown in the book)
coef(fit2PL)[2] / coef(fit2PLprob)[2]
##  zscore
##  1.6601
#--------------

#--------------
# estimated regression curves (code not shown in the book)
# regression model (logistic)
mod_logit <- function(x, b0, b1) {
  exp(b0 + b1 * x) / (1 + exp(b0 + b1 * x))
}

# regression model (probit)
mod_probit <- function(x, b0, b1) {
  pnorm(b0 + b1 * x)
}

df <- data.frame(
  x = sort(unique(zscore)),
  y = tapply(HCI[, 13], zscore, mean),
  Count = as.numeric(table(zscore))
)

# plot of estimated curves
ggplot(df, aes(x = x, y = y)) +
  geom_point(aes(size = Count),
             color = "darkblue",
             fill = "darkblue",
             shape = 21, alpha = 0.5
  ) +
  stat_function(
    fun = mod_logit, geom = "line",
    args = list(
      b0 = coef(fit2PL)[1],
      b1 = coef(fit2PL)[2]
    ),
    size = 0.8,
    aes(color = "logit", linetype = "logit")
  ) +
  stat_function(
    fun = mod_probit, geom = "line",
    args = list(
      b0 = coef(fit2PLprob)[1],
      b1 = coef(fit2PLprob)[2]
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
  theme_fig() + 
  theme(legend.position = c(0.88, 0.42),
        legend.box = "vertical") + 
  geom_segment(aes(
    y = 0, yend = mod_probit(x = 0, b0 = coef(fit2PLprob)[1], b1 = coef(fit2PLprob)[2]),
    x = 0, xend = 0
  ),
  color = "gray", size = 0.8, linetype = "dashed"
  ) +
  geom_segment(aes(
    y = 0, yend = mod_probit(x = 1, b0 = coef(fit2PLprob)[1], b1 = coef(fit2PLprob)[2]),
    x = 1, xend = 1
  ),
  color = "gray", size = 0.8, linetype = "dashed"
  ) +
  geom_segment(aes(
    y = mod_probit(x = 1, b0 = coef(fit2PLprob)[1], b1 = coef(fit2PLprob)[2]), yend = mod_probit(x = 1, b0 = coef(fit2PLprob)[1], b1 = coef(fit2PLprob)[2]),
    x = -2.64, xend = 1
  ),
  color = "gray", size = 0.8, linetype = "dashed"
  ) +
  geom_segment(aes(
    y = mod_probit(x = 0, b0 = coef(fit2PLprob)[1], b1 = coef(fit2PLprob)[2]), yend = mod_probit(x = 0, b0 = coef(fit2PLprob)[1], b1 = coef(fit2PLprob)[2]),
    x = -2.64, xend = 0
  ),
  color = "gray", size = 0.8, linetype = "dashed"
  ) 
#--------------

#-----------------------------------------------------------------
# 6.4.3  IRT parametrization
#-----------------------------------------------------------------

#--------------
# model parameters in IRT parametrization
IRTpars13 <- c(coef(fit2PL)[2], -coef(fit2PL)[1] / coef(fit2PL)[2])
setNames(IRTpars13, c("a", "b"))
##      a       b
## 1.1824 -0.4696
#--------------

#--------------
# delta method to calculate SE using deriv() function 
#  (code not shown in the book)
g <- list(~ beta1, ~ -beta0 / beta1) # formula
# covariance matrix of original item parameters
Sigma <- matrix(vcov(fit2PL), ncol = 2, nrow = 2,
                dimnames = list(c("beta0", "beta1"), 
                                c("beta0", "beta1")))
syms <- colnames(Sigma)
coefs_fit2PL <- coef(fit2PL)
for (i in 1:2) assign(syms[i], coef(fit2PL)[i])
# calculation of gradient
nabla <- t(sapply(g, function(form) {
  as.numeric(attr(eval(deriv(form, syms)), "gradient"))
}))
new.covar <- nabla %*% Sigma %*% t(nabla)

# new standard errors
sqrt(diag(new.covar))
## [1] 0.1084 0.0823
#--------------

#--------------
# delta method to calculate SE using deltamethod() function
library(msm)
deltamethod(list(~x2, ~ -x1 / x2), 
            mean = coef(fit2PL), cov = vcov(fit2PL))
## [1] 0.1084 0.0823
#--------------

#-----------------------------------------------------------------
# 6.4.4  Nonlinear regression models
#-----------------------------------------------------------------

#--------------
# 3PL model for item 13
mod_3PL <- function(x, a, b, c) {
  c + (1 - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}
#--------------

#--------------
fit3PL <- nls(HCI[, 13] ~ mod_3PL(zscore, a, b, c),
              algorithm = "port", start = c(a = 0.7, b = -0.9, c = 0),
              lower = c(-Inf, -Inf, 0), upper = c(Inf, Inf, 1))
#--------------

#--------------
# model summary (code not shown in the book)
summary(fit3PL)
#--------------

#--------------
# estimated model parameters and their standard errors
coef(fit3PL)
##      a      b      c
## 2.6692 0.2708 0.3214 
sqrt(diag(vcov(fit3PL)))
##      a      b      c 
## 0.5276 0.0908 0.0393
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
      a = coef(fit3PL)[1],
      b = coef(fit3PL)[2],
      c = coef(fit3PL)[3]
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
# 4PL model for item 13
mod_4PL <- function(x, a, b, c, d) {
  c + (d - c) * exp(a * (x - b)) / (1 + exp(a * (x - b)))
}
#--------------

#--------------
fit4PL <- nls(HCI[, 13] ~ mod_4PL(zscore, a, b, c, d),
              algorithm = "port", 
              start = c(a = 0.7, b = -0.9, c = 0, d = 1),
              lower = c(-Inf, -Inf, 0, 0), upper = c(Inf, Inf, 1, 1))
#--------------

#--------------
# model summary (code not shown in the book)
summary(fit4PL)
#--------------

#--------------
# estimated model parameters and their standard errors
coef(fit4PL)
##      a      b      c      d
## 3.3405 0.2326 0.3379 0.9569 
sqrt(diag(vcov(fit4PL)))
##      a      b      c      d 
## 0.9844 0.0992 0.0363 0.0500 
#--------------

#-----------------------------------------------------------------
# 6.6  Model selection and model fit
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 6.6.2 Akaike information criterion
#-----------------------------------------------------------------

#--------------
# AIC
AIC(fit3PL, fit4PL)
##            df      AIC
## fit3PL      4 740.8746
## fit4PL      5 742.4880
#--------------

#-----------------------------------------------------------------
# 6.6.2 Bayesian information criterion
#-----------------------------------------------------------------

#--------------
# BIC
BIC(fit3PL, fit4PL)
##            df      BIC
## fit3PL      4 758.7886
## fit4PL      5 764.8806
#--------------

#-----------------------------------------------------------------
# 6.7  Models for polytomous items
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 6.7.1  Ordinal regression models
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 6.7.1.1 Cumulative logit model
#-----------------------------------------------------------------

#--------------
data("Anxiety", package = "ShinyItemAnalysis")
zscore <- Anxiety$zscore
Anxiety_items <- Anxiety[, paste0("R", 1:29)] - 1
Anxiety_items$R18 <- ordered(factor(Anxiety_items$R18, 
                                    levels = 0:max(Anxiety_items$R18)))
#--------------

#--------------
# cumulative logit model for item 18
library(VGAM)
fit.cum <- vglm(Anxiety_items$R18 ~ zscore,
                family = cumulative(reverse = TRUE, parallel = TRUE))
#--------------

#--------------
# model summary (code not shown in the book)
summary(fit.cum)
#--------------

#--------------
# coefficients for item 18
coef(fit.cum)
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##        0.4155       -1.2807       -3.4374       -5.1978    1.7325
# SE
sqrt(diag(vcov(fit.cum)))
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##        0.0865        0.1031        0.1825        0.2880    0.0955
#--------------

#--------------
# IRT parametrization
c(-coef(fit.cum)[1:4] / coef(fit.cum)[5], coef(fit.cum)[5])
## (Intercept):1 (Intercept):2 (Intercept):3 (Intercept):4    zscore
##       -0.2398        0.7392        1.9841        3.0002    1.7325
#--------------

#--------------
# SE using delta method
deltamethod(
  list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
  mean = coef(fit.cum),
  cov = vcov(fit.cum)
)
## [1] 0.0486 0.0643 0.1033 0.1610 0.0955
#--------------

#--------------
# plotting cumulative probabilities
library(ShinyItemAnalysis)
plotCumulative(fit.cum, type = "cumulative")
#--------------

#--------------
# more plotting options (code not shown in the book)
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
plotCumulative(fit.cum, type = "category")
#--------------

#--------------
# more plotting options (code not shown in the book)
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

#-----------------------------------------------------------------
# 6.7.1.2 Adjacent category logit model
#-----------------------------------------------------------------

#--------------
# adjacent category logit model for item 18
fit.adj <-  vglm(Anxiety_items$R18 ~ zscore,
                 family = acat(reverse = FALSE, parallel = TRUE))
#--------------

#--------------
# model summary (code not shown in the book)
summary(fit.adj)
#--------------

#--------------
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
#--------------

#--------------
# SE using delta method
deltamethod(
  list(~ -x1 / x5, ~ -x2 / x5, ~ -x3 / x5, ~ -x4 / x5, ~x5),
  mean = coef(fit.adj), cov = vcov(fit.adj))
## [1] 0.1035 0.1163 0.1874 0.2769 0.0722
#--------------

#--------------
# plotting category probabilities
plotAdjacent(fit.adj, matching.name = "Z-score")
#--------------

#--------------
# more plotting options (code not shown in the book)
plotAdjacent(fit.adj, matching.name = "Z-score") +
  theme_fig() + 
  scale_size_continuous(breaks = c(5, 10, 30, 50)) + 
  theme(legend.position = c(0.8, 0.7),
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0),
        legend.background = element_blank())
#--------------

#-----------------------------------------------------------------
# 6.7.2  Nominal response models
#-----------------------------------------------------------------

#--------------
data(HCItest, HCIkey, package = "ShinyItemAnalysis")
key <- unlist(HCIkey)
zscore <- scale(HCItest$total)
HCItest[, 13] <- relevel(HCItest[, 13], ref = paste(key[13]))
#--------------

#--------------
# multinomial model for item 13
library(nnet)
fit.mult <- multinom(HCItest[, 13] ~ zscore)
#--------------

#--------------
# model summary (code not shown in the book)
summary(fit.mult)
#--------------

#--------------
# item parameters and their standard errors
coef(fit.mult)
##   (Intercept)  zscore
## B     -2.3463 -1.4234
## C     -1.6374 -1.2354
## D     -1.2726 -1.0560
# SE
matrix(sqrt(diag(vcov(fit.mult))), ncol = 2, byrow = TRUE)
##        [,1]   [,2]
## [1,] 0.1976 0.1860
## [2,] 0.1410 0.1458
## [3,] 0.1187 0.1289
#--------------

#--------------
# IRT parametrization - difficulties
cbind(-coef(fit.mult)[, 1] / coef(fit.mult)[, 2], coef(fit.mult)[, 2])
##      [,1]    [,2]
## B -1.6484 -1.4234
## C -1.3253 -1.2354
## D -1.2052 -1.0560
#--------------

#--------------
# SE using delta method
subst_vcov <- function(vcov, cat) {
  ind <- grep(cat, colnames(vcov))
  vcov[ind, ind]
}
t(sapply(
  rownames(coef(fit.mult)),
  function(.x) {
    vcov_subset <- subst_vcov(vcov(fit.mult), .x)
    deltamethod(list(~ -x1 / x2, ~x2),
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
plotMultinomial(fit.mult, matching = zscore)
#--------------

#--------------
# more plotting options (code not shown in the book)
plotMultinomial(
  fit.mult,
  matching = zscore, matching.name = "Z-score"
)  +
  theme_fig() + 
  scale_size_continuous(breaks = c(5, 10, 30, 50)) + 
  theme(legend.position = c(0.22, 0.75),
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0),
        legend.background = element_blank())
#--------------

#-----------------------------------------------------------------
# 6.8  Joint model
#-----------------------------------------------------------------

#--------------
# data preparation
data("HCIlong", package = "ShinyItemAnalysis")
#--------------

#--------------
head(HCIlong)
summary(HCIlong)
HCIlong$zscore <- (HCIlong$total - mean(HCI$total)) / sd(HCI$total)
HCIlong$id <- factor(HCIlong$id)
HCIlong$item <- factor(gsub("Item ", "", HCIlong$item), levels = 1:20)
summary(HCIlong)
#--------------

#--------------
# joint model for all items - 1PL model with common discrimination
fit1PLjoint <- glm(rating ~ -1 + item + zscore, data = HCIlong,
                   family = binomial)
# coefficients
coef(fit1PLjoint)
##  item1   item2   item3 ...  item13 ...  zscore 
## 1.0098  1.3320  2.0333 ...  0.5244 ...  0.9861 
#--------------

#--------------
# code not shown in the book
library(grDevices)
library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
colours <- getPalette(20)

fit1PLjoint_b0 <- coef(fit1PLjoint)[1:20]
fit1PLjoint_b1 <- coef(fit1PLjoint)[21]

ICC_IS_parametric <- function(x, b0, b1) {
  return(1 / (1 + exp(-b0 - b1 * x)))
}
(g <- ggplot(data.frame(x = 0), mapping = aes(x = x)) +
    xlim(-4, 4) +
    labs(
      y = "Probability of correct answer",
      x = expression(Observed~ability~X[p])
    ) +
    ylim(c(0, 1)) +
    theme_fig())

for (i in 1:20) {
  g <- g + 
    stat_function(
      fun = ICC_IS_parametric,
      args = list(b0 = fit1PLjoint_b0[i], b1 = fit1PLjoint_b1[1]),
      linewidth = .8,
      geom = "line", show.legend = FALSE, col = colours[i], linetype = i
    )
}
g

annotation <- data.frame(x = seq(-3, 2, length.out = 20), 
                         y = ICC_IS_parametric(x = seq(-3, 2, length.out = 20), b0 = fit1PLjoint_b0, b1 = fit1PLjoint_b1),
                         label = paste("Item", 1:20))
g + geom_text(data = annotation, aes(x = x, y = y, label = label),        
              color = colours, size = 3.5, angle = 45, fontface = "bold")
#--------------

#--------------
# joint model for all items - 2PL model
fit2PLjoint <- glm(rating ~ -1 + item + zscore:item, data = HCIlong,
                   family = binomial)
# coefficients
coef(fit2PLjoint)
##        item1         item2         item3 ...        item13 ...
##       1.0118        1.2723        2.3746 ...        0.5552 ...
## item1:zscore  item2:zscore  item3:zscore ... item13:zscore ...
##       0.9925        0.8294        1.4842 ...        1.1824 ...
#--------------

#--------------
# code not shown in the book
fit2PLjoint_b0 <- coef(fit2PLjoint)[1:20]
fit2PLjoint_b1 <- coef(fit2PLjoint)[21:40]

ICC_IS_parametric <- function(x, b0, b1) {
  return(1 / (1 + exp(-b0 - b1 * x)))
}
(g <- ggplot(data.frame(x = 0), mapping = aes(x = x)) +
    xlim(-4, 4) +
    labs(
      y = "Probability of correct answer",
      x = expression(Observed~ability~X[p])
    ) +
    ylim(c(0, 1)) +
    theme_fig())

for (i in 1:20) {
  g <- g + 
    stat_function(
      fun = ICC_IS_parametric,
      args = list(b0 = fit2PLjoint_b0[i], b1 = fit2PLjoint_b1[i]),
      linewidth = .8,
      geom = "line", show.legend = FALSE, col = colours[i], linetype = i
    )
}
g

annotation <- data.frame(x = seq(-3, 2, length.out = 20), 
                         y = ICC_IS_parametric(x = seq(-3, 2, length.out = 20), b0 = fit2PLjoint_b0, b1 = fit2PLjoint_b1),
                         label = paste("Item", 1:20))
g + geom_text(data = annotation, aes(x = x, y = y, label = label),        
              color = colours, size = 3.5, angle = 45, fontface = "bold")
#--------------

#--------------
anova(fit1PLjoint, fit2PLjoint, test = "LRT")
## Analysis of Deviance Table
## 
## Model 1: rating ~ -1 + item + zscore
## Model 2: rating ~ -1 + item + zscore:item
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)    
## 1     12999      13789
## 2     12980      13632 19   157.04   <2e-16 ***

AIC(fit1PLjoint, fit2PLjoint)
##              df   AIC
## fit1PLjoint  21 13831
## fit2PLjoint  40 13712

BIC(fit1PLjoint, fit2PLjoint)
##              df   BIC
## fit1PLjoint  21 13988
## fit2PLjoint  40 14011
#--------------

#-----------------------------------------------------------------
# 6.8.1  Person-item map
#-----------------------------------------------------------------

#--------------
ggWrightMap(theta = HCI$zscore, b = -coef(fit1PLjoint)[1:20], 
            ylab.theta = "Respondent Z-score")
#--------------

#--------------
data("HeightInventory", package = "ShinyItemAnalysis")
names(HeightInventory)

HeightInventory$Person <- 1:nrow(HeightInventory)

# converting data to the long format
HI.long <- reshape(
  data = HeightInventory,
  varying = list(names(HeightInventory)[1:26]), timevar = "Item", v.names = "Rating",
  idvar = c("Person", "Gender", "HeightCM"),
  direction = "long", new.row.names = 1:(26*nrow(HeightInventory))
)

# adding binarized rating
HI.long$Rating01 <- 1 * (HI.long$Rating > 2) # 3 = agree, 4 = strongly agree 
head(HI.long, n = 2)
##   Person Gender HeightCM Item Rating
## 1      1      M      170    1      1
## 2      2      M      180    1      0

# person ID and item ID as factors:
HI.long$Person <- as.factor(HI.long$Person)
HI.long$Item <- as.factor(HI.long$Item)

summary(HI.long)
#--------------


# Difficulties:
# fit_Rasch_heights <- mirt(df01, model = 1, itemtype = "Rasch")
# coef(fit_Rasch_heights, IRTpars = TRUE, simplify = TRUE)
# b_heights <- coef(fit_Rasch_heights, IRTpars = TRUE, simplify = TRUE)$items[,2]
# 1PL model for all items
fit1PLjointHI <- glm(Rating01 ~ -1 + Item + HeightCM, data = HI.long,
                     family = binomial)
coef(fit1PLjointHI)
#--------------

#--------------
# Heights:
# fs_heights <- mirt::fscores(fit_Rasch_heights)
hist(HeightInventory$HeightCM)
#--------------

#--------------
ggWrightMap(theta = HeightInventory$HeightCM, 
            b = -coef(fit1PLjointHI)[1:26] / coef(fit1PLjointHI)[27],
            ylab.theta = "Height (cm)",
            ylab.b = "Item popularity",
            item.names = names(HeightInventory)[1:26])
#--------------

#--------------
# selection of items to display
item.sel <- which(names(HeightInventory)[1:26] %in% 
                    c("ShortBed", "CarefullHead", "ShortBlanket",
                      "HeightForBasketball", "AskMeToReach", 
                      "TopShelfEasy", "NotFasterWalk", "NotClothChildSize",
                      "NotWishLowerChair", "TallerThanF"))
item.sel
## [1]  3  4  5  8  9 11 13 20 22 24
#--------------

#--------------
ShinyItemAnalysis::ggWrightMap(theta = HeightInventory$HeightCM, 
                               b = -coef(fit1PLjointHI)[item.sel]/coef(fit1PLjointHI)[27],
                               ylab.theta = "Height (cm)",
                               ylab.b = "Item popularity",
                               item.names = names(HeightInventory)[item.sel],
                               binwidth = 5)
#--------------

#-----------------------------------------------------------------
# 6.9 Regression models in interactive application
#-----------------------------------------------------------------

startShinyItemAnalysis()

