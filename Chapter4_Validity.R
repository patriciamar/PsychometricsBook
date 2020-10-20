#-----------------------------------------------------------------
# Chapter 4 - Validity
# Introduction to psychometric methods
# in education, psychology, and health.
# With examples in R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(corrplot)
library(ggdendro)
library(ggplot2)
library(psych)
library(lme4)
library(lmerTest)
library(ShinyItemAnalysis)

theme_side_by_side <- theme_app(base_size = 19)

#-----------------------------------------------------------------
# Selected R code
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 4.3.1  Ratios
#-----------------------------------------------------------------

#-------------------
n_e <- c(34, 20, 34, 26, 27, 17, 22)
N <- 37
# cvr
(n_e - N / 2) / (N / 2)
## [1]  0.8378  0.0811  0.8378  0.4054  0.4595 -0.0811  0.1892
#-------------------

#-------------------
prop.test(x = 34, n = 37, alternative = "greater")
##         1-sample proportions test with continuity correction
##
## data:  34 out of 37, null probability 0.5
## X-squared = 24, df = 1, p-value = 4e-07
## alternative hypothesis: true p is not equal to 0.5
## 95 percent confidence interval:
##  0.7965 1.0000
## sample estimates:
##      p
## 0.9189

proptests <- lapply(n_e, prop.test, n = N, alternative = "greater")
# p-values
sapply(proptests, function(x) x$p.value)
## [1] 4.07e-07 3.71e-01 4.07e-07 1.07e-02 4.26e-03 6.29e-01 1.62e-01
# confidence intervals
sapply(proptests, function(x) x$conf.int)
##        [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]
## [1,] 0.7965 0.3948 0.7965 0.5548 0.5829 0.3198 0.4466
## [2,] 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#-------------------

#-----------------------------------------------------------------
# 4.3.1  Correlation coefficients
#-----------------------------------------------------------------

#-------------------
load("datasets/PROM/dataPROM.rda")
head(dataPROM, n = 3)
##     Zrak  MI  MAS   BBS tres DYSDI ATAXIE POSTUR   KZ   nhp
## 2  44.00 313 19.5 51.25  4.0  6.25    3.0   22.0 5.50 20.12
## 6  28.00 307 16.0 46.25  6.5  6.25    4.0   21.0 4.50 27.52
## 10 35.33 274 18.0 38.75 11.0  6.75    5.0   35.0 5.00 28.12
## ...

ggplot(dataPROM, aes(x = EDSS)) +
  geom_histogram(bins = 7, alpha = 0.5, col = "black") +
  theme_app()

ggplot(dataPROM, aes(x = MI)) +
  geom_histogram(bins = 9, alpha = 0.5, col = "black") +
  theme_app()

ggplot(dataPROM, aes(x = MI, y = EDSS)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_app()
# ggsave("figures/validity_scatterplot_dataPROM.png",
#       width = 6, height = 4, dpi = 300, bg = "transparent")
#-------------------

#-------------------
cor(dataPROM$MI, dataPROM$EDSS)
## [1] -0.4622
cor.test(dataPROM$MI, dataPROM$EDSS)
##         Pearson's product-moment correlation
##
## data:  dataPROM$MI and dataPROM$EDSS
## t = -2, df = 15, p-value = 0.06
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
## -0.7714  0.0238
## sample estimates:
##     cor
## -0.4622
#-------------------

#-------------------
data(HCI, package = "ShinyItemAnalysis")
score <- rowSums(HCI[, 1:20])

cor.test(score, HCI$major)
##         Pearson's product-moment correlation
##
## data:  score and HCI$major
## t = 5.7, df = 649, p-value = 2e-08
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##   0.1432 0.2897
## sample estimates:
##   cor
## 0.2177
#-------------------

#-------------------
ggplot(
  data.frame(
    score = score,
    major = as.factor(HCI$major)
  ),
  aes(x = major, y = score)
) +
  geom_point() +
  ylab("Total score") +
  xlab("Plan to major in science") +
  theme_app()
# ggsave("figures/validity_scatterplot_HCI.png",
#       width = 6, height = 4, dpi = 300, bg = "transparent")
#-------------------

#-------------------
ggplot(dataPROM, aes(x = rank(MI), y = rank(EDSS))) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_app()
# ggsave("figures/validity_scatterplot_ranks_dataPROM.png",
#       width = 6, height = 4, dpi = 300, bg = "transparent")
#-------------------

#-------------------
cor(dataPROM$MI, dataPROM$EDSS, method = "spearman")
## [1] -0.5996
cor.test(dataPROM$MI, dataPROM$EDSS, method = "spearman")
##         Spearman's rank correlation rho
##
## data:  dataPROM$MI and dataPROM$EDSS
## S = 1305, p-value = 0.01
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##    rho
## -0.5996
#-------------------

#-------------------
cor(dataPROM$MI, dataPROM$EDSS, method = "kendall")
## [1] -0.4275
cor.test(dataPROM$MI, dataPROM$EDSS, method = "kendall")
##         Kendall's rank correlation tau
##
## data:  dataPROM$MI and dataPROM$EDSS
## z = -2.3, p-value = 0.02
## alternative hypothesis: true tau is not equal to 0
## sample estimates:
##     tau
## -0.4275
#-------------------

#-----------------------------------------------------------------
# 4.3.2  Student's t-tests
#-----------------------------------------------------------------

#-------------------
load("datasets/HCI/dataPrePost.RData")
ggplot(data.frame(
  score = c(dataPrePost$score.pre, dataPrePost$score.post),
  group = factor(rep(c("Pre", "Post"), each = 16),
                 levels = c("Pre", "Post"))),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() + ylab("Total score") + xlab("") + theme_app()
# ggsave("figures/validity_boxplot_HCI_prepost.png",
#        width = 6, height = 4, dpi = 300, bg = "transparent")
#-------------------

#-------------------
ggplot(data.frame(
  score = dataPrePost$score.post - dataPrePost$score.pre,
  group = factor(rep(c("Post-Pre"), each = 16), levels = "Post-Pre")),
  aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  ylab("Posttest - pretest score") + xlab("") + theme_app()
# ggsave("figures/validity_boxplot_HCI_prepostDif.png",
#        width = 6, height = 4, dpi = 300, bg = "transparent")
#-------------------

#-------------------
# differences between post and pretest
(dif <- dataPrePost$score.post - dataPrePost$score.pre)
## [1]  7  2  0  2  4  1  0  5  3  1  5 -2  4  8  1 -4
# mean difference
(M <- mean(dif, na.rm = TRUE))
## [1] 2.3125
# standard deviation of difference
(s <- sd(dif, na.rm = TRUE))
## [1] 3.1563
# number of observations
n <- length(dif)
# t-value
(t <- M / (s / sqrt(n)))
## [1] 2.9306
# p-value
2 * pt(-abs(t), df = n - 1)
## [1] 0.0103
# confidence interval
M + qt(0.05 / 2, df = n - 1) * s / sqrt(n)
## [1] 0.6306
M + qt(1 - 0.05 / 2, df = n - 1) * s / sqrt(n)
## [1] 3.9944
#-------------------


#-------------------
# one sample t-test
t.test(dif, mu = 0)
##         One Sample t-test
##
## data:  dif
## t = 2.9306, df = 15, p-value = 0.0103
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##   0.6306 3.9944
## sample estimates:
##   mean of x
## 2.3125
#-------------------

#-------------------
# paired t-test
t.test(dataPrePost$score.post, dataPrePost$score.pre, paired = TRUE)
##         Paired t-test
##
## data:  dataPrePost$score.post and dataPrePost$score.pre
## t = 2.9306, df = 15, p-value = 0.0103
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   0.6306 3.9944
## sample estimates:
##   mean of the differences
## 2.3125
#-------------------

#-------------------
load("datasets/HCI/dataGrads.RData")
score_grads <- rowSums(dataGrads[, paste0("QR", 1:20)])
score_undergrads <- rowSums(HCI[, 1:20])

# two sample t-test
t.test(score_grads, score_undergrads, alternative = "greater")
##         Welch Two Sample t-test
##
## data:  score_grads and score_undergrads
## t = 2.2, df = 9.3, p-value = 0.03
## alternative hypothesis: true difference in means is greater than 0
## 95 percent confidence interval:
##  0.38  Inf
## sample estimates:
## mean of x mean of y
## 14.50     12.21
#-------------------

#-------------------
df <- data.frame(
  score = c(score_grads, score_undergrads),
  group = as.factor(c(rep("Graduate", length(score_grads)),
                      rep("Undergraduate", length(score_undergrads))))
)

ggplot(df, aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.25) +
  xlab("") + ylab("Total score") +
  theme_app()
# ggsave("figures/validity_boxplot_HCI_undergraduateVSgraduate.png",
#        width = 6, height = 4, dpi = 300, bg = "transparent")
#-------------------

#-----------------------------------------------------------------
# 4.3.3 ANOVA
#-----------------------------------------------------------------

#-------------------
load("datasets/HCI/dataHCI.RData")

# density plot
ggplot(dataHCI, aes(total, fill = typeSCH)) +
  geom_density(aes(y = ..density..,
                   color = typeSCH,
                   linetype = typeSCH),
               position = "identity",
               alpha = 0.5,
               size = 1) +
  xlab("Total score on HCI") +
  ylab("Density") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.16)) +
  theme_app() +
  theme(legend.position = "right")
# ggsave("figures/validity_densityplot_HCI.png",
#        width = 6, height = 4, dpi = 300, bg = "transparent")
#-------------------

#-------------------
anovaHCI <- aov(total ~ typeSCH, data = dataHCI)
summary(anovaHCI)
##              Df Sum Sq Mean Sq F value  Pr(>F)
## typeSCH       3    603   201.0    16.1 4.2e-10 ***
## Residuals   665   8306    12.5
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(anovaHCI)
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
##
## Fit: aov(formula = total ~ typeSCH, data = dataHCI)
##
## $typeSCH
##            diff     lwr     upr  p adj
## PUI-CC  -0.6563 -1.7003  0.3877 0.3685
## R1-CC    1.1537  0.2457  2.0617 0.0062
## REG-CC  -1.4024 -2.3982 -0.4065 0.0018
## R1-PUI   1.8100  0.7699  2.8500 0.0001
## REG-PUI -0.7461 -1.8637  0.3715 0.3143
## REG-R1  -2.5561 -3.5478 -1.5643 0.0000
#-------------------

#-----------------------------------------------------------------
# 4.3.4  Linear regression model
#-----------------------------------------------------------------

#-------------------
lmF <- lm(total ~ gender + major + as.factor(yearc5) +
            minority + EnglishF + typeSCH, data = dataHCI)
anova(lmF)
summary(lmF)
## Call:
## lm(formula = total ~ gender + major + as.factor(yearc5) + minority +
##    EnglishF + typeSCH, data = dataHCI)
##
## Residuals:
##    Min     1Q Median     3Q    Max
## -8.871 -2.020  0.108  2.131  7.362
##
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)
##   (Intercept)          10.902      0.532   20.48  < 2e-16 ***
##   genderF              -0.809      0.260   -3.11  0.00193 **
##   gendernone           -2.228      0.831   -2.68  0.00754 **
##   major                 1.287      0.266    4.84  1.6e-06 ***
##   as.factor(yearc5)2    2.042      0.510    4.00  7.0e-05 ***
##   as.factor(yearc5)3    1.191      0.495    2.41  0.01641 *
##   as.factor(yearc5)4    1.794      0.522    3.44  0.00063 ***
##   as.factor(yearc5)5    3.703      0.589    6.28  6.0e-10 ***
##   minoritymin          -1.593      0.326   -4.89  1.3e-06 ***
##   minoritynone         -2.300      0.628   -3.66  0.00027 ***
##   EnglishFno           -1.416      0.315   -4.50  8.1e-06 ***
##   typeSCHPUI            0.257      0.406    0.63  0.52583
##   typeSCHR1             0.735      0.368    2.00  0.04619 *
##   typeSCHREG           -1.303      0.420   -3.10  0.00203 **
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Residual standard error: 3.14 on 655 degrees of freedom
## Multiple R-squared:  0.274,	Adjusted R-squared:  0.26
## F-statistic:   19 on 13 and 655 DF,  p-value: <2e-16
#-------------------


#-----------------------------------------------------------------
# 4.4.1  Internal correlation structure
#-----------------------------------------------------------------

#-------------------
load("datasets/HCI/dataHCI.RData")
# polychoric correlation calculation
corP <- polychoric(dataHCI[, paste0("QR", 1:20)])
# correlation matrix
round(corP$rho, 2)
# correlation plot
corrplot(corP$rho)
#-------------------

#-----------------------------------------------------------------
# 4.4.2  Cluster analysis
#-----------------------------------------------------------------

#-------------------
hc <- hclust(as.dist(1 - corP$rho), method = "ward.D") # hierarchical clustering
ggdendrogram(hc) # dendrogram

# correlation plot with 3 clusters using Ward method
corrplot(corP$rho,
  order = "hclust", hclust.method = "ward.D",
  addrect = 3
)
#-------------------

#-----------------------------------------------------------------
# 4.4.3.  Factor analysis and dimensionality
#-----------------------------------------------------------------

#-------------------
# Exploratory factor analysis (EFA) with psych
# Optimal number of factors
VSS(dataHCI[, paste0("QR", 1:20)])
# BIC supports unidimensionality of the measure (1-factor solution)
# RMSEA 0.04 acceptable for 1-factor model

(FA1 <- fa(dataHCI[, paste0("QR", 1:20)], nfactors = 1))
(FA2 <- fa(dataHCI[, paste0("QR", 1:20)], nfactors = 2))
(FA3 <- fa(dataHCI[, paste0("QR", 1:20)], nfactors = 3))

fa.diagram(FA1)
fa.diagram(FA2)
fa.diagram(FA3)

# Higher order factor solution
(om.h <- omega(dataHCI[, paste0("QR", 1:20)], sl = FALSE))

# Scree plot (but on continuous data!)
pca <- princomp(dataHCI[, paste0("QR", 1:20)], cor = TRUE)
plot(pca$sdev^2, type = "b", pch = 16, xlab = "Component number", ylab = "Eigenvalue")

round(
  cbind(
    "Variance" = pca$sdev^2, "%" = 100 * pca$sdev^2 / sum(pca$sdev^2),
    "Cumulative %" = 100 * cumsum(pca$sdev^2) / sum(pca$sdev^2)
  ),
  d = 2
)
#-------------------
