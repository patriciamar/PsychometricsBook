#-----------------------------------------------------------------
# Chapter 3 - Internal structure of test and factor analysis
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
# 3.1  Correlation structure
#-----------------------------------------------------------------

#--------------
# code not shown in the book:
data(EPIA, package = "ShinyItemAnalysis")
EPIA_items <- EPIA[, 1:5]
colnames(EPIA_items) <- paste0("i", 1:5)
head(EPIA_items, n = 3)
##   i1 i2 i3 i4 i5
## 1 96 36 80 78 79
## 2 42  2  1  1  1
## 3 64 23 36 13 38

cor(EPIA_items)
##        i1     i2     i3     i4     i5
## i1 1.0000 0.1962 0.1888 0.2326 0.1786
## i2 0.1962 1.0000 0.1453 0.2535 0.2511
## i3 0.1888 0.1453 1.0000 0.1586 0.1282
## i4 0.2326 0.2535 0.1586 1.0000 0.3140
## i5 0.1786 0.2511 0.1282 0.3140 1.0000

# Pearson correlation between Item 1 and Item 2, with CI (not shown in book)
cor.test(EPIA_items$i1, EPIA_items$i2)

# correlation heat map (code not shown in the book)
library(ShinyItemAnalysis)
plot_corr(Data = EPIA_items)
#--------------

#-----------------------------------------------------------------
# Tetrachoric and polychoric correlation
#-----------------------------------------------------------------

#--------------
data(HCI, package = "ShinyItemAnalysis")
#--------------

#--------------
library(psych)
tetrachoric(table(HCI$"Item 1", HCI$"Item 2"))
## [1] 0.2338
##
##  with tau of
##       0       0
## -0.5213 -0.6830
#--------------

#--------------
# Pearson correlation (code not shown in the book)
cor(HCI$"Item 1", HCI$"Item 2")
## [1] 0.1360
#--------------

#--------------
# visualization of the tetrachoric correlation
draw.tetra(r = 0.23, t1 = -0.52, t2 = -0.68)
#--------------

#--------------
# Manual calculation of tau1 and tau2 
# Contingency table
(tab <- table(HCI$"Item 1", HCI$"Item 2"))
##     0   1
## 0  66 130
## 1  95 360
#--------------

#--------------
# Empirical joint probabilities and 
# marginal empirical probabilities P(Item1 = 1) and P(Item2 = 1)
(ptab <- tab / sum(tab)) 
##        0      1
## 0 0.1014 0.1997
## 1 0.1459 0.5530
(pi_1o <- sum(ptab[2, ]))
## [1] 0.6989
(pi_o1 <- sum(ptab[, 2])) 
## [1] 0.7527
#--------------

#--------------
# tau1 and tau2
qnorm(1 - pi_1o)
## [1] -0.5213
qnorm(1 - pi_o1)
## [1] -0.6830
#--------------

#--------------
# Joint empirical probability P(Item1 = 1, Item2 = 1), code not shown in the book
(pi_11 <- ptab[2, 2])
## [1] 0.5530
#--------------

#--------------
# Approximation of tetrachoric correlation
cos(pi / (1 + sqrt(tab[1, 1] * tab[2, 2] / (tab[1, 2] * tab[2, 1]))))
## [1] 0.2519
#--------------

#--------------
# Alternative calculations (code not shown)
cos(pi / (1 + sqrt(ptab[1, 1] * ptab[2, 2] / (ptab[1, 2] * ptab[2, 1]))))
## [1] 0.2519
a <- tab[1, 1]; b <- tab[1, 2]
c <- tab[2, 1]; d <- tab[2, 2]
cos(pi / (1 + sqrt(a * d / (b * c))))
## [1] 0.2519
#--------------

#--------------
data(Anxiety, package = "ShinyItemAnalysis")
polychoric(table(Anxiety$R1, Anxiety$R2))
## $rho
## [1] 0.8334
##
## $objective
## [1] 1.5587
##
## $tau.row
##      1      2      3      4
## 0.4608 1.1583 1.8434 2.6068
##
## $tau.col
##      1      2      3      4
## 0.5236 1.2898 2.0104 3.2148
#--------------

#--------------
# Pearson correlation (code not shown in the book)
cor(Anxiety$R1, Anxiety$R2)
## [1] 0.7813
#--------------

#--------------
# polychoric is generalization of tetrachoric correlation
# the same results are obtained for binary data (code not shown in the book)
polychoric(x = HCI[, 1:20])$rho
tetrachoric(x = HCI[, 1:20])$rho
#--------------

#--------------
(corHCI <- polychoric(x = HCI[, 1:20])$rho)
##        Item 1 Item 2 Item 3 Item 4 ...
## Item 1 1.0000 0.2338 0.2818 0.0707 ...
## Item 2 0.2338 1.0000 0.4800 0.1273 ...
## Item 3 0.2818 0.4800 1.0000 0.0508 ...
## Item 4 0.0707 0.1272 0.0508 1.0000 ...
#--------------

#--------------
# correlation heat map (not shown in the book)
plot_corr(Data = HCI[, 1:20], cor = "polychoric")
#--------------

#-----------------------------------------------------------------
# 3.2  Cluster analysis
#-----------------------------------------------------------------

#--------------
# hierarchical clustering
hc <- hclust(d = as.dist(1 - corHCI), method = "ward.D2")
# dendrogram
library(ggdendro)
ggdendrogram(data = hc)
#--------------

#--------------
plot_corr(Data = HCI[, 1:20], cor = "poly", clust_method = "ward.D2")
#--------------

#-----------------------------------------------------------------
# 3.3. Factor analysis
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 3.3.1 Exploratory factor analysis
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 3.3.1.1 The single factor model
#-----------------------------------------------------------------

#--------------
fa(r = corHCI, nfactors = 1, fm = "ml")
fa(r = corHCI, nfactors = 1, fm = "ml", n.obs = 651)
#--------------

#--------------
FA1 <- fa(r = HCI[, 1:20], cor = "tetrachoric", nfactors = 1, fm = "ml")
#--------------

#--------------
# model summary, code not presented in the book
summary(FA1) 
## Factor analysis with Call: fa(r = HCI[, 1:20], nfactors = 1, fm = "ml", cor = "tetrachoric")
## 
## Test of the hypothesis that 1 factor is sufficient.
## The degrees of freedom for the model is 170  and the objective function was  1.91 
## The number of observations was  651  with Chi Square =  1226.23  with prob <  2.7e-159 
##
## The root mean square of the residuals (RMSA) is  0.07 
## The df corrected root mean square of the residuals is  0.08 
##
## Tucker Lewis Index of factoring reliability =  0.622
## RMSEA index =  0.098  and the 10 % confidence intervals are  0.093 0.103
## BIC =  124.88
#--------------

#--------------
print(FA1$loadings, cutoff = 0)
## Loadings:
##         ML1  
## Item 1  0.444
## Item 2  0.386
## Item 3  0.615
## Item 4  0.199
## ...
##                  ML1
## SS loadings    4.418
## Proportion Var 0.221
#--------------

#--------------
FA1$communality
##  Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8  Item 9 Item 10 
##  0.1973  0.1492  0.3780  0.0395  0.1731  0.2635  0.0232  0.2804  0.0802  0.1755 
## Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 Item 17 Item 18 Item 19 Item 20 
##  0.2268  0.2004  0.3195  0.2904  0.2118  0.3048  0.0020  0.4853  0.3679  0.2489
#--------------

#--------------
# calculation check, code not presented in the book
FA1$loadings[1]^2
## [1] 0.1973 
#--------------

#--------------
FA1$uniquenesses
##  Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8  Item 9 Item 10 
##  0.8027  0.8508  0.6220  0.9605  0.8269  0.7365  0.9768  0.7196  0.9198  0.8245  
## Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 Item 17 Item 18 Item 19 Item 20 
##  0.7732  0.7996  0.6805  0.7096  0.7882  0.6952  0.9980  0.5147  0.6321  0.7511 
#--------------

#--------------
sum(FA1$loadings^2)
## [1] 4.418
#--------------

#--------------
# Reproduced correlation
# loadings(FA1) %*% t(loadings(FA1)) transposed vector cross-product
(HCI.rcor <- tcrossprod(loadings(FA1)) + diag(FA1$uniquenesses))
##         Item 1 Item 2 Item 3  Item 4 Item 5 Item 6  Item 7 Item 8
## Item 1  1.0000 0.1716 0.2731 0.08823 0.1848 0.2280 0.06760 0.2352
## Item 2  0.1716 1.0000 0.2375 0.07674 0.1607 0.1983 0.05880 0.2046
## Item 3  0.2731 0.2375 1.0000 0.12214 0.2558 0.3156 0.09358 0.3256
## ...
#--------------

#--------------
round(FA1$model, 2)
round(tcrossprod(loadings(FA1)), 2)
var(HCI[, 1])
#--------------

#--------------
## Residual matrix
(HCI.resid <- corHCI - HCI.rcor)
#--------------

#--------------
# with factanal() (code not shown in the book)
# (FA1b <- factanal(x = HCI[, 1:20], covmat = corHCI, factors = 1, 
#                   rotation = "none"))
# names(FA1b)
(FA1b <- factanal(covmat = corHCI, factors = 1, rotation = "none"))
FA1b$loadings
FA1b$uniquenesses
#--------------

#-----------------------------------------------------------------
# 3.3.1.2 EFA with more factors
#-----------------------------------------------------------------

#--------------
data(TestAnxietyCor, package = "ShinyItemAnalysis")
# FA unrotated:
(FA2_tAnxiety <- fa(r = TestAnxietyCor, nfactors = 2, n.obs = 335, 
                    rotate = "none"))
## Factor Analysis using method =  minres
## Call: fa(r = TestAnxietyCor, nfactors = 2, n.obs = 335, rotate = "none")
## Standardized loadings (pattern matrix) based upon correlation matrix
##      MR1   MR2   h2   u2 com
## i1  0.62 -0.08 0.39 0.61 1.0
## i2  0.62 -0.17 0.41 0.59 1.1
## i3  0.54  0.24 0.35 0.65 1.4
## i4  0.65  0.09 0.44 0.56 1.0
## i5  0.52  0.49 0.50 0.50 2.0
## ...
plot(FA2_tAnxiety, xlim = c(-.5, 1), ylim = c(-.5, 1))
#--------------

#--------------
# communalities are sum of squared loadings:
# calculation check, code not shown in the book
FA2_tAnxiety$communalities
apply(FA2_tAnxiety$loadings^2, 1, sum)
#--------------

#--------------
plot(FA2_tAnxiety, xlim = c(-.5, 1), ylim = c(-.5, 1))
text(x = 0.95, y = -0.05, expression(hat(alpha)[i1])) # code not shown in the book
text(x = -0.05, y = 0.95, expression(hat(alpha)[i2])) # code not shown in the book
#--------------

#-----------------------------------------------------------------
# 3.3.2 Factor rotation
#-----------------------------------------------------------------
# FA oblimin rotation
FA2_tAnxiety_obl <- fa(r = TestAnxietyCor, nfactors = 2, n.obs = 335, 
                       rotate = "oblimin")
print(FA2_tAnxiety_obl$loadings, cutoff = 0.4)
## Loadings:
##        MR1    MR2   
## i1   0.565       
## i2   0.662       
## i3          0.473
## i4   0.400     
## i5          0.795
## ...
plot(FA2_tAnxiety_obl, xlim = c(-.5, 1), ylim = c(-.5, 1))
text(x = 0.95, y = -0.05, expression(hat(alpha)[i1r])) # code not shown in the book
text(x = -0.05, y = 0.95, expression(hat(alpha)[i2r])) # code not shown in the book
#--------------

#--------------
# rotation matrix, code not shown in the book:
FA2_tAnxiety_obl$rot.mat
##         [,1]   [,2]
## [1,]  0.7725 0.3022
## [2,] -1.0996 1.3094  
FA2_tAnxiety$loadings %*% FA2_tAnxiety_obl$rot.mat
#--------------

#--------------
# code not shown in the book
FA2_tAnxiety_obl$rot.mat
solve(FA2_tAnxiety_obl$rot.mat)

# original plot with rotated oblique axes
plot(FA2_tAnxiety, xlim = c(-.5, 1), ylim = c(-.5, 1))
text(x = 0.95, y = -0.05, expression(hat(alpha)[i1]))
text(x = -0.05, y = 0.95, expression(hat(alpha)[i2]))
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[1, 1]), c(0, solve(FA2_tAnxiety_obl$rot.mat)[1, 2]), lty = 3)
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[2, 1]), c(0, solve(FA2_tAnxiety_obl$rot.mat)[2, 2]), lty = 3)
text(x = 0.75, y = 0.6, labels = expression(hat(alpha)[i2r]))
text(x = 0.9, y = -0.25, labels = expression(hat(alpha)[i1r]))
#--------------

#--------------
# with factanal() and GPArotation()
library(GPArotation)
?rotations
(FA2b_tAnxiety <- factanal(covmat = TestAnxietyCor, factors = 2, 
                           rotation = "none", n.obs = 335))
(FA2b_tAnxiety_obl <- factanal(covmat = TestAnxietyCor, factors = 2, 
                               rotation = "oblimin", n.obs = 335))
update(FA2b_tAnxiety, rotation = "oblimin")
#--------------

#-----------------------------------------------------------------
# 3.3.3 Factor scores
#-----------------------------------------------------------------

#--------------
# code not shown in the book:
# inverse matrix calculation
solve(corHCI)
# product of inverse and original matrix gives identity matrix as expected
round(solve(corHCI) %*% corHCI, 2) 
#------

#------
# factor score coefficients (weights)
(fscore.coef <- solve(corHCI) %*% FA1$loadings)
##            ML1
## Item 1  0.0754
## Item 2  0.0619
## Item 3  0.1347
## Item 4  0.0282
## ...
# factor scores
FSa <- scale(HCI[, 1:20]) %*% fscore.coef
head(FSa, n = 3)
##         ML1
## [1,] 0.8109
## [2,] 1.1817
## [3,] 0.8838
#--------------

#--------------
(FS <- psych::factor.scores(x = HCI[, 1:20], f = FA1, rho = corHCI, 
                            method = "Thurstone"))
## $scores
##          ML1
## [1,]  0.8109
## [2,]  1.1817
## [3,]  0.8838
## [4,]  1.2243
## ...
## $weights
##            ML1
## Item 1  0.0754
## Item 2  0.0619
## Item 3  0.1347
## Item 4  0.0282
## ...
#--------------

#--------------
# code not shown in the book:
(FA1 <- fa(r = HCI[, 1:20], cor = "tet", nfactors = 1, 
           fm = "ml", scores = "Thurstone"))
FA1$scores[1:4]
# ## [1] 1.203982 1.806541 1.331732 1.875942
# seems to give different results, somewhat different setting
plot(FS$scores ~ FA1$scores)
cor(FS$scores, FA1$scores) # but highly correlated
##        ML1
## ML1 0.9988

hist(FS$scores)
plot(FS$scores ~ rowSums(HCI[, 1:20]))
cor(FS$scores, rowSums(HCI[, 1:20]))
mean(FS$scores)
sd(FS$scores)
#--------------

#-----------------------------------------------------------------
# 3.3.4 The number of factors
#-----------------------------------------------------------------

#--------------
# eigen values of the original cor. matrix
eigen(TestAnxietyCor)$values
##  [1] 8.7790 1.3495 0.9710 0.8880 0.7744 0.7416 0.7062 ...
scree(TestAnxietyCor) # code not shown in the book
#--------------

#--------------
# code not shown in the book:
# eigen values on the common factor solution
FA1_tAnxiety <- fa(TestAnxietyCor, nfactors = 1, n.obs = 335)
FA1_tAnxiety$e.values
## [1] 8.7790 1.3495 0.9710 0.8880 0.7744 0.7416 0.7062 
FA1_tAnxiety$values
eigen(tcrossprod(loadings(FA1_tAnxiety)))$values
eigen(FA1_tAnxiety$model)$values
#--------------

#--------------
fa_parallel(Data = TestAnxietyCor, n_obs = 335, method = "pca")
## The input was recognized as a correlation matrix.
## Assuming 335 observations in the original data.
## According to the parallel analysis, the optimal number of principal components is 1. 
## Following the Kaiser rule, 2 components are recommended.
#--------------

#--------------
# not shown in the book
VSS(TestAnxietyCor, n.obs = 335)
## Very Simple Structure
## Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
##           n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
## VSS complexity 1 achieves a maximimum of 0.9  with  1  factors
## VSS complexity 2 achieves a maximimum of 0.92  with  2  factors
## 
## The Velicer MAP achieves a minimum of 0.01  with  2  factors 
## BIC achieves a minimum of  -590  with  2  factors
## Sample Size adjusted BIC achieves a minimum of  -132.5  with  4  factors
#--------------

#-----------------------------------------------------------------
# 3.3.5 Confirmatory factor analysis
#-----------------------------------------------------------------

#--------------
data(BFI2, package = "ShinyItemAnalysis")
head(BFI2, n = 2)
##   i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 i19 i20 ...
## 1  5  5  2  3  5  4  4  3  4   5   5   4   3   4   2   3   5   3   2   5 ...
## 2  4  5  4  3  3  3  5  3  2   4   4   2   4   2   3   4   5   4   3   4 ...
summary(BFI2) # code not shown in the book
#--------------

#--------------
model_EN <- "E =~ i1 + i6 + i11 + i16 + i21 + i26 + 
                  i31 + i36 + i41 + i46 + i51 + i56
             N =~ i4 + i9 + i14 + i19 + i24 + i29 + 
                  i34 + i39 + i44 + i49 + i54 + i59"
#--------------

#--------------
library(lavaan)
fit_EN <- cfa(model_EN, data = BFI2)
#--------------

#--------------
parTable(fit_EN)
##    id lhs op rhs user block group free ustart exo label plabel start ...
## 1   1   E =~  i1    1     1     1    0      1   0         .p1. 1.000 ...
## 2   2   E =~  i6    1     1     1    1     NA   0         .p2. 0.813 ...
## ...
## 51 51   E ~~   N    0     1     1   49     NA   0        .p51. 0.000 ...
summary(fit_EN)
summary(fit_EN, fit.measures = TRUE, standardized = TRUE)
#--------------

#--------------
parameterEstimates(fit_EN)
parameterEstimates(fit_EN, ci = FALSE, standardized = TRUE)
##    lhs op rhs    est    se       z pvalue std.lv std.all std.nox
## 1    E =~  i1  1.000 0.000      NA     NA  0.699   0.665   0.665
## 2    E =~  i6  0.969 0.041  23.611      0  0.678   0.644   0.644
## ... 
## 48 i59 ~~ i59  0.801 0.030  27.106      0  0.801   0.590   0.590
## 49   E ~~   E  0.489 0.033  14.870      0  1.000   1.000   1.000
## 50   N ~~   N  0.587 0.039  15.191      0  1.000   1.000   1.000
## 51   E ~~   N -0.196 0.017 -11.514      0 -0.365  -0.365  -0.365
#--------------

#--------------
model_ENs <- "E =~ NA * i1 + i6 + i11 + i16 + i21 + i26 + 
                   i31 + i36 + i41 + i46 + i51 + i56
              N =~ NA * i4 + i9 + i14 + i19 + i24 + i29 + 
                   i34 + i39 + i44 + i49 + i54 + i59
              E ~~ 1 * E
              N ~~ 1 * N"
fit_ENs <- cfa(model = model_ENs, data = BFI2)
parameterEstimates(fit_ENs, ci = FALSE, standardized = TRUE)
##    lhs op rhs    est    se       z pvalue std.lv std.all std.nox
## 1    E =~  i1  0.699 0.024  29.740      0  0.699   0.665   0.665
## 2    E =~  i6  0.678 0.024  28.552      0  0.678   0.644   0.644
## ...
#--------------

#--------------
# code not shown in the book:
inspect(fit_EN)
## $lambda
##      E  N
## i1   0  0
## i6   1  0
## i11  2  0
## ...
## $theta
##     i1 i6 i11 i16 i21 i26 i31 i36 i41 i46 i51 i56 i4 i9 i14 i19 i24 i29 i34 i39 i44 i49 i54 i59
## i1  23                                                                                         
## i6   0 24                                                                                      
## i11  0  0 25
## ...
## $psi
##    E  N 
## E 47   
## N 49 48
lavInspect(fit_EN, what = "est")$theta
lavInspect(fit_EN, what = "est")$lambda
lavInspect(fit_EN, what = "std")$lambda
lavInspect(fit_EN, what = "est")$psi
lavInspect(fit_EN, what = "std")$psi
#--------------

#--------------
psych::lavaan.diagram(fit_ENs)
semPlot::semPaths(fit_EN, what = "stdest", rotation = 4)
semPlot::semPaths(fit_ENs, what = "est", rotation = 4) # code not shown in the book
#--------------

#--------------
FS <- predict(fit_EN)
head(FS, n = 3)
##            E       N
## [1,]  0.5944  0.2344
## [2,]  0.6298 -0.6944
## [3,] -1.4920  1.6955
#--------------

#-----------------------------------------------------------------
# 3.3.6 Hierarchical and more complex structures
#-----------------------------------------------------------------

#--------------
model_EN_hier <- "Escb =~ i1 + i16 + i31 + i46
                  Easr =~ i6 + i21 + i36 + i51
                  Eenl =~ i11 + i26 + i41 + i56
                  Nanx =~ i4 + i19 + i34 + i49
                  Ndep =~ i9 + i24 + i39 + i54
                  Nemt =~ i14 + i29 + i44 + i59
                  E =~ Escb + Easr + Eenl
                  N =~ Nanx + Ndep + Nemt"
fit_EN_hier <- cfa(model = model_EN_hier, data = BFI2)
#--------------

#--------------
# code not shown in the book:
summary(fit_EN_hier, fit.measures = TRUE, standardized = TRUE)
parTable(fit_EN_hier)
parameterEstimates(fit_EN_hier)
#--------------

#--------------
semPlot::semPaths(fit_EN_hier, what = "std.est", rotation = 4)
#--------------

#--------------
FSh <- predict(fit_EN_hier)
head(FSh, n = 3)
##         Escb    Easr    Eenl    Nanx    Ndep    Nemt       E       N
## [1,]  0.4401  0.6603  0.5546 -0.1261 -0.1289  0.8747  0.5131  0.0041
## [2,]  0.7308  0.4984  0.3823 -0.5643 -0.7703 -0.5638  0.6025 -0.6832
## [3,] -1.5153 -1.4035 -0.5333  1.7516  1.3825  1.4776 -1.2671  1.6671
#--------------

#--------------
fitMeasures(fit_EN, c("cfi", "tli", "rmsea", "bic"))
##   cfi        tli      rmsea        bic 
## 0.778      0.756      0.092 114975.452 
fitMeasures(fit_EN_hier, c("cfi", "tli", "rmsea", "bic"))
##   cfi        tli      rmsea        bic 
## 0.880      0.865      0.069 113303.631 
#--------------

#-----------------------------------------------------------------
# 3.5 Internal structure and FA in interactive application
#-----------------------------------------------------------------

ShinyItemAnalysis::run_app()

