#-----------------------------------------------------------------
# Chapter 7 - Factor analytic approach and multidimensional IRT
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(Cairo)
library(ggplot2)
library(GPArotation)
library(lavaan)
library(mirt)
library(psych)
library(semPlot)
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
# 7.1 Exploratory factor analysis
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 7.2.1 Single factor model
#-----------------------------------------------------------------

#--------------
# Single factor model
# with fa() of the psych package

#--------------
corHCI <- tetrachoric(HCI[,1:20])$rho
(FA1 <- psych::fa(corHCI, nfactors = 1, fm = "ml"))
(FA1 <- psych::fa(corHCI, nfactors = 1, fm = "ml", n.obs = 651))
#--------------

#--------------
(FA1 <- psych::fa(HCI[, 1:20], cor = "tetrachoric", nfactors = 1, 
                  fm = "ml"))
summary(FA1)
print(FA1$loadings, cutoff = 0)
## Loadings:
##         ML1  
## Item 1  0.444
## Item 2  0.386
## Item 3  0.615
## Item 4  0.199
## Item 5  0.416
## Item 6  0.513
## Item 7  0.152
## Item 8  0.530
## Item 9  0.283
## Item 10 0.419
## Item 11 0.476
## Item 12 0.448
## Item 13 0.565
## Item 14 0.539
## Item 15 0.460
## Item 16 0.552
## Item 17 0.045
## Item 18 0.697
## Item 19 0.607
## Item 20 0.499
## 
##                  ML1
## SS loadings    4.418
## Proportion Var 0.221
#--------------

#--------------
round(FA1$communality, 3)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8 
##  0.197   0.149   0.378   0.039   0.173   0.263   0.023   0.280 
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 
##  0.080   0.175   0.227   0.200   0.319   0.290   0.212   0.305 
## Item 17 Item 18 Item 19 Item 20 
##   0.002   0.485   0.368   0.249 
#--------------

#--------------
FA1$loadings[1]^2
## [1] 0.1973 
#--------------

#--------------
round(FA1$uniquenesses, 3)
## Item 1  Item 2  Item 3  Item 4  Item 5  Item 6  Item 7  Item 8 
##  0.803   0.851   0.622   0.961   0.827   0.737   0.977   0.720 
## Item 9 Item 10 Item 11 Item 12 Item 13 Item 14 Item 15 Item 16 
##  0.920   0.825   0.773   0.800   0.681   0.710   0.788   0.695 
## Item 17 Item 18 Item 19 Item 20 
##   0.998   0.515   0.632   0.751 
#--------------

#--------------
sum(FA1$loadings^2)
## [1] 4.418
#--------------

#--------------
## Reproduced correlation
# loadings(FA1) %*% t(loadings(FA1)) transposed vector cross-product
HCI.rcor <- tcrossprod(loadings(FA1)) + diag(FA1$uniquenesses)
round(HCI.rcor, d = 2)
##         Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 ...
## Item 1    1.00   0.17   0.27   0.09   0.18   0.23 ...
## Item 2    0.17   1.00   0.24   0.08   0.16   0.20 ...
## Item 3    0.27   0.24   1.00   0.12   0.26   0.32 ...
## ...
#--------------

#--------------
round(FA1$model, 2)
round(tcrossprod(loadings(FA1)), 2)
var(HCI[,1])
#--------------

#--------------
## Residual matrix
HCI.resid <- corHCI - HCI.rcor
round(HCI.resid, d = 2)
#--------------

#--------------
# with factanal()
(FA1b <- factanal(x = HCI[, 1:20], covmat = corHCI, factors = 1, 
                  rotation = "none"))
names(FA1b)
#--------------

#-----------------------------------------------------------------
# 7.2.2 General factor model
#-----------------------------------------------------------------

#--------------
data(TestAnxietyCor, package = "ShinyItemAnalysis")
# FA unrotated:
(FA2_tAnxiety <- psych::fa(TestAnxietyCor, nfactors = 2, 
                           n.obs = 335, rotate = "none"))
## Factor Analysis using method =  minres
## Call: psych::fa(r = TestAnxietyCor, nfactors = 2, n.obs = 335, rotate = "none")
## Standardized loadings (pattern matrix) based upon correlation matrix
##      MR1   MR2   h2   u2 com
## i1  0.62 -0.08 0.39 0.61 1.0
## i2  0.62 -0.17 0.41 0.59 1.1
## i3  0.54  0.24 0.35 0.65 1.4
## i4  0.65  0.09 0.44 0.56 1.0
## i5  0.52  0.49 0.50 0.50 2.0
## ...
#--------------

#--------------
# communalities are sum of squared loadings:
FA2_tAnxiety$communalities
apply(FA2_tAnxiety$loadings^2, 1, sum)
#--------------

#--------------
plot(FA2_tAnxiety, xlim = c(-.5, 1), ylim = c(-.5, 1))

# label unrotated axes
text(x = 0.95, y = -0.05, expression(paste(hat(alpha), "i1")))
text(x = -0.05, y = 0.95, expression(paste(hat(alpha), "i2")))
#--------------

#-----------------------------------------------------------------
# 7.2.3 Factor rotation
#-----------------------------------------------------------------
# FA oblimin rotation
FA2_tAnxiety_obl <- psych::fa(TestAnxietyCor, nfactors = 2, 
                              n.obs = 335, rotate = "oblimin")
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
#--------------

#--------------
# rotation matrix:
FA2_tAnxiety_obl$rot.mat
##         [,1]   [,2]
## [1,]  0.7725 0.3022_obl
## [2,] -1.0996 1.3094  
FA2_tAnxiety$loadings %*% FA2_tAnxiety_obl$rot.mat
#--------------

#--------------
FA2_tAnxiety_obl$rot.mat
solve(FA2_tAnxiety_obl$rot.mat)

# rotated oblique axes
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[1,1]), c(0,solve(FA2_tAnxiety_obl$rot.mat)[1,2]), lty = 3)
lines(c(0, solve(FA2_tAnxiety_obl$rot.mat)[2,1]), c(0,solve(FA2_tAnxiety_obl$rot.mat)[2,2]), lty = 3)

# label rotated axes
text(x = 0.75, y = 0.6, labels = expression(paste(hat(alpha), "i2r")))
text(x = 0.9, y = - 0.25, labels = expression(paste(hat(alpha), "i1r")))
#--------------

#--------------
# rotated loadings
# points(loadings(FA2_tAnxiety_obl), pch = 16, col = "grey")
plot(FA2_tAnxiety_obl, xlim = c(-.5,1), ylim = c(-.5, 1))
text(x = 0.95, y = -0.05, expression(paste(hat(alpha), "i1r")))
text(x = -0.07, y = 0.95, expression(paste(hat(alpha), "i2r")))
#--------------

#--------------
# with factanal() and GPArotation()
?GPArotation::rotations
(FA2b_tAnxiety <- factanal(covmat = TestAnxietyCor, factors = 2, 
                           rotation = "none", n.obs = 335))
(FA2b_tAnxiety_obl <- factanal(covmat = TestAnxietyCor, factors = 2, 
                               rotation = "oblimin", n.obs = 335))
update(FA2b_tAnxiety, rotation = "oblimin")
#--------------

#-----------------------------------------------------------------
# 7.2.4 Factor scores
#-----------------------------------------------------------------

#--------------
# inverse matrix calculation
solve(corHCI)
# product of inverse and original matrix gives identity matrix as expected
round(solve(corHCI) %*% corHCI, 2) 
#------

#------
# factor score coefficients (weights)
(fscore.coef <- solve(corHCI) %*% FA1$loadings)
##              ML1
## Item 1  0.075394
## Item 2  0.061870
## Item 3  0.134698
## Item 4  0.028183
## ...
# factor scores
FSa <- scale(HCI[,1:20]) %*% fscore.coef
head(FSa, n = 3)
##         ML1
## [1,] 0.8109
## [2,] 1.1817
## [3,] 0.8838
#--------------

#--------------
(FS <- psych::factor.scores(HCI[,1:20], FA1, 
                            rho = corHCI, method = "Thurstone"))
## $scores
##            ML1
## [1,]  0.810914
## [2,]  1.181697
## [3,]  0.883752
## [4,]  1.224317
## ...
## $weights
##              ML1
## Item 1  0.075394
## Item 2  0.061870
## Item 3  0.134698
## Item 4  0.028183
## ...
#--------------

#--------------
FA1 <- psych::fa(HCI[,1:20], cor = "tet", nfactors = 1, 
                 fm = "ml", scores = "Thurstone")
# ## [1] 1.203982 1.806541 1.331732 1.875942
# seems to give different results?? Somewhat different setting?
FA1$scores[1:4]
plot(FS$scores ~ FA1$scores)
#--------------

#--------------
hist(FS$scores)
plot(FS$scores ~ rowSums(HCI[,1:20]))
cor(FS$scores, rowSums(HCI[,1:20]))
mean(FS$scores)
sd(FS$scores)
#--------------

#-----------------------------------------------------------------
# 7.2.5 Number of factors
#-----------------------------------------------------------------

#--------------
# eigen values of the original cor. matrix
eigen(TestAnxietyCor)$values
##  [1] 8.7790 1.3495 0.9710 0.8880 0.7744 0.7416 0.7062
psych::scree(TestAnxietyCor)
#--------------

#--------------
# eigen values on the common factor solution
FA1_tAnxiety <- psych::fa(TestAnxietyCor, nfactors = 1, n.obs = 335)
FA1_tAnxiety$e.values
## [1] 8.7790 1.3495 0.9710 0.8880 0.7744 0.7416 0.7062 
FA1_tAnxiety$values
eigen(tcrossprod(loadings(FA1_tAnxiety)))$values
eigen(FA1_tAnxiety$model)$values
#--------------
fa.parallel(TestAnxietyCor, n.obs = 335)
## Parallel analysis suggests that the number of factors =  1  
## and the number of components =  1
#--------------

#--------------
fa_parallel(TestAnxietyCor, n_obs = 335, method = "pca")
## The input was recognized as a correlation matrix.
## Assuming 335 observations in the original data.
## According to the parallel analysis, the optimal number of principal components is 1. 
## Following the Kaiser rule, 2 components are recommended.
#--------------

#--------------
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
# 7.3. Confirmatory factor analysis
#-----------------------------------------------------------------

#--------------
data(BFI2, package = "ShinyItemAnalysis")
head(BFI2, n = 2)
summary(BFI2)
#--------------

#--------------
model_EN <- 'E =~ i1 + i6 + i11 + i16 + i21 + i26 + 
                  i31 + i36 + i41 + i46 + i51 + i56
             N =~ i4 + i9 + i14 + i19 + i24 + i29 + 
                  i34 + i39 + i44 + i49 + i54 + i59'
fit_EN <- lavaan::cfa(model_EN, data = BFI2)
#--------------

#--------------
lavaan::parTable(fit_EN)
##    id lhs op rhs user block group free ustart exo label plabel start ...
## 1   1   E =~  i1    1     1     1    0      1   0         .p1. 1.000 ...
## 2   2   E =~  i6    1     1     1    1     NA   0         .p2. 0.813 ...
## ...
## 51 51   E ~~   N    0     1     1   49     NA   0        .p51. 0.000 ...
lavaan::summary(fit_EN)
lavaan::summary(fit_EN, fit.measures = TRUE, standardized = TRUE)
#--------------

#--------------
lavaan::parameterEstimates(fit_EN)
##    lhs op rhs    est    se       z pvalue ci.lower ci.upper
## 1    E =~  i1  1.000 0.000      NA     NA    1.000    1.000
## 2    E =~  i6  0.969 0.041  23.611      0    0.889    1.049
## ...
## 13   N =~  i4  1.000 0.000      NA     NA    1.000    1.000
## 14   N =~  i9  0.833 0.039  21.509      0    0.757    0.909
## ...
## 25  i1 ~~  i1  0.617 0.023  26.337      0    0.571    0.663
## 26  i6 ~~  i6  0.647 0.024  26.664      0    0.599    0.694
## ...
## 49   E ~~   E  0.489 0.033  14.870      0    0.425    0.554
## 50   N ~~   N  0.587 0.039  15.191      0    0.512    0.663
## 51   E ~~   N -0.196 0.017 -11.514      0   -0.229   -0.162

lavaan::parameterEstimates(fit_EN, ci = FALSE, standardized = TRUE)
##    lhs op rhs    est    se       z pvalue std.lv std.all std.nox
## 1    E =~  i1  1.000 0.000      NA     NA  0.699   0.665   0.665
## 2    E =~  i6  0.969 0.041  23.611      0  0.678   0.644   0.644
## 3    E =~ i11  0.470 0.041  11.423      0  0.329   0.296   0.296
## 4    E =~ i16  1.420 0.052  27.066      0  0.993   0.757   0.757
## 5    E =~ i21  1.150 0.047  24.647      0  0.804   0.677   0.677
## 6    E =~ i26  0.510 0.034  14.790      0  0.357   0.387   0.387
## 7    E =~ i31  1.265 0.050  25.257      0  0.885   0.697   0.697
## 8    E =~ i36  0.547 0.039  14.137      0  0.382   0.369   0.369
## 9    E =~ i41  0.813 0.039  20.583      0  0.568   0.552   0.552
## 10   E =~ i46  1.126 0.045  25.099      0  0.787   0.692   0.692
## 11   E =~ i51  1.052 0.045  23.559      0  0.736   0.643   0.643
## 12   E =~ i56  0.669 0.036  18.468      0  0.468   0.491   0.491
## 13   N =~  i4  1.000 0.000      NA     NA  0.766   0.672   0.672
## 14   N =~  i9  0.833 0.039  21.509      0  0.639   0.569   0.569
## 15   N =~ i14  1.069 0.044  24.397      0  0.819   0.653   0.653
## 16   N =~ i19  0.817 0.037  22.095      0  0.626   0.585   0.585
## 17   N =~ i24  0.807 0.039  20.685      0  0.618   0.545   0.545
## 18   N =~ i29  1.171 0.042  27.874      0  0.898   0.761   0.761
## 19   N =~ i34  0.828 0.038  21.852      0  0.634   0.578   0.578
## 20   N =~ i39  1.155 0.043  26.963      0  0.885   0.732   0.732
## 21   N =~ i44  0.730 0.038  19.372      0  0.560   0.508   0.508
## 22   N =~ i49  0.836 0.039  21.611      0  0.641   0.571   0.571
## 23   N =~ i54  1.176 0.044  26.900      0  0.901   0.730   0.730
## 24   N =~ i59  0.974 0.041  23.968      0  0.747   0.641   0.641
#--------------

#--------------
model_ENs <- 'E =~ NA*i1 + i6 + i11 + i16 + i21 + i26 + 
                   i31 + i36 + i41 + i46 + i51 + i56
              N =~ NA*i4 + i9 + i14 + i19 + i24 + i29 + 
                   i34 + i39 + i44 + i49 + i54 + i59
              E ~~ 1*E
              N ~~ 1*N'
fit_ENs <- lavaan::cfa(model_ENs, data = BFI2)
lavaan::parameterEstimates(fit_ENs, ci = FALSE, standardized = TRUE)
##    lhs op rhs    est    se       z pvalue std.lv std.all std.nox
## 1    E =~  i1  0.699 0.024  29.740      0  0.699   0.665   0.665
## 2    E =~  i6  0.678 0.024  28.552      0  0.678   0.644   0.644
## ...
#--------------

#--------------
lavaan::inspect(fit_EN)
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
lavaan::lavInspect(fit_EN, what = "est")$theta
lavaan::lavInspect(fit_EN, what = "est")$lambda
lavaan::lavInspect(fit_EN, what = "std")$lambda
lavaan::lavInspect(fit_EN, what = "est")$psi
lavaan::lavInspect(fit_EN, what = "std")$psi
#--------------

#--------------
psych::lavaan.diagram(fit_ENs)
semPlot::semPaths(fit_EN, what = "stdest", rotation = 4)
semPlot::semPaths(fit_ENs, what = "est", rotation = 4)
#--------------

#--------------
FS <- lavaan::predict(fit_EN)
head(FS, n = 3)
##            E       N
## [1,]  0.5944  0.2344
## [2,]  0.6298 -0.6944
## [3,] -1.4920  1.6955
#--------------

#-----------------------------------------------------------------
# 7.3.1 Hierarchical and more complex structures
#-----------------------------------------------------------------

#--------------
model_EN_hier <- 'Escb =~ i1 + i16 + i31 + i46
                  Easr =~ i6 + i21 + i36 + i51
                  Eenl =~ i11 + i26 + i41 + i56
                  Nanx =~ i4 + i19 + i34 + i49
                  Ndep =~ i9 + i24 + i39 + i54
                  Nemt =~ i14 + i29 + i44 + i59
                  E =~ Escb + Easr + Eenl
                  N =~ Nanx + Ndep + Nemt'
fit_EN_hier <- lavaan::cfa(model_EN_hier, data = BFI2)
#--------------

#--------------
lavaan::summary(fit_EN_hier, fit.measures = TRUE, standardized = TRUE)
lavaan::parTable(fit_EN_hier)
lavaan::parameterEstimates(fit_EN_hier)
semPlot::semPaths(fit_EN_hier, what = "std.est", rotation = 4)
#--------------

#--------------
FSh <- lavaan::predict(fit_EN_hier)
head(FSh, n = 3)
##         Escb    Easr    Eenl    Nanx    Ndep    Nemt       E       N
## [1,]  0.4401  0.6603  0.5546 -0.1261 -0.1289  0.8747  0.5131  0.0041
## [2,]  0.7308  0.4984  0.3823 -0.5643 -0.7703 -0.5638  0.6025 -0.6832
## [3,] -1.5153 -1.4035 -0.5333  1.7516  1.3825  1.4776 -1.2671  1.6671
#--------------

#--------------
lavaan::fitMeasures(fit_EN, c("cfi", "tli", "rmsea", "bic"))
##   cfi        tli      rmsea        bic 
## 0.778      0.756      0.092 114975.452 
lavaan::fitMeasures(fit_EN_hier, c("cfi", "tli", "rmsea", "bic"))
##   cfi        tli      rmsea        bic 
## 0.880      0.865      0.069 113303.631 
#--------------

#-----------------------------------------------------------------
# 7.4 Multidimensional IRT models
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# 7.4.1 Multidimensional 2PL model
#-----------------------------------------------------------------

#--------------
BFI2en <- BFI2[,c(1,6,11,16,21,26,31,36,41,46,51,56,
                  4,9,14,19,24,29,34,39,44,49,54,59)]
BFI2en01 <- 1* (BFI2en > 2)
#--------------

#--------------
m2PL <- mirt::mirt(BFI2en01, model = 2, itemtype = "2PL")
#--------------

#--------------
coef(m2PL, simplify = TRUE)
## $items
##         a1     a2      d g u
## i1  -0.037  1.854  2.312 0 1
## i6  -0.121  1.886  2.325 0 1
## ...
## i4   1.531 -0.556  0.903 0 1
## ...
## i54  2.008 -1.219  0.150 0 1
## i59  2.154  0.000  0.414 0 1
#--------------

#--------------
MDIFF(m2PL)
##       MDIFF_1
## i1  -1.246493
## i6  -1.230540 ...

MDISC(m2PL)
##     i1     i6    i11    i16    i21    i26    i31    i36    i41    i46    i51    i56 
## 1.8548 1.8896 0.5401 2.3346 1.7972 0.9268 2.2508 0.8626 1.4868 2.1646 1.7246 1.2201 
##     i4     i9    i14    i19    i24    i29    i34    i39    i44    i49    i54    i59 
## 1.6293 1.2734 1.9687 1.3930 1.2216 2.5930 1.4385 2.3060 1.5733 1.3502 2.3491 2.1542 
#--------------

#--------------
itemplot(m2PL, item = 1) # item loading strongly on one factor
itemplot(m2PL, item = 13) # item loading strongly on other factor
#itemplot(m2PL, item = 1, rotate = "oblimin")
#--------------

#--------------
# model fit indices
M2(m2PL)
##         M2  df p   RMSEA RMSEA_5 RMSEA_95   SRMSR    TLI   CFI
## stats 1660 229 0 0.06007 0.05736  0.06278 0.04468 0.9168 0.931
#--------------

#--------------
head(fscores(m2PL), n = 3)
##           F1      F2
## [1,]  0.05246  1.3830
## [2,] -0.97199  1.2715
## [3,]  1.19615 -2.1297
#--------------

#-----------------------------------------------------------------
# 7.4.2 Multidimensional Graded Response Model
#-----------------------------------------------------------------

#--------------
mGRM <- mirt::mirt(BFI2en, model = 2, itemtype = "graded")
#--------------

#--------------
coef(mGRM, simplify = TRUE)
## $items
##         a1     a2    d1     d2     d3     d4
## i1  -0.125  1.863 4.925  2.293  0.864 -2.174
## i6  -0.187  1.594 4.611  2.090  0.608 -2.067
## ...
## i4   1.747 -0.581 3.453  0.963 -0.578 -3.228
## ...
## i54  1.938 -1.066 2.539  0.179 -1.261 -3.919
## i59  1.973  0.000 3.153  0.435 -0.992 -3.636
## 
## $means
## F1 F2 
## 0  0 
## 
## $cov
## F1 F2
## F1  1  0
## F2  0  1

#--------------
itemplot(mGRM, item = 1) # item loading strongly on one factor
itemplot(mGRM, item = 13) # item loading strongly on other factor
#--------------

#--------------
head(fscores(mGRM), n = 3)
##           F1     F2
## [1,]  0.4821  1.274
## [2,] -1.1366  1.160
## [3,]  3.4887 -3.497
#--------------

#-----------------------------------------------------------------
# 7.4.3 Confirmatory multidimensional PCM
#-----------------------------------------------------------------

#--------------
model_ENirt <- 'E = 1-12
                N = 13-24
                COV = E*N'
#fit_ENirtRasch <- mirt::mirt(BFI2en, model = model_ENirt, itemtype = "Rasch")
fit_ENirtGRM <- mirt::mirt(BFI2en, model = model_ENirt, itemtype = "graded")
#--------------

#--------------
coef(fit_ENirtGRM, simplify = TRUE)
## $items
##        a1    a2    d1     d2     d3     d4
## i1  1.815 0.000 4.847  2.264  0.855 -2.141
## i6  1.668 0.000 4.685  2.136  0.626 -2.104
## ...
## i4  0.000 1.855 3.468  0.969 -0.579 -3.242
## ...
## i54 0.000 2.164 2.511  0.187 -1.237 -3.857
## i59 0.000 1.679 2.900  0.405 -0.902 -3.341
## 
## $means
## E N 
## 0 0 
## 
## $cov
## E      N
## E  1.000 -0.366
## N -0.366  1.000
#--------------

#--------------
itemplot(fit_ENirtGRM, item = 1) # item loading on 1st factor
itemplot(fit_ENirtGRM, item = 13) # item loading on 2nd factor
#--------------

#--------------
MDIFF(fit_ENirtGRM)
##     MDIFF_1   MDIFF_2  MDIFF_3 MDIFF_4
## i1   -2.670 -1.247133 -0.47093   1.179
## i6   -2.809 -1.280900 -0.37535   1.262

MDISC(fit_ENirtGRM)
##     i1     i6    i11    i16    i21    i26    i31    i36    i41    i46    i51    i56 
## 1.8153 1.6678 0.6313 2.3159 1.8016 0.8033 1.9004 0.7631 1.3134 1.9334 1.6300 1.1060 
##     i4     i9    i14    i19    i24    i29    i34    i39    i44    i49    i54    i59 
## 1.8549 1.3335 1.6935 1.4488 1.2662 2.4583 1.4914 2.1672 1.1882 1.4682 2.1639 1.6792 
#--------------

#--------------
head(fscores(fit_ENirtGRM), n = 3)
##            E       N
## [1,]  0.7727  0.2232
## [2,]  0.7267 -0.7867
## [3,] -2.5039  2.8328
#--------------