#-----------------------------------------------------------------
# Chapter 9 - Computerized Adaptive Testing and Other Topics
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Packages
#-----------------------------------------------------------------

library(mirt)
library(mirtCAT)
library(ShinyItemAnalysis)
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

#-----------------------------------------------------------------
# 9.2  Item banks and automatic item generation
#-----------------------------------------------------------------

#--------------
# simple algebra - multiplication tables 1 to 10 (open answer)
n <- 10
Type <- rep("text", n * n)
Question <- rep("", n * n)
Answer <- rep("", n * n)
#--------------

#--------------
# setting difficulty of items
Diff <- rep(99, n * n)
ipos <- 0
for (i in 1:n) {
  for (j in 1:n) {
    ipos <- ipos + 1
    Question[ipos] <- paste0(i, " * ", j, " = ?")
    Answer[ipos] <- i * j
    
    if (min(i, j) == 1 & Diff[ipos] == 99) {
      Diff[ipos] <- -1.5
    }
    if (max(i, j) == 10 & Diff[ipos] == 99) {
      Diff[ipos] <- -1
    }
    if (min(i, j) == 2 & Diff[ipos] == 99 & max(i, j) != 10) {
      Diff[ipos] <- -0.5
    }
    if (Diff[ipos] == 99) {
      Diff[ipos] <- 0 + (i + j - 9) / 10
    }
    if (i > j) {
      Diff[ipos] <- Diff[ipos] + 0.05
    }
  }
}
#--------------

#--------------
# summary of defined item difficulties (not displayed in book)
summary(Diff)
hist(Diff)
#--------------

#-----------------------------------------------------------------
# 9.3. Computerized adaptive testing
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 9.3.4  CAT implementation in interactive application
#-----------------------------------------------------------------

#--------------
# creating data frame with item wording
df <- data.frame(Question = Question, Answer = Answer, Type = Type,
                 stringsAsFactors = FALSE)
head(df, n = 2)
##     Question Answer Type
##  1 1 * 1 = ?      1 text
##  2 1 * 2 = ?      2 text
#--------------

#--------------
# creating mirt object for item bank
library(mirtCAT)
pars <- data.frame(a1 = 1, d = -Diff)
model <- generate.mirt_object(pars, itemtype = "2PL")
#--------------

#--------------
# summary of model coefficients, display of item functioning (not displayed in book)
coef(model, simplify = TRUE, IRTpars = TRUE)
plot(model, type = "trace", facet_items = FALSE)
plot(model, type = "infotrace", facet_items = FALSE)
#--------------

#--------------
# running simple CAT
results <- mirtCAT(df, mo = model, method = "MAP", criteria = "MI",
                   design = list(max_items = 10, min_SEM = 0.6))
#--------------

# Note: to match with the results presented, only the 1st, 6th and 8th (last)
#        items are answered correctly

#--------------
# Print and plot results
print(results)
##  n.items.answered  Theta_1  SE.Theta_1
##                 8  -0.8352     0.5886
#--------------

#--------------
summary(results)
##  $final_estimates
##  Theta_1
##  Estimates -0.8352
##  SEs        0.5886
##
##  $raw_responses
##  [1] "1"  " "  " "  " "  " "  "20" " "  "30"
##
##  $scored_responses
##  [1] 1 0 0 0 0 1 0 1
##
##  $items_answered
##  [1]   1 54 24 12 92 20 93 30
##
##  $thetas_history
##  Theta_1
##  [1,]  0.0000
## ...
##  [8,] -1.0098
##  [9,] -0.8352
##
##  $thetas_SE_history
##  Theta_1
##  [1,] 1.0000
##  ..
##  [8,] 0.6185
##  [9,] 0.5886
##
##  $terminated_sucessfully
##  [1] TRUE
#--------------

#--------------
plot(results)
#--------------

#--------------
# Further setting the CAT (not displayed in the book)
df <- data.frame(
  Question = Question, Answer = Answer, Type = Type, Timer = rep(15, 100),
  stringsAsFactors = FALSE) # item timer set
design <- list(max_items = 15, min_SEM = 0.6) # stopping criterion combines 2 rules
preCAT <- list(min_items = 3, max_items = 3, criteria = "random") # preCAT set

# Setting the CAT guide
title <- "Multiplication - CAT"
author <- "Author Name"
firstpage <- list(
  h1("Welcome"),
  sprintf("This CAT serves to practice multiplication. ")
)
begin_message <- "You have 15 seconds for each item. Test will be started by pushing button 'Next'. "
lastpage <- function(person) {
  return(list(h5("Multiplication CAT was ended, close the tab to end the application. ")))
}

shinyGUI <- list(
  title = title, authors = author,
  firstpage = firstpage, begin_message = begin_message,
  forced_choice = FALSE, lastpage = lastpage
)

# Starting the CAT application
results <- mirtCAT(df,
                   mo = model, preCAT = preCAT,
                   method = "MAP", criteria = "MI", start_item = "MI", design = design, shinyGUI = shinyGUI
)

# Summary of the results
summary(results)
plot(results)
#--------------

#-----------------------------------------------------------------
# 9.3.4 Post-hoc analysis
#-----------------------------------------------------------------

#--------------
data("dataMedical", package = "ShinyItemAnalysis")
data <- dataMedical[, 1:100]
#--------------

#--------------
library(mirt)
fit2pl <- mirt(data, model = 1, itemtype = "2PL", SE = TRUE)
coef(fit2pl, IRTpars = TRUE, simplify = TRUE)$items
##            a       b g u
## X2001 0.2751  2.8194 0 1
## X2002 0.6036  4.1328 0 1
## X2003 1.0267  0.5342 0 1
## ...
#---------------

#---------------
# IICs and ICCs
plot(fit2pl, type = "trace", facet_items = FALSE)
plot(fit2pl, type = "infotrace", facet_items = FALSE)
#---------------

#---------------
# CAT using SE as a stopping criterion
posthocSim1 <- mirtCAT(
  mo = fit2pl, local_pattern = data[1, ],
  start_item = "MI", method = "MAP", criteria = "MI",
  design = list(min_SEM = 0.30)
)

print(posthocSim1)
## n.items.answered Theta_1 SE.Theta_1
##               19  0.3609     0.2976
summary(posthocSim1)
## $final_estimates
##           Theta_1
## Estimates  0.3609
## SEs        0.2976
## $raw_responses
## [1] "2" "1" "1" "2" "2" "2" "1" "1" "1" "2" "1" "1" "2" "2" "2" "2" "2" "1" "2"
## $scored_responses
## [1] 1 0 0 1 1 1 0 0 0 1 0 0 1 1 1 1 1 0 1
## $items_answered
## [1] 81 72 57 82 73 76 18 78 56 74 90 30 80 53 89 37 70 79 69
## $thetas_history
## Theta_1
##  [1,]  0.0000
##  [2,]  0.5691
## ...
## [20,]  0.3609
##
## $thetas_SE_history
## Theta_1
##  [1,]  1.0000
##  [2,]  0.7292
## ...
## [19,]  0.3012
## [20,]  0.2976
#---------------


#---------------
# CAT ability estimates with SE
plot(posthocSim1)
#---------------

#---------------
# CAT for classification (passed/fail)
posthocSim2 <- mirtCAT(mo = fit2pl, local_pattern = data[1, ],
                       start_item = "MI", method = "MAP", criteria = "MI",
                       design = list(classify = -0.1, classify_CI = 0.90))

print(posthocSim2)
## n.items.answered Theta_1 SE.Theta_1
##               20  0.4210     0.2947
#---------------

#---------------
# more detailed summary (not displayed in the book)
summary(posthocSim2)
## $final_estimates
## Theta_1
## Estimates  0.4210
## SEs        0.2947
## $raw_responses
## [1] "2" "1" "1" "2" "2" "2" "1" "1" "1" "2" "1" "1" "2" "2" "2" "2" "2" "1" "2" "2"
## $scored_responses
## [1] 1 0 0 1 1 1 0 0 0 1 0 0 1 1 1 1 1 0 1 1
## $items_answered
## [1] 81 72 57 82 73 76 18 78 56 74 90 30 80 53 89 37 70 79 69 29
## $thetas_history
## Theta_1
##  [1,]  0.0000
##  [2,]  0.5691
## ...
## [20,]  0.3609
## [21,]  0.4210
##
## $thetas_SE_history
## Theta_1
##  [1,]  1.0000
##  [2,]  0.7292
## ...
## [20,]  0.2976
## [21,]  0.2947
## $classification
## Theta_1
## "above cutoff"
#---------------

#---------------
# calculation of 90% CI for the final ability estimate
posthocSim2$thetas - qnorm(0.95) * posthocSim2$SE_thetas
## -0.06373
posthocSim2$thetas + qnorm(0.95) * posthocSim2$SE_thetas
## 0.9058
#---------------

#---------------
# CAT ability estimates with 90% CI
plot(posthocSim2, SE = qnorm(0.95))
#---------------

#---------------
# more plots (not displayed in the book)
# this would plot CAT ability estimates with SE
plot(posthocSim2)
# this would plot CAT ability estimates with 95% CI
plot(posthocSim2, SE = qnorm(0.975))
#--------------- 

#-----------------------------------------------------------------
# 9.3.6 CAT simulation study with MCMC
#-----------------------------------------------------------------

#--------------
# simulation of 100 person abilities from standard normal distribution
set.seed(123)
thetas <- rnorm(100)
#--------------

#--------------
# Exploration of the generated data (not shown in the book)
thetas
hist(thetas)
#--------------

#--------------
# Simulation of 100 person responses to the test of the same item parameters as those estimated with 2PL model on dataMedical
set.seed(123)
responses <- simdata(model = fit2pl, Theta = as.matrix(thetas))
Zscores <- scale(rowSums(responses))
cor(thetas, Zscores)
## 0.9680
#--------------

#--------------
# scatterplot Z-scores vs. underlying true scores (not displayed in the book)
plot(thetas, Zscores)
abline(0, 1, col = "blue")
#--------------

#--------------
# MCMC simulation
# TODO: select other criteria, MI does not work well in initial steps
MCMCsim <- list()

for (p in 1:length(thetas)) {
  MCMCsim[[p]] <- mirtCAT(
    mo = fit2pl, local_pattern = responses[p, ],
    start_item = "MI", method = "MAP", criteria = "MI",
    design = list(min_SEM = 0.30)
  )
}
#--------------

#--------------
# CAT plot for the first "hypothetical" respondent, whose responses were generated assuming theta = -0.5605
thetas[1]
## -0.5605
print(MCMCsim[[1]])
## n.items.answered Theta_1 SE.Theta_1
##               19 -0.4098     0.2994

#--------------
# more detailed display (not shown in the book)
summary(MCMCsim[[1]])
plot(MCMCsim[[1]])
#--------------

#--------------
# extract thetas, plot them against true scores, etc.  
CAT_thetas <- sapply(MCMCsim, function(x) x$thetas)
cor(thetas, CAT_thetas)
## 0.9491 (only slightly lower than when estimate is done from all items)
cor(Zscores, CAT_thetas)
## 0.9559

CAT_SE <- sapply(MCMCsim, function(x) x$SE_thetas)
summary(CAT_SE)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.2920  0.2959  0.2978  0.2975  0.2986  0.3122
sd(CAT_SE)
## 0.0024

CAT_items <- sapply(MCMCsim, function(x) length(x$items_answered))
summary(CAT_items)
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  18.0    19.0    20.5    25.5    24.0   100.0 
sd(CAT_items)
## [1] 14.7172
#--------------

#--------------
# plot CAT ability estimates against true scores (code not provided in the book)  
library(ggplot2)
ggplot(data.frame(thetas, CAT_thetas), aes(x = thetas, y = CAT_thetas)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0) + theme_fig() + 
  ylab(expression(theta~"based on CAT")) + 
  xlab(expression("True"~theta))
#--------------

#--------------
# plot number of items against true scores (code not provided in the book)  
ggplot(data.frame(thetas, CAT_items), aes(x = thetas, y = CAT_items)) + 
  geom_point() + theme_fig() + ylim(0, 100) + 
  xlab(expression("True"~theta)) + 
  ylab("Number of items in CAT")
#--------------

#--------------
# plot number of items against true scores (code not provided in the book)  
ggplot(data.frame(CAT_thetas, CAT_items), aes(x = thetas, y = CAT_SE)) + 
  geom_point() + geom_abline(slope = 0, intercept = 0.3) + theme_fig() + ylim(0, 0.4) + 
  xlab(expression("True"~theta)) + 
  ylab(expression(paste("SE(", theta, ") in CAT")))
#--------------

#--------------
# base plots (not displayed in the book):
plot(thetas, CAT_thetas)
abline(0, 1, col = "blue")

plot(Zscores, CAT_thetas)
abline(0, 1, col = "blue")

plot(thetas, CAT_items, ylim = c(0, 100))

plot(thetas, CAT_SE, ylim = c(0, 0.4))
abline(0.3, 0, col = "blue")
#--------------
