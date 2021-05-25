#-----------------------------------------------------------------
# Chapter 9 - Further Topics
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
# 9.1. Computerized adaptive tests
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 9.1.3  Item bank and automatic item generation,
#-----------------------------------------------------------------

#--------------
# simple algebra - multiplication tables 1 to 10
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
# creating data frame with item wording
df <- data.frame(Question = Question, Answer = Answer, Type = Type,
                 stringsAsFactors = FALSE)
head(df, n = 2)
##     Question Answer Type
##  1 1 * 1 = ?      1 text
##  2 1 * 2 = ?      2 text
#--------------

#--------------
# Creating mirt object for item bank
pars <- data.frame(a1 = 1, d = -Diff)
model <- generate.mirt_object(pars, itemtype = "2PL")
coef(model, simplify = TRUE, IRTpars = TRUE)
plot(model, type = "trace", facet_items = FALSE)
plot(model, type = "infotrace", facet_items = FALSE)
#--------------

#-----------------------------------------------------------------
# 9.1.7  CAT implementation
#-----------------------------------------------------------------

#--------------
# Running simple CAT
results <- mirtCAT(df, mo = model, method = "MAP", criteria = "MI",
                   design = list(max_items = 10, min_SEM = 0.6))
#--------------

# Note: to match with the results presented, only the 1st, 6th and 8th (last)
#        items are answered correctly

#--------------
# Print and plot results
print(results)
##  n.items.answered  Theta_1  SE.Theta_1
##                 8  -0.9387      0.5971

summary(results)
##  $final_estimates
##  Theta_1
##  Estimates -0.9387
##  SEs        0.5971
##
##  $raw_responses
##  [1] "1"  " "  " "  " "  " "  "28" " "  "26"
##
##  $scored_responses
##  [1] 1 0 0 0 0 1 0 1
##
##  $items_answered
##  [1]   1 315  53 265  47  34  84 242
##
##  $thetas_history
##  Theta_1
##  [1,]  0.00000000
## ...
##  [8,] -1.11769885
##  [9,] -0.93869989
##
##  $thetas_SE_history
##  Theta_1
##  [1,] 1.0000000
##  ..
##  [8,] 0.6285105
##  [9,] 0.5971391
##
##  $terminated_sucessfully
##  [1] TRUE
##
##  $item_time
##  [1] 3.20 2.63 1.64 1.62 1.38 4.01 1.87 3.73
#--------------

#--------------
plot(results)
#--------------

#--------------
# Further setting the CAT
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
#--------------

#--------------
plot(results)
#--------------

#-----------------------------------------------------------------
# 9.1.6.1 Post-hoc analysis
#-----------------------------------------------------------------

#--------------
data("dataMedical", package = "ShinyItemAnalysis")
data <- dataMedical[, 1:100]
#--------------

#--------------
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
# CAT for classification (passed/fail)
i <- 1 # index of a test-taker
posthocSim1 <- mirtCAT(mo = fit2pl, local_pattern = data[i, ],
                       start_item = "MI", method = "MAP", criteria = "MI",
                       design = list(classify = -0.1, classify_CI = 0.90))

print(posthocSim1)
## n.items.answered Theta_1 SE.Theta_1
##               20  0.4210     0.2947
summary(posthocSim1)
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
plot(posthocSim1)
#---------------

#---------------
# CAT using SE as a stopping criterion
i <- 1 # index of a test-taker
posthocSim2 <- mirtCAT(
  mo = fit2pl, local_pattern = data[i, ],
  start_item = "MI", method = "MAP", criteria = "MI",
  design = list(min_SEM = 0.30)
)

print(posthocSim2)
## n.items.answered Theta_1 SE.Theta_1
##               19  0.3609     0.2976

plot(posthocSim2, facet_items = FALSE, auto.key = FALSE, main = FALSE)
#--------------- 