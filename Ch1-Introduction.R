#-----------------------------------------------------------------
# Chapter 1 - Introduction
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 1.4.1 Obtaining and running R and RStudio
#-----------------------------------------------------------------

#--------------
# add 1 and 2
1 + 2
## [1] 3

# square root of 25
sqrt(25)
## [1] 5
#--------------

#--------------
v <-  c(1:10)  # define v as a vector of numbers 1 to 10
v              # print v 
## [1] 1 2 3 4 5 6 7 8 9 10
#--------------

#--------------
(v <-  c(1:10))  # define v as a vector of numbers 1 to 10 and print
## [1] 1 2 3 4 5 6 7 8 9 10
#--------------

#--------------
v * 3
## [1] 3 6 9 12 15 18  21 24 27 30
#--------------

#--------------
help(sqrt)
?install.packages
#--------------

#-----------------------------------------------------------------
# 1.4.2 R packages
#-----------------------------------------------------------------
#
# Note: go to file InstallPackages.R to obtain full code for installation of 
#       all packages needed in the book.  
#       For this reason, this code is commented below.
#--------------
# install.packages("remotes")
# install.packages("ShinyItemAnalysis", dependencies = TRUE)
#--------------

#--------------
# remotes::install_github("patriciamar/ShinyItemAnalysis")
#--------------

#--------------
library(ShinyItemAnalysis)
#--------------

#-----------------------------------------------------------------
# 1.4.3 Data handling
#-----------------------------------------------------------------

#--------------
# loading data
data(HCI)
data(HCI, package = "ShinyItemAnalysis")
?HCI
#--------------

#--------------
# data dimension (number of rows, columns)
dim(HCI)
## [1] 651  22
nrow(HCI)
## [1] 651
ncol(HCI)
## [1] 22
#--------------

#--------------
# variable names
names(HCI)
colnames(HCI)
##  [1] "Item 1"  "Item 2"  "Item 3"  "Item 4"  "Item 5"  "Item 6"
##  [7] "Item 7"  "Item 8"  "Item 9"  "Item 10" "Item 11" "Item 12"
## [13] "Item 13" "Item 14" "Item 15" "Item 16" "Item 17" "Item 18"
## [19] "Item 19" "Item 20" "gender"  "major"
#--------------

#--------------
# view data head
head(HCI, n = 3)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## 1      1      1      1      1      1      0      0      1      1
## 2      1      1      1      1      1      1      0      1      1
## 3      1      1      1      1      0      1      0      1      1
## ...
#--------------

#--------------
# view data structure of an R object
str(HCI)
## 'data.frame':	651 obs. of  22 variables:
##  $ Item 1 : num  1 1 1 1 1 1 1 0 1 1 ...
##  $ Item 2 : num  1 1 1 1 1 1 1 1 1 1 ...
##  ...
##  $ id     : int  1 2 3 4 5 6 7 8 9 10 ...
#--------------

#--------------
HCI$gender
##  [1] 0 0 1 1 1 0 0 0 1 0 1 1 0 0 1 0 0 0 0 0 0 0 0 1 0 1 0 1 0 0 0
## [32] 0 1 0 0 1 0 1 0 0 0 0 1 0 0 1 1 1 0 1 1 1 0 0 1 0 0 0 0 1 0 1
## ...
HCI[, "gender"]
HCI[, 21]

HCI$"Item 1"
##  [1] 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 0 0 1 0 1 0 1 1 0 1 1 1 1 0 1 0
## [32] 0 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1

HCI[1, 1]
##  [1] 1
#--------------

#--------------
# add new variable: person ID
HCI$id <- as.factor(1:nrow(HCI))
str(HCI$id)
## Factor w/ 651 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
#--------------

#--------------
# reshape data to the long format
HCI_long <- reshape(
  data = HCI,
  varying = list(paste("Item", 1:20)), timevar = "item", 
  v.names = "rating", idvar = c("id"),
  direction = "long", new.row.names = 1:13020
)

head(HCI_long, n = 3)
##   gender major id item rating
## 1      0     1  1    1      1
## 2      0     1  2    1      1
## 3      1     1  3    1      1
#--------------

#--------------
# reshape back to wide format
HCI_wide <- reshape(
  data = HCI_long, v.names = "rating", timevar = "item", 
  idvar = c("id"), direction = "wide"
)

head(HCI_wide, n = 3)
##   gender major id rating.1 rating.2 rating.3 ... 
## 1      0     1  1        1        1        1 ...
## 2      0     1  2        1        1        1 ...
## 3      1     1  3        1        1        1 ...
## ...
#--------------

#--------------
# tidyverse approach
library(tidyverse)
data(HCI, package = "ShinyItemAnalysis")
HCI <- mutate(HCI, id = row_number())    # add variable with row number
head(HCI)
#--------------

#--------------
HCI_long_tidy <- pivot_longer(
  data = HCI,
  cols = starts_with("Item"),
  names_to = "item", values_to = "rating"
)
head(HCI_long_tidy, n = 3)
## # A tibble: 3 x 6
##   gender major    id  item   rating
##    <int> <int> <int>  <chr>   <dbl>
## 1      0     1     1  Item 1      1
## 2      0     1     1  Item 2      1
## 3      0     1     1  Item 3      1
#--------------

#--------------
# tidyverse approach: all in once
HCI_long_tidy <- HCI %>%         # take HCI dataset, then
  mutate(id = row_number()) %>%  # add variable with row number, then
  pivot_longer(starts_with("Item"),   # pivot to long form
               names_to = "item", values_to = "rating")
head(HCI_long_tidy, n = 3)
#--------------

#--------------
# tidyverse pivot back to wide
HCI_long_tidy %>% pivot_wider(names_from = item, values_from = rating)
 # A tibble: 651 x 23
##   gender major    id `Item 1` `Item 2` `Item 3` `Item 4` ...
##    <int> <int> <int>    <dbl>    <dbl>    <dbl>    <dbl> ...
## 1      0     1     1        1        1        1        1 ...
## 2      0     1     2        1        1        1        1 ...
## 3      1     1     3        1        1        1        1 ...
##  ...
#--------------

#-----------------------------------------------------------------
# 1.4.4 Graphics
#-----------------------------------------------------------------

#--------------
data(HCIdata, package = "ShinyItemAnalysis")
#--------------

#--------------
# histogram with base R
hist(HCIdata$total)
hist(HCIdata$total, breaks = seq(3, 20, 1), col = "gold",
     main = "", xlab = "Total score", ylab = "Number of respondents")
#--------------

#--------------
# histogram with ggplot
library(ggplot2)
qplot(total, data = HCIdata)
ggplot(data = HCIdata, aes(total)) +
  geom_histogram(binwidth = 1, fill = "gold", col = "black")
#--------------

#--------------
# define theme
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
#--------------

#--------------
ggplot(data = HCIdata, aes(total)) +
  geom_histogram(binwidth = 1, fill = "gold", col = "black") +
  xlab("Total score") + ylab("Number of respondents") + 
  scale_y_continuous(breaks = seq(0, 70, 10)) + 
  theme_fig()
#--------------

#--------------
# histograms by gender with lattice
library(lattice)
histogram(~ total | gender, data = HCIdata, type = "count", 
          col = "gold", breaks = seq(3, 20, 1), xlab = "Total score")
#--------------

#-----------------------------------------------------------------
# 1.4.5 Interactive psychometrics with shiny
#-----------------------------------------------------------------

#--------------
# simple shiny app
library(shiny)
# Define global variables
n <- 100

# Define the UI
ui <- bootstrapPage(
  numericInput('n', 'Number of obs', n),
  plotOutput('plot')
)

# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    hist(rnorm(input$n))
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
#--------------

#--------------
ShinyItemAnalysis::run_app()
#--------------



#-----------------------------------------------------------------
# 1.5 Exploring measurement data
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 1.5.1 Data types
#-----------------------------------------------------------------

#--------------
class(HCIdata)
## [1] "data.frame"
class(HCIdata$gender)
## [1] "factor"
summary(HCIdata$gender)
##    M    F none 
##  246  405   18 
#--------------

#--------------
class(HCI$gender)
## [1] "integer"
summary(HCI$gender)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.0000  0.0000  0.0000  0.3779  1.0000  1.0000 
#--------------

#--------------
summary(factor(HCI$gender, labels = c("F", "M")))
##   F   M 
## 405 246 
#--------------

#--------------
class(HCIdata$yearc5)
## [1] "integer"
summary(HCIdata$yearc5)
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 1.000   2.000   3.000   3.151   4.000   5.000
#--------------

#--------------
summary(as.factor(HCIdata$yearc5))
##  1   2   3   4   5 
## 67 137 171 216  78 

min(as.factor(HCIdata$yearc5))
## Error in Summary.factor(c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, ...
##  ‘min’ not meaningful for factors

min(as.ordered(HCIdata$yearc5))
## [1] 1
## Levels: 1 < 2 < 3 < 4 < 5

mean(as.ordered(HCIdata$yearc5))
## Warning message:
## In mean.default(as.ordered(HCIdata$yearc5)) :
##  argument is not numeric or logical: returning NA
#--------------

#-----------------------------------------------------------------
# 1.5.2 Item scores
#-----------------------------------------------------------------
#-----------------------------------------------------------------
# Nominal items
#-----------------------------------------------------------------

#--------------
# loading data
data(HCItest, package = "ShinyItemAnalysis")

# view data head
head(HCItest, n = 3)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## 1      D      B      A      D      B      B      B      C      D
## 2      D      B      A      D      B      C      B      C      D
## 3      D      B      A      D      C      C      B      C      D
## ...
#--------------

#--------------
table(HCItest$"Item 1")
##  A   B   C   D 
## 27  59 110 455

proportions(table(HCItest$"Item 1"))
prop.table(table(HCItest$"Item 1"))
##      A      B      C      D 
## 0.0415 0.0906 0.1690 0.6989 
#--------------

#--------------
data("HCIkey", package = "ShinyItemAnalysis")
unlist(HCIkey)
##  key1  key2  key3  key4  key5  key6  key7  key8  key9 key10 key11
##     D     B     A     D     B     C     C     C     D     A     A
## key12 key13 key14 key15 key16 key17 key18 key19 key20
##     D     A     A     C     A     C     C     C     D
## Levels: A B C D
#--------------

#--------------
HCIscored <- as.data.frame(mirt::key2binary(HCItest[, 1:20], HCIkey))
head(HCIscored, n = 3)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## 1      1      1      1      1      1      0      0      1      1
## 2      1      1      1      1      1      1      0      1      1
## 3      1      1      1      1      0      1      0      1      1
## ...
#--------------

#--------------
head(HCI, n = 3)
##   Item 1 Item 2 Item 3 Item 4 Item 5 Item 6 Item 7 Item 8 Item 9
## 1      1      1      1      1      1      0      0      1      1
## 2      1      1      1      1      1      1      0      1      1
## 3      1      1      1      1      0      1      0      1      1
## ...
#--------------

#-----------------------------------------------------------------
# Binary items
#-----------------------------------------------------------------

#--------------
table(HCI$"Item 1")
##   0   1 
## 196 455 
#--------------

#--------------
proportions(table(HCI$"Item 1"))
prop.table(table(HCI$"Item 1"))
##      0      1
## 0.3011 0.6989
#--------------

#--------------
summary(HCI)
##      Item1           Item2            Item3            Item4
## Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000
## 1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:0.000
## Median :1.0000   Median :1.0000   Median :1.0000   Median :0.000
## Mean   :0.6989   Mean   :0.7527   Mean   :0.8479   Mean   :0.404
## 3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.000
## Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.000
## ...
#--------------

#-----------------------------------------------------------------
# Ordinal items
#-----------------------------------------------------------------

#--------------
data("BFI2", package = "ShinyItemAnalysis")

head(BFI2, n = 3)
##   i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 
## 1  5  5  2  3  5  4  4  3  4   5   5   4   3   4   2   3   5   3 
## 2  4  5  4  3  3  3  5  3  2   4   4   2   4   2   3   4   5   4 
## 3  1  4  3  5  2  1  3  1  5   4   4   1   5   5   2   1   5   5   
##  ...
#--------------

#--------------
table(BFI2$i1)
##  1   2   3   4   5
## 50 264 321 758 340
#--------------

#--------------
BFI2binary <- 1 * as.data.frame(BFI2[, 1:60] >= 3)
head(BFI2binary, n = 3)
##   i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18
## 1  1  1  0  1  1  1  1  1  1   1   1   1   1   1   0   1   1   1
## 2  1  1  1  1  1  1  1  1  0   1   1   0   1   0   1   1   1   1
## 3  0  1  1  1  0  0  1  0  1   1   1   0   1   1   0   0   1   1 
## ...
#--------------

#-----------------------------------------------------------------
# Continuous items
#-----------------------------------------------------------------

#--------------
data(EPIA, package = "EstCRM")

head(EPIA, n = 2)
##   Item 1 Item 2 Item 3 Item 4 Item 5
## 1     96     36     80     78     79
## 2     42      2      1      1      1
#--------------

#--------------
summary(EPIA$"Item 1")
##   Min.   1st Qu.  Median     Mean  3rd Qu.     Max. 
## 1.0000  40.0000  63.0000  62.8296  89.0000 111.0000
#--------------

#-----------------------------------------------------------------
# 1.5.3 Test scores
#-----------------------------------------------------------------

#--------------
HCI$score <- rowSums(HCI[, 1:20])
HCI$score <- apply(X = HCI[, 1:20], MARGIN = 1, FUN = sum)
#--------------

#--------------
# summary of total score
summary(HCI$score)
##   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
## 3.0000  10.0000  12.0000  12.2120  15.0000  20.0000
#--------------

#--------------
c(Min = min(HCI$score), Max = max(HCI$score),
  Mean = mean(HCI$score), Med = median(HCI$score),
  Var = var(HCI$score), SD = sd(HCI$score),
  Skew = moments::skewness(HCI$score),
  Kurt = moments::kurtosis(HCI$score))
##     Min     Max    Mean     Med     Var      SD    Skew    Kurt
##  3.0000 20.0000 12.2120 12.0000 13.2473  3.6397 -0.1982  2.3474
#--------------

#--------------
psych::describe(HCI$score, type = 1)
##    vars   n  mean   sd median trimmed  mad min max range
## X1    1 651 12.21 3.64     12   12.32 4.45   3  20    17
##    skew kurtosis   se
## X1 -0.2    -0.65 0.14
#--------------

#--------------
# obtaining the same value for kurtosis, not shown in the book
moments::kurtosis(HCI$score) - 3
## [1] -0.6526
#--------------

#-----------------------------------------------------------------
# Standardizations and other transformations
#-----------------------------------------------------------------

#--------------
zscore <- scale(HCI$score) # Z-score
tscore <- 10 * zscore + 50 # T-score
success_rate <- 100 * (HCI$score / max(HCI$score)) # success rate
#--------------

#--------------
plot(ecdf(HCI$score), xlab = "HCI score", ylab = "Percentile")
ecdf(HCI$score)(HCI$score)
## [1] 0.8725 0.9923 0.9401 1.0000 0.9923 1.0000 1.0000 ...
centiles <- round(100 * ecdf(HCI$score)(HCI$score)) # percentiles
#--------------

#--------------
head(data.frame(score = HCI$score, zscore, tscore,
                centiles, success_rate), n = 4)
##   score zscore  tscore centiles success_rate
## 1    16 1.0408 60.4075       87           80
## 2    19 1.8650 68.6500       99           95
## 3    17 1.3155 63.1550       94           85
## 4    20 2.1398 71.3975      100          100
#--------------

#-----------------------------------------------------------------
# 1.5.4 Covariates
#-----------------------------------------------------------------

#--------------
table(HCI$"Item 1", HCI$major)
##    0   1
## 0  97  99
## 1 168 287
#--------------

#--------------
proportions(table(HCI$"Item 1", HCI$major), margin = 2)
prop.table(table(HCI$"Item 1", HCI$major), margin = 2)
##        0      1
## 0 0.3660 0.2565
## 1 0.6340 0.7435
#--------------

#--------------
table(HCItest$gender, HCItest$"Item 1")
##     A   B   C   D
## 0  16  34  65 290
## 1  11  25  45 165
proportions(table(HCItest$gender, HCItest$"Item 1"), margin = 1)
prop.table(table(HCItest$gender, HCItest$"Item 1"), margin = 1)
##        A      B      C      D
## 0 0.0395 0.0840 0.1605 0.7160
## 1 0.0447 0.1016 0.1829 0.6707

by(HCI$"Item 1", HCI$gender, FUN = summary)
## HCI$gender: 0
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.000   0.000   1.000   0.716   1.000   1.000
## ----------------------------------------------------
## HCI$gender: 1
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## 0.0000  0.0000  1.0000  0.6707  1.0000  1.0000
#--------------

#-----------------------------------------------------------------
# 1.6 Modelling measurement data
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# 1.6.1 Discrete random variables
#-----------------------------------------------------------------

#--------------
# Bernoulli distribution
dbinom(x = 1, size = 1, prob = 0.3)
## [1] 0.3
dbinom(x = 0, size = 1, prob = 0.3)
## [1] 0.7
#--------------

#--------------
# simulating a Bernoulli trial
set.seed(42)
(item1 <- rbinom(n = 100, size = 1, prob = 0.3))
##   [1] 0 0 1 0 0 1 0 1 0 0 1 0 0 1 1 0 0 1 1 1 0 1 0 0 1 1 1
##  [28] 0 1 0 0 0 1 0 1 0 1 1 0 0 1 1 1 0 1 0 0 0 0 0 1 1 1 0
##  [55] 1 0 0 1 1 1 0 0 0 1 0 1 1 0 0 1 1 1 1 1 1 0 1 1 1 1 1
##  [82] 1 1 0 0 1 1 1 1 1 0 1 1 0 0 0 1 1 0 0

# sample mean and variance
mean(item1)
## [1] 0.3400
var(item1)
## [1] 0.2267
#--------------

#--------------
# probability of gaining total score of exact value
dbinom(5, size = 10, prob = 0.5)
## [1] 0.2461
dbinom(10, size = 10, prob = 0.5)
## [1] 0.0010
#--------------

#--------------
# probability of gaining total score at least of given value or in range
pbinom(5, size = 10, prob = 0.5)
## [1] 0.6230
pbinom(10, size = 10, prob = 0.5)
## [1] 1.0000
pbinom(8, size = 10, prob = 0.5) - pbinom(2, size = 10, prob = 0.5)
## [1] 0.9346
#--------------

#--------------
# simulating from a binomial distribution
score <- rbinom(n = 100, size = 20, prob = 0.7)

mean(score)
# [1] 13.9600
20 * 0.7
# [1] 14.0000

sd(score)
## [1] 1.9065
sqrt(20 * 0.7 * (1 - 0.7))
## [1] 2.0494
#--------------

#-----------------------------------------------------------------
# 1.6.2 Continuous random variables
#-----------------------------------------------------------------

#--------------
dnorm(x = 5, mean = 10 * 0.5, sd = sqrt(10 * 0.5 * 0.5))
## [1] 0.2523
dnorm(x = 10, mean = 10 * 0.5, sd = sqrt(10 * 0.5 * 0.5))
## [1] 0.0017

pnorm(q = 5, mean = 10 * 0.5, sd = sqrt(10 * 0.5 * 0.5))
## [1] 0.5000
pnorm(q = 10, mean = 10 * 0.5, sd = sqrt(10 * 0.5 * 0.5))
## [1] 0.9992
#--------------

#--------------
qnorm(0.975)
## [1] 1.9600
qnorm(0.025)
## [1] -1.9600
#--------------

#-------------
# Histogram with an estimated normal density
ggplot(data = HCI, aes(score)) +
  geom_histogram(aes(y = ..density..), binwidth = 1,
                 col = "black", fill = "gold") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(HCI$score),
                            sd = sd(HCI$score)), size = 0.8) +
  xlab("Total score") + ylab("Density") +
  theme_fig()
#--------------

#--------------
# QQ plot in base 
qqnorm(HCI$score)
qqline(HCI$score)
#--------------

#--------------
# QQ plot in ggplot 
ggplot(HCI, aes(sample = score)) +
  stat_qq(size = 2,
          shape = 1) +
  stat_qq_line() + 
  theme_fig() +
  ylab("Sample Quantiles") + 
  xlab("Theoretical Quantiles") 
#--------------
