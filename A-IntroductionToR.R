#-----------------------------------------------------------------
# Appendix A - Introduction to R
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

library(Cairo)

par(mgp = c(2.1, 0.7, 0), mar = c(3.4, 3.4, 1.3, 0.9), cex.axis = 1.2,
    cex.lab = 1.2, ann = FALSE, lwd = 0.6)

#-----------------------------------------------------------------
# A.2 Starting with R
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
# A.3 Installation of R packages
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
# A.4 Data handling
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
## [1] 651  23
nrow(HCI)
## [1] 651
ncol(HCI)
## [1] 23
#--------------

#--------------
# variable names
names(HCI)
colnames(HCI)
##  [1] "Item 1"  "Item 2"  "Item 3"  "Item 4"  "Item 5"  "Item 6"
##  [7] "Item 7"  "Item 8"  "Item 9"  "Item 10" "Item 11" "Item 12"
## [13] "Item 13" "Item 14" "Item 15" "Item 16" "Item 17" "Item 18"
## [19] "Item 19" "Item 20" "gender"  "major"   "total" 
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
##  $ total  : num  16 19 17 20 19 20 20 14 18 17 ...
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

#-----------------------------------------------------------------
# A.4.1 Types of measurement data
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
## Error in Summary.factor(c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, ...
##  ‘min’ not meaningful for factors

min(as.ordered(HCIdata$yearc5))
## [1] 1
## Levels: 1 < 2 < 3 < 4 < 5

mean(as.ordered(HCIdata$yearc5))
## [1] NA
## Warning message:
## In mean.default(as.ordered(HCIdata$yearc5)) :
##  argument is not numeric or logical: returning NA
#--------------

#-----------------------------------------------------------------
# A.4.2 Wide and long data format
#-----------------------------------------------------------------

#--------------
data(HCIlong, package = "ShinyItemAnalysis")
head(HCIlong, n = 2)
##    id   item rating gender major total
##  1  1 Item 1      1      0     1    16
##  2  1 Item 2      1      0     1    16
#--------------

#--------------
# reshape data to the long format
HCI_long <- reshape(data = HCI,
  varying = list(paste("Item", 1:20)), timevar = "item", v.names = "rating", 
  idvar = "id", direction = "long", new.row.names = 1:13020)
head(HCI_long, n = 2)
##   gender major total id item rating
## 1      0     1    16  1    1      1
## 2      0     1    19  2    1      1
#--------------

#--------------
# reshape back to wide format
HCI_wide <- reshape(data = HCI_long, 
  v.names = "rating", timevar = "item", idvar = "id", direction = "wide")
head(HCI_wide, n = 2)
##   gender major id rating.1 rating.2 rating.3 ... 
## 1      0     1  1        1        1        1 ...
## 2      0     1  2        1        1        1 ...
#--------------

#-----------------------------------------------------------------
# A.4.3 Data handling with tidyverse
#-----------------------------------------------------------------

#--------------
# tidyverse approach
library(tidyverse)
HCI <- mutate(HCI, id = row_number())    # add variable with row number
head(HCI)
#--------------

#--------------
HCI_long_tidy <- pivot_longer(data = HCI, cols = starts_with("Item"), 
                              names_to = "item", values_to = "rating")
head(HCI_long_tidy, n = 2)
## # A tibble: 3 x 6
##   gender major total    id item   rating
##    <int> <int> <dbl> <int> <chr>   <dbl>
## 1      0     1    16     1 Item 1      1
## 2      0     1    16     1 Item 2      1
#--------------

#--------------
# tidyverse approach: all in once
HCI_long_tidy <- HCI %>%              # take HCI dataset, then
  mutate(id = row_number()) %>%       # add variable with row number, then
  pivot_longer(starts_with("Item"),   # pivot to long form
               names_to = "item", values_to = "rating")
#--------------

#--------------
# code not shown in the book
head(HCI_long_tidy, n = 2)
## # A tibble: 2 × 6
##   gender major total    id item   rating
##    <int> <int> <dbl> <int> <chr>   <dbl>
## 1      0     1    16     1 Item 1      1
## 2      0     1    16     1 Item 2      1
#--------------

#--------------
# tidyverse pivot back to wide
HCI_long_tidy %>% pivot_wider(names_from = item, values_from = rating)
## # A tibble: 651 x 24
##   gender major    id `Item 1` `Item 2` `Item 3` `Item 4` ...
##    <int> <int> <int>    <dbl>    <dbl>    <dbl>    <dbl> ...
## 1      0     1     1        1        1        1        1 ...
## 2      0     1     2        1        1        1        1 ...
#--------------

#-----------------------------------------------------------------
# A.1.4 Graphics
#-----------------------------------------------------------------

#--------------
data(HCIdata, package = "ShinyItemAnalysis")
#--------------

#--------------
# histogram with base R
hist(HCIdata$total)
hist(HCIdata$total, breaks = 3:20, col = "gold", main = "", 
     xlab = "Total score", ylab = "Number of respondents")
#--------------

#-------------- save plot
# CairoPNG(file = "figures/chapter1/data_histogram_HCI_base.png", width = 6, height = 4, dpi = 300, pointsize = 12, unit = "in")
# par(mgp = c(2.1, 0.7, 0), mar = c(3.4, 3.4, 1.3, 0.9), cex.axis = 1.2, cex.lab = 1.2, lwd = 0.6)
# hist(HCIdata$total, breaks = 3:20, col = "gold",
#     main = "", xlab = "Total score", ylab = "Number of respondents")
# dev.off()
#--------------

#--------------
# histogram with ggplot
library(ggplot2)
qplot(total, data = HCIdata)
g <- ggplot(data = HCIdata, aes(x = total)) +
  geom_histogram(binwidth = 1, fill = "gold", col = "black")
#--------------

#--------------
# define theme
theme_fig <- function(base_size = 17, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_blank(),
      legend.key = element_rect(fill = "white", colour = NA),
      legend.background = element_blank()
    )
}
#--------------

#--------------
g + xlab("Total score") + ylab("Number of respondents") + 
  scale_y_continuous(breaks = seq(0, 70, 10)) + theme_fig()
#--------------

#-------------- save plot
# ggsave("figures/chapter1/data_histogram_HCI_ggplot.png",
#       width = 6, height = 4, dpi = 300, bg = "transparent")
#--------------

#--------------
# histograms by gender with lattice
library(lattice)
histogram(~ total | gender, data = HCIdata, type = "count", col = "gold", 
          breaks = 3:20, xlab = "Total score", ylab = "Number of respondents")
#--------------

#-------------- save plot
# CairoPNG(file = "figures/chapter1/data_histogram_HCI_lattice.png", width = 6, height = 3.5, dpi = 300, pointsize = 12, unit = "in")
# par(mgp = c(2.1, 0.7, 0), mar = c(3.4, 3.4, 1.3, 0.9), cex.axis = 1.2, cex.lab = 1.2, lwd = 0.6)
# lattice::histogram(~ total | gender, data = HCIdata,
#                   type = "count", col = "gold", breaks = 3:20, xlab = "Total score", 
#                   ylab = "Number of respondents")
# dev.off()
#--------------

#-----------------------------------------------------------------
# A.1.5 Interactive psychometrics with shiny
#-----------------------------------------------------------------

#--------------
# simple shiny app
library(shiny)
# Define global variables
n <- 100

# Define the UI
ui <- bootstrapPage(numericInput('n', 'Number of obs', n),
                    plotOutput('plot'))

# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({hist(rnorm(input$n))})
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
#--------------
