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
# 1.4.2. R packages
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

# Note: In supplementary code for each chapter, we provide code for uploading
#       packages at the top. For this reason, this code is commented below.

#--------------
library(ShinyItemAnalysis)
#--------------

#-----------------------------------------------------------------
# 1.4.3. Data handling
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
HCI$person <- as.factor(1:nrow(HCI))
str(HCI$person)
## Factor w/ 651 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
#--------------

#--------------
# Reshape data to the long format
HCI_long <- reshape(
  data = HCI,
  varying = list(paste("Item", 1:20)), timevar = "item", 
  v.names = "rating", idvar = c("person"),
  direction = "long", new.row.names = 1:13020
)

head(HCI_long, n = 3)
##   gender major person item rating
## 1      0     1      1    1      1
## 2      0     1      2    1      1
## 3      1     1      3    1      1
#--------------

#--------------
# Reshape back to wide format
HCI_wide <- reshape(
  data = HCI_long, v.names = "rating", timevar = "item", 
  idvar = c("person"), direction = "wide"
)

head(HCI_wide, n = 3)
##   gender major person rating.1 rating.2 rating.3 ... 
## 1      0     1      1        1        1        1 ...
## 2      0     1      2        1        1        1 ...
## 3      1     1      3        1        1        1 ...
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
# 1.4.4. Graphics
#-----------------------------------------------------------------

#--------------
HCI$score <- rowSums(HCI[, 1:20])
#--------------

#--------------
# histogram
hist(HCI$score)
hist(HCI$score, breaks = seq(3, 20, 1), col = "gold",
     main = "", xlab = "Total score", ylab = "Number of respondents")
#--------------

#--------------
qplot(score, data = HCI)
ggplot(data = HCI, aes(score)) +
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
ggplot(data = HCI, aes(score)) +
  geom_histogram(binwidth = 1, fill = "gold", col = "black") +
  xlab("Total score") + ylab("Number of respondents") + 
  scale_y_continuous(breaks = seq(0, 70, 10)) + 
  theme_fig()
#--------------

#-----------------------------------------------------------------
# 1.4.5. Interactive psychometrics with shiny
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
