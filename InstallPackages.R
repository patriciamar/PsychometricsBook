#-----------------------------------------------------------------
# Computational aspects of psychometric methods. With R.
# P. Martinkova & A. Hladka
# 
# Code for installation of packages
#-----------------------------------------------------------------

install.packages("remotes")

install.packages("aod")
install.packages("brms")
install.packages("catR")
install.packages("deltaPlotR")
install.packages("DFIT")
install.packages("difNLR")
install.packages("difR")
install.packages("equateIRT")
install.packages("eRm")
install.packages("ggdendro")
install.packages("ggplot2")
install.packages("GPArotation")
install.packages("gtheory")
install.packages("hemp")
install.packages("lattice")
install.packages("lavaan")
install.packages("lme4")
install.packages("lordif")
install.packages("ltm")
install.packages("mirt")
install.packages("mirtCAT")
install.packages("moments")
install.packages("msm")
install.packages("nlme")
install.packages("nnet")
install.packages("psych")
install.packages("psychometric")
install.packages("semPlot")
install.packages("ShinyItemAnalysis", dependencies = TRUE)
install.packages("TAM")
install.packages("tidyverse")
install.packages("VGAM")

# install from GitHub (packages not available on CRAN, or newest versions)
remotes::install_github("adelahladka/difNLR")  # newest GitHub version
remotes::install_github("cddesja/hemp")
remotes::install_github("patriciamar/ShinyItemAnalysis") # newest GitHub version