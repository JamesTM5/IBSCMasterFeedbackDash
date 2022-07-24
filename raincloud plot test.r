#install.packages("ggplot2", "ggdist", "gghalves")
library(ggplot2)
library(ggforce)
library(ggdist)
library(gghalves)

ggplot(nodeList[[1]], aes(GPA, co.curricular)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) + 
  ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.05, binwidth = .1)
