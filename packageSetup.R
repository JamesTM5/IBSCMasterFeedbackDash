# install packages

#nb. To install use the commented code below, but then
# re-comment it or the dash won't successfully publish to shinyApps.io

# packages = c("igraph","networkD3", "openxlsx",
#              "flexdashboard", "DT", "shiny",
#              "tidyverse", "knitr", "plotly",
#              "humaniformat", "UpSetR", "dplyr",
#              "data.table", "ggplot2", "ggdendro")
#  installed_packages <- packages %in% rownames(installed.packages())
#  if (any(installed_packages == FALSE)) {
#    install.packages(packages[!installed_packages])
#  }
# invisible(lapply(packages, library, character.only = TRUE))

#call required libraries
library("igraph")
library("networkD3")
library("openxlsx")
library("flexdashboard")
library("DT")
library("shiny")
library("tidyverse")
library("knitr")
library("plotly")
library("humaniformat")
library("UpSetR")
library("dplyr")
library("data.table")
library("ggplot2")
library("ggdendro")
