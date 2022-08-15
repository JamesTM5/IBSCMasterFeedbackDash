library(tidyverse)

gatherNodes <- function (fileList) {
  
  nodesAmalgam <- list()
  for(i in 1:length(fileList)) {
   nodesAmalgam[[length(nodesAmalgam)+1]] <- fileList[[i]]$nodes
  }
  
  nodesAmalgam <- bind_rows(nodesAmalgam, .id = "column_label")
}