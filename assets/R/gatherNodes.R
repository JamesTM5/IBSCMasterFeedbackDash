library(tidyverse)

gatherNodes <- function (filePath = "./test data/") {
  files = list.files(path = filePath, pattern = '^.*rds$')
  fileList <- list()
  
  for (i in 1:length(files)) {
    file <- readRDS(paste("./test data", files[i], sep = "/"))
    name <- as.character(files[i])
    fileList[[name]] <- file
  }
  
  nodesAmalgam <- list()
  for(i in 1:length(fileList)) {
   nodesAmalgam[[length(nodesAmalgam)+1]] <- fileList[[i]]$nodes
  }
  
  nodesAmalgam <- bind_rows(nodesAmalgam, .id = "column_label")
}


#gatherNodes(filePath = "./data/")