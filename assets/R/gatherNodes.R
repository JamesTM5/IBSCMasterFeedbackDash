gatherNodes <- function (filePath = "./data/") {
  files = list.files(path = filePath, pattern = '^.*rds$')
  fileList <- list()
  
  for (i in 1:length(files)) {
    file <- readRDS(paste(filePath, files[i], sep = "/"))
    name <- as.character(files[i])
    fileList[[name]] <- file
  }
  
  nodesAmalgam <- list()
  for(i in 1:length(fileList)) {
   nodesAmalgam[[length(nodesAmalgam)+1]] <- fileList[[i]]$nodes
  }
  
  nodesAmalgam <- bind_rows(nodesAmalgam, .id = "classNumber")
  
  if (!length(unique(nodesAmalgam$id)) == nrow(nodesAmalgam)) {
    for (i in 1:nrow(nodesAmalgam)) { nodesAmalgam[i, "id"] <- i}
    nodesAmalgam$id <- as.character(nodesAmalgam$id)
  }
  return(nodesAmalgam)
}
