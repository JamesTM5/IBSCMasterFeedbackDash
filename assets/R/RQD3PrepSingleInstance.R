#Prep the data for the D3 maps
collateDataSingleInstance <- function(file, number) {
  nodes <- file$D3NodesList[[number]]
  edges <- file$edgeDataList[[number]]
  t <- list(nodes = nodes, edges = edges)
}

prepDataSingleInstance <- function(fileSet, index) {
  First <- collateDataSingleInstance(fileSet, number = index)
  JSONOut <- list(First = First)
}

d3RQPrepSingleInstance <- function (fileList) {
  #run d3 prep for all classes' questions in the organisation
  for(i in 1:length(fileList)) {
    #run d3 prep for all relationship questions in each class
    for (j in 1:length(fileList[[i]]$D3NodesList)) {
      fileName <- paste("./compilationOutput/data/D3JSData",j, i, ".json", sep = "")
      D3JSData <- prepDataSingleInstance(fileSet = fileList[[i]], index = j)
      D3JSData <- jsonlite::toJSON(D3JSData)
      write(D3JSData, fileName)
    }
  }
}