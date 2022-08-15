# #run d3 prep for each overall class map in the organisation
collateOverallData <- function(file) {
  nodes <- file$D3NodesList[[length(file$D3NodesList)]]
  edges <- file$overallSSEdgesDF
  t <- list(nodes = nodes, edges = edges)
}

prepOverallData <- function(fileSet) {
  First <- collateOverallData(fileSet)
  JSONOut <- list(First = First)
}

d3ClassOverviewPrepSingleInstance <- function (fileList) {
  for(i in 1:length(fileList)) {
    #run d3 prep for all relationship questions in each class
    fileNameOverall <- paste("D3JSDataOverall",i, ".json", sep = "")
    D3JSDataOverall <- prepOverallData(fileSet = fileList[[i]])
    D3JSDataOverall <- jsonlite::toJSON(D3JSDataOverall)
    write(D3JSDataOverall, fileNameOverall)
  }
}