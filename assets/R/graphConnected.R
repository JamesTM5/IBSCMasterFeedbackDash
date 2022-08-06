#convenience function to find out if a graph is fragmented.
graphConnected <- function(graph) {
  CD <- component_distribution(graph)
  if(length(CD[!CD==0])>1) {
    connected <- FALSE
  } else {
    connected <- TRUE
  }
}
