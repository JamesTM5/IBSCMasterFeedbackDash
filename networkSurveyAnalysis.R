#function to make graphs and do all the network analysis needed for peer to peer
#network generation and analysis

#Function returns totalNetworkInfo, a list containing:
#  1. Degree In
#  2. Degree Out
#  3. Degree all
#  4. Diameter
#  5. Edge Density
#  6. Reciprocity
#  7. Hub Score
#  8. Authority Score
#  9. Key Relationships
#  10. Closeness
#  11. Isolates


surveyDataAnalysis <- function(questionData) {
  graphFromDataset <- data.frame(questionData$Source, questionData$Target,
                                 ... = questionData$Network)
  network <- graph_from_data_frame(graphFromDataset, directed = T)
  
  degreeIn <- degree(network, mode = "in", normalized = T)
  degreeIn <- round(degreeIn, digits = 2)
  degreeIn <- sort(degreeIn, decreasing = T)
  degreeInDataFrame <- data.frame(degreeIn, names(degreeIn))
  names(degreeInDataFrame)[names(degreeInDataFrame) == "degreeIn"] <- "Degree"
  names(degreeInDataFrame)[names(degreeInDataFrame) == "names.degreeIn."] <- "People"
  
  degreeOut <- degree(network, mode = "out", normalized = T)
  degreeOutDataFrame <- data.frame(degreeOut, names(degreeOut))
  names(degreeOutDataFrame)[names(degreeOutDataFrame) == "degreeOut"] <- "Degree"
  names(degreeOutDataFrame)[names(degreeOutDataFrame) == "names.degreeOut."] <- "People"
  
  degreeAll <- degree(network, mode = "all", normalized=T)
  degreeAllDataFrame <- data.frame(degreeAll, names(degreeAll))
  names(degreeAllDataFrame)[names(degreeAllDataFrame) == "degreeAll"] <- "Degree"
  names(degreeAllDataFrame)[names(degreeAllDataFrame) == "names.degreeAll."] <- "People"
  
  diameter <- diameter(network)
  edgeDensity <- edge_density(network, loops = FALSE)
  reciprocity <- reciprocity (network)
  
  hubScore <- igraph::hub_score(network)$vector
  hubScore <- sort(hubScore, decreasing = TRUE)
  authorityScore <- authority.score(network)$vector
  authorityScore <- sort(authorityScore, decreasing = TRUE)
  
  edge_betweennessScore <- edge_betweenness (network)
  names(edge_betweennessScore) <- E(network)
  edge_betweennessScore <- sort(edge_betweennessScore, decreasing = T)
  top3EdgeBetweenness <- edge_betweennessScore[1:3]
  top3EdgeBetweennessLookup <- as.numeric(names(top3EdgeBetweenness))
  keyRelationships <- ends(network, top3EdgeBetweennessLookup, names = TRUE)
  
  isolated <- which(degree(network)==0)
  closenessNetwork <- delete.vertices(network, isolated)
  closeness <- closeness(closenessNetwork, mode = 'all', weights = NA)

  networkInfo <- list(degreeInDataFrame, degreeOutDataFrame,
                      degreeAllDataFrame, diameter,
                      edgeDensity, reciprocity,
                      hubScore, authorityScore,
                      keyRelationships, closeness)
}

#K-means Decomposition value of coreness
#kc <- coreness(HEATMAPnetwork, mode = 'all')
#plot(HEATMAPnetwork, vertex.size=kc*6, vertex.label=kc, vertex.color=colors[kc])

