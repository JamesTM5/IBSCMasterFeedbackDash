calculateHomophyly <- function (homophylyEdgeList, testingVariables, nodes) {
 
  subsetting <- c("id", testingVariables)
  # subsetting <- str_replace_all(subsetting, ".", " ")
  # names(nodes) <- str_replace_all(names(nodes), " ", ".")
  homophylyLookup <- nodes[,subsetting]

    #testing
  homophylyEdgeList = surveyDataFiltered[[1]]
  
  
  homophylyNetwork <- graph_from_data_frame(homophylyEdgeList,
                                            directed = TRUE,
                                            vertices = homophylyLookup)
  
  assort <- list()
  homophylySDV <- str_replace_all(testingVariables, " ", ".")
  
  assort <- list()
  for(i in 1:length(vertex.attributes(homophylyNetwork))) {
      homophylyNetworkFiltered <- delete_vertices(homophylyNetwork,
        V(homophylyNetwork)[which(is.na(as.numeric(as.factor(vertex.attributes(
          homophylyNetwork)[[i]]))))])
      assort[[length(assort)+1]] <- assortativity(homophylyNetworkFiltered,
        as.numeric(as.factor(vertex.attributes(homophylyNetworkFiltered)[[i]])),
        directed = TRUE)
  }


   predictiveChoices <- list()
   nonPredictiveChoices <- list()
  
   for (i in 1:length(assort)) {
     if (is.nan(assort[[i]])) {
       nonPredictiveChoices[i] <- paste0(names(homophylyLookup[i]))
     } else if (is.na(assort[[i]])) {
       nonPredictiveChoices[i] <- paste0(names(homophylyLookup[i])) 
     } else if (assort[[i]] == "id") {
     } else if (assort[[i]] > 0.2) {
       predictiveChoices[i] <- paste0(names(homophylyLookup[i]), " (high)")
     } else if (assort[[i]] > 0.1) {
       predictiveChoices[i] <- paste0(names(homophylyLookup[i]), " (moderate)")
     } else if (assort[[i]] < -0.2) {
       predictiveChoices[i] <- paste0(names(homophylyLookup[i]), " (high)") 
     } else if (assort[[i]] < -0.1) {
       predictiveChoices[i] <- paste0(names(homophylyLookup[i]), " (moderate)")
     } else {
       nonPredictiveChoices[i] <- paste0(names(homophylyLookup[i]))
     }
   }

   nonPredictiveChoices <- unlist(nonPredictiveChoices)
   nonPredictiveChoices <- str_replace_all(nonPredictiveChoices, "\\.", " ")
   
   predictiveChoices <- unlist(predictiveChoices)
   predictiveChoices <- str_replace_all(predictiveChoices, "\\.", " ")
   
 homophylyOutput <- list(predictiveChoices, nonPredictiveChoices)
}
