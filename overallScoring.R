overallScoring <- function (totalNetworkInfo, SSNQGraph, SSNQMembers) {
  
  #convenience function to find out if a graph is fragmented.
  graphConnected <- function(graph) {
    CD <- component_distribution(graph)
    if(length(CD[!CD==0])>1) {
      connected <- FALSE
    } else {
      connected <- TRUE
    }
  }
  
  score <- c(100,100)
  
  #if Graph is not connected -3, else modify scores based on diameter
  if(graphConnected(SSNQGraph) == FALSE){
    score[1] <- score[1] - 10
  } else {
    if (totalNetworkInfo[[4]] <= 3) {
      score[1] <- score[1] + 10
    } else if (totalNetworkInfo[[4]] == 4) {
      score[1] <- score[1] + 3
    } else if (totalNetworkInfo[[4]] == 5) {
      score[1] <- score[1] + 1
    } else if (totalNetworkInfo[[4]] == 6) {
      score[1] <- score[1] - 1
    } else if (totalNetworkInfo[[4]] >= 7) {
      score <- score -3
    } else {
      warning("diameter of graph 
              is not available to the scoring alghorithm")
    }
  }
  
  #Density Scoring modifiers
  if (totalNetworkInfo[[5]] >= 0.6) {
    score[1] <- score[1] + 10
    score[2] <- score[2] - 5
  } else if (totalNetworkInfo[[5]] >= 0.5) {
    score[1] <- score[1] + 5
    score[2] <- score[2] - 2.5
  } else if (totalNetworkInfo[[5]] >= 0.3) {
    score[1] <- score[1] + 3
    score[2] <- score[2] - 1
  } else if (totalNetworkInfo[[5]] >= 0.2) {
    score[1] <- score[1] - 3
    score[2] <- score[2] + 1
  } else if (totalNetworkInfo[[5]] >= 0.1) {
    score[1] <- score[1] - 5
    score[2] <- score[2] + 3
  } else if (totalNetworkInfo[[5]] < 0.1) {
    score[1] <- score[1] - 10
    score[2] <- score[2] + 5
  } else {
    warning("edge density of graph
              is not available to the scoring alghorithm")
  }
  
  #Reciprocity scoring modifiers
  if(totalNetworkInfo[[6]] > 0.6) {
    score[1] <- score[1] + 10
    score[2] <- score[2] + 10
  } else if (totalNetworkInfo[[6]] >= 0.5) {
    score[1] <- score[1] + 5
    score[2] <- score[2] + 5
  } else if (totalNetworkInfo[[6]] >= 0.45) {
    score[1] <- score[1] + 0
    score[2] <- score[2] + 0    
  } else if (totalNetworkInfo[[6]] >= 0.4) {
    score[1] <- score[1] - 5
    score[2] <- score[2] - 5
  } else if (totalNetworkInfo[[6]] < 0.4) {
    score[1] <- score[1] - 10
    score[2] <- score[2] - 10
  } else {
    warning("reciprocity of graph 
              is not available to the scoring alghorithm")
  }
  
#Isolate scoring modifiers (-5% for each isolate)
  if(length(totalNetworkInfo) == 11) {
    if (length(totalNetworkInfo[[11]]) > 0) {
     for (i in 1:length(totalNetworkInfo[[10]])) {
        score[1] <- score[1] - (score[1]/20)
      }
    }
  }
  
  # Community Scoring modifiers: count the number of communities which have
  # members over a certain membership threshold and drop the score by 10 percent
  # for each community identified.
  
  MembersThreshold <- 3
  MembersCount <- table(SSNQMembers)
  drop <- vector()
  for(i in 1:length(MembersCount)) {
    if (MembersCount[i] < MembersThreshold) {
      drop <- append(i, drop)
    }
  }
  CommunityNumber <- length(MembersCount) - length(drop)
  if(CommunityNumber > 0 ) {
    for (i in 1:CommunityNumber) {
      score[1] <- score[1] - (score[1]/10)
      score[2] <- score[2] - (score[2]/10)
    }
  }
  
  overallScore <- score
}

computeHealth <- function(overallScores) {
  if(overallScores[1] > 140) {
    health <- "Excellent"
    healthColour <- "success"
  } else if (overallScores[1] > 100) {
    health <- "Good"
    healthColour <- "success"
  } else if (overallScores[1] > 60) {
    health <- "Moderate"
    healthColour <- "warning"
  } else if (overallScores[1] < 60) {
    health <- "Poor"
    healthColour <- "danger"
  } else {
    health <- "NA"
    healthColour <- "info"
  }
  overallHealth <- c(health, healthColour)
}

#convert potential for improvement score (PfI) into valuebox info
computePfI <- function(overallScores) {
  if(overallScores[2] > 140) {
    PfI <- "Excellent"
    PfIColour <- "success"
  } else if (overallScores[2] > 100) {
    PfI <- "Good"
    PfIColour <- "success"
  } else if (overallScores[2] > 60) {
    PfI <- "Moderate"
    PfIColour <- "warning"
  } else if (overallScores[2] < 60) {
    PfI <- "Poor"
    PfIColour <- "danger"
  } else {
    PfI <- "NA"
    PfIColour <- "info"
  }
  overallPfI <- c(PfI, PfIColour)
}
