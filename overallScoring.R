overallScoring <- function (networkInfo, SSNQGraph, SSNQMembers) {
  
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
    if (networkInfo[[4]] <= 3) {
      score[1] <- score[1] + 10
    } else if (networkInfo[[4]] == 4) {
      score[1] <- score[1] + 3
    } else if (networkInfo[[4]] == 5) {
      score[1] <- score[1] + 1
    } else if (networkInfo[[4]] == 6) {
      score[1] <- score[1] - 1
    } else if (networkInfo[[4]] >= 7) {
      score <- score -3
    } else {
      warning("diameter of graph 
              is not available to the scoring alghorithm")
    }
  }
  
  #Density Scoring modifiers
  if (is.nan(networkInfo[[5]])) {
    score[1] <- score[1] - 15
    score[2] <- score[2] + 5
  } else if (networkInfo[[5]] >= 0.6) {
    score[1] <- score[1] + 10
    score[2] <- score[2] - 5
  } else if (networkInfo[[5]] >= 0.5) {
    score[1] <- score[1] + 5
    score[2] <- score[2] - 2.5
  } else if (networkInfo[[5]] >= 0.3) {
    score[1] <- score[1] + 3
    score[2] <- score[2] - 1
  } else if (networkInfo[[5]] >= 0.2) {
    score[1] <- score[1] - 3
    score[2] <- score[2] + 1
  } else if (networkInfo[[5]] >= 0.1) {
    score[1] <- score[1] - 5
    score[2] <- score[2] + 3
  } else if (networkInfo[[5]] < 0.1) {
    score[1] <- score[1] - 10
    score[2] <- score[2] + 5
  } else {
    warning("edge density of graph
              is not available to the scoring alghorithm")
  }
  
  #Reciprocity scoring modifiers
  if (is.nan(networkInfo[[6]])) {
    score[1] <- score[1] - 15
    score[2] <- score[2] - 15
  } else if(networkInfo[[6]] > 0.6) {
    score[1] <- score[1] + 10
    score[2] <- score[2] + 10
  } else if (networkInfo[[6]] >= 0.5) {
    score[1] <- score[1] + 5
    score[2] <- score[2] + 5
  } else if (networkInfo[[6]] >= 0.45) {
    score[1] <- score[1] + 0
    score[2] <- score[2] + 0    
  } else if (networkInfo[[6]] >= 0.4) {
    score[1] <- score[1] - 5
    score[2] <- score[2] - 5
  } else if (networkInfo[[6]] < 0.4) {
    score[1] <- score[1] - 10
    score[2] <- score[2] - 10
  } else {
    warning("reciprocity of graph 
              is not available to the scoring alghorithm")
  }
  
#Isolate scoring modifiers (-5% for each isolate)
  if(length(networkInfo) == 11) {
    if (length(networkInfo[[11]]) > 0) {
     for (i in 1:length(networkInfo[[10]])) {
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
    health <- "Gold"
    healthColour <- "#F8E114"
  } else if (overallScores[1] > 90) {
    health <- "Gold"
    healthColour <- "#F8E114"
  } else if (overallScores[1] > 60) {
    health <- "Silver"
    healthColour <- "#D7D7D7"
  } else if (overallScores[1] < 60) {
    health <- "Bronze"
    healthColour <- "#AD8A56"
  } else {
    health <- "NA"
    healthColour <- "info"
  }
  overallHealth <- c(health, healthColour)
}

#convert potential for improvement score (PfI) into valuebox info
computePfI <- function(overallScores) {
  if(overallScores[2] > 140) {
    PfI <- "Gold"
    PfIColour <- "#F8E114"
  } else if (overallScores[2] > 90) {
    PfI <- "Gold"
    PfIColour <- "#F8E114"
  } else if (overallScores[2] > 60) {
    PfI <- "Silver"
    PfIColour <- "#D7D7D7"
  } else if (overallScores[2] < 60) {
    PfI <- "Bronze"
    PfIColour <- "#AD8A56"
  } else {
    PfI <- "NA"
    PfIColour <- "info"
  }
  overallPfI <- c(PfI, PfIColour)
}
