#This script takes data output from Polinode, and processes it for use in the
#IBSC Class dashboard.  To use, put the .xlsx output from all the surveys for a
#given school from polinode into the working directory and then edit the survey
#information and the client information in surveyConfig.R and clientConfig.R and
#run the script. The output will generate in the working directory the .RDS
#Files needed by the dashboard. The .xlsx and this file do not need to be
#published to shinyApps.io


source("packageSetup.R")
source("surveyConfig.R")
source("clientConfig.R")

DashAnalysis <- function (dataFile,
                          clientName,
                          className,
                          participantType,
                          socioDemographicVariables,
                          anonymous, answersSSNQ,
                          responseColumnSSNQ,
                          thresholdForEdgeDrawing,
                          seed,
                          dataSource) {
  
#import student data
  nodes <- read.xlsx(dataFile, sheet = 1)
  edges1 <- read.xlsx(dataFile, sheet = 2)
  edges2 <- read.xlsx(dataFile, sheet = 3)
  edges3 <- read.xlsx(dataFile, sheet = 4)

  #set random seed from config file
  set.seed (seed)
 
  source("assets/R/makeResponseNumeric.R")
 
#convert data to numeric where necessary
  
  #make the numeric keypair dataframes from which to convert node answers;
  #function in makeResponseNumeric.R
  nodesKey <- keyToNumeric(scoreData = nodes[,responseColumns],
                      answers = nodeAnswers,
                      normalized = FALSE)
  
  #loop over each relevant column and bind a numeric column to nodes
  for(i in 1:length(nodes[,responseColumns])) {
    numericReplacementVector <- makeResponseNumeric(
                                              data = nodes[,responseColumns[i]],
                                              conversionKey = nodesKey)
    numericReplacementColumn <- data.frame(numericReplacementVector)
    getNames <- names(nodes)
    names(numericReplacementColumn) <- paste0(getNames[[responseColumns[[i]]]],
                                              ".numeric")
  nodes <- cbind(nodes, numericReplacementColumn)
  
  }
  
  #make the numeric keypair dataframes from which to convert
  #relationship question answers
  edgesKey <- keyToNumeric(scoreData = edges[,xxxxxxxxx],
                           answers = edgeAnswers,
                           normalized = TRUE)
  
  #for each network question, bind a numeric column to edges data frames with
  #numeric answers in, called 'Network'
    numericReplacementVector <- makeResponseNumeric(data = edges1$Q2,
                                                    conversionKey = edgesKey)
    numericReplacementColumn <- data.frame(numericReplacementVector)
    names(numericReplacementColumn) <- "Network"
    edges1 <- cbind(edges1, numericReplacementColumn)

    numericReplacementVector <- makeResponseNumeric(data = edges2$Q3,
                                                    conversionKey = edgesKey)
    numericReplacementColumn <- data.frame(numericReplacementVector)
    names(numericReplacementColumn) <- "Network"
    edges2 <- cbind(edges2, numericReplacementColumn)
    
    numericReplacementVector <- makeResponseNumeric(data = edges3$Q4,
                                                    conversionKey = edgesKey)
    numericReplacementColumn <- data.frame(numericReplacementVector)
    names(numericReplacementColumn) <- "Network"
    edges3 <- cbind(edges3, numericReplacementColumn)
  
  #List network question data to enable looping over it
    surveyData <- list(edges1, edges2, edges3)
    
#Filter edges of each network question for most positive only    
    surveyDataFiltered <- list()
    for(i in 1:length(surveyData)) {
      surveyDataFiltered[[i]] <- dplyr::filter(surveyData[[i]], 
                                               surveyData[[i]]$Network > 
                                               thresholdForEdgeDrawing)
  }

#perform summative network analysis on each relationship question
    source("networkSurveyAnalysis.R")
    totalNetworkInfo <- list()
    for(i in 1:length(surveyDataFiltered)) {
    totalNetworkInfo[[length(totalNetworkInfo)+1]] <-
      surveyDataAnalysis(questionData = surveyDataFiltered[[i]])
    }   

#perform homophylytic analysis
    source("calculateHomophyly.R")
    homophylyList <- list()
    for(i in 1:length(surveyDataFiltered)) {
      homophylyList[[length(homophylyList)+1]] <- calculateHomophyly(
                                    homophylyEdgeList = surveyDataFiltered[[i]],
                                    socioDemographicVariable =
                                    socioDemographicVariables,
                                    nodes = nodes)
      
    }
  
#Insert HTML into a list to display it properly in-dash
    formatDataForDisplay <- function(Data) {
      Data <- na.omit(Data)
      ogValue <- Data[length(Data)]
      formattedValue <- paste(Data, "<br>", sep=" ")
      formattedValue[length(formattedValue)] <- ogValue
      return(formattedValue)
    }

#format homophylyList for display in-dash
  for(i in 1:length(homophylyList)) {   
    predictiveSDVSSNQ <- levels(as.factor(unlist(homophylyList[[i]][[1]])))
    nonPredictiveSDVSSNQ <- levels(as.factor(unlist(homophylyList[[i]][[2]])))
    if(length(predictiveSDVSSNQ > 0)) {
      homophylyList[[i]][[1]] <-
        formatDataForDisplay(predictiveSDVSSNQ)
    }
    if(length(nonPredictiveSDVSSNQ > 0)) {
      homophylyList[[i]][[2]] <-
        formatDataForDisplay(nonPredictiveSDVSSNQ)
    }
  }
    
#find isolates
  isolatesList <- list()
  for(i in 1:length(totalNetworkInfo)) {
    isolatesList[[i]] <- nodes$Name [!(nodes$Name %in% totalNetworkInfo[[i]][[3]][["People"]])]
    isolatesList[[i]][isolatesList[[i]] == ""] <- NA
  }
#format isolates list for display in dash, or display 'None'
  for(i in 1:length(isolatesList)) {
    if(length(isolatesList[[i]])>0) {
      isolatesList[[i]] <- formatDataForDisplay(isolatesList[[i]])
    } else {
      isolatesList[[i]] <- "None"
    }
  }

#Prepare data for D3.JS forceDirected
  SSNQGraphList <- list()
  SSNQCommunitiesDataList <- list()
  SSNQMembersList <- list()
  for (i in 1:length(surveyDataFiltered)) {
    SSNQGraphFromDataset <- data.frame(
      surveyDataFiltered[[i]]$Source,
      surveyDataFiltered[[i]]$Target,
      ... = surveyDataFiltered[[i]]$Network)
    #Make an igraph object for each question for the D3 R port.
    SSNQGraphList[[i]] <- graph_from_data_frame(SSNQGraphFromDataset, directed = T)
    # Perform Communities Analysis for each igraph object
    SSNQCommunitiesDataList[[i]] <- cluster_walktrap(SSNQGraphList[[i]])
    
    SSNQMembersList[[i]] <- membership(SSNQCommunitiesDataList[[i]])
  }

    #Convert igraph to D3
  SSNQNetworkD3 <- list()
  for(i in 1:length(SSNQGraphList)) {
    SSNQNetworkD3[[i]] <- igraph_to_networkD3(SSNQGraphList[[i]], group = SSNQMembersList[[i]])
  }
    
    #Set up the edge data for porting to D3.JS
    edgeDataSetup <- function(SSNQGraph) {
      #Find every co-occuring join and assign each 2, while the others are assigned 1
      x <- SSNQGraphFromDataset
      colnamesx <- colnames(x)
      for(i in 1:length(colnamesx)) {
        if(colnamesx[[i]] == "...") {
          colnamesx[[i]] <- "scoreStrength"
        }
      }
      colnames(x) <- colnamesx
      #find the second instance of each duplication and assign it TRUE
      duplicates <- duplicated(t(apply(x[,1:2], 1, sort)))
      x <- cbind(x, duplicates = duplicates)
      #split between true and false and search for the mutual partners in z, then use
      # these to change x
      y <- filter(as_tibble(x), duplicates == TRUE)
      z <- filter(as_tibble(x), duplicates == FALSE)
      y <- y[,c(2,1,3,4)]
      colnames(y) <- colnames(x)
      a <- inner_join(z[,1:2],y[,1:2])
      x <- as_tibble(x)
      for (i in 1:nrow(a)) {
        for (j in 1:nrow(x)) {
          if (identical(x[j,1:2], a[i,1:2])) {
            x[j,4] <- TRUE
          }
        }
      }
      #reorder x to suit D3.JS port formatting of source, target, mutual, score
      x <- x[,c(1,2,4,3)]
      colnames(x) <- c("source", "target", "mutual", "score")
      
      cols <- sapply(x, is.logical)
      x[,cols] <- lapply(x[,cols], as.numeric)
      for(i in 1:nrow(x[,3])) {
        x[i,3] <- x[i,3]+1
      }
      #reassign x to the original variable
      return(x)
    }
    
    edgeDataList <- list()
    for(i in length(SSNQGraphList)) {
      edgeDataList[[i]] <- edgeDataSetup(SSNQGraphList[[i]])
    }
#prepare Dendrograms
    dendrogramList <- list()
    dendrogramList[[1]] <- as.dendrogram(SSNQCommunitiesDataList[[1]])
    dendrogramList[[2]] <- as.dendrogram(SSNQCommunitiesDataList[[2]])
    dendrogramList[[3]] <- as.dendrogram(SSNQCommunitiesDataList[[3]])

# Overall Scoring calculation
  source("overallScoring.R")
  overallScoresList <- list()
  for(i in 1:length(totalNetworkInfo)) {
    overallScoresList[[i]] <- overallScoring (totalNetworkInfo[[i]], SSNQGraphList[[i]], SSNQMembersList[[i]])
  }
  
# Format overall scoring data for display in dash
  overallHealthList <- list()
  overallPfIList <- list()
  for(i in 1:length(overallScoresList)) {
    overallHealthList[[i]] <- computeHealth(overallScoresList[[i]])
    overallPfIList[[i]] <- computePfI(overallScoresList[[i]])
  }

#Generate the polar graphs for individual pages
  source("generatePolar.R")
  polarGraphList <- list()
  for(i in 1:length(surveyData)) {
    polarGraphList[[i]] <- generatePolar(data = surveyData[[i]])
  }
  
  source("generateDegreeHistogram.R")
  degreeAllHistogramList <- list()
  degreeInHistogramList <- list()
  degreeOutHistogramList <- list()
  for(i in 1:length(totalNetworkInfo)) {
    degreeAllHistogramList[[i]] <- generateDegreeHistogram(totalNetworkInfo[[i]][[3]])
    degreeInHistogramList[[i]] <- generateDegreeHistogram(totalNetworkInfo[[i]][[1]])
    degreeOutHistogramList[[i]] <- generateDegreeHistogram(totalNetworkInfo[[i]][[2]])
  }

#Extract and format Authority Score
  ASList <- list()
  for(i in 1:length(totalNetworkInfo)) {
    AS <- setDT(as.data.frame(totalNetworkInfo[[i]][[8]]),
                            keep.rownames = TRUE) []
    ASFormatted <- AS[[1]]
    for (j in 1:length(ASFormatted)) {
      ASFormatted[[j]] <- paste(ASFormatted[[j]], "<br>", sep = " ")
    }
    ASList[[i]] <- ASFormatted[1:3]
  }

#Extract and format Hub Score
  HSList <- list()
  for(i in 1:length(totalNetworkInfo)) {  
    HS <- data.table::setDT(as.data.frame(totalNetworkInfo[[i]][[7]]),
                            keep.rownames = TRUE) []
    HSFormatted <- HS[[1]]
    for (j in 1:length(HSFormatted)) {
      HSFormatted[[j]] <- paste(HSFormatted[[j]], "<br>", sep = " ")
    }
    HSList[[i]] <- HSFormatted[1:3]
  }
  
  
}

  