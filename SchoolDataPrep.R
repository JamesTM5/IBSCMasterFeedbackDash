#This script takes data output from Polinode, and processes it for use in the
#IBSC Class dashboard.  To use, put the .xlsx output from all the surveys for a
#given school from polinode into a directory, add the path to fileList below and
#make sure the file name is in the format ".Student <class name>.xlsx".
#then edit the survey information and the client information in surveyConfig.R
#and clientConfig.R and source the script. The output will be in the working
#directory (the .RDS files needed by the dashboard). The .xlsx and this file do
#not need to be published to shinyApps.io

fileList <- list.files(path = "test data/", pattern = ".Student.*.xlsx",
                       full.names=TRUE)

SSDashAnalysis <- function (file) {
  
#Call Libraries
  source("packageSetup.R")
  
#Pull in config info
  source("surveyConfig.R")
  source("clientConfig.R")
  
#import student data
  nodes <- read.xlsx(file, sheet = 1)
  edges1 <- read.xlsx(file, sheet = 2)
  edges2 <- read.xlsx(file, sheet = 3)
  edges3 <- read.xlsx(file, sheet = 4)

#extract class name from file  
  className <- substring(file, regexpr("Student", file) + 8)
  className <- substring(className, 1, nchar(className)-5) #remove ".xlsx"
  
  #set random seed from config file
  set.seed (seed)
 
  source("assets/R/makeResponseNumeric.R")
 
#convert data to numeric where necessary
  
  #make the numeric keypair dataframes from which to convert node answers;
  #function in makeResponseNumeric.R
  
  nodesKeyBelongingness <- keyToNumeric(scoreData = nodes[,responseColumnsBelongingness],
                                        answers = nodeAnswersBelongingness,
                                        normalized = FALSE)
  
 
  nodesKeyBelongingnessCategorical <- data.frame(nodeAnswersBelongingness)
  nodesKeyBelongingnessCategorical$newAnswers <- c("High", "Moderate", "Moderate", "Low")
  
  nodesKeyStudentTeacher <- keyToNumeric(scoreData = nodes[,responseColumnsStudentTeacher],
                                         answers = nodeAnswersStudentTeacher,
                                         normalized = FALSE)
  
  nodesKeyStudentTeacherCategorical <- data.frame(nodeAnswersStudentTeacher)
  nodesKeyStudentTeacherCategorical$newAnswers <- c("High", "High", "Moderate", "Moderate", "Low", "Low")
  names(nodesKeyStudentTeacherCategorical) <- c("answers", "newAnswers")
  
  #loop over each relevant column and bind a numeric column to nodes
  
  responseColumnsBelongingness <- nodes %>% select(contains(c('Q1-')))
  responseColumnNamesBelongingness <- names(responseColumnsBelongingness)
  
  responseColumnsStudentTeacher <- nodes %>% select(contains(c('Q5-')))
  responseColumnNamesStudentTeacher <- names(responseColumnsStudentTeacher)
  
  #get column numbers
  studentTeacherResponseColumnIndices <- grep("Q5", colnames(nodes))
  
  studentTeacherRegExQuestionText <- c(
    "I\\.can\\.talk\\.to\\.or\\.contact\\.my\\.",
    "It\\.is\\.worth\\.building\\.a\\.good\\.relationship\\.with\\.my\\.",
    "\\.and\\.I\\.have\\.shared\\.goals\\.for\\.my\\.progress\\.and\\.development",
    "cares\\.about\\.me",
    "has\\.a\\.good\\.understanding\\.of\\.my\\.skills\\.and\\.interests",
    "inspires\\.and\\.motivates\\.me",
    "recognises\\.and\\.rewards\\.my\\.efforts"
  )
  studentTeacherResponseColumnIndices <- vector()
  for(i in 1:length(studentTeacherRegExQuestionText)) {
   studentTeacherResponseColumnIndices[[length(studentTeacherResponseColumnIndices)+1]] <- grep(studentTeacherRegExQuestionText[i], colnames(nodes))
  }
  if(length(studentTeacherResponseColumnIndices)>7) {
    warning("more student - teacher questions have been matched than there should be.  Consider checking the studentTeacherRegExQuestionText for unwanted matches")
  }
  
  belongingnessRegExQuestionText <- c("I\\.feel\\.awkward\\.and\\.out\\.of\\.place",
                                      "I\\.feel\\.like\\.an\\.outsider",
                                      "I\\.feel\\.like\\.I\\.belong\\.at",
                                      "I\\.feel\\.lonely\\.at",
                                      "I\\.make\\.friends\\.easily\\.at",
                                      "\\.seem\\.to\\.like\\.me"
                                      )
  belongingnessResponseColumnIndices <- vector()
  for(i in 1:length(belongingnessRegExQuestionText)) {
     belongingnessResponseColumnIndices[[length(belongingnessResponseColumnIndices)+1]] <- grep(belongingnessRegExQuestionText[i], colnames(nodes))
  }
  if(length(belongingnessResponseColumnIndices)>6) {
    warning("more belongingness questions have been matched than there should be.  Consider checking the belongingnessRegExQuestionText for unwanted matches")
  }

#make numeric columns for belongingness (0-3)
  for(i in 1:length(nodes[,belongingnessResponseColumnIndices])) {
    numericReplacementVector <- makeResponseNumeric(
      data = nodes[,belongingnessResponseColumnIndices[i]],
      conversionKey = nodesKeyBelongingness)
    numericReplacementColumn <- data.frame(numericReplacementVector)
    getNames <- names(nodes)
    names(numericReplacementColumn) <- paste0(
      getNames[[belongingnessResponseColumnIndices[[i]]]], ".numeric")
    nodes <- cbind(nodes, numericReplacementColumn)
    
  }

#make stratified columns for belongingness (low, medium, high)  
  modifiedNodes <- nodes[,belongingnessResponseColumnIndices]
  originalNames <- names(modifiedNodes)
  names(modifiedNodes) <- c("a", "b", "c", "d", "e", "f")
  replacementNames <- names(modifiedNodes)
  
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("a" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[1], "stratified", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("b" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[2], "stratified", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("c" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[3], "stratified", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("d" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[4], "stratified", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("e" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[5], "stratified", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("f" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[6], "stratified", sep = ".")
  modifiedNodes <- modifiedNodes[,7:ncol(modifiedNodes)]
  nodes <- cbind(nodes, modifiedNodes)

  #make numeric columns for student/teacher questions (0-5) 
  for(i in 1:length(nodes[,studentTeacherResponseColumnIndices])) {
    numericReplacementVector <- makeResponseNumeric(
      data = nodes[,studentTeacherResponseColumnIndices[i]],
      conversionKey = nodesKeyStudentTeacher)
    numericReplacementColumn <- data.frame(numericReplacementVector)
    getNames <- names(nodes)
    names(numericReplacementColumn) <- paste0(
      getNames[[studentTeacherResponseColumnIndices[[i]]]], ".numeric")
    nodes <- cbind(nodes, numericReplacementColumn)
    
  }
  
#make stratified columns for student - teacher questions (low, medium, high)  
  modifiedNodesST <- nodes[,studentTeacherResponseColumnIndices]
  originalNamesST <- names(modifiedNodesST)
  names(modifiedNodesST) <- c("a", "b", "c", "d", "e", "f", "g")
  replacementNames <- names(modifiedNodesST)
  
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("a" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[1], "stratified", sep = ".")
 
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("b" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[2], "stratified", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("c" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[3], "stratified", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("d" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[4], "stratified", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("e" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[5], "stratified", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("f" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[6], "stratified", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("g" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[7], "stratified", sep = ".")
  modifiedNodesST <- modifiedNodesST[,8:ncol(modifiedNodesST)]
  nodes <- cbind(nodes, modifiedNodesST)
  
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
  #save a list containing the raw data for passing raw to the dash
    rawEdgesList <- surveyData
    
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
    isolatesList[[i]] <- nodes$Name [!(nodes$Name %in%
                                         totalNetworkInfo[[i]][[3]][["People"]])]
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
  Graph2 <- list()
  for (i in 1:length(surveyDataFiltered)) {
    SSNQGraphFromDataset <- data.frame(
      surveyDataFiltered[[i]]$Source,
      surveyDataFiltered[[i]]$Target,
      ... = surveyDataFiltered[[i]]$Network)
    #Make an igraph object for each question for the D3 R port.
    Graph2[[i]] <- SSNQGraphFromDataset
    SSNQGraphList[[i]] <- graph_from_data_frame(SSNQGraphFromDataset,
                                                directed = T)
    # Perform Communities Analysis for each igraph object
    SSNQCommunitiesDataList[[i]] <- cluster_walktrap(SSNQGraphList[[i]])
    
    SSNQMembersList[[i]] <- membership(SSNQCommunitiesDataList[[i]])
  }

    #Convert igraph to D3
  SSNQNetworkD3 <- list()
  for(i in 1:length(SSNQGraphList)) {
    SSNQNetworkD3[[i]] <- igraph_to_networkD3(SSNQGraphList[[i]],
                                              group = SSNQMembersList[[i]])
  }
    
    #Set up the edge data for porting to D3.JS
    edgeDataSetup <- function(SSNQGraph) {
    #Find every co-occuring join and assign each 2, while the others are assigned 1
      x <- SSNQGraph
      colnamesx <- colnames(x)
      for(l in 1:length(colnamesx)) {
        if(colnamesx[[l]] == "...") {
          colnamesx[[l]] <- "scoreStrength"
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
      for (k in 1:nrow(a)) {
        for (j in 1:nrow(x)) {
          if (identical(x[j,1:2], a[k,1:2])) {
            x[j,4] <- TRUE
          }
        }
      }
      #reorder x to suit D3.JS port formatting of source, target, mutual, score
      x <- x[,c(1,2,4,3)]
      colnames(x) <- c("source", "target", "mutual", "score")
      
      cols <- sapply(x, is.logical)
      x[,cols] <- lapply(x[,cols], as.numeric)
      for(m in 1:nrow(x[,3])) {
        x[m,3] <- x[m,3]+1
      }
      #reassign x to the original variable
      return(x)
    }
    
    edgeDataList <- list()
    thing <- SSNQGraphList[[1]]
    edgeDataList[[1]] <- edgeDataSetup(Graph2[[1]])
    edgeDataList[[2]] <- edgeDataSetup(Graph2[[2]])    
    edgeDataList[[3]] <- edgeDataSetup(Graph2[[3]])

# Prepare Dendrograms
    dendrogramList <- list()
    dendrogramList[[1]] <- ggplotly(ggdendro::ggdendrogram(as.dendrogram(SSNQCommunitiesDataList[[1]])))
    dendrogramList[[2]] <- ggplotly(ggdendro::ggdendrogram(as.dendrogram(SSNQCommunitiesDataList[[2]])))
    dendrogramList[[3]] <- ggplotly(ggdendro::ggdendrogram(as.dendrogram(SSNQCommunitiesDataList[[3]])))

# Overall Scoring calculation
  source("overallScoring.R")
  overallScoresList <- list()
  for(i in 1:length(totalNetworkInfo)) {
    overallScoresList[[i]] <- overallScoring (totalNetworkInfo[[i]],
                                              SSNQGraphList[[i]],
                                              SSNQMembersList[[i]])
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
    degreeAllHistogramList[[i]] <- generateDegreeHistogram(
      totalNetworkInfo[[i]][[3]])
    degreeInHistogramList[[i]] <- generateDegreeHistogram(
      totalNetworkInfo[[i]][[1]])
    degreeOutHistogramList[[i]] <- generateDegreeHistogram(
      totalNetworkInfo[[i]][[2]])
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

#Add degree data to nodes for output to .RDS
    names(nodes)[1] <- "id"
    nodes <- nodes[order(nodes$id),]
    for(i in 1:length(totalNetworkInfo)) {
       names(totalNetworkInfo[[i]][[3]])[names(totalNetworkInfo[[i]][[3]])
                                         == "People"] <- "id"
       names(totalNetworkInfo[[i]][[3]])[names(totalNetworkInfo[[i]][[3]])
                                         == "Degree"] <- 
         paste("Degree Relationship Question", i, sep = " ")
    }
    nodes <- merge(nodes, totalNetworkInfo[[1]][[3]])
    nodes <- merge(nodes, totalNetworkInfo[[2]][[3]])
    nodes <- merge(nodes, totalNetworkInfo[[3]][[3]])
    
#add community data to nodes for output to .RDS
    communityList <- list()
    for(i in 1:length(totalNetworkInfo)) {
      community <- t(as.data.frame(as.list(SSNQMembersList[[i]])))
      community <- as.data.frame(community)
      community$id <- rownames(community)
      rownames(community) <- NULL
      community$V1 <- sapply(community$V1, function(i) letters[i])
      names(community)[names(community)=="V1"] <- paste(
        "Communities Relationship Question", i, sep = " ")
      community$id <- sapply(community$id,
                             function(v) {gsub("\\."," ", as.character(v))})
      rownames(community) <- community$id
      communityList[[i]] <- community
    }
    nodes <- merge(nodes, communityList[[1]])
    nodes <- merge(nodes, communityList[[2]])
    nodes <- merge(nodes, communityList[[3]])
    
# Prepare Nodes Dataframe for each D3 Graph
    #adjust Q1 and Q5 to high/medium/low
    #sum Q1 and Q5, with high/medium/low
    

    
        
     D3NodesList <- list()
     for(i in 1:length(totalNetworkInfo)) {
      nodesColNamesList <- c("id", socioDemographicVariables, paste(
        "Communities Relationship Question", i, sep = " "), paste(
          "Degree Relationship Question", i, sep = " "))
      names(nodes) <- sapply(names(nodes),
                         function(v) {gsub("\\."," ", as.character(v))})
      nodesDF <- nodes[,nodesColNamesList]
#Rename Communities Relationship Question i to community to suit the D3 vis code
      communityName <- length(names(nodesDF))-1
      names(nodesDF)[communityName] <- "community"
#Rename Degree Relationship Question i to community to suit the D3 vis code      
      degreeName <- length(names(nodesDF))
      names(nodesDF)[degreeName] <- "Degree"
      
      
      D3NodesList[[i]] <- nodesDF
     }
   
#remove numeric response columns from nodes
    nodes <- nodes %>% select(!contains('numeric'))
    

  
    classDashAnalysisOutput <- list(clientName,
                                    className,
                                    seed,
                                    totalNetworkInfo,
                                    homophylyList,
                                    isolatesList,
                                    SSNQGraphList,
                                    SSNQCommunitiesDataList,
                                    SSNQNetworkD3,
                                    edgeDataList,
                                    nodes,
                                    dendrogramList,
                                    polarGraphList,
                                    degreeAllHistogramList,
                                    degreeInHistogramList,
                                    degreeOutHistogramList,
                                    HSList,
                                    ASList,
                                    overallHealthList,
                                    overallPfIList,
                                    overallScoresList,
                                    D3NodesList,
                                    rawEdgesList)

    names(classDashAnalysisOutput) <-  c("clientName",
                                         "className",
                                         "seed",
                                         "totalNetworkInfo",
                                         "homophylyList",
                                         "isolatesList",
                                         "SSNQGraphList",
                                         "SSNQCommunitiesDataList",
                                         "SSNQNetworkD3",
                                         "edgeDataList",
                                         "nodes",
                                         "dendrogramList",
                                         "polarGraphList",
                                         "degreeAllHistogramList",
                                         "degreeInHistogramList",
                                         "degreeOutHistogramList",
                                         "HSList",
                                         "ASList",
                                         "overallHealthList",
                                         "overallPfIList",
                                         "overallScoresList",
                                         "D3NodesList",
                                         "rawEdgesList")
    
    filename <- paste(clientName, className, "S to S Dash Data.rds", sep = " ")
    write_rds(classDashAnalysisOutput, as.character(filename))
  
}


for(i in 1:length(fileList)) {
  SSDashAnalysis(file = fileList[[i]])
}
  