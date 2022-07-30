#This script takes data output from Polinode, and processes it for use in the
#IBSC Class dashboard.  To use, put the .xlsx output from all the surveys for a
#given school from polinode into a directory, add the path to fileList below and
#make sure the file name is in the format ".Student <class name>.xlsx".
#then edit the survey information and the client information in surveyConfig.R
#and clientConfig.R and source the script. The output will be in the working
#directory (the .RDS files needed by the dashboard). The .xlsx and this file do
#not need to be published to shinyApps.io


SSDashAnalysis <- function (schoolDataInput, listNumberStudent) {
  
  #listNumberStudent <- 8
  
#Pull in config info
  source("surveyConfig.R")
  source("clientConfig.R")
  
  #import student responses for a given class as nodes, edges1...edgesn and assign data to
  #appropriate variables
  organiseClassData <- function(...) {
    dfNameVecS <- vector()
      dfNameVecS[[1]] <- "nodes"
      for(j in 2:length(schoolDataList$studentResponses[[listNumberStudent]])) {
        dfNameVecS[j] <- paste("edges", j-1, sep = "")
      }
      for(j in 1:length(dfNameVecS)) {
        do.call("<<-",list(dfNameVecS[j],schoolDataList$studentResponses[[listNumberStudent]][[j]]))
      }
    }
  organiseClassData(schoolDataList, listNumberStudent)
  
  
#extract class name from file  
  className <- substring(names(schoolDataInput$studentResponses)[[listNumberStudent]], regexpr("Student", names(schoolDataInput$studentResponses)[[listNumberStudent]]) + 8)
  className <- substring(className, 1, nchar(className)-5) #remove ".xlsx"

  #set random seed from config file
  set.seed (seed)
 
  source("assets/R/makeResponseNumeric.R")
 
#convert data to numeric where necessary
  
  #make the numeric keypair dataframes from which to convert node answers;
  #function in makeResponseNumeric.R
  
  nodesKeyBelongingnessPositive <- keyToNumeric(scoreData = nodes[,responseColumnsBelongingness],
                                        answers = nodeAnswersBelongingness,
                                        normalized = FALSE)
  
  nodesKeyBelongingnessNegative <- nodesKeyBelongingnessPositive
  for (i in 1:length(nodesKeyBelongingnessNegative$newAnswers)) {
    nodesKeyBelongingnessNegative$newAnswers[i] <- 3 - nodesKeyBelongingnessNegative$newAnswers[i]
  } 
  
 
  nodesKeyBelongingnessCategorical <- data.frame(nodeAnswersBelongingness)
  nodesKeyBelongingnessCategorical$newAnswers <- c("High", "Moderate", "Moderate", "Low")
  
  nodesKeyStudentTeacher <- keyToNumeric(scoreData = nodes[,responseColumnsStudentTeacher],
                                         answers = nodeAnswersStudentTeacher,
                                         normalized = FALSE)
  
  nodesKeyStudentTeacherCategorical <- data.frame(nodeAnswersStudentTeacher)
  nodesKeyStudentTeacherCategorical$newAnswers <- c("High", "High", "Moderate", "Moderate", "Low", "Low")
  names(nodesKeyStudentTeacherCategorical) <- c("answers", "newAnswers")
  
  #loop over each relevant column for additional measures such as belongingness and student-teacher questions and bind a numeric column to nodes
  #get column numbers
  studentTeacherRegExQuestionText <- c(
    "I can talk to or contact my",
    "It is worth building a good relationship with my ",
    " and I have shared goals for my progress and development",
    "cares about me",
    "has a good understanding of my skills and interests",
    "inspires and motivates me",
    "recognises and rewards my efforts"
  )
  studentTeacherResponseColumnIndices <- vector()
  for(i in 1:length(studentTeacherRegExQuestionText)) {
   studentTeacherResponseColumnIndices[[length(studentTeacherResponseColumnIndices)+1]] <- grep(studentTeacherRegExQuestionText[i], colnames(nodes))
  }
  if(length(studentTeacherResponseColumnIndices)>7) {
    warning("more student - teacher questions have been matched than there should be.  Consider checking the studentTeacherRegExQuestionText for unwanted matches")
  }
  if(length(studentTeacherResponseColumnIndices)<7) {
    warning("fewer student - teacher questions have been matched than there should be.  Consider checking the data frame colnames() for altered wording or misspelling")
  }
  
  belongingnessRegExQuestionTextPositive <- c(
    "I feel like I belong at",
    "I make friends easily at",
    " seem to like me"
  )
  
  belongingnessRegExQuestionTextNegative <- c(
    "I feel awkward and out of place",
    "I feel like an outsider",
    "I feel lonely at"
  )
  
  belongingnessResponseColumnIndicesPositive <- vector()
  for(i in 1:length(belongingnessRegExQuestionTextPositive)) {
    belongingnessResponseColumnIndicesPositive[[length(belongingnessResponseColumnIndicesPositive)+1]] <- grep(belongingnessRegExQuestionTextPositive[i], colnames(nodes))
  }
  if(length(belongingnessResponseColumnIndicesPositive)>3) {
    warning("more belongingness questions have been matched than there should be.  Consider checking the belongingnessRegExQuestionTextPositive for unwanted matches")
  }
  
  belongingnessResponseColumnIndicesNegative <- vector()
  for(i in 1:length(belongingnessRegExQuestionTextNegative)) {
    belongingnessResponseColumnIndicesNegative[[length(belongingnessResponseColumnIndicesNegative)+1]] <- grep(belongingnessRegExQuestionTextNegative[i], colnames(nodes))
  }
  if(length(belongingnessResponseColumnIndicesNegative)>3) {
    warning("more belongingness questions have been matched than there should be.  Consider checking the belongingnessRegExQuestionTextNegative for unwanted matches")
  }
  
  #make numeric columns for belongingness positive questions (0-3)
  for(i in 1:length(nodes[,belongingnessResponseColumnIndicesPositive])) {
    numericReplacementVectorPositive <- makeResponseNumeric(
      data = nodes[,belongingnessResponseColumnIndicesPositive[i]],
      conversionKey = nodesKeyBelongingnessPositive)
    numericReplacementColumnPositive <- data.frame(numericReplacementVectorPositive)
    getNames <- names(nodes)
    names(numericReplacementColumnPositive) <- paste0(
      getNames[[belongingnessResponseColumnIndicesPositive[[i]]]], ".numeric")
    nodes <- cbind(nodes, numericReplacementColumnPositive)
  }
  
  #make numeric columns for belongingness negative questions (3-0)
  for(i in 1:length(nodes[,belongingnessResponseColumnIndicesNegative])) {
    numericReplacementVectorNegative <- makeResponseNumeric(
      data = nodes[,belongingnessResponseColumnIndicesNegative[i]],
      conversionKey = nodesKeyBelongingnessNegative)
    numericReplacementColumnNegative <- data.frame(numericReplacementVectorNegative)
    getNames <- names(nodes)
    names(numericReplacementColumnNegative) <- paste0(
      getNames[[belongingnessResponseColumnIndicesNegative[[i]]]], ".numeric")
    nodes <- cbind(nodes, numericReplacementColumnNegative)
  }
  
  belongingnessResponseColumnIndices <- c(
    belongingnessResponseColumnIndicesPositive,
    belongingnessResponseColumnIndicesNegative)
  belongingnessResponseColumnIndices <- sort(belongingnessResponseColumnIndices)

#make stratified columns for belongingness (low, medium, high)  
  modifiedNodes <- nodes[,belongingnessResponseColumnIndices]
  originalNames <- names(modifiedNodes)
  names(modifiedNodes) <- c("a", "b", "c", "d", "e", "f")
  replacementNames <- names(modifiedNodes)
  
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("a" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[1], "(stratified)", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("b" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[2], "(stratified)", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("c" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[3], "(stratified)", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("d" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[4], "(stratified)", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("e" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[5], "(stratified)", sep = ".")
  modifiedNodes <- modifiedNodes %>%
    left_join(nodesKeyBelongingnessCategorical, by = c("f" = "nodeAnswersBelongingness"))
  names(modifiedNodes)[names(modifiedNodes) == 'newAnswers'] <-  paste(originalNames[6], "(stratified)", sep = ".")
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
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[1], "(stratified)", sep = ".")
 
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("b" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[2], "(stratified)", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("c" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[3], "(stratified)", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("d" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[4], "(stratified)", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("e" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[5], "(stratified)", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("f" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[6], "(stratified)", sep = ".")
  modifiedNodesST <- modifiedNodesST %>%
    left_join(nodesKeyStudentTeacherCategorical, by = c("g" = "answers"))
  names(modifiedNodesST)[names(modifiedNodesST) == 'newAnswers'] <-  paste(originalNamesST[7], "(stratified)", sep = ".")
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

    #Function to set up the edge data for porting to D3.JS
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
            x[j,3] <- x[j,3]+1
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
    
    #calculate reciprocity on the whole dataset to later slot into totalNetworkInfo list]
    reciprocityFullEdgeList <- function(edgeList) {
      rec <- edgeList 
      rec$Network <- as.character(rec$Network)
      recLevels <- levels(as.factor(rec$Network))
      recList <- list()
      nList <- list()
      for(i in 1:length(recLevels)) {
        filteredRec <- filter(rec, Network == recLevels[i])
        nList[[i]] <- nrow(filteredRec)
        if(nList[[i]]>0){
          r <- graph_from_data_frame(filteredRec, directed = T)
          recList[[i]] <- reciprocity(r)
        }
      }
      #combine all the values in recList into a single reciprocity score
      recDF <- data.frame(unlist(recList), unlist(nList))
      normalizedRec <- list()
      for(i in 1:nrow(recDF)){
        normalizedRec[i] <- as.numeric(recDF[i,1])*as.numeric(recDF[i,2])
      }
      overallReciprocity <-sum(unlist(normalizedRec))/sum(unlist(nList))
      
    }
    
    questionReciprocityList <- list()
    for(i in 1:length(rawEdgesList)) {
      questionReciprocityList[i] <- reciprocityFullEdgeList(rawEdgesList[[i]])
    }
    
    overallReciprocity <- mean(unlist(questionReciprocityList))
    questionReciprocityListComplete <- unlist(append(questionReciprocityList, overallReciprocity))

    #Build an overall edgelist for a class summary page.
    #build from unfiltered data
    overallEdgeList <- list()
    for(i in 1:length(rawEdgesList)){
      overallEdgeList[[i]] <- rawEdgesList[[i]][c("Source", "Target", "Network")]
    }
    
    overallEdgesList <- do.call("rbind", overallEdgeList)
    overallEdgesList$Score <- rep(1, nrow(overallEdgesList))
    #filter entries under a given score threshold 0.5 
    overallEdgesList <- filter(overallEdgesList, Network > 0.5)
    # Reorder values in first 2 cols and cbind with df[,3]
    m <- cbind(t(apply(overallEdgesList[1:2], 1, sort)), overallEdgesList[,3])
    # Sum third column grouped by first and second column
    overallSSEdgesDF <- aggregate(as.numeric(m[,3]), by = list(m[,1],m[,2]),FUN=sum)
    #set mutual if score is higher than 3
    overallSSEdgesDF <- edgeDataSetup(overallSSEdgesDF)
    
    for(i in 1:nrow(overallSSEdgesDF)) {
      if(overallSSEdgesDF[i,"score"]>3){
        overallSSEdgesDF[i,"mutual"]<- 2
      }
      overallSSEdgesDF[i,"mutual"]
    }
    #filter all edges that lack reciprocity
    overallSSEdgesDF <- filter(overallSSEdgesDF, mutual == 2)
    
    #Generate summative statistics about the overall network map
    source("networkSurveyAnalysis.R")
    overallSSEdgesDF2 <- overallSSEdgesDF
    names(overallSSEdgesDF2) <- c("Source", "Target", "Mutual", "Network")
    
    overallNetworkInfo <- surveyDataAnalysis(questionData = overallSSEdgesDF2)
    #TODO: fix broken reciprocity score to factor in all of the edges drawn or undrawn


#perform summative network analysis on each relationship question
    totalNetworkInfo <- list()
    for(i in 1:length(surveyDataFiltered)) {
    totalNetworkInfo[[length(totalNetworkInfo)+1]] <-
      surveyDataAnalysis(questionData = surveyDataFiltered[[i]])
    }

    #Combine question and overall NetworkInfo lists
    totalNetworkInfo[[length(totalNetworkInfo)+1]] <- overallNetworkInfo

    for (i in 1:length(totalNetworkInfo)){
      totalNetworkInfo[[i]][[6]] <- questionReciprocityListComplete[i]
    }
        
#Insert HTML into a list to display it properly in-dash
    formatDataForDisplay <- function(Data) {
      Data <- na.omit(Data)
      ogValue <- Data[length(Data)]
      formattedValue <- paste(Data, "<br>", sep=" ")
      formattedValue[length(formattedValue)] <- ogValue
      return(formattedValue)
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
  surveyDataFiltered[[length(surveyDataFiltered)+1]] <- overallSSEdgesDF2
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
    

    edgeDataList <- list()
    thing <- SSNQGraphList[[1]]
    edgeDataList[[1]] <- edgeDataSetup(Graph2[[1]])
    edgeDataList[[2]] <- edgeDataSetup(Graph2[[2]])    
    edgeDataList[[3]] <- edgeDataSetup(Graph2[[3]])
    edgeDataList[[4]] <- overallSSEdgesDF2
    #TODO: maybe reset score column in each to all 1's
    
# Prepare Dendrograms
    dendrogramList <- list()
    for (i in 1:length(SSNQCommunitiesDataList)){
      if(length(membership(SSNQCommunitiesDataList[[i]]))>1){
        dendrogramList[[i]] <- ggplotly(ggdendro::ggdendrogram(as.dendrogram(SSNQCommunitiesDataList[[i]])))
      } else {
        dendrogramList[[i]] <- ggplotly(NULL)
      }
    }
    
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
  overallSSEdgesDF2 <- as.data.frame(overallSSEdgesDF2)
  polarList <- c(surveyData, list(overallSSEdgesDF2))
  source("generatePolar.R")
  polarGraphList <- list()
  for(i in 1:length(polarList)) {
    polarGraphList[[i]] <- generatePolar(data = polarList[[i]])
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
      for(j in 1:4) {
       names(totalNetworkInfo[[i]][[j]])[names(totalNetworkInfo[[i]][[j]])
                                         == "People"] <- "id"
       names(totalNetworkInfo[[i]][[j]])[names(totalNetworkInfo[[i]][[j]])
                                         == "Degree"] <- 
         paste("Degree Relationship Question", i, j, sep = " ") 
      }
    }
    
    for(i in 1:length(totalNetworkInfo)) {
       nodes <- merge(nodes, totalNetworkInfo[[i]][[3]])
    }
    
#add community data to nodes for output to .RDS
    communityList <- list()
    for(i in 1:length(totalNetworkInfo)) {
      community <- t(as.data.frame(as.list(SSNQMembersList[[i]]), check.names = FALSE))
      community <- as.data.frame(community)
      community$id <- rownames(community)
      rownames(community) <- NULL
      community$V1 <- sapply(community$V1, function(i) letters[i])
      names(community)[names(community)=="V1"] <- paste(
        "Communities Relationship Question", i, sep = " ")
  #    community$id <- sapply(community$id,
  #                           function(v) {gsub("\\."," ", as.character(v))})
      rownames(community) <- community$id
      communityList[[i]] <- community
    }
    nodes <- merge(nodes, communityList[[1]], all = T)
    nodes <- merge(nodes, communityList[[2]], all = T)
    nodes <- merge(nodes, communityList[[3]], all = T)
    nodes <- merge(nodes, communityList[[4]], all = T) #overall data
    
    
# Prepare Nodes Dataframe for each D3 Graph
    #sum Q1 and Q5, with high/medium/low
    
    # Process student - teacher numeric data by finding the relevant numeric columns in nodes
    studentTeacherRegExQuestionTextNumeric <- c(
      "I can talk to or contact my .*numeric$",
      "It is worth building a good relationship with my .*numeric$",
      " and I have shared goals for my progress and development.*numeric$",
      "cares about me.*numeric$",
      "has a good understanding of my skills and interests.*numeric$",
      "inspires and motivates me.*numeric$",
      "recognises and rewards my efforts.*numeric$"
    )
    studentTeacherResponseColumnIndicesNumeric <- vector()
    for(i in 1:length(studentTeacherRegExQuestionTextNumeric)) {
      studentTeacherResponseColumnIndicesNumeric[[length(studentTeacherResponseColumnIndicesNumeric)+1]] <- grep(studentTeacherRegExQuestionTextNumeric[i], colnames(nodes))
    }
    if(length(studentTeacherResponseColumnIndicesNumeric)>7) {
      warning("more student - teacher question numeric columns have been matched than there should be.  Consider checking the studentTeacherRegExQuestionTextNumeric for unwanted matches")
    }
    
    #add a 'studentTeacherSumNumeric' column to nodes
    stNumeric <- nodes[,studentTeacherResponseColumnIndicesNumeric]
    nodes$StudentTeacherSumNumeric <- rowSums(stNumeric)

    #add a 'student - teacher score' column to nodes containing stratified data
    #current stratification thresholds 25% - 50% - 25%
    # 0-35 total
    # 0-9 low
    # 10-25 moderate
    # 26-35 high
  stStratified <- vector()
  for(i in 1:length(nodes$StudentTeacherSumNumeric)) {
    if(is.na(nodes$StudentTeacherSumNumeric[i])){
      stStratified[i] <- "Not Reported"
    } else if(nodes$StudentTeacherSumNumeric[i] >25) {
      stStratified[i] <- "High"
    } else if (nodes$StudentTeacherSumNumeric[i] >9) {
      stStratified[i] <- "Moderate"
    } else {
      stStratified[i] <- "Low"
    }
  }
  nodes$`Student - Teacher Relationship Score` <- stStratified

# Process belongingness numeric data by finding the relevant numeric columns in nodes
  belongingnessRegExQuestionTextNumeric <- c(
    "I feel like I belong at.*numeric$",
    "I make friends easily at.*numeric$",
    " seem to like me.*numeric$",
    "I feel awkward and out of place.*numeric$",
    "I feel like an outsider.*numeric$",
    "I feel lonely at.*numeric$"
  )
  
  belongingnessResponseColumnIndicesNumeric <- vector()
  for(i in 1:length(belongingnessRegExQuestionTextNumeric)) {
    belongingnessResponseColumnIndicesNumeric[[length(belongingnessResponseColumnIndicesNumeric)+1]] <- grep(belongingnessRegExQuestionTextNumeric[i], colnames(nodes))
  }  
    
  if(length(belongingnessResponseColumnIndicesNumeric)>6) {
    warning("more belongingness question numeric columns have been matched than there should be.  Consider checking the belongingnessRegExQuestionTextNumeric for unwanted matches")
  }
  
  #add a 'belongingnessMeanNumeric' column to nodes
  belongingnessNumeric <- nodes[,belongingnessResponseColumnIndicesNumeric]
  nodes$belongingnessMeanNumeric <- rowMeans(belongingnessNumeric)
  for (i in 1:length(nodes$belongingnessMeanNumeric)) {
    nodes$belongingnessMeanNumeric[[i]] <- round(nodes$belongingnessMeanNumeric[[i]], 2)
  } 
  
  #add a 'Belongingness Score' column to nodes containing stratified data
  #current stratification thresholds 25% - 50% - 25%
  #0-3 x6
  #0-0.8333333 Low (0-1)
  #1-2 Moderate (1-2)
  #2.166667-3 High (2-3)
  
  belongingnessStratified <- vector()
  for(i in 1:length(nodes$belongingnessMeanNumeric)) {
    if(is.na(nodes$belongingnessMeanNumeric[i])) {
      belongingnessStratified[i] <- "Not Reported"    
    } else if(nodes$belongingnessMeanNumeric[i] > 2) {
      belongingnessStratified[i] <- "High"
    } else if (nodes$belongingnessMeanNumeric[i] > 1) {
      belongingnessStratified[i] <- "Moderate"
    } else if (nodes$belongingnessMeanNumeric[i] >= 0) {
      belongingnessStratified[i] <- "Low"
    } else {
      belongingnessStratified[i] <- "Not Reported"
    }
  }
  nodes$`Belongingness Score` <- belongingnessStratified

     stratifiedData <- grep("\\(stratified\\)$", colnames(nodes))
     stratifiedDataNames <- names(nodes[stratifiedData])
     stratifiedDataNames <- sapply(stratifiedDataNames,
                                   function(v) {gsub("\\."," ", as.character(v))})
     D3NodesList <- list()
     for(i in 1:length(totalNetworkInfo)) {
      nodesColNamesList <- c("id", socioDemographicVariables, "Student - Teacher Relationship Score", "Belongingness Score", stratifiedDataNames, paste(
        "Communities Relationship Question", i, sep = " "), paste(
          "Degree Relationship Question", i, "3", sep = " "))
      names(nodes) <- sapply(names(nodes),
                             function(v) {gsub("\\."," ", as.character(v))})
      names(nodesColNamesList) <- sapply(names(nodesColNamesList),
                                         function(v) {gsub("\\."," ", as.character(v))})
      
      nodesDF <- nodes[,nodesColNamesList]
#Rename Communities Relationship Question i to community to suit the D3 vis code
      communityName <- length(names(nodesDF))-1
      names(nodesDF)[communityName] <- "community"
#Rename Degree Relationship Question i to community to suit the D3 vis code      
      degreeName <- length(names(nodesDF))
      names(nodesDF)[degreeName] <- "Degree"
      
      names(nodes) <- sapply(names(nodes),
                             function(v) {gsub("\\."," ", as.character(v))})
      names(nodesColNamesList) <- sapply(names(nodesColNamesList),
                                         function(v) {gsub("\\."," ", as.character(v))})
      
      D3NodesList[[i]] <- nodesDF
     }
     
   #perform homophylytic analysis
   source("calculateHomophyly.R")
   testingVariables <- c(socioDemographicVariables,
                         "Belongingness Score",
                         "Student - Teacher Relationship Score")
   homophylyList <- list()
   for(i in 1:length(surveyDataFiltered)) {
     homophylyList[[length(homophylyList)+1]] <- calculateHomophyly(
       homophylyEdgeList = surveyDataFiltered[[i]],
       testingVariables = testingVariables,
       nodes = nodes)
     
   }

   #format homophylyList for display in-dash
   for(i in 1:length(homophylyList)) {   
     predictiveSDVSSNQ <- levels(as.factor(unlist(homophylyList[[i]][[1]])))
     nonPredictiveSDVSSNQ <- levels(as.factor(unlist(homophylyList[[i]][[2]])))
     indeterminateSDV <- levels(as.factor(unlist(homophylyList[[i]][[3]])))
     if(length(predictiveSDVSSNQ > 0)) {
       homophylyList[[i]][[1]] <-
         formatDataForDisplay(predictiveSDVSSNQ)
     }
     if(length(nonPredictiveSDVSSNQ > 0)) {
       homophylyList[[i]][[2]] <-
         formatDataForDisplay(nonPredictiveSDVSSNQ)
     }
     if(length(indeterminateSDV > 0)) {
       homophylyList[[i]][[3]] <-
         formatDataForDisplay(indeterminateSDV)
     }
   }
   
  #collate belongingness data frame
  socioDemographicVariables <- sapply(socioDemographicVariables,
                                      function(v) {gsub("\\."," ", as.character(v))})
  
  deleteVector <- vector()
  for (i in 1:length(socioDemographicVariables)) {
    if(socioDemographicVariables[i] %in% names(nodes)) {
      if(length(levels(as.factor(nodes[,socioDemographicVariables[i]]))) <= 1) {
        deleteVector[length(deleteVector)+1] <- i
      } else if (length(levels(as.factor(nodes[,socioDemographicVariables[i]]))) == nrow(nodes)) {
        deleteVector[length(deleteVector)+1] <- i
      }
    }
  }
  
  indeterminateChoicesBelongingness <- socioDemographicVariables[c(deleteVector)]
  socioDemographicVariablesBelongingness <- socioDemographicVariables[-c(deleteVector)]
  
  belongingnessDF <- nodes[,c("id",
                               socioDemographicVariablesBelongingness,
                               "Communities Relationship Question 1",
                               "Communities Relationship Question 2",
                               "Communities Relationship Question 3",
                               "Student - Teacher Relationship Score",
                               "Belongingness Score")]
  names(belongingnessDF)[names(belongingnessDF) == "Belongingness Score"] <- "Belongingness Stratified" 
  names(belongingnessNumeric) <- c("I feel like I belong",
                                   "I make friends easily",
                                   "Other students like me",
                                   "I feel awkward",
                                   "I feel like an outsider",
                                   "I feel lonely")
  belongingnessDF <- cbind(belongingnessDF, belongingnessNumeric)
  belongingnessDF <- cbind(belongingnessDF, nodes$belongingnessMeanNumeric)
  names(belongingnessDF)[names(belongingnessDF) == "nodes$belongingnessMeanNumeric"] <- "Belongingness Mean"
  #add student teacher measure to belongingnessDF
  names(modifiedNodesST) <- c("Teacher Contact S-T",
                              "Relationship Building S-T",
                              "Shared Goals S-T",
                              "Care S-T",
                              "Understanding S-T",
                              "Inspiration S-T",
                              "Recognition S-T")
  if(!nrow(modifiedNodesST) == nrow(belongingnessDF)) {
    modifiedNodesST<- modifiedNodesST %>% filter(if_any(everything(), ~ !is.na(.)))
  }
  belongingnessDF <- cbind(belongingnessDF, modifiedNodesST)

  #Add peer to peer network characteristics to belongingnessDF
  degreeList <- list()
  for(i in 1:length(totalNetworkInfo)) {
  networkInfo <- merge(totalNetworkInfo[[i]][[1]],
                           totalNetworkInfo[[i]][[2]],
                           by.x = "id", by.y = "id")
  networkInfo <- merge(networkInfo,
                           totalNetworkInfo[[i]][[3]],
                           by.x = "id", by.y = "id")
  names(networkInfo) <- c("id",
                              paste("Degree In S-S", RQTitles[i], sep = " "),
                              paste("Degree Out S-S", RQTitles[i], sep = " "),
                              paste("Degree All S-S", RQTitles[i], sep = " "))
  degreeList[[i]] <- networkInfo
  }
  
  for (i in 1:length(degreeList)) {
    belongingnessDF <- merge(belongingnessDF, degreeList[[i]],
                             by.x = "id", by.y = "id")
  }
  
  degreeIndices <- ncol(belongingnessDF) -9
for (i in degreeIndices:ncol(belongingnessDF)) {
  belongingnessDF[,i] <-as.character(belongingnessDF[,i])
}

#gather data for school summary page raincloud plots of extra measures (belongingness/S-T etc.)
  nodesRainCloud <- nodes
  
#remove numeric response columns from nodes
    nodes <- nodes %>% select(!contains('numeric'))
#remove stratified response columns from nodes
    nodes <- nodes %>% select(!contains('(stratified)'))    
  
#Assemble output object
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
                                    overallSSEdgesDF,
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
                                    rawEdgesList,
                                    belongingnessDF,
                                    nodesRainCloud)

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
                                         "overallSSEdgesDF",
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
                                         "rawEdgesList",
                                         "belongingnessDF",
                                         "nodesRainCloud")
#Write output object to disk as a .rds    
    filename <- paste(clientName, className, "S to S Dash Data.rds", sep = " ")
    write_rds(classDashAnalysisOutput, as.character(filename))
  
}

  