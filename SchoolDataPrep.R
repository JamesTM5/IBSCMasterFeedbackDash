#This script takes data output from Polinode, and processes it for use in the
#IBSC Class dashboard.  To use, put the .xlsx output from all the surveys for a
#given school from polinode into a directory, add the path to fileList below and
#make sure the file name is in the format ".Student <class name>.xlsx".
#then edit the survey information and the client information in surveyConfig.R
#and clientConfig.R and source the script. The output will be in the working
#directory (the .RDS files needed by the dashboard). The .xlsx and this file do
#not need to be published to shinyApps.io


SSDashAnalysis <- function (studentDataInput,
                            teacherDataInput,
                            listNumberStudent) {
  
       # listNumberStudent <- 3
       # studentDataInput <- schoolDataList$studentResponses
       # teacherDataInput <- schoolDataList$teacherResponses

#Pull in config info
  source("surveyConfig.R")
  source("clientConfig.R")
  
  #import student responses for a given class as nodes, edges1...edgesn and assign data to
  #appropriate variables
  organiseClassData <- function(...) {
    dfNameVecS <- vector()
      dfNameVecS[[1]] <- "nodes"
      for(j in 2:length(studentDataInput[[listNumberStudent]])) {
        dfNameVecS[j] <- paste("edges", j-1, sep = "")
      }
      for(j in 1:length(dfNameVecS)) {
        do.call("<<-",list(dfNameVecS[j],studentDataInput[[listNumberStudent]][[j]]))
      }
    }
  organiseClassData(studentDataInput, listNumberStudent)
  
#extract class name from file  
  className <- substring(names(studentDataInput)[[listNumberStudent]], regexpr("Student", names(studentDataInput)[[listNumberStudent]]) + 8)
  className <- substring(className, 1, nchar(className)-5) #remove ".xlsx"

  #set random seed from config file
  set.seed (seed)
  
  source("assets/R/makeResponseNumeric.R")
  
  #marry the corresponding teacher data with the student data where necessary
  teacherClassNameList <- list()
  for (m in 1:length(teacherDataInput)){
    TclassName <- substring(names(teacherDataInput)[[m]], regexpr("Teacher",
                  names(teacherDataInput)[[m]]) + 8)
    teacherClassNameList[[m]] <- substring(TclassName, 1, nchar(TclassName)-5) #remove ".xlsx"
  }
  teacherDataPresent <- FALSE
  if (className %in% teacherClassNameList) {
    teacherDataPresent <- TRUE
   #   isolate this class' teacher data
    teacherData <- teacherDataInput[[which(grepl(className,teacherClassNameList)==TRUE)]]
    teacherData <- teacherData[2:length(teacherData)]
   # for each of 2:length teacherData, if ncol = 4, take cols 2&3, order by name
    # (col2) and add it to the nodes frame

    nameList <- vector()
    for (i in 1:length(teacherData)) {
      nameList <- c(nameList, teacherData[[i]]$Target)
    }
    Target <- unique(sort(nameList))
    teacherAnswersFrame <- data.frame(Target)
    for (j in 1:length(teacherData)){
      if(ncol(teacherData[[j]]) == 4) {
        questionData <- teacherData[[j]][2:3]
        questionData <- questionData[order(questionData$Target),]
        teacherAnswersFrame <- merge(teacherAnswersFrame, questionData, by = "Target", all.x = T, all.y = T)
        }
      }

    if(ncol(teacherAnswersFrame)==7){
      names(teacherAnswersFrame) <- c(
        "Name",
        "Teacher-My communication with this student is highly effective.",
        "Teacher-Our relationship has a strong 'story' or timeline" ,
        "Teacher-I know this student well.",
        "Teacher-Our relationship is fair and respectful.",
        "Teacher-We are aligned in purpose and values.",
        "Teacher-There are opportunities to build our relationship")
      #make numeric columns for teacher/student questions (0-5)
      teacherDataNames <- teacherAnswersFrame[[1]]
      teacherAnswersFrame <- teacherAnswersFrame[2:ncol(teacherAnswersFrame)]
      numericAnswerColumns <- list()
      for(i in 1:ncol(teacherAnswersFrame)) {
        numericReplacementVector <- makeResponseNumeric(
          data = teacherAnswersFrame[[i]],
          conversionKey = nodesKeyStudentTeacher)
        numericReplacementColumn <- data.frame(numericReplacementVector)
        getNames <- names(teacherAnswersFrame)
        names(numericReplacementColumn) <- paste0(
          getNames[[i]], ".numeric")
        numericAnswerColumns[[i]] <- numericReplacementColumn
      }
      numericAnswers <- data.frame(numericAnswerColumns)
      #For each student, Take the mean
      numericAnswers$`Teacher-Student Mean` <- rowMeans(numericAnswers)

      #make stratified columns for teacher/student questions (low, medium, high)
      modifiedNodesTS <- teacherAnswersFrame
      originalNamesTS <- names(teacherAnswersFrame)
      names(modifiedNodesTS) <- c("a", "b", "c", "d", "e", "f")
      replacementNames <- names(modifiedNodesTS)

      modifiedNodesTS <- modifiedNodesTS %>%
        left_join(nodesKeyStudentTeacherCategorical, by = c("a" = "answers"))
      names(modifiedNodesTS)[names(modifiedNodesTS) == 'newAnswers'] <-  paste(originalNamesTS[1], "(stratified)", sep = ".")

      modifiedNodesTS <- modifiedNodesTS %>%
        left_join(nodesKeyStudentTeacherCategorical, by = c("b" = "answers"))
      names(modifiedNodesTS)[names(modifiedNodesTS) == 'newAnswers'] <-  paste(originalNamesTS[2], "(stratified)", sep = ".")

      modifiedNodesTS <- modifiedNodesTS %>%
        left_join(nodesKeyStudentTeacherCategorical, by = c("c" = "answers"))
      names(modifiedNodesTS)[names(modifiedNodesTS) == 'newAnswers'] <-  paste(originalNamesTS[3], "(stratified)", sep = ".")

      modifiedNodesTS <- modifiedNodesTS %>%
        left_join(nodesKeyStudentTeacherCategorical, by = c("d" = "answers"))
      names(modifiedNodesTS)[names(modifiedNodesTS) == 'newAnswers'] <-  paste(originalNamesTS[4], "(stratified)", sep = ".")

      modifiedNodesTS <- modifiedNodesTS %>%
        left_join(nodesKeyStudentTeacherCategorical, by = c("e" = "answers"))
      names(modifiedNodesTS)[names(modifiedNodesTS) == 'newAnswers'] <-  paste(originalNamesTS[5], "(stratified)", sep = ".")

      modifiedNodesTS <- modifiedNodesTS %>%
        left_join(nodesKeyStudentTeacherCategorical, by = c("f" = "answers"))
      names(modifiedNodesTS)[names(modifiedNodesTS) == 'newAnswers'] <-  paste(originalNamesTS[6], "(stratified)", sep = ".")
      stratifiedAnswers <- modifiedNodesTS[,7:ncol(modifiedNodesTS)]
      teacherAnswersFrame1 <- cbind(teacherAnswersFrame, numericAnswers)
      teacherAnswersFrame1 <- cbind(teacherAnswersFrame1, stratifiedAnswers)
      Name <- teacherDataNames
      teacherAnswersFrame1 <- cbind(Name, teacherAnswersFrame1)
    }
  }
   #nb this gets added to the nodes frame later in the script
 
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
  STFrame <- list()
  for(i in 1:length(nodes[,studentTeacherResponseColumnIndices])) {
    numericReplacementVector <- makeResponseNumeric(
      data = nodes[,studentTeacherResponseColumnIndices[i]],
      conversionKey = nodesKeyStudentTeacher)
    numericReplacementColumn <- data.frame(numericReplacementVector)
    getNames <- names(nodes)
    names(numericReplacementColumn) <- paste0(
      getNames[[studentTeacherResponseColumnIndices[[i]]]], ".numeric")
    nodes <- cbind(nodes, numericReplacementColumn)
    STFrame[[i]] <- numericReplacementColumn
  }
  STFrame <- as.data.frame(STFrame)
  STFrame$`Student-Teacher Mean` <- rowMeans(STFrame)
  `Student-Teacher Mean` <- STFrame$`Student-Teacher Mean`
  nodes <- cbind(nodes, `Student-Teacher Mean`)

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
  
  
  #Add teacher Answer Data to the nodes frame
  #confirm the nodes frame is the right order to add the teacher/student data to
  nodes <- nodes[order(nodes[[1]]),]
  names(nodes)[[1]] <- "Name"
  if(teacherDataPresent == TRUE) {
    nodes <- merge(nodes, teacherAnswersFrame1, by = "Name", all.x = T)
    teacherStudentData <- nodes
  } else {
    teacherStudentData <- data.frame()
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
      for(j in 1:3) {
       names(totalNetworkInfo[[i]][[j]])[names(totalNetworkInfo[[i]][[j]])
                                         == "People"] <- "id"
       names(totalNetworkInfo[[i]][[j]])[names(totalNetworkInfo[[i]][[j]])
                                         == "Degree"] <- 
         paste("Degree Relationship Question", i, j, sep = " ") 
      }
    }
    
    for(i in 1:length(totalNetworkInfo)) {
       nodes <- merge(nodes, totalNetworkInfo[[i]][[3]], all = T)
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

  prepBelongingness <- function(belongingnessDF){
    belongingnessDataPrepList[[1]] <- belongingnessDF
    belongingnessDataPrepList[[2]] <- belongingnessDataPrepList[[1]]
    belongingnessDataPrepList[[3]] <- belongingnessDataPrepList[[1]]
    belongingnessDataPrepList[[4]] <- as.list(names(belongingnessDataPrepList[[3]]))
    names(belongingnessDataPrepList[[4]]) <- paste(names(belongingnessDataPrepList[[3]]), " (", map(belongingnessDataPrepList[[3]],~length(unique(.x))), ")") 
    belongingnessDataPrepList[[5]] <- names(select_if(belongingnessDataPrepList[[3]], is.numeric))
    belongingnessDataPrepList[[4]] <- belongingnessDataPrepList[[4]][!belongingnessDataPrepList[[4]] %in% belongingnessDataPrepList[[5]]]
    belongingnessDataPrepList[[6]] <- "9pt"
    names(belongingnessDataPrepList) <- c("dataFileBelongingness",
                                          "unprocessedBelongingnessData",
                                          "WBNumeric",
                                          "GBChoices",
                                          "metricChoices",
                                          "belongingnessFontSize")
    belongingnessDataPrepList
  }
  
  #significance testing for belongingness dashboard
  #A function to find significant differences between factors in the belongingness survey
  
  # belongingnessSignificanceTesting <- function (data){
  #   
  #   #prep data for analysis
  #   prunedDataSDV <- data %>% discard(is.numeric)
  #   prunedDataExperimental <- data %>% keep(is.numeric)
  #   #make sure each column of prunedDataSDV is class(factor)
  #   for(i in 1:ncol(prunedDataSDV)) {
  #     prunedDataSDV[,i] <- as.factor(prunedDataSDV[,i])
  #   }
  #   #remove "id" from the first column
  #   prunedDataSDV <- prunedDataSDV[,-1]
  #   #remove belongingness Stratified
  #   prunedDataSDV <- subset(prunedDataSDV, select = -`Belongingness Stratified`)
  #   
  #   #initiate some lists
  #   significance <- list()
  #   significantPredictors <- list()
  #   interactionEffects <- list()
  #   predictors <- list()
  #   
  #   
  #   #Choose the right comparison of means test
  #   #compare each subset of each sdv with every single experimental variable
  #   for (i in 1:ncol(prunedDataSDV)){
  #     for (j in 1:ncol(prunedDataExperimental)) {
  #       #if predictor categorical
  #       if (class(prunedDataSDV[[i]]) == "factor") {
  #         x <- prunedDataExperimental[[j]]
  #         y <- as.numeric(as.factor(as.character(prunedDataSDV[[i]])))
  #         #t-test
  #         if (length(levels(prunedDataSDV[[i]])) < 2) {
  #           result <- 1
  #         } else if (length(levels(prunedDataSDV[[i]])) == 2) {
  #           factorCountVec <- as.vector(table(prunedDataSDV[[i]]))
  #           factorCountVecPruned <- factorCountVec[factorCountVec>1]
  #             if(length(factorCountVecPruned)<2){
  #               result <- 1
  #             } else if (length(unique(x)) == 1 && length(unique(y)) == 1){
  #               result <- 1
  #             } else {
  #               tt <- t.test(x ~ y)
  #               result <- tt$p.value
  #             }
  #           #ANOVA
  #         } else if (length(levels(prunedDataSDV[[i]])) > 2) {
  #           factorCountVec <- as.vector(table(prunedDataSDV[[i]]))
  #           factorCountVecPruned <- factorCountVec[factorCountVec>2]
  #           if(length(factorCountVecPruned)<2){
  #             result <- 1
  #           } else {
  #           anova <- aov(x ~ y)
  #           #TODO: check for balance with replications function
  #           result <- summary(anova)[[1]][["Pr(>F)"]][[1]]
  #         }
  #         resultName <- paste(names(prunedDataExperimental)[j], "by", 
  #                             names(prunedDataSDV)[i], sep = " ")
  #         names(result) <- resultName
  #         significance <- append(result, significance)
  #         if (result < .01) {
  #           significantPredictor <- "Highly Significant"
  #           names(significantPredictor) <- paste(names(prunedDataExperimental)[j], "by", 
  #                                                names(prunedDataSDV)[i], sep = " ")
  #           significantPredictors <- append(significantPredictor,
  #                                           significantPredictors)
  #           predictors <- append(names(prunedDataSDV)[i], predictors)
  #         } else if (result < .05) {
  #           significantPredictor <- "Significant"
  #           names(significantPredictor) <- paste(names(prunedDataExperimental)[j], "by", 
  #                                                names(prunedDataSDV)[i], sep = " ")
  #           significantPredictors <- append(significantPredictor,
  #                                           significantPredictors)
  #           predictors <- append(names(prunedDataSDV)[i], predictors)
  #         }
  #       } else {
  #         cat ("data in an unsupported format")
  #       }
  #     }
  #   }
  #   
  #   #For every predictive variable, pair it with every other SDV in turn and look for an interaction effect
  #   
  #   #find every unique combination of those SDVs
  #   expand.grid.unique <- function(x, y, include.equals=FALSE) {
  #     x <- unique(x)
  #     y <- unique(y)
  #     g <- function(i) {
  #       z <- setdiff(y, x[seq_len(i-include.equals)])
  #       if(length(z)) cbind(x[i], z, deparse.level=0)
  #     }
  #     do.call(rbind, lapply(seq_along(x), g))
  #   }
  #   
  #   if(length(predictors) > 0) {
  #   ANOVACombos <- expand.grid.unique(predictors, names(prunedDataSDV))
  #   #for every unique combination of 2 SDV combo see if there is highly significant predictive power
  # 
  #     for (k in 1:nrow(ANOVACombos)) {
  #       SDV1 <- unlist(ANOVACombos[k,1])
  #       SDV2 <- unlist(ANOVACombos[k,2])
  #       EV <- prunedDataExperimental$`Belongingness Mean`
  #       if((class(prunedDataSDV[[SDV1]]) == "factor") && (class(prunedDataSDV[[SDV2]]) == "factor")) {
  #         #perform a 2 way ANOVA
  #         IE <- aov(EV ~ prunedDataSDV[[SDV1]] * prunedDataSDV[[SDV2]])
  #         pvalues <- summary(IE)[[1]][["Pr(>F)"]]
  #         resultSDV1 <- pvalues[1]
  #         resultSDV2 <- pvalues[2]
  #         interactionEffect <- pvalues[3]
  #         if (is.na(resultSDV1) || is.na(resultSDV2)) {
  #           
  #         } else if (resultSDV1 > .05 || resultSDV2 > .05) {
  #           if(is.na(interactionEffect)) {
  #             
  #           } else if (interactionEffect < .01) {
  #             significantPredictor <- "Highly Significant"
  #             resultName <- paste(names(prunedDataExperimental)[j], "by", 
  #                                 SDV1, "and", SDV2, sep = " ")
  #             names(significantPredictor) <- resultName
  #             interactionEffects <- append(significantPredictor,
  #                                          interactionEffects)
  #           } else if (interactionEffect > .05) {
  #             significantPredictor <- "Significant"
  #             resultName <- paste(names(prunedDataExperimental)[j], "by", 
  #                                 SDV1, "and", SDV2, sep = " ")
  #             names(significantPredictor) <- resultName
  #           }
  #         }
  #       }
  #     }
  #   }
  # }
  #   
  #   return( list(significantPredictors, interactionEffects))
  #   #TODO: if predictor variable quantitative, figure out the right regression analysis
  # }
  belongingnessClassList <- list()
  belongingnessDataPrepList <- list()
 # prepBelongingness(belongingnessDF = belongingnessDF)
  
  # prepTotalBelongingness <- function (belongingnessDF) {
    belongingnessClassList[[1]] <- prepBelongingness(belongingnessDF = belongingnessDF)
    #remove overall degree columns to keep the function working
    belongingnessClassList[[2]] <- belongingnessClassList[[1]][[2]][1:(length(belongingnessClassList[[1]][[2]])-12)]
    # belongingnessClassList[[3]] <- belongingnessSignificanceTesting(belongingnessClassList[[2]])
    # if(length(belongingnessClassList[[3]])>0) {
    #   belongingnessClassList[[4]] <- belongingnessClassList[[3]][[2]]
    #   belongingnessClassList[[3]] <- belongingnessClassList[[3]][[1]]
    # } else {
    #   belongingnessClassList[[4]] <- list()
    # }
    #belongingnessPrepList = "dataFileBelongingness","unprocessedBelongingnessData","WBNumeric","GBChoices","metricChoices","belongingnessFontSize"
    #belongingnessClassList ... = prepList 2 = predictorsIn 3 = significantPredictors, 4 = interactionEffects
    
    #prep significance data for display
    # if(length(belongingnessClassList[[3]])>0) {
    #   predictionTable <- data.table::setDT(belongingnessClassList[[3]])
    #   values <- str_split(colnames(predictionTable), " by ")
    #   cols <- list()
    #   rows <- list()
    #   
    #   for (j in 1:length(values)) {
    #     first <- values[[j]] [1]
    #     second <- values [[j]] [2]
    #     cols <- append(cols, first)
    #     rows <- append(rows, second)
    #   }
    #   
    #   valueTable <- rbind(predictionTable, rows)
    #   valueTable <- rbind(valueTable, cols)
    #   belongingnessClassList[[5]] <- valueTable
    #   
    #   strengthTable <- data.frame(matrix(ncol = length(unique(unlist(cols))), nrow = 
    #                                        length(unique(unlist(rows)))))
    #   colnames(strengthTable) <- unique(unlist(cols))
    #   rownames(strengthTable) <- unique(unlist(rows))
    #   
    #   SDVTable <- data.frame(matrix(ncol = length(unique(unlist(cols))), nrow = 
    #                                   length(unique(unlist(rows)))))
    #   colnames(SDVTable) <- unique(unlist(cols))
    #   rownames(SDVTable) <- unique(unlist(rows))
    #   
    #   for (k in 1:ncol(valueTable)) {
    #     colName1 <- as.character(valueTable[3,..k])
    #     rowName1 <- as.character(valueTable[2,..k])
    #     strengthTable[rowName1, colName1] <- valueTable[1,..k]
    #   }
    #   belongingnessClassList[[6]] <- strengthTable
    #   
    #   for (l in 1:ncol(valueTable)) {
    #     colName1 <- as.character(valueTable[3,..l])
    #     rowName1 <- as.character(valueTable[2,..l])
    #     SDVTable[rowName1, colName1] <- valueTable[2,..l]
    #   }  
    #   belongingnessClassList[[7]] <- SDVTable
      names(belongingnessClassList) <- c("belongingnessPrepList", "predictorsIn") #edited to allow commenting
    #                                    "significantPredictors",
    #                                    "interactionEffects", "valueTable",
    #                                    "strengthTable", "SDVTable")
    # } else {
    #   names(belongingnessClassList) <- c("belongingnessPrepList", "predictorsIn",
    #                                      "significantPredictors",
    #                                      "interactionEffects")
    # }
      
  # }
  
  # prepTotalBelongingness(belongingnessDF = belongingnessDF)
  
      
  #STTS Graph Make
      STTSnodes <- nodes
      #establish line of best fit and correlation between s-t and t-s
      x <- as.numeric(STTSnodes$`Teacher-Student Mean`)
      y <- as.numeric(STTSnodes$`Student-Teacher Mean`)
      
      fit = lm(STTSnodes$`Teacher-Student Mean` ~ STTSnodes$`Student-Teacher Mean`, data=STTSnodes)
      fitdata <- data.frame(STTSnodes$`Teacher-Student Mean`)
      prediction = predict(fit, fitdata, se.fit=TRUE)
      fitdata$fitted = prediction$fit
      
      fitdata$ymin = fitdata$fitted - 1.96*prediction$se.fit
      fitdata$ymax = fitdata$fitted + 1.96*prediction$se.fit
      
      correlation = cor.test(x,y)[c("estimate","p.value")]
      correlationText = paste(c("R=","p="),signif(as.numeric(correlation,3),3),collapse=" ")
 
   
        STTSnodes %>%
          plot_ly(x = ~STTSnodes$`Teacher-Student Mean`) %>%
          add_markers(x=~STTSnodes$`Teacher-Student Mean`, y = ~STTSnodes$`Student-Teacher Mean`) %>%
          add_trace(data=fitdata,x= ~STTSnodes$`Teacher-Student Mean`, y = ~fitted, 
                    mode = "lines",type="scatter",line=list(color="#8d93ab")) %>%
          add_ribbons(data=fitdata, ymin = ~ ymin, ymax = ~ ymax,
                      line=list(color="#F1F3F8E6"),fillcolor ="#F1F3F880" ) %>%
          layout(
            showlegend = F,
            annotations = list(x = 5, y = 5,
                               text = correlationText,showarrow =FALSE)
          )
      
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
                                    nodesRainCloud,
                                    belongingnessClassList,
                                    teacherStudentData
                                    )

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
                                         "nodesRainCloud",
                                         "belongingnessClassList",
                                         "teacherStudentData"
                                         )
#Write output object to disk as a .rds    
    filename <- paste(clientName, className, "S to S Dash Data.rds", sep = " ")
    filenameWithPath <- paste("./data", filename, sep = "/")
    write_rds(classDashAnalysisOutput, as.character(filenameWithPath))
  
}


  