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
  
  #make the numeric keypair dataframes from which to convert node answers
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
  
  
  
}

  