#Source this script to build the dashboard and associated files needed for 
#the Relationships Foundation School Dashboard.  See the README for more
#information.

#For Testing Purposes
# templateDirectoryName = 'RMDTemplate'
# dataDirectoryName = 'data'
# outputFilename = 'masterDashCompiled'
# outputDirectory = 'compilationOutput'
# runForChecking = TRUE

compileMasterDash <- function(templateDirectoryName,
                              dataDirectoryName,
                              outputDirectory,
                              outputFilename,
                              runForChecking = TRUE) {
  
  packages = c("rmarkdown", "readr", "stringr", "dplyr")
  
  invisible(lapply(packages, library, character.only = TRUE))
  
#Input Code
  #read in all template text files
  files = list.files(path = paste(".", templateDirectoryName, sep = "/"), pattern = '.txt$')
  templateList <- list()
  for (i in 1:length(files)) {
    file <- read_file(paste(templateDirectoryName, files[i], sep = "/"))
    name <- as.character(files[i])
    templateList[[name]] <- file
  }

#make an output directory
  if(!dir.exists(outputDirectory)){
    dir.create(outputDirectory)
  }
#move survey data files to a subdirectory of the output directory
  if(!dir.exists(path = paste(outputDirectory, "data", sep = "/"))) {
    dir.create(path = paste(outputDirectory, "data", sep = "/"))
  }
  ogDataFolder <- paste(getwd(), dataDirectoryName, sep = "/")
  listOfFilesRDS <- list.files(ogDataFolder, pattern = '^.*rds$')
  file.copy(file.path(ogDataFolder,listOfFilesRDS), paste(outputDirectory, "data", sep = "/"))
  listOfFilesJSON <- list.files(ogDataFolder, pattern = '^.*json$')
  file.copy(file.path(ogDataFolder,listOfFilesJSON), paste(outputDirectory, "data", sep = "/"))

  #copy assets folder and all contents to output directory  
  if(!dir.exists(path = paste(outputDirectory, "assets", sep = "/"))){
    file.copy("./assets",outputDirectory,recursive=TRUE)
  }
#read in data to figure out page/section requirements for the dash. 
  fileList <- list()
  for (i in 1:length(listOfFilesRDS)) {
    file <- readRDS(paste(ogDataFolder, listOfFilesRDS[i], sep = "/"))
    name <- as.character(listOfFilesRDS[i])
    fileList[[name]] <- file
  }
#count number of RQs
  numRQ <- vector()
  for(i in 1:length(fileList)){
    numRQ[i] <- length(fileList[[1]]$totalNetworkInfo)
  }
  if(length(numRQ) >= 2) {  
    if(var(numRQ) > 0) {
      warning("number of relationship questions not consistent across all datasets")
    }
    numRQ <- numRQ[1]
    #factor in presence of overview page
    if(numRQ == 1) {
      numRQ <- 1
    } else {
      numRQ <- numRQ - 1
    }
  }
  
#Check for additional measures questions
  #if nodes contains the right columns, belongingness = present
  belongingnessText <- c(
    "I feel like I belong at",
    "I make friends easily at",
    " seem to like me",
    "I feel awkward and out of place",
    "I feel like an outsider",
    "I feel lonely at"
  )
  belongingnessIndices <- vector()
  for(i in 1:length(belongingnessText)){
    belongingnessIndices[[length(belongingnessIndices)+1]] <- grep(belongingnessText[i], colnames(fileList[[1]]$nodes))
  }  
  if(length(belongingnessIndices)>6) {
    warning("more belongingness question columns have been matched than there should be.")
    belongingnessPresent <- TRUE
  } else if (length(belongingnessIndices) == 6) {
    belongingnessPresent <- TRUE
  } else if (length(belongingnessIndices) > 0) {
    warning("fewer belongingness question columns have been matched than there should be.  Check the belongingness pages for errors")
    belongingnessPresent <- TRUE
  } else {
    belongingnessPresent <- FALSE
  }
      
#Template Modification  
#add a YAML header suitable for the dashboard
  setupYaml <- function (template = templateList$YAML.txt, title = "RF Dashboard") {
    titlePlaceholder <- "titleOfDashboard"
    template <- str_replace_all(template, titlePlaceholder, title)
  }

#add a setup code chunk to call libraries, read in .RDS data and set.seed()
  setupSetupChunk <- function (setupChunkTemplate = templateList$setupChunk.txt) {
    setupChunkTemplate
  }

  addToSetupChunk <- function (textToAdd, setupChunk = dash[[2]]) {
    placeholder <- "'dupnlmffjcatgle'"
    setupChunk <- str_replace_all(setupChunk,
                                  placeholder,
                                  paste(textToAdd,
                                        placeholder,
                                        sep = "\n"))
  }

#add a school wide page summary page
  setupSchoolOverviewPage <- function (schoolOverviewTemplate = templateList$RQSchoolWideSummaryPage.txt) {
    schoolOverviewTemplate
  }
  
    
#add a page for a single RQ with no expectation of a class overview.
  setupRQPageSingle <- function(template = templateList$RQPageSingle.txt,
                                fileListNumber = 1,
                                ...){
    RQText <- fileList[[fileListNumber]]$RQTitles
    RQSummary <- fileList[[fileListNumber]]$RQList

    className <- fileList[[fileListNumber]]$className
    template <- str_replace_all(template, "classNamePlaceholderhonrwufzql", className)
    template <- str_replace_all(template, "fileListNumberPlaceholderrmwkpgtffs", as.character(fileListNumber))
    template <- str_replace_all(template, "RQTextPlaceholdersqqpizconj", RQText)
    template <- str_replace_all(template, "RQSummaryPlaceholderxbvmgayrkd", RQSummary)
    dash[[length(dash)+1]] <- template
  }
  
  setupRQPageMultiple <- function(template = templateList$RQPageMulti.txt,
                                  fileListNumber,
                                  questionNumber,
                                  ...){

    RQTextList <- fileList[[fileListNumber]]$RQList
    RQSummaryList <- fileList[[fileListNumber]]$RQTitles
    className <- fileList[[fileListNumber]]$className
    template <- str_replace_all(template, "classNamePlaceholderhonrwufzql", className)
    template <- str_replace_all(template, "fileListNumberPlaceholderrmwkpgtffs", as.character(fileListNumber))
    template <- str_replace_all(template, "RQTextPlaceholdersqqpizconj", RQTextList[[questionNumber]])
    template <- str_replace_all(template, "RQSummaryPlaceholderxbvmgayrkd", RQSummaryList[[questionNumber]])
    template <- str_replace_all(template, "RQNumberPlaceholderjkkfdufsse", as.character(questionNumber))
    dash[[length(dash)+1]] <- template
  }
  
  setupRQClassSummary <- function(template = templateList$RQPageSummary.txt,
                                  fileListNumber,
                                  ...){
    
    className <- fileList[[fileListNumber]]$className
    template <- str_replace_all(template, "classNamePlaceholderhonrwufzql", className)
    template <- str_replace_all(template, "fileListNumberPlaceholderrmwkpgtffs", as.character(fileListNumber))
    dash[[length(dash)+1]] <- template
  }

  setupRQIndividualScores <- function(header = templateList$RQIndividualDataPageHeader.txt,
                                      polarGraphChunk = templateList$RQIndividualPolarGraph.txt,
                                      columnHeader = templateList$RQIndividualDataPageColumnHeader.txt,
                                      degreeGraphChunk = templateList$RQIndividualDegreeGraphs.txt,
                                      fileListNumber,
                                      numRQ,
                                      ...){
    
    RQIndTextList <- fileList[[fileListNumber]]$RQList
    RQIndSummaryList <- fileList[[fileListNumber]]$RQTitles
    
    className <- fileList[[fileListNumber]]$className
    
    individualPage <- list()
    
    header <- str_replace_all(header,"classNamePlaceholderhonrwufzql",
                              as.character(className))
    individualPage[[1]] <- header
    for(j in 1:numRQ) {
      modifiedPolarGraphChunk <- polarGraphChunk
      modifiedPolarGraphChunk <- str_replace_all(modifiedPolarGraphChunk,
                                                 "RQSummaryPlaceholderxbvmgayrkd",
                                                 RQIndSummaryList[[j]])
      modifiedPolarGraphChunk <- str_replace_all(modifiedPolarGraphChunk,
                                                 "fileListNumberPlaceholderrmwkpgtffs",
                                                 as.character(fileListNumber))
      modifiedPolarGraphChunk <- str_replace_all(modifiedPolarGraphChunk,
                                                 "graphListNumberPlaceholdervifktlvodm",
                                                 as.character(j))
      individualPage[[length(individualPage)+1]] <- modifiedPolarGraphChunk
    }
    individualPage[[length(individualPage)+1]] <- columnHeader

    for(k in 1:numRQ) {
      modifiedDegreeGraphChunk <- degreeGraphChunk
      modifiedDegreeGraphChunk <- str_replace_all(modifiedDegreeGraphChunk,
                                                 "RQSummaryPlaceholderxbvmgayrkd",
                                                 RQIndSummaryList[[k]])
      modifiedDegreeGraphChunk <- str_replace_all(modifiedDegreeGraphChunk,
                                                 "fileListNumberPlaceholderrmwkpgtffs",
                                                 as.character(fileListNumber))
      modifiedDegreeGraphChunk <- str_replace_all(modifiedDegreeGraphChunk,
                                                 "graphListNumberPlaceholdervifktlvodm",
                                                 as.character(k))
      individualPage[[length(individualPage)+1]] <- modifiedDegreeGraphChunk
    }  
    
    dash[[length(dash)+1]] <- unlist(individualPage)
  }
  
  #add a class belongingness page
  setupClassBelongingnessPage <- function (belongingnessTemplate = templateList$belongingnessPageWithoutPredictiveContent.txt,
                                           fileListNumber,
                                           classNumber) {
    className <- fileList[[fileListNumber]]$className
    modifiedBelongingnessTemplate <- belongingnessTemplate
    modifiedBelongingnessTemplate <- str_replace_all(modifiedBelongingnessTemplate,
                                               "fileListNumberPlaceholderrmwkpgtffs",
                                               as.character(fileListNumber))
    modifiedBelongingnessTemplate <- str_replace_all(modifiedBelongingnessTemplate,
                                                     "classNumberPlaceholderckkdnsodds",
                                                     as.character(classNumber))
    modifiedBelongingnessTemplate <- str_replace_all(modifiedBelongingnessTemplate,
                                                     "classNamePlaceholderhonrwufzql",
                                                     className)
  }
  
  setupSTPage <- function (template = templateList$STPage.txt,
                           fileListNumber,
                           ...) {
    className <- fileList[[fileListNumber]]$className
    modifiedSTTemplate <- template
    modifiedSTTemplate <- str_replace_all(modifiedSTTemplate,
                                          "fileListNumberPlaceholderrmwkpgtffs",
                                          as.character(fileListNumber))
    modifiedSTTemplate <- str_replace_all(modifiedSTTemplate,
                                          "classNamePlaceholderhonrwufzql",
                                          className)
  }
  
  setupSTTSPage <- function (template = templateList$STTSPage.txt,
                             fileListNumber,
                             ...) {
    className <- fileList[[fileListNumber]]$className
    modifiedSTTSTemplate <- template
    modifiedSTTSTemplate <- str_replace_all(modifiedSTTSTemplate,
                                          "fileListNumberPlaceholderrmwkpgtffs",
                                          as.character(fileListNumber))
    modifiedSTTSTemplate <- str_replace_all(modifiedSTTSTemplate,
                                          "classNamePlaceholderhonrwufzql",
                                          className)
  }
  
 
#pull together packages to properly call them in the setup chunk
  addPackages <- function (setupChunk = dash[[2]], packages = packagesUsed) {
    #make a string of package names
    packages <- unlist(unique(packages))
    packages <- lapply(packages, function(x){paste0("'", x, "'")})
    packages <- paste(packages, sep = '', collapse = ", ")
    setupChunk <- str_replace_all(setupChunk, "packageListPlaceholder", packages)
  }

  dash <- list()
  classRelationshipsList <- list()
  individualRelationshipsList <- list()
  studentTeacherRelationshipsList <- list()
  belongingnessList <- list()
  packagesUsed <- list()

#build dash page list  
  dash[[1]] <- setupYaml(title = "Scotch College Relationships Dash")
  dash[[2]] <- setupSetupChunk()
#add school-wide pages if needed
   if(length(fileList) >= 2) {
     dash[[length(dash)+1]] <- setupSchoolOverviewPage()
     }
#add class pages for each class

   for(i in 1:length(fileList)) {
    if(numRQ == 1) {
       classRelationshipsList[[length(classRelationshipsList)+1]] <- setupRQPageSingle(fileListNumber = i)
       individualRelationshipsList[[length(individualRelationshipsList)+1]] <- setupRQIndividualScores(fileListNumber = i, numRQ = numRQ)
     } else if(numRQ >= 2) {
       classRelationshipsList[[length(classRelationshipsList)+1]] <- setupRQClassSummary(fileListNumber = i)
       for (j in 1:(numRQ)) {
         classRelationshipsList[[length(classRelationshipsList)+1]] <- setupRQPageMultiple(fileListNumber = i, questionNumber = j)
       }
      
       individualRelationshipsList[[length(individualRelationshipsList)+1]] <- setupRQIndividualScores(fileListNumber = i, numRQ = numRQ)
     }
    if (is.vector(fileList[[i]]$STTSPlot) && is.vector(fileList[[i]]$TSPlot) && is.vector(fileList[[i]]$STPlot)) {
     #no student-teacher or teacher-student data to display
   } else if (is.vector(fileList[[i]]$STTSPlot) && is.vector(fileList[[i]]$TSPlot)){
     #make the student-teacher graph only page
     studentTeacherRelationshipsList[[length(studentTeacherRelationshipsList)+1]] <- setupSTPage(fileListNumber = i)
   } else {
     #make the student-teacher-student page
     studentTeacherRelationshipsList[[length(studentTeacherRelationshipsList)+1]] <- setupSTTSPage(fileListNumber = i)
   }
   if(belongingnessPresent == TRUE) {
     belongingnessList[[length(belongingnessList)+1]] <- setupClassBelongingnessPage(fileListNumber = i, classNumber = i)
     }
   }
  dash[[2]] <- addToSetupChunk(textToAdd = "d3RQPrepSingleInstance\\(fileList\\=fileList\\)")
  dash[[2]] <- addToSetupChunk(textToAdd = "d3ClassOverviewPrepSingleInstance\\(fileList\\=fileList\\)")
  dash[[2]] <- addToSetupChunk(textToAdd = "nodesAmalgam \\<\\- gatherNodes\\(\\)")
  # dash[[length(dash)+1]] <- setupRawData(fileList)
  
  #delete setup key string to keep output code clean
  
#insert package list into setup chunk to call relevant libraries
  packagesUsed <- append(packagesUsed, c("rmarkdown", "jsonlite", "tidyverse",
                                         "flexdashboard", "plotly", "DT"))
  dash[[2]] <- addPackages(setupChunk = dash[[2]], packages = packagesUsed)
  
#Append each menu entry into dash
  dash <- c(dash, classRelationshipsList, individualRelationshipsList, studentTeacherRelationshipsList, belongingnessList)

#Output Code
  #define where the .RMD file will be located when it is generated
  outputFilePath <- paste(outputDirectory, "/", outputFilename, ".RMD", sep = "")
  
  #Write out the file to a .RMD document
  dash <- unlist(dash)
  dash <- paste(dash, collapse = '\n', sep = "\n")
  write_file(dash, outputFilePath)
  
  #Run the .RMD for quality assurance purposes
  if(runForChecking == TRUE) {
    rmarkdown::run(outputFilePath)
  }

}

compileMasterDash(templateDirectoryName = 'RMDTemplate',
                  dataDirectoryName = 'data',
                  outputFilename = 'masterDashCompiled',
                  outputDirectory = 'compilationOutput')

