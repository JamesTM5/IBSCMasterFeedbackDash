#Function for compiling a .RMD document from a series of templates, substituting variable names appropriately

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
  
  packages = c("rmarkdown", "tidyverse")
  
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
                                RQText = "How closely do you relate to this person?",
                                RQSummary = "Relatedness",
                                ...){

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
                                  RQTextList = c("Relationships Question 1",
                                                 "Relationships Question 2",
                                                 "Relationships Question 3",
                                                 "Relationships Question 4"
                                                 ),
                                  RQSummaryList = c("RQ1",
                                                    "RQ2",
                                                    "RQ3",
                                                    "RQ4"
                                                    ),
                                  ...){

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
                                  RQIndTextList = c("Relationships Question 1",
                                                 "Relationships Question 2",
                                                 "Relationships Question 3",
                                                 "Relationships Question 4"
                                  ),
                                  RQIndSummaryList = c("RQ1",
                                                    "RQ2",
                                                    "RQ3",
                                                    "RQ4"
                                  ),
                                  numRQ,
                                  ...){
    
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
  
 
#pull together packages to properly call them in the setup chunk
  addPackages <- function (setupChunk = dash[[2]], packages = packagesUsed) {
    #make a string of package names
    packages <- unlist(unique(packages))
    packages <- lapply(packages, function(x){paste0("'", x, "'")})
    packages <- paste(packages, sep = '', collapse = ", ")
    setupChunk <- str_replace_all(setupChunk, "packageListPlaceholder", packages)
  }

  dash <- list()
  packagesUsed <- list()

#build dash page list  
  dash[[1]] <- setupYaml(title = "IBSC Dash")
  dash[[2]] <- setupSetupChunk()
#add school-wide pages if needed
   if(length(fileList) >= 2) {
     dash[[length(dash)+1]] <- setupSchoolOverviewPage()
  #    if(belongingnessPresent == TRUE) {
  #       dash[[length(dash)+1]] <- setupOverallBelongingnessPage(fileList)
     }
  # }
#add class pages for each class

   for(i in 1:length(fileList)) {
    if(numRQ == 1) {
       dash[[length(dash)+1]] <- setupRQPageSingle(fileListNumber = i)
       dash[[length(dash)+1]] <- setupRQIndividualScores(fileListNumber = i, numRQ = numRQ)
     } else if(numRQ >= 2) {
       dash[[length(dash)+1]] <- setupRQClassSummary(fileListNumber = i)
       for (j in 1:(numRQ)) {
         dash[[length(dash)+j]] <- setupRQPageMultiple(fileListNumber = i, questionNumber = j)
       }
      
       dash[[length(dash)+1]] <- setupRQIndividualScores(fileListNumber = i, numRQ = numRQ)
     }
   if (!is.vector(fileList[[i]]$STTSPlot)) {
     dash[[length(dash)+1]] <- setupSTTSPage(fileListNumber = i)
   } else if () {
     #check for just s-t data, and if !is.na, setupSTPage(fileListNumber = i)
   }
   if(belongingnessPresent == TRUE) {
     dash[[length(dash)+1]] <- setupClassBelongingnessPage(fileListNumber = i, classNumber = i)
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

#Output Code
  #define where the .RMD file will be located when it is generated
  outputFilePath <- paste(outputDirectory, "/", outputFilename, ".RMD", sep = "")
  
  #Write out the file to a .RMD document
  fileConn <- file(outputFilePath)
  writeLines(unlist(dash), fileConn)
  close(fileConn)
  
  #Run the .RMD for quality assurance purposes
  if(runForChecking == TRUE) {
    rmarkdown::run(outputFilePath)
  }

}

compileMasterDash(templateDirectoryName = 'RMDTemplate',
                  dataDirectoryName = 'data',
                  outputFilename = 'masterDashCompiled',
                  outputDirectory = 'compilationOutput')

