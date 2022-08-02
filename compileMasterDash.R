#Function for compiling a .RMD document from a series of templates, substituting variable names appropriately

#For Testing Purposes
# templateDirectoryName = 'RMDTemplate'
# dataDirectoryName = 'test data'
# outputFilename = 'masterDashCompiled'
# outputDirectory = 'compilationOutput'
# runForChecking = TRUE

compileMasterDash <- function(templateDirectoryName,
                              dataDirectoryName,
                              outputDirectory,
                              outputFilename,
                              runForChecking = TRUE) {
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
  listOfFiles <- list.files(ogDataFolder, pattern = '^.*rds$')
  file.copy(file.path(ogDataFolder,listOfFiles), paste(outputDirectory, "data", sep = "/"))

#read in data to figure out page/section requirements for the dash. 
  fileList <- list()
  for (i in 1:length(listOfFiles)) {
    file <- readRDS(paste(ogDataFolder, listOfFiles[i], sep = "/"))
    name <- as.character(listOfFiles[i])
    fileList[[name]] <- file
  }
#count number of RQs
  numRQ <- vector()
  for(i in 1:length(fileList)){
    numRQ[i] <- length(fileList[[1]]$totalNetworkInfo)
  }
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
#pull together packages to properly call them in the setup chunk
  addPackages <- function (packages = packagesUsed) {
    #make a string of package names
    packages <- unlist(unique(packages))
    packages <- lapply(packages, function(x){paste0("'", x, "'")})
    packages <- paste(packages, sep = '', collapse = ", ")
    dash[[2]] <- sub("packageListPlaceholder", packages, dash[[2]])
  }

  dash <- list()
  packagesUsed <- list()

#build dash page list  
  dash[[1]] <- setupYaml(title = "IBSC Dash")
  dash[[2]] <- setupSetupChunk()
#add school-wide pages if needed
  # if(length(fileList) >= 2) {
  #   dash[[length(dash)+1]] <- setupSchoolOverviewPage(fileList)
  #    if(belongingnessPresent == TRUE) {
  #       dash[[length(dash)+1]] <- setupOverallBelongingnessPage(fileList)
  #   }
  # }
#add class pages for each class
  # for(i in 1:length(fileList)) {
  #   if(numRQ == 1) {
  #     dash[[length(dash)+1]] <- setupRQPageSingle(fileList[[i]])
  #     dash[[length(dash)+1]] <- setupIndividualScores(fileList[[i]])
  #   } else if(numRQ >= 2) {
  #     dash[[length(dash)+1]] <- setupRQClassOverviewPage(fileList[[i]])
  #     for (j in 1:numRQ) {
  #       dash[[length(dash)+j]] <- setupRQPage(fileList[[i]])
  #     }
  #     dash[[length(dash)+1]] <- setupIndividualScores(fileList[[i]])
  #   }
  #   if(belongingnessPresent == TRUE) {
  #     dash[[length(dash)+1]] <- setupBelongingnessPage(fileList[[i]])
  #   }
  # }
  # dash[[length(dash)+1]] <- setupRawData(fileList)
  
#insert package list into setup chunk to call relevant libraries
  dash[[2]] <- addPackages()

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
                  dataDirectoryName = 'test data',
                  outputFilename = 'masterDashCompiled',
                  outputDirectory = 'compilationOutput')

