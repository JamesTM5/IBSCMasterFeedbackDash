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
  
#Check for additional measures questions
  #if nodes contains the right columns, belongingness = present

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

  addPackages <- function (packages = packagesUsed) {
    #make a string of package names
    packages <- unlist(unique(packages))
    packages <- lapply(packages, function(x){paste0("'", x, "'")})
    packages <- paste(packages, sep = '', collapse = ", ")
    dash[[2]] <- sub("packageListPlaceholder", packages, dash[[2]])
  }

  dash <- list()
  packagesUsed <- list()
  
  dash[[1]] <- setupYaml(title = "IBSC Dash")
  dash[[2]] <- setupSetupChunk()
  #other additions go here
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

