#Function for compiling a .RMD document from a series of templates, substituting variable names appropriately

library(readr)
library(rmarkdown)
library(stringr)

compileMasterDash <- function(templateDirectoryName,
                              outputPath,
                              outputFilename,
                              runForChecking = TRUE) {

  #read in files
  files = list.files(path = paste(".", templateDirectoryName, sep = "/"), pattern = '.txt$')
  fileList <- list()
  for (i in 1:length(files)) {
    file <- read_file(paste(templateDirectoryName, files[i], sep = "/"))
    name <- as.character(files[i])
    fileList[[name]] <- file
  }
  
  #substitute some substrings with others in given template strings
  foo <- "smile"
  fileList$YAML.txt <- str_replace_all(fileList$YAML.txt, "Compilation Test", foo)
  
  #define where the .RMD file will be located when it is generated
  outputFilePath <- paste(outputPath, "/", outputFilename, ".RMD", sep = "")
  
  #Write out the file to a .RMD document
  fileConn <- file(outputFilePath)
  writeLines(c(fileList$YAML.txt, fileList$body.txt), fileConn)
  close(fileConn)
  
  #Run the .RMD for quality assurance purposes
  if(runForChecking == TRUE) {
    rmarkdown::run(outputFilePath)
  }

}

compileMasterDash(templateDirectoryName = 'compilationTest',
                  outputFilename = 'output',
                  outputPath = 'compilationTest')

