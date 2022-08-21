
#Use this function to take a list of survey data .xlsx worksheets and read them
#into a list of data frames ready for processing.  Each file needs to be in the 
#format ".Student <class name>.xlsx" for student-completed questions, or
#".Teacher <class name>.xlsx" for teacher completed ones.

#list all student and teacher files in a given directory
loadSchoolData <- function (path = "data/") {
  path = "data/"
  packages = c("rmarkdown", "tidyverse", "readxl", "igraph", "networkD3", "data.table", "ggplot2", "ggdendro", "plotly")
  invisible(lapply(packages, library, character.only = TRUE))
  
  fileListStudent <- list.files(path = path, pattern = ".Student.*.xlsx",
                         full.names=TRUE)
  fileListTeacher <- list.files(path = path, pattern = ".Teacher.*.xlsx",
                              full.names=TRUE)
  
  #turn a multi sheet .xlsx file into a list of data frames or tibbles
  readExcelAll <- function(filename, tibble = FALSE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
  }
  
  #read the lists of .xlsx files into r as lists of data frames
  schoolDataList <- list()
  
  if(length(fileListStudent) > 0){
    fileListS <- list()
    for(i in 1:length(fileListStudent)) {
     fileListS[[i]] <- as.list(readExcelAll(fileListStudent[i], tibble = F))
    }
    names(fileListS) <- fileListStudent
    schoolDataList[[length(schoolDataList)+1]] <- as.list(fileListS)
    names(schoolDataList)[length(schoolDataList)] <- "studentResponses"
  }
  if(length(fileListTeacher) > 0){
    fileListT <- list()
    for(i in 1:length(fileListTeacher)) {
      fileListT[[i]] <- as.list(readExcelAll(fileListTeacher[i], tibble = F))
    }
    names(fileListT) <- fileListTeacher
    schoolDataList[[length(schoolDataList)+1]] <- as.list(fileListT)
    names(schoolDataList)[length(schoolDataList)] <- "teacherResponses"
  }
  schoolDataList
}

schoolDataList <- loadSchoolData()

source("SchoolDataPrep.r")


for(z in 1:length(schoolDataList$studentResponses)) {
  SSDashAnalysis (schoolDataList$studentResponses, listNumberStudent = z)
  print(paste("Number", z, "Generated", sep = " "))
}
