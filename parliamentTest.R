
library(ggparliament)
library(ggrepel)
library(tidyverse)


# 
# upSetDataSetup <- function (fileListx) {
#   #find the indices of each level of each factor and make it into a named list
#   listInputData <- list()
#   for(j in 1:length(fileListx)) {
#     factorLevels <- levels(as.factor(fileListx[[j]]))
#     indexInfo <- list()
#     for(i in 1:length(factorLevels)) {
#       indexInfo[[i]] <- grep(paste0("^", factorLevels[i], "$"), fileListx[[j]])
#     }
#     for(k in 1:length(factorLevels)) {
#       factorLevels[k] <- paste(names(fileListx[j]), factorLevels[k])
#     }
#     names(indexInfo) <- factorLevels
#     listInputData <- append(listInputData, indexInfo)
#   }
#   listInputData
# }
# 
# 
# listInputData1 <- upSetDataSetup(fileListx = overallFileList1)
# listInputData2 <- upSetDataSetup(fileListx = overallFileList2)
# 
# 
# #filter ListInputData
# removeListElem <- function(inlist,elem_remove){
#   outlist = lapply(inlist,setdiff,elem_remove)
#   outlist[lengths(outlist) > 0]
# }
# removeListElemComplete = function(inlist, elem_remove) {
#   outlist = lapply(inlist, removeListElem, elem_remove = elem_remove)
#   outlist[lengths(outlist) > 0]
# }
# 
# #input handling functions for upset plot arguments
# setOrder <- reactive({
#   if(isTRUE(input$setorder)){
#     return(TRUE)
#   }
#   else{
#     return(FALSE)
#   }
# })
# 
# orderdat <- reactive({
#   orderdat <- as.character(input$order)
#   if(orderdat == "degree"){
#     orderdat <- c("degree")
#   }
#   else if(orderdat == "freq"){
#     orderdat <- "freq"
#   }
#   return(orderdat)
# })
# 
# decrease <- reactive({
#   decrease <- as.character(input$decreasing)
#   if(decrease == "inc"){
#     decrease <- FALSE
#   }
#   else if(decrease == "dec"){
#     decrease <- TRUE
#   }
#   return(decrease)
# })
# 
# mat_prop <- reactive({
#   mat_prop <- input$mbratio
# })
# 
# bar_prop <- reactive({
#   bar_prop <- (1 - input$mbratio)
# })
# 
# emptyIntersects <- reactive({
#   if(isTRUE(input$empty)){choice <- "on"
#   return(choice)
#   }
#   else{
#     return(NULL)
#   }
# })
# 
# filterRowwise <- reactive({
#   if (is.null(input$filterSDV)) listInputData else
#     removeListElemComplete(listInputData, c(input$filterSDV))
# })
# 
# filterColumnwise <- reactive({
#   if (is.null(input$SDV)) listInputData else 
#     listInputData[names(listInputData) %in% c(input$SDV) == FALSE]
# })

#questions for the parliament plot to answer:
#1. what is the sample size of the group of boys who like football in year seven?
#2. Are those with specific needs achieving less well academically?
# == highlight factoral levels (boys, football players etc.)
# == filter nodes (for selecting a form)
# == sort nodes 

nodeList <- list()

fileList[[2]] <- fileList[[1]]
for(i in 1:length(fileList)) {
  nodeList[[i]] <- data.frame(fileList[[i]]$nodes)
}
parliamentData <- dplyr::bind_rows(nodeList)


parliamentData <- parliament_data(election_data = parliamentData,
                               type = "semicircle",
                               parl_rows = 2, #ideally nrow(parliamentData)/number of classes #will round down to nearest whole integer
                               party_seats = nrow(parliamentData))  #number of dots = one per person                               

#sort df before passing to ggplot to order dots
ggplot(parliamentData, aes(x = x, y = y, colour = co.curricular)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  labs(title = "school test") +
  draw_totalseats(n = nrow(parliamentData), type = "semicircle") +
 # geom_highlight_government(government == 1) +
  geom_parliament_bar(colour = co.curricular, party = co.curricular, label = TRUE)
