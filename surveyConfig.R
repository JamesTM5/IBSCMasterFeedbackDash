#This file contains the survey details that need to be manually updated to
#interpret the .xlsx data output from Polinode and process it for the IBSC
#Dashboard.

#What are the possible answers to the network question?
#any number of responses or values accepted, in descending order of positivity
#(a string containing "don't know" is treated exceptionally and may appear anywhere
#in the list without disrupting analysis)
answersSSNQ <- c("6", "5", "4", "3", "2", "1")

# Where should the system find the responses to the question from participants?
#(These are the spreadsheet column names - the system requires an exact match)
responseColumnSSNQ <- "Network"

# under what threshold should edge writing be rejected?
thresholdForEdgeDrawing <- 0.99

#Arbitrary seed, keep consistent for comparability.
seed <- 184

#Legacy: Remove
#From which system is the data being drawn?
dataSource <- "Polinode"
