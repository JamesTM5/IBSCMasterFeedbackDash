#This file contains the survey details that need to be manually updated to
#interpret the .xlsx data output from Polinode and process it for the IBSC
#Dashboard.

#What are the possible answers to the network question?
#any number of responses or values accepted, in descending order of positivity
#(a string containing "don't know" is treated exceptionally and may appear
#anywhere in the list without disrupting analysis)
nodeAnswers <- c("Strongly Agree", "Agree", "Slightly Agree", "Slightly Disagree", 
             "Disagree", "Strongly Disagree")
edgeAnswers <- c("Strongly Agree", "Agree", "Slightly Agree", "Slightly Disagree", 
                 "Disagree", "Strongly Disagree")

# Where should the system find the responses to the question from participants?
#(These are the spreadsheet column names - the system requires an exact match)
  #For Non-Relationship Questions (in the nodes datasheet, excluding
  #sociodemographics; labelled Q1-* and Q5*)
responseColumns <- c(11:16, 17:26)
  #For Relationship Questions
responseColumnSSNQ <- "Network"

# under what threshold should edge writing be rejected?
thresholdForEdgeDrawing <- 0.99

#Arbitrary seed, keep consistent for comparability.
seed <- 184
