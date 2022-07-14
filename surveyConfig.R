#This file contains the survey details that need to be manually updated to
#interpret the .xlsx data output from Polinode and process it for the IBSC
#Dashboard.

#What are the possible answers to the Student/Teacher questions?
#any number of responses or values accepted, in descending order of positivity
#(a string containing "don't know" is treated exceptionally and may appear
#anywhere in the list without disrupting analysis)
nodeAnswersStudentTeacher <- c("Strongly Agree", "Agree", "Slightly Agree",
                               "Slightly Disagree", "Disagree",
                               "Strongly Disagree")
#What are the possible answers to the Belongingness questions?
#any number of responses or values accepted, in descending order of positivity
#(a string containing "don't know" is treated exceptionally and may appear
#anywhere in the list without disrupting analysis)
nodeAnswersBelongingness <- c("Strongly Agree", "Agree", "Disagree",
                               "Strongly Disagree")
#What are the possible answers to the network question?
#any number of responses or values accepted, in descending order of positivity
#(a string containing "don't know" is treated exceptionally and may appear
#anywhere in the list without disrupting analysis)
edgeAnswers <- c("Strongly Agree", "Agree", "Slightly Agree", "Slightly Disagree", 
                 "Disagree", "Strongly Disagree")

# under what threshold should edge writing be rejected?
thresholdForEdgeDrawing <- 0.99

#Arbitrary seed, keep consistent for comparability.
seed <- 184
