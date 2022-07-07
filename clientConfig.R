#This file contains the school details that need to be manually updated to
#interpret the .xlsx data output from Polinode and process it for the IBSC
#Dashboard.

#What is the file that contains the data?
  #Demo Data
  # dataFile <- "Time Academy Class 4E Data.xlsx"
  #dataFile <- "Time Academy Student Survey 4D.xlsx"

dataFile <- "test data/IBSC Student Scots 7TGAR1.xlsx"

#What does the client call themselves?
clientName <- "Organisation One"

className <- "Class One"

#Which SDV have been supplied for analysis alongside network data? (must be
#factorial data, in the order it appears in the .xlsx and is case sensitive.
#Additionally, it must not contain the character string "numeric".)

socioDemographicVariables <- c(
                                "co-curricular",
                                "Day/Boarder",
                                "House",
                                "International /Domestic",
                                "TSC siblings",
                                "winter sport")
