#convenience functions for making survey data numeric.

#call key to numeric to make a conversion key from textual to numeric values
#normalized = T ties the scores to 0-1

#call makeResponseNumeric to return a vector of numeric values corresponding to
#a given conversion key

keyToNumeric <- function(scoreData,
                         answers,
                         normalized = FALSE) {
  unknown <- grepl("don't know", answers)
  answers <- data.frame(answers, unknown)
  answers <- answers[answers$unknown != TRUE, ]
  score <- 0:(nrow(answers)-1)
  numericAnswers <- vector()
  if(normalized == TRUE) {
    division <- 1/(nrow(answers)-1)
    for (i in 1:nrow(answers)) {
      numericAnswers[i] <- 1 - (division*score[i])
    }
  } else if (normalized == FALSE) {
    numericAnswers <- rev(score)
  } else {
    warning("incorrect input to normalize perameter which requires logical")
  }
  conversionKey <- cbind(answers, numericAnswers)
 
}

makeResponseNumeric <- function(data,
                                conversionKey) {
  numericScoreVector <- vector("numeric")
  for(i in 1:length(data)) {
    if(data[i] %in% conversionKey$answers) {
      result <- which(conversionKey$answers == data[i])
      numericScore <- conversionKey$numericAnswers[result]
      numericScoreVector <- append(numericScoreVector, numericScore)
    } else {
      numericScore <- NA
      numericScoreVector <- append(numericScoreVector, numericScore)
    }
  }
  numericScoreVector
}
  