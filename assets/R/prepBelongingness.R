#Set up Belongingness Dashboard

prepBelongingness <- function(belongingnessDF){
  belongingnessDataPrepList <- list()
  belongingnessDataPrepList[[1]] <- fileList[[1]]$belongingnessDF
  belongingnessDataPrepList[[2]] <- belongingnessDataPrepList[[1]]
  belongingnessDataPrepList[[3]] <- belongingnessDataPrepList[[1]]
  belongingnessDataPrepList[[4]] <- as.list(names(belongingnessDataPrepList[[3]]))
  names(belongingnessDataPrepList[[4]]) <- paste(names(belongingnessDataPrepList[[3]]), " (", map(belongingnessDataPrepList[[3]],~length(unique(.x))), ")") 
  belongingnessDataPrepList[[5]] <- names(select_if(belongingnessDataPrepList[[3]], is.numeric))
  belongingnessDataPrepList[[4]] <- belongingnessDataPrepList[[4]][!belongingnessDataPrepList[[4]]] %in% belongingnessDataPrepList[[5]]
  belongingnessDataPrepList[[5]] <- "9pt"
  names(belongingnessDataPrepList) <- c("dataFileBelongingness",
                                        "unprocessedBelongingnessData",
                                        "WBNumeric",
                                        "GBChoices",
                                        "metricChoices",
                                        "belongingnessFontSize")
  belongingnessDataPrepList
}

#significance testing for belongingness dashboard
#A function to find significant differences between factors in the belongingness survey

belongingnessSignificanceTesting <- function (data){
  
  #prep data for analysis
  prunedDataSDV <- data %>% discard(is.numeric)
  prunedDataExperimental <- data %>% keep(is.numeric)
  #make sure each column of prunedDataSDV is class(factor)
  for(i in 1:ncol(prunedDataSDV)) {
    prunedDataSDV[,i] <- as.factor(prunedDataSDV[,i])
  }
  #remove "id" from the first column
  prunedDataSDV <- prunedDataSDV[,-1]
  #remove belongingness Stratified
  prunedDataSDV <- subset(prunedDataSDV, select = -`Belongingness Stratified`)
  
  #initiate some lists
  significance <- list()
  significantPredictors <- list()
  interactionEffects <- list()
  predictors <- list()
  
  
  #Choose the right comparison of means test
  #compare each subset of each sdv with every single experimental variable
  for (i in 1:ncol(prunedDataSDV)){
    for (j in 1:ncol(prunedDataExperimental)) {
      #if predictor categorical
      if (class(prunedDataSDV[[i]]) == "factor") {
        x <- prunedDataExperimental[[j]]
        y <- as.numeric(as.factor(as.character(prunedDataSDV[[i]])))
        #t-test
        if(length(levels(prunedDataSDV[[i]])) <= 2) {
          tt <- t.test(x ~ y)
          result <- tt$p.value
          #ANOVA
        } else if (length(levels(prunedDataSDV[[i]])) >2) {
          anova <- aov(x ~ y)
          #TODO: check for balance with replications function
          result <- summary(anova)[[1]][["Pr(>F)"]][[1]]
        }
        resultName <- paste(names(prunedDataExperimental)[j], "by", 
                            names(prunedDataSDV)[i], sep = " ")
        names(result) <- resultName
        significance <- append(result, significance)
        if (result < .01) {
          significantPredictor <- "Highly Significant"
          names(significantPredictor) <- paste(names(prunedDataExperimental)[j], "by", 
                                               names(prunedDataSDV)[i], sep = " ")
          significantPredictors <- append(significantPredictor,
                                          significantPredictors)
          predictors <- append(names(prunedDataSDV)[i], predictors)
        } else if (result < .05) {
          significantPredictor <- "Significant"
          names(significantPredictor) <- paste(names(prunedDataExperimental)[j], "by", 
                                               names(prunedDataSDV)[i], sep = " ")
          significantPredictors <- append(significantPredictor,
                                          significantPredictors)
          predictors <- append(names(prunedDataSDV)[i], predictors)
        }
      } else {
        cat ("data in an unsupported format")
      }
    }
  }
  
  #For every predictive variable, pair it with every other SDV in turn and look for an interaction effect
  
  #find every unique combination of those SDVs
  expand.grid.unique <- function(x, y, include.equals=FALSE) {
    x <- unique(x)
    y <- unique(y)
    g <- function(i) {
      z <- setdiff(y, x[seq_len(i-include.equals)])
      if(length(z)) cbind(x[i], z, deparse.level=0)
    }
    do.call(rbind, lapply(seq_along(x), g))
  }
  
  ANOVACombos <- expand.grid.unique(predictors, names(prunedDataSDV))
  
  #for every unique combination of 2 SDV combo see if there is highly significant predictive power
  
  for (k in 1:nrow(ANOVACombos)) {
    SDV1 <- unlist(ANOVACombos[k,1])
    SDV2 <- unlist(ANOVACombos[k,2])
    EV <- prunedDataExperimental$`Belongingness Mean`
    if((class(prunedDataSDV[[SDV1]]) == "factor") && (class(prunedDataSDV[[SDV2]]) == "factor")) {
      #perform a 2 way ANOVA
      IE <- aov(EV ~ prunedDataSDV[[SDV1]] * prunedDataSDV[[SDV2]])
      pvalues <- summary(IE)[[1]][["Pr(>F)"]]
      resultSDV1 <- pvalues[1]
      resultSDV2 <- pvalues[2]
      interactionEffect <- pvalues[3]
      if (is.na(resultSDV1) || is.na(resultSDV2)) {
        
      } else if (resultSDV1 > .05 || resultSDV2 > .05) {
        if(is.na(interactionEffect)) {
          
        } else if (interactionEffect < .01) {
          significantPredictor <- "Highly Significant"
          resultName <- paste(names(prunedDataExperimental)[j], "by", 
                              SDV1, "and", SDV2, sep = " ")
          names(significantPredictor) <- resultName
          interactionEffects <- append(significantPredictor,
                                       interactionEffects)
        } else if (interactionEffect > .05) {
          significantPredictor <- "Significant"
          resultName <- paste(names(prunedDataExperimental)[j], "by", 
                              SDV1, "and", SDV2, sep = " ")
          names(significantPredictor) <- resultName
        }
      }
    }
    
  }
  
  return( list(significantPredictors, interactionEffects))
  #TODO: if predictor variable quantitative, figure out the right regression analysis
}

belongingnessClassesList <- list()
for (i in 1:length(fileList)) {
  belongingnessClassesList[[i]][[1]] <- prepBelongingness(belongingnessDF = fileList[[i]]$belongingnessDF)
  #remove overall degree columns to keep the function working
  belongingnessClassesList[[i]][[2]] <- belongingnessDataPrepList[[2]][1:(length(belongingnessDataPrepList[[2]])-3)]
  belongingnessClassesList[[i]][[3]] <- belongingnessSignificanceTesting(predictorsIn)
  belongingnessClassesList[[i]][[4]] <- significantPredictors[[2]]
  belongingnessClassesList[[i]][[3]] <- significantPredictors[[1]]
  
  #prep significance data for display
  predictionTable <- data.table::setDT(significantPredictors)
  values <- str_split(colnames(predictionTable), " by ")
  cols <- list()
  rows <- list()
  
  for (j in 1:length(values)) {
    first <- values[[j]] [1]
    second <- values [[j]] [2]
    cols <- append(cols, first)
    rows <- append(rows, second)
  }
  
  valueTable <- rbind(predictionTable, rows)
  valueTable <- rbind(valueTable, cols)
  belongingnessClassesList[[i]][[5]] <- valueTable
  
  strengthTable <- data.frame(matrix(ncol = length(unique(unlist(cols))), nrow = 
                                       length(unique(unlist(rows)))))
  colnames(strengthTable) <- unique(unlist(cols))
  rownames(strengthTable) <- unique(unlist(rows))

  SDVTable <- data.frame(matrix(ncol = length(unique(unlist(cols))), nrow = 
                                  length(unique(unlist(rows)))))
  colnames(SDVTable) <- unique(unlist(cols))
  rownames(SDVTable) <- unique(unlist(rows))
 
  for (k in 1:ncol(valueTable)) {
    colName1 <- as.character(valueTable[3,..k])
    rowName1 <- as.character(valueTable[2,..k])
    strengthTable[rowName1, colName1] <- valueTable[1,..k]
  }
  belongingnessClassesList[[i]][[6]] <- strengthTable
  
  for (l in 1:ncol(valueTable)) {
    colName1 <- as.character(valueTable[3,..l])
    rowName1 <- as.character(valueTable[2,..l])
    SDVTable[rowName1, colName1] <- valueTable[2,..l]
  }  
  belongingnessClassesList[[i]][[7]] <- SDVTable
  
  names(belongingnessClassesList) <- c("belongingnessPrepList", "predictorsIn",
                                       "significantPredictors",
                                       "interactionEffects", "valueTable",
                                       "strengthTable", "SDVTable")
}
