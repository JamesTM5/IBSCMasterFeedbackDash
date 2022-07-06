# > inputData <- totalNetworkInfo[[1]][[1]]
# > inputData
# Degree                   People
# 5f22def133050e0011eee156   0.38 5f22def133050e0011eee156
# 5f22def133050e0011eee157   0.25 5f22def133050e0011eee157
# 5f22def133050e0011eee159   0.25 5f22def133050e0011eee159
# 5f22def133050e0011eee155   0.25 5f22def133050e0011eee155
# 5f22def133050e0011eee15b   0.25 5f22def133050e0011eee15b
# 5f22def133050e0011eee158   0.12 5f22def133050e0011eee158
# 5f22def133050e0011eee15a   0.12 5f22def133050e0011eee15a
# 5f22def133050e0011eee15d   0.12 5f22def133050e0011eee15d
# 5f22def133050e0011eee16a   0.12 5f22def133050e0011eee16a
# > 

generateDegreeHistogram <- function(inputData) {

#make a histogram of the degree data
  degreeDataFrame <- inputData[order(inputData$Degree),]
  
     degreeHistogram <- plot_ly(
       x = degreeDataFrame$Degree,
       y = degreeDataFrame$People,
       name = "All",
       type = "bar") %>%
       layout(
               yaxis = list(categoryarray =
                             degreeDataFrame$People, categoryorder = "array")
     )

}
