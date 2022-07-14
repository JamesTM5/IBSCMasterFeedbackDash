
generateDegreeHistogram <- function(inputData) {

#make a histogram of the degree data
  degreeDataFrame <- inputData[order(inputData$Degree),]
  
     degreeHistogram <- plot_ly(
       x = degreeDataFrame$Degree,
       y = degreeDataFrame$People,
       name = "All",
       type = "bar",
       height = 225) %>%
       layout(
               yaxis = list(categoryarray =
                             degreeDataFrame$People, categoryorder = "array"),
               xaxis = list(range = list(0,1.01))
     )

}
