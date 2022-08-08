# make a colour palette
listOfColours <- c("#9E0142", "#66c2a5", "#C99f47","#D55E00", "#0072B2",
                   "#332288", "#661100")
# make it d3 compatible
colors <- paste(sapply(listOfColours, function(x) {
  paste0("d3.rgb(",paste(c(col2rgb(x), 0.5),
                         collapse = "," ),
         ")") }),
  collapse = ", ")
colorJS <- paste0('d3.scaleOrdinal([', colors, '])')

#choose a set of colours equally spaced on the colour wheel
ColourPicker <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}