library(shiny)

forcedirected <- function(title, value) {
  list(title = title, value = value)
}

forcedirectedOutput <- function(id){
  el <- shiny::tags$div(
    id = id, class = 'forcedirected',
  )
  path <- normalizePath('assets/forcedirected')
  
  deps <- list(
    htmltools::htmlDependency(
      name = 'forcedirected',
      version = '1.0.0',
      src = c(file = path),
      script = c('binding.js','fd.js','repeated-fd.js'),
      stylesheet = 'styles.css'
    ),
    htmltools::htmlDependency(
      name = 'd3',
      version = '7.2.1',
      src = c(file = normalizePath('assets')),
      script = c('d3.min.js')
    )
  )
  htmltools::attachDependencies(el, deps)
}

renderForcedirected <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  function(){
    val <- func()
    return(val)
  }
}


repeatedforcedirected <- function(title, value) {
  list(title = title, value = value)
}

repeatedforcedirectedOutput <- function(id){
  el <- shiny::tags$div(
    id = id, class = 'repeatedforcedirected',
  )
  path <- normalizePath('assets/forcedirected')
  
  deps <- list(
    htmltools::htmlDependency(
      name = 'forcedirected',
      version = '1.0.0',
      src = c(file = path),
      script = c('binding.js','fd.js','repeated-fd.js'),
      stylesheet = 'styles.css'
    ),
    htmltools::htmlDependency(
      name = 'd3',
      version = '7.2.1',
      src = c(file = normalizePath('assets')),
      script = c('d3.min.js')
    )
  )
  htmltools::attachDependencies(el, deps)
}

renderRepeatedforcedirected <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  function(){
    val <- func()
    return(val)
  }
}