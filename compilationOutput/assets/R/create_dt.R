#Convenience wrapper for raw data with download options
create_dt <- function(x){
  DT::datatable(x,
                extensions = 'Buttons',
                options = list(dom = 'Blfrti',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               lengthMenu = list(c(50,100, 200, -1),
                                                 c(50, 100, 200, "All")),
                               paginate = TRUE
                )
  )
}
