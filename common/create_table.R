# library(DT)
create_table <- function(df, capt=NULL){
  DT::datatable(df, extensions = c('FixedColumns'),
                options = list(scrollX = TRUE, paging=TRUE), 
                caption=htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: center; color: black;',
                  htmltools::em(capt)
                ))
}