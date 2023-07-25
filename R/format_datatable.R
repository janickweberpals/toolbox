#' Creates a functional default datatable for interactive html outputs
#'
#' @param data dataframe, tibble or tableone object with table content
#' @param caption datatable caption
#' @param col_names character vector of column names
#' @param cols_show character vector of columns to show (other are hidden but can be selected); if not specified, all are shown by default
#' @param ... more parameters passed on
#'
#' @return returns a pretty table formatted for html or latex
#'
#' @importFrom magrittr '%>%'
#' @importFrom DT datatable
#'
#' @export
#'
format_datatable <- function(data = NULL, # tableone object or final df
                             caption = NULL, # table caption
                             col_names = NULL,
                             cols_show = NULL, # character vector of columns that should should be shown, all others are hidden
                             ...
                             ){

  # take more arguments
  arguments <- list(...)

  if(!is.null(col_names)){

    col_names <- col_names

  }else{

    col_names <- names(data)

  }


  # all other columns get collapsed by default
  if(!is.null(cols_show)){

    cols_hide <- which(!colnames(data) %in% cols_show)

  }else{

    cols_hide <- NULL

  }


  tbl <- DT::datatable(
    data,
    caption = caption,
    rownames = FALSE,
    filter = "top",
    extensions = 'Buttons',
    options = list(
      #autoWidth = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'colvis',
          columns = cols_hide-1
        ),
        list(
          extend= "colvisGroup",
          text="Show All",
          show=":hidden"
        ) ,
        list(
          extend = 'collection',
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          text = 'Download'
        )
      ),
      columnDefs = list(
        list(
          visible = FALSE,
          targets= cols_hide-1
        )
      )
    )
  )

  return(tbl) # returns a DT object

}
# END
