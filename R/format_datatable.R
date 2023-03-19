#' Creates a functional default datatable for interactive html outputs
#'
#' @param data dataframe, tibble or tableone object with table content
#' @param caption datatable caption
#' @param col_names character vector of column names
#' @param cols_show character vector of columns to show (other are hidden but can be selected)
#' @param ... more parameters passed on
#'
#' @return returns a pretty table formatted for html or latex
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr as_tibble
#' @importFrom kableExtra add_indent
#' @importFrom kableExtra row_spec
#' @importFrom kableExtra kable
#' @importFrom kableExtra kbl
#' @importFrom kableExtra kable_classic
#' @importFrom stringr str_starts
#' @importFrom tableone CreateTableOne
#'
#' @export
#'
format_datatable <- function(data = NULL, # tableone object or final df
                             caption = "Table.",
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
    rownames = FALSE,
    filter = "top",
    extensions = 'Buttons',
    options = list(
      autoWidth = TRUE,
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
          buttons = c('csv', 'excel', 'pdf'),
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
