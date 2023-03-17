#' Creates pretty formatted tables with few lines of code for both html and pdfs
#'
#' @param data dataframe, tibble or tableone object with table content
#' @param format_out character, one of "html" or "latex" to choose from for optimized output
#' @param font_size numeric, font size of table
#' @param caption string, if table caption is desired
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
format_table <- function(data = NULL, # tableone object or final df
                         format_out = "html", # can be of type html or latex
                         font_size = 16,
                         caption = NULL
                         ){

  # checks
  if("TableOne" %in% class(data)){

    table_tmp <- print(
      data,
      smd = TRUE,
      printToggle = FALSE
    ) %>%
      dplyr::as_tibble(rownames = "Variable")

  }else if("data.frame" %in% class(data)){

    table_tmp <- as.data.frame(data)

  }else(

    stop("data is not a TableOne or data.frame object")

  )


  vec_indent <- as.numeric(which(stringr::str_starts(table_tmp$Variable, pattern = " ")==TRUE))
  vec_bold <- as.numeric(which(stringr::str_starts(table_tmp$Variable, pattern = " ", negate = TRUE)==TRUE))

  # table call starts
  return_table <- table_tmp %>%

    kableExtra::kable(
      booktabs = TRUE,
      linesep = "",
      longtable = TRUE,
      format = format_out,
      caption = caption
      ) %>%

    kableExtra::kable_classic(
      lightable_options = "hover",
      font_size = font_size,
      full_width = FALSE,
      fixed_thead = TRUE,
      html_font = "Minion"
      ) %>%

    # indentation of rows with categorical variables
    kableExtra::add_indent(
      vec_indent,
      level_of_indent = 1,
      all_cols = FALSE
      ) %>%

    # table header in bold font
    kableExtra::row_spec(0, bold = TRUE)
  #kableExtra::row_spec(vec_bold, bold = TRUE) %>%
  #kableExtra::column_spec(1:1, bold = TRUE)

  return(return_table)

}
# END
