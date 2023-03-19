#' Creates pretty formatted tables with few lines of code for both html and pdfs
#'
#' @param data dataframe, tibble or tableone object with table content
#' @param col_bold should first column be printed bold
#' @param font_size numeric, font size of table
#' @param caption string, if table caption is desired
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
format_table <- function(data = NULL, # tableone object or final df
                         col_bold = FALSE, # should first column be printed bold
                         font_size = 15,
                         caption = NULL,
                         ...
                         ){

  arguments <- list(...)

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

    stop("data is not a TableOne or data.frame/tibble object")

  )


  vec_indent <- as.numeric(which(stringr::str_starts(table_tmp$Variable, pattern = " ")==TRUE))
  vec_bold <- as.numeric(which(stringr::str_starts(table_tmp$Variable, pattern = " ", negate = TRUE)==TRUE))

  # table call starts
  return_table <- table_tmp %>%

    kableExtra::kable(
      format = "html",
      booktabs = TRUE,
      linesep = "",
      longtable = TRUE,
      caption = caption
      ) %>%

    kableExtra::kable_classic(
      lightable_options = "hover",
      font_size = font_size,
      full_width = FALSE,
      fixed_thead = TRUE,
      html_font = "Minion"
      ) %>%

    # kableExtra::kable_styling(
    #   latex_options = c("repeat_header"),
    #   repeat_header_continued = "\\textit{(Continued on next page...)}"
    #   ) %>%

    # indentation of rows with categorical variables
    kableExtra::add_indent(
      vec_indent,
      level_of_indent = 1,
      all_cols = FALSE
      ) %>%

    # table header in bold font
    kableExtra::row_spec(0, bold = TRUE)

  if(isTRUE(return_table)){

    # first column bold
    return_table <- return_table %>%
      kableExtra::column_spec(1:1, bold = TRUE)

    }

  return(return_table)

}
# END
