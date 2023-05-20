#' Creates pretty formatted tables with few lines of code for both html and pdfs
#'
#' @param data dataframe, tibble or tableone object with table content
#' @param col_bold should first column be printed bold
#' @param font_size numeric, font size of table
#' @param caption string, if table caption is desired
#' @param html_font_size_x numeric, html table font size multiplier compared to font_size
#' @param ... more parameters passed on to `tableone`
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
                         col_bold = FALSE, # should first column be printed bold?
                         font_size = 10,
                         caption = NULL,
                         html_font_size_x = 1.6, # html table font size multiplier compared to font_size
                         ... # tableone print additions
                         ){

  arguments <- list(...)

  # checks
  if("TableOne" %in% class(data)){

    table_tmp <- print(
      data,
      smd = TRUE,
      printToggle = FALSE,
      ...
      ) %>%
      dplyr::as_tibble(rownames = "Variable")

    }else if("data.frame" %in% class(data)){

      table_tmp <- as.data.frame(data)

    }else{

      stop("data is not a TableOne or data.frame/tibble object")

      }

  # pdf table call starts
  return_table <- table_tmp %>%

    kableExtra::kable(
      format = "latex",
      caption = caption,
      booktabs = TRUE,
      longtable = TRUE,
      align = "l",
      linesep = ""
      ) %>%

    kableExtra::kable_styling(
      font_size = font_size,
      latex_options = c("repeat_header", "scale down")
      )

  # if html
  if(knitr::is_html_output()){

    return_table <- table_tmp %>%

      kableExtra::kable(
        format = "html",
        caption = caption,
        booktabs = TRUE,
        longtable = TRUE,
        align = "l",
        linesep = ""
        ) %>%

      kableExtra::kable_classic(
        lightable_options = "hover",
        font_size = font_size*html_font_size_x,
        full_width = F,
        fixed_thead = TRUE,
        html_font = "Minion"
        )
  }

  # table header in bold font
  return_table <- return_table %>%
    kableExtra::row_spec(0, bold = TRUE)

  if("TableOne" %in% class(data)){

    # add indentation if true
    return_table <- return_table %>%
      kableExtra::add_indent(
        positions = as.numeric(which(stringr::str_starts(table_tmp$Variable, pattern = " ")==TRUE)),
        level_of_indent = 1,
        all_cols = FALSE
      )

  }

  # first column bold
  if(isTRUE(col_bold)){

    vec_bold <- as.numeric(which(stringr::str_starts(table_tmp$Variable, pattern = " ", negate = TRUE)==TRUE))

    return_table <- return_table %>%
      kableExtra::column_spec(1:1, bold = TRUE)

  }

  return(return_table)

}

