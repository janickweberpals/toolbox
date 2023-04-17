#' Automated \code{\link[gt:gt]{gt()}} Markdown/Quarto output for html or pdf
#'
#' @description
#' For complex tables, \code{\link[gt:gt]{gt()}} output formats are not optimized. This function
#' takes a finished \code{\link[gt:gt]{gt()}} table and outputs it either directly as an html table
#' or pdf table.
#'
#' Note that in an interactive session no output will be displayed and the output is dependent on if the
#' Markdown/Quarto documented is being rendered towards html or pdf
#'
#' @param tbl_gt gt object
#' @param img_file_path file path where table images should be stored in pdf compilations; default is the current project directory
#' @param zoom A number specifying the zoom factor. A zoom factor of 2 will result in twice as many pixels vertically and horizontally. Note that using 2 is not exactly the same as taking a screenshot on a HiDPI (Retina) device: it is like increasing the zoom to 200 doubling the height and width of the browser window. This differs from using a HiDPI device because some web pages load different, higher-resolution images when they know they will be displayed on a HiDPI device (but using zoom will not report that there is a HiDPI device).
#' @param dpi DPI (dots per inch) value. Used to calculate the output width (in inches) of the images. This will be their actual width in pixels, divided by dpi. If not provided, the chunk option dpi is used; if NA, the output width will not be calculated.
#' @return returns a pretty table formatted for html or latex
#'
#' @importFrom gt gtsave
#' @importFrom here here
#' @importFrom knitr is_latex_output is_html_output
#' @importFrom magrittr '%>%'
#' @importFrom webshot2 webshot
#'
#' @export
#'
format_gt <- function(tbl_gt = NULL, # gt table object
                      img_file_path = NULL, # file path for table images in pdf compilations
                      zoom = 2,
                      dpi = 300
                      ){


  # only works at compiling stages of markdown/quarto documents
  if(!isTRUE(knitr::is_html_output()) & !isTRUE(knitr::is_latex_output())){
    message("This function considers only tables in Markdown or Quarto documents, not interactively created tables.")
  }

  # html output is just bypassed
  if(knitr::is_html_output()){

    return(tbl_gt)

  }

  # pdf output is converted into an image
  # and then displayed via knitr::include_graphics()

  # first: where should image be stored?
  if(!knitr::is_html_output()){

    if(is.null(img_file_path)){

      img_file_path <- here::here()

    }

    img_file_directory <- paste0(img_file_path, "/gt_tables")

    if(!dir.exists(img_file_directory)){

      dir.create(img_file_directory)

    }


    # save gtsave output as an image
    gt::gtsave(
      data = tbl_gt,
      filename = paste0(substitute(tbl_gt), ".png"),
      path = img_file_directory,
      zoom = zoom
      )

    # "plot" table
    knitr::include_graphics(
      path = here::here(img_file_directory, paste0(substitute(tbl_gt), ".png")),
      dpi = dpi
      )

    }

  }

