#' shade: Shiny Shadow Fixer
#'
#' R package/Shiny app for fixing VIAME CSV output files. 
#' See [this bug](https://github.com/Kitware/dive/issues/1415) for more details.
#'
#' @name shade-package
#' @aliases shade
#' @title Shiny Shadow Fixer
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#'
#' @import dplyr
#' @import shiny
#' 
#' @importFrom DT DTOutput renderDT
#' @importFrom rclipboard rclipboardSetup rclipButton
#' @importFrom readr cols read_csv write_csv write_lines
#' @importFrom stringr str_glue str_pad str_remove str_replace_all str_split_1
#' @importFrom tidyr replace_na
#' @importFrom utils head
#'
#' @keywords package
"_PACKAGE"
