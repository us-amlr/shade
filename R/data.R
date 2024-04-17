#' ESD glider deployment data
#'
#' ESD glider deployments and project information. Incomplete list. Used for
#' generating lists for the shade Shiny app and Google Cloud Storage (GCS) URLs
#'
#' @format ## `glider.deployments`
#' A data frame:
#' \describe{
#'   \item{deployment}{Glider deployment code}
#'   \item{project}{ESD glider project code; either SANDIEGO or FREEBYRD}
#'   \item{year}{Year of deployment, matches GCS path}
#' }
"glider.deployments"


#' VIAME CSV column names
#'
#' The column names of a VIAME CSV file downloaded from VIAME-Web
#' 
#' @source 
#' <https://kitware.github.io/dive/DataFormats/>, 
#' <https://viame.readthedocs.io/en/latest/section_links/detection_file_conversions.html>
"viame.csv.names"


#' VIAME CSV metadata
#'
#' Components of line 2 of a VIAME CSV file, i.e. the static metadata
#' information. This data is used by `viame_csv_metadata()` to construct the
#' full metadata info
#' 
#' @source 
#' <https://kitware.github.io/dive/DataFormats/>, 
#' <https://viame.readthedocs.io/en/latest/section_links/detection_file_conversions.html>
"viame.csv.metadata"