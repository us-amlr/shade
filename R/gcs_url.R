#' Generate image URL
#' 
#' Generate the GCS Authenticated URL for the user-specified shadowgraph image
#' 
#' @param bucket_name string; name of GCP bucket
#' @param glider_deployment string; name of glider deployment as glider-YYYYmmdd
#' @param image_type string; image type, aka name of intermediary path folder
#' @param directory_name string; directory name
#' @param image_name string; name of the shadowgraph image
#' 
#' @details
#' Glues the inputs together to generate the proper GCS Authenticated URL.
#' 
#' @return
#' The authenticated URL, as a string
#' 
#' @examples
#' gcs_url("amlr-gliders-imagery-raw-dev", "amlr08-20220513", "images", 
#'         "Dir0000", "SG01 20220513-190538-011.jpg")
#' 
#' @export
gcs_url <- function(bucket_name, glider_deployment, image_type, 
                    directory_name, image_name) 
{
  image_name <- str_replace_all(image_name, " ", "%20")
  
  depl.df <- glider.deployments %>% filter(deployment == glider_deployment)
  
  str_glue("https://storage.cloud.google.com/{bucket_name}/", 
           "{depl.df$project}/{depl.df$year}/{glider_deployment}/", 
           "{image_type}/{directory_name}/{image_name}")
}
