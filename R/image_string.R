#' Generate image URL
#' 
#' @description
#' These functions perform different parts of image name processing:
#' * `gcs_url()` Glues the inputs together to generate the proper GCS
#' Authenticated URL, returning this URL as a string
#' * `region_image_name_parse()`: Parses a region file name, and outputs a string
#' with the name of the user-specificed version of the parent image

#' Generate the GCS Authenticated URL for the user-specified shadowgraph image
#' 
#' @param z string; name of GCP bucket
#' @param bucket_name string; name of GCP bucket
#' @param glider_deployment string; name of glider deployment as glider-YYYYmmdd
#' @param image_type string; image type, aka name of intermediary path folder.
#'   Must be one of: 'images-ffPCG', 'images-imgff', 'jpgorig-regions'
#' @param directory_name string; directory name
#' @param image_name string; name of the shadowgraph image. Note that for
#'   `region_image_name_parse()`, this must be the name of a region image (see
#'   Examples section)
#' 
#' @rdname image_string
#' 
#' @return 
#' string
#' 
#' @examples
#' gcs_url("amlr-gliders-imagery-raw-dev", "amlr08-20220513", "images", 
#'         "Dir0000", "SG01 20220513-190538-011.jpg")
#' gcs_url("amlr-gliders-imagery-proc-dev", "amlr08-20220513", "images-ffPCG", 
#'         "Dir0000", "SG01 20220513-190538-011-ffPCG.png")
#'         
#' region_image_name_parse("SG01 20220510-213508-001.jpg-out0.jpg", 
#'                          "amlr-gliders-imagery-proc-dev", "images-ffPCG")
#' region_image_name_parse("SG01 20220510-213508-001.jpg-out0.jpg", 
#'                          "amlr-gliders-imagery-raw-dev")
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


#' @rdname image_string
#' @export
region_image_name_parse <- function(image_name, bucket_name, image_type = "") {
  # Split image name by "-", and remove last item. This removes eg "-out0.jpg"
  img.name <- paste(head(
    unlist(str_split_1(image_name, "-")), 
    -1
  ), collapse = "-")
  
  # Based on the user-provided bucket, add file suffix
  if (bucket_name == "amlr-gliders-imagery-raw-dev") {
    img.name
  } else if (bucket_name == "amlr-gliders-imagery-proc-dev") {
    if (identical(image_type, "")) stop("Image type must not be null for proc bucket")
    paste0(
      str_remove(img.name, "\\.jpg$"), 
      switch(image_type,
             `images-ffPCG` = "-ffPCG.png",
             `images-imgff` = "-imgff.png",
             `jpgorig-regions` = ".jpgorig-regions.jpg")
    )
  } else {
    stop("Invalid bucket name")
  }
}
