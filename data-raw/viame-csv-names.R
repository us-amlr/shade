## code to prepare `viame_csv_names` dataset goes here

viame.csv.names <- c(
  "# 1: Detection or Track-id" = "track_id", 
  "2: Video or Image Identifier" = "image_name", 
  "3: Unique Frame Identifier" = "frame_id", 
  "4-7: Img-bbox(TL_x" = "TL_x", 
  "TL_y" = "TL_y", 
  "BR_x" = "BR_x", 
  "BR_y)" = "BR_y", 
  "8: Detection or Length Confidence" = "confidence", 
  "9: Target Length (0 or -1 if invalid)" = "target_length",
  "10-11+: Repeated Species" = "class_01", 
  "Confidence Pairs or Attributes" = "threshold_01"
)

viame.csv.metadata <- c(
  "# metadata", "exported_by: \"shade:r\""
)

usethis::use_data(viame.csv.names, viame.csv.metadata, overwrite = TRUE)
