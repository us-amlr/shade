## code to prepare `glider-deployments` dataset goes here

glider.deployments <- tibble::tribble(
  ~deployment,       ~project, ~year, 
  "amlr08-20220513", "SANDIEGO", 2022,
  "amlr07-20221204", "FREEBYRD", 2023,
  "john-20240312",   "SANDIEGO", 2024,
  "ringo-20240312",  "SANDIEGO", 2024
)

usethis::use_data(glider.deployments, overwrite = TRUE)
