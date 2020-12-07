## code to prepare `Gigante_control` dataset goes here
file <- "./data/Gigante_removal.lat9.000lon-79.000.css"
Gigante_removal <- read.table(file,header = TRUE)

usethis::use_data(Gigante_removal)

