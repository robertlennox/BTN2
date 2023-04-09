## code to prepare `DATASET` dataset goes here


osterfjord<-sf::st_read("StudySite.shp")
usethis::use_data(osterfjord, overwrite = TRUE)
