# install latest dev googledrive
# pak::pkg_install("tidyverse/googledrive")

# authorize google drive
library(googledrive)
drive_auth()

# Download the googledrive into docx
my_drive_id <- as_id("https://docs.google.com/document/d/1odvZbrnBuWLw-oxFyDhaaL5TdG2P23Oxr93iwp_XZ-0/edit#")

drive_download(my_drive_id,
               type = "text/plain",
               path = "article/geospatial-stroke",
               verbose = TRUE)

file.rename(from = "article/geospatial-stroke.txt",
            to = "article/geospatial-stroke.Rmd")


