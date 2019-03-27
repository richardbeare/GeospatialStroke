# convert all .png into .tiff

library(magick)
library(fs)

old_png_path <- dir_ls(path = here::here("article/figs/"),
                       type = "file",
                       regexp = "*.png$")

new_tiff_path <-  sub(pattern = "*.png$", 
                       replacement = ".tiff",
                       x = old_png_path)

png_to_tiff <- function(current_image_path,
                        new_image_path){
  image_read(current_image_path) %>%
    image_convert(format = "tiff") %>%
    image_write(new_image_path)
}

purrr::walk2(.x = old_png_path,
             .y = new_tiff_path,
             .f = png_to_tiff)
