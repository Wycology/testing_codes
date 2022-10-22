# Testing codes ----------------------------------------------------------------------------------

# Created on 22nd 

# This is a repo for testing R codes

# Looping through raster files -------------------------------------------------------------------

library(terra)

files <- list.files(system.file("external", package = "sdm"),
                    pattern = ".asc$", full.names = TRUE)

b <- list()

for (i in 1:length(files)) {
  b[[i]] <- terra::rast(files[[i]])
  plot(b[[i]])
}

# END --------------------------------------------------------------------------------------------