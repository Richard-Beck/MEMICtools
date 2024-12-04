library(tiff)
library(jpeg)
library(imager)

# Define input and output directories
input_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/flattened_images/"
output_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/display_outputs/"

# Histogram equalization function using imager
histogram_equalization <- function(channel) {
  # Convert to imager format
  img <- as.cimg(channel)
  # Apply histogram equalization
  equalized_img <- imager::equalize(img)
  # Convert back to matrix
  as.matrix(equalized_img)
}

# List all TIFF files in the input directory
file_list <- list.files(input_dir, pattern = "\\.tiff$", full.names = TRUE)

# Loop through each file
for (file_path in file_list) {
  # Read the TIFF image as an array
  img_array <- readTIFF(file_path)
  
  # Apply histogram equalization to channel 1
  ch1 <- img_array[,,1]
  ch1 <- histogram_equalization(ch1)  # Equalize histogram for channel 1
  
  # Apply histogram equalization to channel 2
  ch2 <- img_array[,,2]
  ch2 <- histogram_equalization(ch2)  # Equalize histogram for channel 2
  
  # Create an RGB array using pmin(ch1 + ch2, 1) for intensity capping
  rgb_array <- array(0, dim = c(dim(ch1), 3))   # Initialize RGB array
  rgb_array[,,1] <- ch2/2         # Red channel for blended grayscale
  rgb_array[,,2] <- pmin(ch1 + ch2/2, 1)          # Green channel for green-tinted ch1
  rgb_array[,,3] <- ch2/2          # Blue channel for blended grayscale
  
  # Generate output path
  output_path <- file.path(output_dir, paste0(basename(tools::file_path_sans_ext(file_path)), ".jpg"))
  
  # Save as JPEG
  writeJPEG(rgb_array, target = output_path, quality = 1)
  
  print(paste("Processed and saved:", output_path))
}
