inpath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/LoG_Transforms_Full_Merged/"
outpath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/slice_maps/"
dir.create(outpath)
library(tiff)
library(abind)

create_gaussian_kernel <- function(size, sigma) {
  center <- (size - 1) / 2
  x <- matrix(rep(0:(size - 1), size), nrow = size) - center
  y <- t(x)
  kernel <- exp(-(x^2 + y^2) / (2 * sigma^2))
  kernel / sum(kernel)  # Normalize to make the sum 1
}

fft_smooth <- function(large_matrix){
  kernel_size <- 25   # Adjust for smoother or sharper smoothing
  sigma <- 10
  kernel <- create_gaussian_kernel(kernel_size, sigma)
  padded_dim <- dim(large_matrix) + dim(kernel) - 1
  pad_large_matrix <- matrix(0, nrow = padded_dim[1], ncol = padded_dim[2])
  pad_kernel <- matrix(0, nrow = padded_dim[1], ncol = padded_dim[2])
  pad_large_matrix[1:dim(large_matrix)[1], 1:dim(large_matrix)[2]] <- large_matrix
  pad_kernel[1:dim(kernel)[1], 1:dim(kernel)[2]] <- kernel
  fft_large_matrix <- fft(pad_large_matrix)
  fft_kernel <- fft(pad_kernel)
  fft_result <- fft_large_matrix * fft_kernel
  smoothed_matrix <- Re(fft(fft_result, inverse = TRUE) / prod(padded_dim))
  smoothed_matrix <- smoothed_matrix[1:nrow(large_matrix), 1:ncol(large_matrix)]
  return(smoothed_matrix)
}

ff <- list.files(inpath)
pbapply::pblapply(ff,function(fi){
  x <- readTIFF(paste0(inpath,fi),all=T)
  x <- lapply(x,fft_smooth)
  y <- abind(x,along=3)
  y <- apply(y,c(1,2),which.max)
  fi <- gsub(".tiff",".Rds",fi)
  saveRDS(y,paste0(outpath,fi))
  return(0)
})





 