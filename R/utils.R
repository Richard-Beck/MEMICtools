
library(stringr)
library(tiff)
library(abind)

extract_fileids <- function(input_string){
  matches <- str_match_all(input_string, "([a-z]+)(\\d+)")[[1]]
  
  # Extract the names (prefixes) and numbers
  prefixes <- matches[, 2]
  numbers <- matches[, 3]
  
  # Create a named vector with numbers as strings
  named_vector <- setNames(as.character(numbers), prefixes)
  return(named_vector)
}

organize_filenames <- function(ff){
  df <- do.call(rbind,lapply(ff,extract_fileids))
  rownames(df) <- ff
  return(df)
}

compile_files <- function(ff,compile_on){
  df <- data.frame(organize_filenames(ff))
  compile_on <- unlist(strsplit(compile_on,split="_"))
  splitting_factors <- lapply(compile_on,function(ci) df[,ci])
  split(df,splitting_factors,drop=T)
}

flatten <- function(A,map){
  ##takes a 3d Image (A) and a 2D map.
  ##2d map contains indices for the 3d dimension of A
  ## result is a 2D image.
  
  # Create matrices for indexing into the map.
  i_map <- c(matrix(lut(1:nrow(A), nrow(A)/nrow(map)), nrow = nrow(A), ncol = ncol(A), byrow = FALSE))
  j_map <- c(matrix(lut(1:ncol(A), ncol(A)/ncol(map)), nrow = nrow(A), ncol = ncol(A), byrow = TRUE))
  
  # Extract slices from 'map' using vectorized indexing
  k_A <- map[cbind(i_map,j_map)]
  
  # Generate indices for extracting elements from 'A'
  i_A <- c(matrix(1:nrow(A), nrow = nrow(A), ncol = ncol(A), byrow = FALSE))
  j_A<- c(matrix(1:ncol(A), nrow = nrow(A), ncol = ncol(A), byrow = TRUE))
  
  
  # Reshape the result into a matrix
  y <- matrix(A[cbind(i_A, j_A,k_A)], nrow = nrow(A), ncol = ncol(A))
  return(y)
}

assemble_from_file <- function(mi,im_dir,field_map,ch,px,py){
  mi <- mi[mi[,"ch"]==ch,]
  mi <- split(mi,f=mi$p)
  print(paste("assembling image channel",ch))
  A <- pbapply::pblapply(mi, function(mij){
    A <- lapply(1:nrow(field_map),function(j){
      ims <- lapply(1:ncol(field_map),function(k){
        if(file.exists(paste0(im_dir,rownames(mij)[field_map[j,k]]))){
          return(readTIFF(paste0(im_dir,rownames(mij)[field_map[j,k]])))
        }else{
          return(matrix(0,nrow=px,ncol=py))
        }
      })
      abind(ims,along=2)
    })
    A <- abind(A,along=1)
  })
  A <- abind(A,along=3)
  return(A)
}

assemble_from_list <- function(input,field_map,mi){
  mi<- data.frame(mi)
  mi$lut <- 1:nrow(mi)
  mi <- split(mi,f=mi$p)
  A <- pbapply::pblapply(mi, function(mij){
    A <- lapply(1:nrow(field_map),function(j){
      ims <- lapply(1:ncol(field_map),function(k){
        input[[mij$lut[field_map[j,k]]]]
      })
      abind(ims,along=2)
    })
    A <- abind(A,along=1)
  })
  A <- abind(A,along=3)
  return(A)
}

create_gaussian_kernel <- function(size, sigma) {
  center <- (size - 1) / 2
  x <- matrix(rep(0:(size - 1), size), nrow = size) - center
  y <- t(x)
  kernel <- exp(-(x^2 + y^2) / (2 * sigma^2))
  kernel / sum(kernel)  # Normalize to make the sum 1
}

fft_smooth <- function(large_matrix,kernel_size,sigma){
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







 