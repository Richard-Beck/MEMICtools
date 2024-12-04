library(tiff)
library(abind)
base_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/"
setwd(base_dir)

input_dir <- "processed_images/flattened_images/"
output_dir <- "processed_images/zstack_validation/"
dir.create(output_dir,recursive = T)

ff <- list.files(input_dir)
images_per_set <- 30
image_size <- 100

training <- lapply(1:images_per_set,function(dummyVar){
  fi <- sample(ff,1)
  ch1 <- gsub("ch2","ch1",fi)
  ch2 <- gsub("ch1","ch2",fi)
  ch1 <- readTIFF(paste0(input_dir,ch1))
  ch2 <- readTIFF(paste0(input_dir,ch2))
  
  dxy <- dim(ch1)
  xmin <- sample(1:(dxy[1]-image_size),1)
  xmax <- xmin+image_size-1
  ymin <- sample(1:(dxy[2]-image_size),1)
  ymax <- ymin+image_size-1
  
  ch1 <- ch1[xmin:xmax,ymin:ymax]
  ch2 <- ch2[xmin:xmax,ymin:ymax]
  
  x <- abind(ch1,ch2,along = 3)
  })

training <- abind(training(along=4))
writeTIFF(training,paste0(output_dir,"training.tiff"),bits.per.sample = 16)




