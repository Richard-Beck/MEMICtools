library(tiff)
library(abind)
base_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/"
setwd(base_dir)

input_dir <- "processed_images/flattened_images/"
train_dir <- "processed_images/object_validation/train/"
test_dir <- "processed_images/object_validation/test/"
metadata_dir <- "processed_images/object_validation/metadata/" 
dir.create(train_dir,recursive = T)
dir.create(test_dir,recursive = T)
dir.create(metadata_dir,recursive = T)

ff <- list.files(input_dir)
images_per_set <- 30
image_size <- 200

training <- do.call(rbind,lapply(1:images_per_set,function(i){
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
  
  R <- ch2
  G <- matrix(pmin(1,ch1 + ch2),nrow = nrow(ch2))
  B <- ch2
  
  x <- array(c(R,G,B),dim=c(nrow(ch1),ncol(ch2),3))
  
  #x <- abind(ch1,ch2,along = 3)
  writeTIFF(x,paste0(train_dir,"train",i,".tiff"),bits.per.sample = 16)
  data.frame(URL=fi,input_dir,xmin,ymin,dimROI=image_size)
  }))

saveRDS(training,paste0(metadata_dir,"train.Rds"))

test <- do.call(rbind,lapply(1:images_per_set,function(i){
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
  
  R <- ch2
  G <- matrix(pmin(1,ch1 + ch2),nrow = nrow(ch2))
  B <- ch2
  
  x <- array(c(R,G,B),dim=c(nrow(ch1),ncol(ch2),3))
  
  #x <- abind(ch1,ch2,along = 3)
  writeTIFF(x,paste0(test_dir,"train",i,".tiff"),bits.per.sample = 16)
  data.frame(URL=fi,input_dir,xmin,ymin,dimROI=image_size)
}))

saveRDS(test,paste0(metadata_dir,"test.Rds"))




