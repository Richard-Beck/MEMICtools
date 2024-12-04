library(magick)
base_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/"
setwd(base_dir)

output_dir <- "processed_images/zstack_validation/"
dir.create(output_dir,recursive = T)
input_dir <- "raw_images/"

x <- readRDS("processed_images/metadata.Rds")
x <- x[x$Channel==2,]
x <- split(x,f=interaction(x$Row,x$Col))

lapply(x,function(xi){
  x1 <- xi[xi$Timepoint==1,]
  fields <- unique(x1$Field)
  x1 <- x1[x1$Field==sample(fields,1),]
  x1 <- x1[order(x1$Plane),]
  tiff_files <- x1$URL
  processed_images <- lapply(tiff_files,function(file){
    image <- image_read(paste0(input_dir,file))
    image <- image_resize(image, "1000x1000!")
    image <- image_normalize(image)
    image <- image_convert(image, depth = 8)
    return(image)
  })
  multichannel_image <- image_join(processed_images)
  saveAs <- paste0(output_dir,"r",x1$Row[1],"c",x1$Col[1],"f",x1$Field[1],"t",x1$Timepoint[1],".tiff")
  image_write(multichannel_image, path = saveAs, format = "tiff")
  
  x1 <- xi[xi$Timepoint==10,]
  fields <- unique(x1$Field)
  x1 <- x1[x1$Field==sample(fields,1),]
  x1 <- x1[order(x1$Plane),]
  tiff_files <- x1$URL
  
  processed_images <- lapply(tiff_files,function(file){
    image <- image_read(paste0(input_dir,file))
    image <- image_resize(image, "1000x1000!")
    image <- image_normalize(image)
    image <- image_convert(image, depth = 8)
    return(image)
  })
  multichannel_image <- image_join(processed_images)
  saveAs <- paste0(output_dir,"r",x1$Row[1],"c",x1$Col[1],"f",x1$Field[1],"t",x1$Timepoint[1],".tiff")
  image_write(multichannel_image, path = saveAs, format = "tiff")
  
})


