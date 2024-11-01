im_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/raw_images/"
foc_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/LoG_Transforms/"
outpath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/flattened_images/"
source("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/MEMICtools/R/utils.R")
ff <- list.files(im_dir)
m <- compile_files(ff,compile_on="r_c_sk")
library(tiff)

mi <- m[[1]]

field_map <- rbind(2:10,19:11,c(20:23,1,24:27),36:28,37:45)
Ai1 <- assemble_from_file(mi,im_dir,field_map,1,2160,2160)
Ai2 <- assemble_from_file(mi,im_dir,field_map,2,2160,2160)


mi2 <- mi[mi[,"ch"]==2,]
mapFiles <- paste0(foc_dir,rownames(mi2))
maps <- lapply(mapFiles,function(mf){
  if(file.exist(mf)){
    return(readTIFF(mf))
  } else{
    return(matrix(0,nrow=270,ncol=270))
  }
})
maps <- lapply(maps,fft_smooth,kernel_size=25,sigma=10)
map <- assemble_from_list(maps,field_map,mi2)

y1 <- flatten(A1,map)
y2 <- flatten(A2,map)

a <- array(c(y1,y2),dim = c(nrow(y1), ncol(y2), 2))
writeTIFF(a,"/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/tst_pipe.tiff", bits.per.sample = 16)







 