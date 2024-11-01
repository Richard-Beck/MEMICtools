
proc <- function(mi){
  im_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/raw_images/"
  foc_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/LoG_Transforms/"
  outpath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/flattened_images/"
  mappath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/zstack_maps/"
  source("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/MEMICtools/R/utils.R")
  
  library(tiff)
  library(abind)
  
  gc()
  
  nm <- mi[1,apply(mi,2,function(xij) length(unique(xij))==1)]
  nm <- paste0(paste0(colnames(nm),nm[1,],sep=""),collapse="")
  
  field_map <- rbind(2:10,19:11,c(20:23,1,24:27),36:28,37:45)
  Ai1 <- assemble_from_file(mi,im_dir,field_map,1,2160,2160)
  Ai2 <- assemble_from_file(mi,im_dir,field_map,2,2160,2160)
  
  
  mi2 <- mi[mi[,"ch"]==2,]
  mapFiles <- paste0(foc_dir,rownames(mi2))
  maps <- lapply(mapFiles,function(mf){
    if(file.exists(mf)){
      return(readTIFF(mf))
    } else{
      return(matrix(0,nrow=270,ncol=270))
    }
  })
  maps <- pbapply::pblapply(maps,fft_smooth,kernel_size=25,sigma=10)
  map <- assemble_from_list(maps,field_map,mi2)
  map <- abind(map,along=3)
  map <- apply(map,c(1,2),which.max)
  writeTIFF(map/max(map),paste0(mappath,nm,".tiff"))
  
  Ai1 <- flatten(Ai1,map)
  Ai2 <- flatten(Ai2,map)
  
  a <- array(c(Ai1,Ai2),dim = c(nrow(Ai1), ncol(Ai2), 2))
  Ai1 <- 0
  Ai2 <- 0
  gc()
  writeTIFF(a,paste0(outpath,nm,".tiff"), bits.per.sample = 16)

  return(0)
}

library(parallel)
im_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/raw_images/"
source("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/MEMICtools/R/utils.R")

ff <- list.files(im_dir)
m <- compile_files(ff,compile_on="r_c_sk")
cl <- makeCluster(getOption("cl.cores", 8))
pbapply::pblapply(X = m,FUN=proc,cl=cl)




 