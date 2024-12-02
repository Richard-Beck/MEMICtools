x <- readRDS("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/Images/metadata.Rds")
#x <- readRDS("~/projects/017_jax/MEMICtools/data/metadata.Rds")
library(tiff)
library(abind)
map_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/sliceMaps2/"
outDir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/flattenedImages2/"
x <- split(x,f=interaction(x$Row,x$Col,x$Channel,x$Timepoint,x$Field))

lapply(x,function(xi){
  tryCatch({
    xi <- xi[order(xi$Plane),]
    
    ims <- lapply(xi$URL,function(imname){
      readTIFF(paste0("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/raw_images/",imname))
    })
    ims <- abind(ims,along=3)
    
    mapname <- xi$URL[1]
    saveAs <- paste0(substr(mapname,1,9),substr(mapname,13,nchar(mapname)))
    mapname <- gsub(".tiff",".Rds",mapname)
    mapname <- paste0(substr(mapname,1,6),substr(mapname,13,nchar(mapname)))
    maps <- list.files(map_dir)
    mapsMod <- gsub("fxxpxx","",maps)
    mapID <- maps[mapsMod==mapname]
    map <- readRDS(paste0(map_dir,mapID))
    
    lut <- function(coord,sf) ceiling(coord/sf)
    y1 <- do.call(rbind,pbapply::pblapply(1:nrow(ims),function(j){
      il <- lut(j,nrow(ims)/nrow(map))
      k <- 1:ncol(ims)
      l <- map[il,lut(k,nrow(ims)/nrow(map))]
      sapply(1:length(l),function(xx) ims[j,k[xx],l[xx]])
    }))
    writeTIFF(y1,paste0(outDir,saveAs))
  },error=function(e) print(xi$URL[1]))
  
})
