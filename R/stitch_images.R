x <- readRDS("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/Images/metadata.Rds")
#x <- readRDS("~/projects/017_jax/MEMICtools/data/metadata.Rds")

library(parallel)

x <- x[x$Plane==1,]
x <- split(x,f=interaction(x$Row,x$Col,x$Timepoint))

ncores <- 20
cl <- makeCluster(getOption("cl.cores", ncores))

dir.create("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/finalImages/")
dir.create("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/finalPNGImages/")

parLapplyLB(cl=cl,X=x,fun=function(xi){
  tryCatch({
    library(tiff)
    library(png)
    library(abind)
    flattenedDir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/flattenedImages2/"
    outDir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/finalImages/"
    pngDir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/finalPNGImages/"
    nxy <- 2160
    empty_image <- matrix(0,nxy,nxy)
    ff <- list.files(flattenedDir)
    xi <- xi[order(xi$PositionX,-xi$PositionY),]
    c1 <- xi[xi$Channel==1,]
    c1 <- split(c1,f=c1$PositionX)
   
    c1 <- lapply(c1,function(ci){
      c1Col <- lapply(ci$URL,function(path){
        mapname <- paste0(substr(path,1,9),substr(path,13,nchar(path)))
        if(mapname%in%ff) return(readTIFF(paste0(flattenedDir,mapname)))
        return(empty_image)
      })
      abind(c1Col,along=1)
    })
    c1=abind(c1,along=2)
    
    c2 <- xi[xi$Channel==2,]
    c2 <- split(c2,f=c2$PositionX)
    
    c2 <- lapply(c2,function(ci){
      c2Col <- lapply(ci$URL,function(path){
        mapname <- paste0(substr(path,1,9),substr(path,13,nchar(path)))
        if(mapname%in%ff) return(readTIFF(paste0(flattenedDir,mapname)))
        return(empty_image)
      })
      abind(c2Col,along=1)
    })
    c2=abind(c2,along=2)
    
    a <- array(c(c1,c2),dim = c(nrow(c1), ncol(c2), 2))
    
    id <- paste0("r",stringr::str_pad(xi$Row[1],2,pad=0),
                 "c",stringr::str_pad(xi$Col[1],2,pad=0),
                 "t",stringr::str_pad(xi$Timepoint[1],2,pad=0),
                 ".tiff")
    
    writeTIFF(a, paste0(outDir,id), bits.per.sample = 16)
    
    normalize_image <- function(image) {
      # Identify the minimum and maximum pixel values
      min_val <- min(image)
      max_val <- max(image)
      
      # Apply linear normalization to stretch pixel values
      normalized_image <- (image - min_val) / (max_val - min_val)
      
      return(normalized_image)
    }
    
    c1 <- normalize_image(c1)
    c2 <- normalize_image(c2)
    
    R <- c2
    G <- matrix(pmin(1,c1 + c2),nrow = nrow(c2))
    B <- c2
    
    b <- array(c(R,G,B),dim=c(nrow(c1),ncol(c2),3))
    
    writePNG(b,paste0(pngDir,gsub(".tiff",".png",id)))
    
    
    
  },error=function(e) print(xi$URL[1]))
  
})
