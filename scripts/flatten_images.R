map_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/slice_maps/"
im_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/raw_images/"
outpath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/flattened_images/"
source("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/MEMICtools/R/utils.R")
maps <- list.files(map_dir)
ff <- list.files(im_dir)
dir.create(outpath)


m <- compile_files(ff,compile_on="r_c_sk")
names(m) <- sapply(m,function(xi){
  nm <- xi[1,apply(xi,2,function(xij) length(unique(xij))==1)]
  nm <- paste0(paste0(colnames(nm),nm[1,],sep=""),collapse="")
  return(nm)
})

names(maps) <- gsub("ch2","",maps)
names(maps) <- gsub(".Rds","",names(maps))

m <- m[names(m)%in%names(maps)]
maps <- maps[names(maps)%in%names(m)]
field_map <- rbind(2:10,19:11,c(20:23,1,24:27),36:28,37:45)
library(tiff)
library(abind)
lut <- function(coord,sf) ceiling(coord/sf)

for(i in 1:length(m)){
  
  id <- names(maps)[i]
  map <- readRDS(paste0(map_dir,maps[id]))
  print(id)
  mi0 <- data.frame(m[[id]])
  mi <- mi0[mi0$ch==2,]
  mi <- split(mi,f=mi$p)
  ch2 <- lapply(mi, function(mij){
    ch2 <- lapply(1:nrow(field_map),function(j){
      ims <- lapply(1:ncol(field_map),function(k){
        readTIFF(paste0(im_dir,rownames(mij)[field_map[j,k]]))
      })
      abind(ims,along=2)
    })
    ch2 <- abind(ch2,along=1)
  })
  ch2 <- abind(ch2,along=3)
  
  y2 <- do.call(rbind,pbapply::pblapply(1:nrow(ch2),function(j){
    il <- lut(j,nrow(ch2)/nrow(map))
    k <- 1:ncol(ch2)
    l <- map[il,lut(k,nrow(ch2)/nrow(map))]
    sapply(1:length(l),function(xx) ch2[j,k[xx],l[xx]])
  }))
  
  mi <- mi0[mi0$ch==1,]
  mi <- split(mi,f=mi$p)
  ch1 <- lapply(mi, function(mij){
    ch1 <- lapply(1:nrow(field_map),function(j){
      ims <- lapply(1:ncol(field_map),function(k){
        readTIFF(paste0(im_dir,rownames(mij)[field_map[j,k]]))
      })
      abind(ims,along=2)
    })
    ch1 <- abind(ch1,along=1)
  })
  ch1 <- abind(ch1,along=3)
  
  y1 <- do.call(rbind,pbapply::pblapply(1:nrow(ch1),function(j){
    il <- lut(j,nrow(ch1)/nrow(map))
    k <- 1:ncol(ch1)
    l <- map[il,lut(k,nrow(ch1)/nrow(map))]
    sapply(1:length(l),function(xx) ch1[j,k[xx],l[xx]])
  }))
  
  a <- array(c(y1,y2),dim = c(nrow(y1), ncol(y2), 2))
  writeTIFF(a, paste0(outpath,id,".tiff"), bits.per.sample = 16)
}







 