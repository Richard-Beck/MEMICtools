x <- readRDS("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/trial_objects.Rds")
#x <- readRDS("~/projects/017_jax/MEMICtools/data/trial_objects.Rds")
nxy <- 2160

x <- split(x,f=x$Field)
library(tiff)
outDir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/objectImages/"
dir.create(outDir)
x <- x[order(sapply(x,nrow))]
lapply(x,function(xi){
  x0 <- matrix(0,nxy,nxy)
  for(b in xi$`Bounding Box`){
    b <- as.numeric(unlist(strsplit(substr(b,2,nchar(b)-1),split="[,]")))
    v1 <- b[1]:b[3]
    v2 <- b[2]:b[4]
    bcoords <- rbind(cbind(c(v1,v1),c(rep(b[2],length(v1)),rep(b[4],length(v1)))),
                     cbind(c(rep(b[1],length(v2)),rep(b[3],length(v2))),c(v2,v2)))
    x0[bcoords] <- 1
  }
  print(sum(x0))
  
  
  id <- paste0("r",stringr::str_pad(xi$Row[1],2,pad=0),
               "c",stringr::str_pad(xi$Col[1],2,pad=0),
               "f",stringr::str_pad(xi$Field[1],2,pad=0),
               "t",stringr::str_pad(xi$Timepoint[1],2,pad=0),
               ".tiff")
  
  writeTIFF(x0,paste0(outDir,id),bits.per.sample = 16)
})





