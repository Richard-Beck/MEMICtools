inpath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/LoG_Transforms/"
outpath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/LoG_Transforms_Merged/"
dir.create(outpath)
source("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/MEMICtools/R/utils.R")

ff <- list.files(inpath)
ff <- ff[grepl("ch2",ff)]
compile_on <- "r_c_p_sk"

x <- compile_files(ff,compile_on)

field_map <- rbind(2:10,19:11,c(20:23,1,24:27),36:28,37:45)

x <- x[sapply(x,nrow)==length(field_map)]

library(tiff)
library(abind)

lapply(x,function(xi){
  nm <- xi[1,apply(xi,2,function(xij) length(unique(xij))==1)]
  nm <- paste0(c(paste0(colnames(nm),nm[1,],sep=""),".tiff"),collapse="")
  print(nm)
  rows <- lapply(1:nrow(field_map),function(i){
    ims <- lapply(1:ncol(field_map),function(j){
      readTIFF(paste0(inpath,rownames(xi)[field_map[i,j]]))
    })
    abind(ims,along=2)
  })
  im <- abind(rows,along=1)
  writeTIFF(im,paste0(outpath,nm),bits.per.sample = 16)
})
















 