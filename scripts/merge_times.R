inpath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/LoG_Transforms_Merged/"
outpath <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/LoG_Transforms_Full_Merged/"
dir.create(outpath)
source("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/MEMICtools/R/utils.R")
library(tiff)
ff <- list.files(inpath)
ff <- ff[grepl("ch2",ff)]
compile_on <- "r_c_sk"

x <- compile_files(ff,compile_on)
x <- x[sapply(x,nrow)==7]
y <- lapply(x,function(xi){
  nm <- xi[1,apply(xi,2,function(xij) length(unique(xij))==1)]
  nm <- paste0(c(paste0(colnames(nm),nm[1,],sep=""),".tiff"),collapse="")
  xi <- xi[order(xi[,"p"]),]
  yi <- lapply(rownames(xi),readTIFF)
  writeTIFF(yi,paste0(outpath,nm),bits.per.sample = 16)
})

















 