
LoG_transform <- function(filename,inpath,outpath){
  inpath <- paste0(inpath,filename)
  outpath <- paste0(inpath,filename)
  xi <- image_read(inpath)
  xi <- image_resize(xi,geometry_size_percent(50),filter = "Gaussian")
  xi <- image_convolve(image=xi,kernel="Laplacian")
  
  xi <- image_resize(xi,geometry_size_percent(50),filter = "Gaussian")
  xi <- image_resize(xi,geometry_size_percent(50),filter = "Gaussian")
  xi <- image_median(image=xi,radius=10)
  image_write(xi,outpath)
  image_destroy(xi)
  gc()
  return(0)
}

args <- commandArgs(trailingOnly = TRUE)
inpath <- args[1]
outpath <- args[2]
ncores <- args[3]


library(parallel)

ff <- list.files(inpath)
ff <- ff[order(ff)]
cl <- makeCluster(getOption("cl.cores", ncores))
clusterEvalQ(cl,{
  Sys.setenv(MAGICK_THREAD_LIMIT = "1")
  library(magick)
})
parLapplyLB(cl,X = ff,fun=LoG_transform,inpath=inpath,outpath=outpath)











 