
LoG_transform <- function(filename,inpath,outpath){
  result <- tryCatch({
    inpath <- paste0(inpath,filename)
    outpath <- paste0(outpath,filename)
    xi <- image_read(inpath)
    xi <- image_resize(xi,geometry_size_percent(50),filter = "Gaussian")
    xi <- image_convolve(image=xi,kernel="Laplacian")
    
    xi <- image_resize(xi,geometry_size_percent(50),filter = "Gaussian")
    xi <- image_resize(xi,geometry_size_percent(50),filter = "Gaussian")
    xi <- image_median(image=xi,radius=10)
    image_write(xi,outpath)
    image_destroy(xi)
    gc()
    result <- 0
  },error = function(e) return(1))
  return(result)
}

#args <- commandArgs(trailingOnly = TRUE)
#inpath <- args[1]
#outpath <- args[2]
#ncores <- args[3]

inpath <- "raw_images/"
outpath <- "LoG_Transforms/"
ncores <- 25

library(parallel)
ff <- list.files(inpath)
fdone <- list.files(outpath)
ff <- ff[!ff%in%fdone]
ff <- ff[order(ff)]
print(paste("found",length(ff),"files in",inpath))
cl <- makeCluster(getOption("cl.cores", ncores))
clusterEvalQ(cl,{
  Sys.setenv(MAGICK_THREAD_LIMIT = "4")
  library(magick)
})
print(paste("established cluster with",ncores,"cores"))
pbapply::pblapply(X = ff,FUN=LoG_transform,inpath=inpath,outpath=outpath,cl=cl)










 