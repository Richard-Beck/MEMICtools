x <- readRDS("processed_images/metadata.Rds")
manDir <- "processed_images/zstack_validation/"
ff <- list.files(manDir)
ff <- ff[grepl(".csv",ff)]
mapDir <- "processed_images/zstack_maps/"
ftar <- list.files(mapDir)

resultsPath <- "processed_images/zstacK_val_results.Rds"

## need to remember frame is irrelevant - we use same map on all frames.
res <- do.call(rbind,lapply(ff,function(fi){
  ids <- strsplit(fi, "[^0-9]+")[[1]]
  ids <- ids[!ids==""]
  r_id <- paste0("r",stringr::str_pad(ids[1],2,pad="0"))
  c_id <- paste0("c",stringr::str_pad(ids[2],2,pad="0"))
  cond_id <- paste0(r_id,c_id)
  fsel <- ftar[grepl(cond_id,ftar)]
  other_id <- paste0("ch2sk",ids[4],"fk")
  id <- fsel[grepl(other_id,fsel)]
  xmach <- readRDS(paste0(mapDir,id))
  xman <- read.csv(paste0(manDir,fi))
  
  ##images come up as 6.45x6.45cm.
  xman$Xpx <- round(2160*xman$X/6.45)
  xman$Ypx <- round(2160*xman$Y/6.45)
  
  xman$SliceEst <- xmach[cbind(xman$Xpx,xman$Ypx)]
  
  xman$id <- fi

  return(xman)  
  
}))

saveRDS(res,resultsPath)

