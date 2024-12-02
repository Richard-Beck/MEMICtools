x <- readRDS("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/Images/metadata.Rds")
library(tiff)
library(parallel)

ncores <- 40

x <- split(x,f=interaction(x$Row,x$Col,x$Channel,x$Timepoint))

cl <- makeCluster(getOption("cl.cores", ncores))

parLapplyLB(cl=cl,X=x,fun=function(xi){
  tryCatch({
    dxy <- 0.001217608
    nxy <- 2160  
    id <- paste0("r",stringr::str_pad(xi$Row[1],2,pad=0),
                 "c",stringr::str_pad(xi$Col[1],2,pad=0),
                 "fxxpxx-",
                 "ch",xi$Channel[1],
                 "sk",xi$Timepoint[1],
                 "fk1fl1.tiff")
    
    fit <- lm(AbsPositionZ~PositionX+PositionY,data=xi)
    y <- split(xi,f=xi$Field)
    
    yi <- y[[1]]
    x0 <- yi$PositionX[1]
    y0 <- yi$PositionY[1]
    
    im <- expand.grid(PositionX=seq(0,dxy,length.out = nxy),
                      PositionY=seq(0,dxy,length.out = nxy))
    
    
    im$AbsPositionZ <- predict(fit,im)
    
    
    im$plane <- pbapply::pbsapply(im$AbsPositionZ,function(z){
      yi$Plane[which.min(abs(z-yi$AbsPositionZ))]
    })
    
    m <- reshape2::acast(im,PositionX~-PositionY,value.var = "plane")
    
    saveRDS(m,paste0("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/sliceMaps2/",gsub(".tiff",".Rds",id)))
    return(0)
  },error=function(e) print(xi$URL[1]))
  
})
