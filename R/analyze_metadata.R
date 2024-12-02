x <- readRDS("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/Images/metadata.Rds")
library(tiff)
dxy <- 0.001217608
nxy <- 2160

x <- split(x,f=interaction(x$Row,x$Col,x$Channel,x$Timepoint))

lapply(x,function(xi){
  
  id <- paste0("r",stringr::str_pad(xi$Row[1],2,pad=0),
               "c",stringr::str_pad(xi$Col[1],2,pad=0),
               "fxxpxx-",
               "ch",xi$Channel[1],
               "sk",xi$Timepoint[1],
               "fk1fl1.tiff")
  
  fit <- lm(AbsPositionZ~PositionX+PositionY,data=xi)
  y <- split(xi,f=xi$Field)
  y <- split(x,f=x$Field)
  
  yi <- y[[1]]
  x0 <- yi$PositionX[1]
  y0 <- yi$PositionY[1]
  
  im <- expand.grid(PositionX=seq(0,dxy,length.out = nxy),
                    PositionY=seq(0,dxy,length.out = nxy))
  
  
  im$AbsPositionZ <- predict(fit,im)
  
  
  im$plane <- pbapply::pbsapply(im$AbsPositionZ,function(z){
    yi$Plane[which.min(abs(z-yi$AbsPositionZ))]
  })
  
  m <- reshape2::acast(im,PositionX~-PositionY,value.var = "plane")/max(yi$Plane)
  
  writeTIFF(m,paste0("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/sliceMaps2/",id))
  
})
