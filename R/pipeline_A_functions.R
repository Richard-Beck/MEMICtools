process_metadata <- function(xml_file){
  require(xml2)
  ns <- xml_ns(xml_file)
  
  # Navigate to the `Images` node
  images_node <- xml_find_first(xml_file, ".//d1:Images", ns)
  
  # Extract all `Image` nodes under `Images`
  images <- xml_find_all(images_node, ".//d1:Image", ns)
  
  # Check if nodes are found
  print(length(images))  # Should give the count of `Image` nodes
  
  # Create a data frame by extracting the required elements
  image_data <- pbapply::pblapply(images, function(image) {
    data.frame(
      Row=as.numeric(xml_text(xml_find_first(image, ".//d1:Row", ns))),
      Col=as.numeric(xml_text(xml_find_first(image, ".//d1:Col", ns))),
      Field=as.numeric(xml_text(xml_find_first(image, ".//d1:FieldID", ns))),
      Plane=as.numeric(xml_text(xml_find_first(image, ".//d1:PlaneID", ns))),
      Timepoint=as.numeric(xml_text(xml_find_first(image, ".//d1:TimepointID", ns))),
      URL = xml_text(xml_find_first(image, ".//d1:URL", ns)),
      Channel = as.numeric(xml_text(xml_find_first(image, ".//d1:ChannelID", ns))),
      PositionX = as.numeric(xml_text(xml_find_first(image, ".//d1:PositionX", ns))),
      PositionY = as.numeric(xml_text(xml_find_first(image, ".//d1:PositionY", ns))),
      PositionZ = as.numeric(xml_text(xml_find_first(image, ".//d1:PositionZ", ns))),
      AbsPositionZ = as.numeric(xml_text(xml_find_first(image, ".//d1:AbsPositionZ", ns))),
      AbsTime = xml_text(xml_find_first(image, ".//d1:AbsTime", ns))
    )
  })
  
  image_data <- do.call(rbind,image_data)
  return(image_data)
}

makeZstackMaps <- function(xi,rds_dir,tiff_dir=NULL,nxy=2160,dxy=0.001276044){
  require(tiff)
  tryCatch({
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
    
    m <- reshape2::acast(im,-PositionY~PositionX,value.var = "plane")
    if(!is.null(tiff_dir)){
      writeTIFF(m/8,paste0(tiff_dir,id))
    }
    if(!file.exists(rds_dir)) dir.create(rds_dir)
    saveRDS(m,paste0(rds_dir,gsub(".tiff",".Rds",id)))
    return(0)
  },error=function(e) print(paste("failed with",xi$URL[1])))
}

flatten_stacks <- function(xi,map_dir,outDir,raw_image_dir){
  require(tiff)
  require(abind)
  require(parallel)
  tryCatch({
    xi <- xi[order(xi$Plane),]
    
    ims <- lapply(xi$URL,function(imname){
      readTIFF(paste0(raw_image_dir,imname))
    })
    ims <- abind(ims,along=3)
    
    mapname <- xi$URL[1]
    saveAs <- paste0(substr(mapname,1,9),substr(mapname,13,nchar(mapname)))
    mapname <- gsub(".tiff",".Rds",mapname)
    mapname <- paste0(substr(mapname,1,6),substr(mapname,13,nchar(mapname)))
    maps <- list.files(map_dir)
    mapsMod <- gsub("fxxpxx","",maps)
    mapID <- maps[mapsMod==mapname]
    map <- readRDS(paste0(map_dir,mapID))
    
    lut <- function(coord,sf) ceiling(coord/sf)
    y1 <- do.call(rbind,pbapply::pblapply(1:nrow(ims),function(j){
      il <- lut(j,nrow(ims)/nrow(map))
      k <- 1:ncol(ims)
      l <- map[il,lut(k,nrow(ims)/nrow(map))]
      sapply(1:length(l),function(xx) ims[j,k[xx],l[xx]])
    }))
    writeTIFF(y1,paste0(outDir,saveAs), bits.per.sample = 16)
  },error=function(e) print(xi$URL[1]))
  
}

render_bounding_boxes <- function(xi,outDir,nxy=2160){
  require(tiff)
  x0 <- matrix(0,nxy,nxy)
  for(b in xi$`Bounding Box`){
    b <- as.numeric(unlist(strsplit(substr(b,2,nchar(b)-1),split="[,]")))
    v1 <- b[1]:b[3]
    v2 <- b[2]:b[4]
    bcoords <- rbind(cbind(c(rep(b[2],length(v1)),rep(b[4],length(v1))),c(v1,v1)),
                     cbind(c(v2,v2),c(rep(b[1],length(v2)),rep(b[3],length(v2)))))
    x0[bcoords] <- 1
  }
  print(sum(x0))
  
  
  id <- paste0("r",stringr::str_pad(xi$Row[1],2,pad=0),
               "c",stringr::str_pad(xi$Col[1],2,pad=0),
               "f",stringr::str_pad(xi$Field[1],2,pad=0),
               "t",stringr::str_pad(xi$Timepoint[1],2,pad=0),
               ".tiff")
  
  writeTIFF(x0,paste0(outDir,id),bits.per.sample = 8)
}

stitch_images <- function(xi,flattenedDir,outDir,pngDir,objectsDir,nxy=2160){
  tryCatch({
    require(tiff)
    require(png)
    require(abind)

    empty_image <- matrix(0,nxy,nxy)
    ff <- list.files(flattenedDir)
    fobj <- list.files(objectsDir)
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
    
    ##
    c3 <- xi[xi$Channel==2,]
    c3 <- split(c3,f=c3$PositionX)
    
    c3 <- lapply(c3,function(ci){
      c3Col <- lapply(1:nrow(ci),function(i){
        id <- paste0("r",stringr::str_pad(ci$Row[i],2,pad=0),
                     "c",stringr::str_pad(ci$Col[i],2,pad=0),
                     "f",stringr::str_pad(ci$Field[i],2,pad=0),
                     "t",stringr::str_pad(ci$Timepoint[i],2,pad=0),
                     ".tiff")
        if(id%in%fobj) return(readTIFF(paste0(objectsDir,id)))
        return(empty_image)
      })
      abind(c3Col,along=1)
    })
    c3=abind(c3,along=2)
    
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
    
    R <- matrix(pmin(1,c3 + c2),nrow = nrow(c2))
    G <- matrix(pmin(1,c1 + c2),nrow = nrow(c2))
    B <- c2
    
    b <- array(c(R,G,B),dim=c(nrow(c1),ncol(c2),3))
    
    writePNG(b,paste0(pngDir,gsub(".tiff",".png",id)))
    
    
    
  },error=function(e) print(xi$URL[1]))
  
}