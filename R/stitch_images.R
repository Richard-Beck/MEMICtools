x <- readRDS("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/Images/metadata.Rds")
#x <- readRDS("~/projects/017_jax/MEMICtools/data/metadata.Rds")
library(tiff)
library(abind)
map_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/sliceMaps2/"
x <- split(x,f=interaction(x$Row,x$Col,x$Channel,x$Timepoint,x$Field))

lapply(x,function(xi){
  
  xi <- xi[order(xi$Plane),]
  
  ims <- lapply(xi$URL,function(imname){
    readTIFF(paste0("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/raw_images/",imname))
  })
  ims <- abind(ims,along=3)
  
  mapname <- xi$URL[1]
  mapname <- paste0(substr(mapname,1,6),substr(mapname,13,nchar(mapname)))
  maps <- list.files(mapdir)
  mapsMod <- gsub("fxxpxx","",maps)
  mapId <- maps[mapsMod==mapname]
  
})
# Your string
# Your string
target_string <- "r01c01f01p01-ch1sk1.tiff"

# Vector of strings to search in
vector_of_strings <- c(
  "r01c01f01p01-ch1sk1.tiff",
  "r01c01f02p02-ch1sk1.tiff",
  "r01c01f03p03-ch1sk1.tiff",
  "r02c01f01p01-ch1sk1.tiff
)

# Create a regex pattern from the target string, replacing f01p01 with f\\d{2}p\\d{2}
pattern <- gsub("f\\d{2}p\\d{2}", "f\\d{2}p\\d{2}", target_string)

# Escape dots in the pattern
pattern <- gsub("\\.", "\\\\.", pattern)

# Find matches in the vector using grep
matches <- grep(pattern, vector_of_strings, value = TRUE)

# Output matches
print(matches)
