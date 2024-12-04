library(xml2)
library(parallel)

base_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/"
MEMICtools_dir <- "/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/MEMICtools/"
source(paste0(MEMICtools_dir,"R/pipeline_A_functions.R"))
setwd(base_dir)

## these files/folders are required inputs to this pipeline
metadata_input_filepath <- "Images/Index.xml"
object_data_path <- "objects.txt"
raw_image_dir <- "raw_images/"

## these folders will be created as outputs to this pipeline
metadata_processed_filepath <- "processed_images/metadata.Rds"
zstack_map_dir <- "processed_images/zstack_maps/"
zstack_im_dir <- "processed_images/zstack_ims/"
flattened_image_dir <- "processed_images/flattened_images/"
bounding_box_dir <- "processed_images/bounding_boxes/"
png_out_dir <- "processed_images/final_rgb"
tiff_out_dir <- "processed_images/final_tiff"

for(d in c(zstack_map_dir,zstack_im_dir,flattened_image_dir,bounding_box_dir,
           png_out_dir,tiff_out_dir)) dir.create(d,recursive = TRUE)

xml_file <- read_xml(metadata_input_filepath)
metadata <- process_metadata(xml_file)
saveRDS(metadata,metadata_processed_filepath)

cl10 <- makeCluster(getOption("cl.cores", 10))
cl40 <- makeCluster(getOption("cl.cores", 40))

x <- split(metadata,f=interaction(metadata$Row,metadata$Col,metadata$Channel,metadata$Timepoint))
parLapplyLB(cl=cl40,X=x,fun=makeZstackMaps,rds_dir=zstack_map_dir,tiff_dir=zstack_im_dir)

x <- split(metadata,f=interaction(metadata$Row,metadata$Col,metadata$Channel,metadata$Timepoint,metadata$Field))
parLapplyLB(cl=cl40,X=x,fun=flatten_stacks,map_dir=zstack_map_dir,outDir=flattened_image_dir,raw_image_dir=raw_image_dir)


x <- data.table::fread(object_data_path)
x <- split(x,f=interaction(x$Row,x$Col,x$Timepoint,x$Field))
parLapplyLB(cl=cl40,X=x,fun=render_bounding_boxes,outDir=bounding_box_dir)

x <- metadata[metadata$Plane==1,]
x <- split(x,f=interaction(x$Row,x$Col,x$Timepoint))
parLapplyLB(cl=cl10,X=x,fun=stitch_images,flattenedDir=flattened_image_dir,
            outDir=tiff_out_dir,pngDir=png_out_dir,objectsDir=bounding_box_dir)

