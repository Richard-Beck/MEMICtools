#x <- readLines("~/../Downloads/Index.xml",n = 50)
library(xml2)
library(dplyr)

# Load the XML file
xml_file <- read_xml("/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/Images/Index.xml")
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

saveRDS(image_data,"/mnt/andor_lab/Jackson/Jackson_Operaphenix/240717_SUM159_MEMIC/Images/metadata.Rds")
