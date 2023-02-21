# this script loads the unique locations files and extract the regional info for each location from shapefiles
# 1: a table relating each unique location to a shapefile feature ID
# 2: a table containing all regions names in the shapefile names related to the corresponding shapefile IDs

library("data.table");library("rgdal");library("raster")

setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField")

coords_locID <- readRDS("location_locationID") #read file in

wd_shp <- "/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/External_data/External_files/GloNAF_modified_shp"

shp <- readOGR("GloNAF_modified",dsn=wd_shp)  #load shapefile in

coords_sp <- coords_locID
coords_sp <- coords_sp[complete.cases(coords_sp$decimalLongitude),]
coords_sp <- coords_sp[complete.cases(coords_sp$decimalLatitude),]
coordinates(coords_sp) <- ~decimalLongitude+decimalLatitude
proj4string(coords_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

tab <- list()
for(i in 1:ceiling(nrow(coords_sp)/1000000))  
{
  if(i!=ceiling(nrow(coords_sp)/1000000)){
  a <- coords_sp[c((((i-1)*1000000)+1):(i*1000000)),]
  }else{
  a <- coords_sp[c((((i-1)*1000000)+1):nrow(coords_sp)),]
  }
  b <- over(a,shp) #get the region in which each unique location is
  tab[[i]] <- data.frame(locationID = a$locationID, regPlants = b$OBJIDsic) #file relating each locationID to a shapefile region
  print(i)
}

tab2 <- rbindlist(tab)

setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/External_data")

saveRDS(tab2,"locationID_regPlants") #save locID_Spiderregions








