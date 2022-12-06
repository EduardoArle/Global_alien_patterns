library(plyr);library(rgdal);library(raster);library(data.table)
library(plotfunctions);library(maptools);library(rworldmap);library(rgeos)

#list WDs
wd_shp <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Data/Ants/Bentity2_shapefile_fullres"
wd_table <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Data/Ants"
wd_harmo_cl <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Data/Ants"
wd_pts_cont <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Figures/SI/Points_continent"
wd_cont_burden <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Species_burden_continent"
wd_res_tab <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Results/Ants/Tables"
wd_res_maps <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Results/Ants/Maps"
wd_map_stuff <- "/Users/carloseduardoaribeiro/Documents/Soup/Map stuff"

#load shp
shp <- readOGR("Bentity2_shapefile_fullres",dsn = wd_shp,
               use_iconv=TRUE, encoding="UTF-8")

#check if all regions listed in the table are represented
#in the shapefile
setwd(wd_table)
sps_reg_list <- read.csv("Exotic Species Records.csv") #load table
regs <- sort(unique(sps_reg_list$bentity2))
shp_regs <- sort(unique(shp$BENTITY2_N))
missing <- regs[-which(regs %in% shp_regs)]

missing

#fix missing stuf by changing "Norte de Santander" to "Spain" and 
#eliminating the two entries of "USA"

missing_1 <- sps_reg_list[which(sps_reg_list$bentity2 == "Norte de Santander"),]
sps_reg_list$bentity2[which(sps_reg_list$bentity2 == "Norte de Santander")] <-
  "Spain"

missing_2 <- sps_reg_list[which(sps_reg_list$bentity2 == "USA"),]
sps_reg_list <- sps_reg_list[-which(sps_reg_list$bentity2 == "USA"),]

#substitute "." for " " in species names
sps_reg_list$Species <- gsub("\\."," ",sps_reg_list$Species)

#make sps list
sps_list <- unique(sps_reg_list$Species)

#save sps_list
setwd(wd_table)
saveRDS(sps_list,"Sps_list_ants")


##### Use taxonomicHarmonisation script and then get occ from cluster

#load table with occurrence counts (calculated by script occRegionAnts)
setwd(wd_table)
sps_reg_count <- readRDS("Ants_occurrence_region_count")

names(sps_reg_count)[4] <- "n" #rename species counting column

names_regs_count <- unique(sps_reg_count$regAntsMammals)

missing <- names_regs_count[-which(names_regs_count %in% shp_regs)]

missing  #### EVERYTHING MATCHES!

#create column with species and region info in the occurrence count table
sps_reg_count$sps_reg <- paste0(sps_reg_count$species,"_",
                                sps_reg_count$regAntsMammals)

##### Use taxonomicHarmonisation script

#include the harmonised names into the data base table
setwd(wd_table)
harmo <- read.csv("Ants_aliens_harmonised.csv")
harmo2 <- harmo[,c(1:2)]

sps_reg_list2 <- merge(sps_reg_list,harmo2,
                       by.x = "Species",
                       by.y = "entry")

#create column with species and region info in the ants table
sps_reg_list2$sps_reg <- paste0(sps_reg_list2$gbifDarwinCore,"_",
                                sps_reg_list2$bentity2)


#eliminate duplicated rows in the checklists file (probably due to synonyms
#in the original names that have been resolved)

sps_reg_list3 <- unique(as.data.table(sps_reg_list2), #the table has to be in 
                        by = c("sps_reg"))            #data.table

#save final checklist table (harmonised names and no duplicates)

setwd(wd_table)
write.csv(sps_reg_list3,"Final_checklist_ants.csv")

#eliminate rows combining sps_reg_count that are not listed in the taxon occurrence table
sps_reg_count2 <- sps_reg_count[which(sps_reg_count$sps_reg %in% 
                                        sps_reg_list3$sps_reg),]

#check which sps_region combination in the taxon table have at least 1 GBIF 
#occurrence
sps_reg_list3$confirmed <- as.numeric(sps_reg_list3$sps_reg %in% 
                                        sps_reg_count2$sps_reg)

#calculate the percentage of species per regions confirmed by GBIF and
#the regional species burden

perc_confirmed <- ddply(sps_reg_list3,.(bentity2),summarise,
                        confirmed=mean(confirmed)*100,
                        n_sps=length(c(bentity2)))

#include the number of species and the percentage of species listed confirmed in 
#the shapefile

shp2 <- shp #create a copy of the shp
shp2$confirmed <- rep(9999,nrow(shp2))  #include percentage of confirmed sps
shp2$n_sps <- rep(9999,nrow(shp2))  #include n_species  

for(i in 1:nrow(shp2))
{
  a <- which(perc_confirmed$bentity2 == shp2$BENTITY2_N[i])
  if(length(a) > 0)
  {
    shp2$confirmed[i] <- perc_confirmed$confirmed[a]  
    shp2$n_sps[i] <- perc_confirmed$n_sps[a]  
  }else{
    shp2$confirmed[i] <- NA 
    shp2$n_sps[i] <- 0 
  }
}


#####check if there are at least 50 records in the same continent to 
#model the species occurrence 

#load region/continent lookup table (made in script Region_continent_relation)
setwd(wd_table)
reg_continent <- read.csv("Lookup_table_region_cont.csv")

reg_continent <- reg_continent[,-1]

#merge continent info into sps_reg_list_rep2
sps_reg_list4 <- merge(sps_reg_list3,reg_continent,
                       by.x = "bentity2",
                       by.y = "Region")

sps_reg_list4$sps_cont <- paste(sps_reg_list4$gbifDarwinCore,
                                sps_reg_list4$Continent,
                                sep="_")

#save checklist table with continent info to calculate the burden
setwd(wd_cont_burden)

write.csv(sps_reg_list4,"Ants_continent.csv",row.names = F)

#merge continent info into sps_reg_count
names(sps_reg_count)[3] <- "Region"
sps_reg_count3 <- merge(sps_reg_count,reg_continent,by="Region")
sps_reg_count3$sps_cont <- paste(sps_reg_count3$species,
                                 sps_reg_count3$Continent,
                                 sep="_")

#save count with continent info
setwd(wd_pts_cont)
write.csv(sps_reg_count3,"Ants_continent.csv",row.names = F)

#count sps_continent number of occurrences
sps_cont_n <- ddply(sps_reg_count3,.(sps_cont),nrow)

#eliminate rows with less than 50 occurrences
sps_cont_n2 <- sps_cont_n[which(sps_cont_n$V1 >=50),]

#check which sps_continent combination in the ants table have at 
#least 50 GBIF occurrence
sps_reg_list4$modelling <- as.numeric(sps_reg_list4$sps_cont %in% 
                                        sps_cont_n2$sps_cont)

#calculate the percentage of species per regions having at least 50 records
perc_modelling <- ddply(sps_reg_list4,.(bentity2),summarise,
                        perc_modelling = mean(modelling)*100)


#include the percentage of species with at least 50 records in the continent,
#and name of continent in the shapefile

shp2$modelling <- rep(9999,nrow(shp2))  #include percentage of sps with 50 recs
shp2$continent <- rep(9999,nrow(shp2))  #include continent

for(i in 1:nrow(shp2))
{
  a <- which(perc_modelling$bentity2 == shp2$BENTITY2_N[i])
  b <- which(reg_continent$Region == shp2$BENTITY2_N[i])
  
  shp2$continent[i] <- reg_continent$Continent[b]
  
  if(length(a) == 1)
  {
    shp2$modelling[i] <- perc_modelling$perc_modelling[a]  
  }else{
    shp2$modelling[i] <- NA 
  }
}


###### calculate range dynamics evidence

#eliminate rows corresponding to years before 1980 and after 2019
#as well as rows not containing year information
sps_reg_count3 <- sps_reg_count2[which(!is.na(sps_reg_count2$year)),]
sps_reg_count3 <- sps_reg_count3[which(sps_reg_count3$year >= 1970 &
                                         sps_reg_count3$year <= 2019),]

#create column informing to with lustre the occurrences belong
sps_reg_count3$lustre <- floor((sps_reg_count3$year - 1970) / 5) + 1

#change col "regAntsMammals" to "Region"
names(sps_reg_count3)[3] <- "Region"

#count sps_reg occurrence in the 5 year period
sps_reg_count4 <- ddply(sps_reg_count3,.(species,Region,sps_reg,lustre),
                        summarise, n_5y = sum(n))

#eliminate rows with combination sps_reg_n_5y < 10
sps_reg_count5 <- sps_reg_count4[-which(sps_reg_count4$n_5y < 10),]

#count how many periods of five years per region have at least 10 rec
sps_reg_count6 <- ddply(sps_reg_count5,.(Region),nrow)

#merge sps number per region to range dynamics value
tab_rd_n <- merge(sps_reg_count6,shp2@data,
                  by.x = "Region", by.y = "BENTITY2_N")

#calculate Rd
tab_rd_n$Rd <- tab_rd_n$V1/tab_rd_n$n_sps*10

#include the range dynamics value in the shp

shp2$Rd <- rep(9999,nrow(shp2))  #include percentage of confirmed sps

for(i in 1:nrow(shp2))
{
  a <- which(tab_rd_n$Region == shp2$BENTITY2_N[i])
  if(length(a) == 1)
  {
    shp2$Rd[i] <- tab_rd_n$Rd[a]  
  }else{
    shp2$Rd[i] <- ifelse(shp2$BENTITY2_N[i] %in% 
                           sps_reg_list4$bentity2,0,NA)
  }
}

#save tables

table_res <- shp2@data
table_res2 <- table_res[,c(1,6,4,3,5,7)]
names(table_res2)[1] <- "Region"

setwd(wd_res_tab)
write.csv(table_res2, "Indices_ants_region.csv", row.names = F)


### plot maps

# Load world map frame and continent outline
setwd(wd_map_stuff)

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

#load world map
w_map <- getMap(resolution = "coarse")
w_map <- spTransform(w_map,CRS(proj4string(world)))

#### SOLUTION TO AVOID FIJI AND RUSSIA EAST SCREWING UP THE MAP ####

b <- as(extent(-180, 180, -21, -12.483), 'SpatialPolygons')
fiji <- crop(shp2[which(shp2$BENTITY2_N == 'Fiji'),],b)
shp2 <- shp2[-which(shp2$BENTITY2_N == 'Fiji'),]
fiji <- spChFIDs(fiji,'Fiji')
shp2 <- spRbind(shp2,fiji)

b2 <- as(extent(-179.998, 179.998, 42.2925, 77.148), 'SpatialPolygons')
rus_east <- crop(shp2[which(shp2$BENTITY2_N == 'Russia East'),],b2)
shp2 <- shp2[-which(shp2$BENTITY2_N == 'Russia East'),]
rus_east <- spChFIDs(rus_east,'Russia East')
shp2 <- spRbind(shp2,rus_east)

# simplify shapefile to facilitate plotting
shp3 <- gSimplify(shp2, tol = 0.1, topologyPreserve = T)
shp3 <- SpatialPolygonsDataFrame(shp3, shp2@data)

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
shp3 <- spTransform(shp3,CRS(proj4string(world)))


##### PLOT THE SPECIES BURDEN MAP

#create vector to populate with the colours
col_n_sps <- rep("xx",nrow(shp3)) 

#create vector to populate with the transparency (use log scale)
#sum 1 to make 0s be 0s
alpha_n_sps <- log(shp3$n_sps+1)/max(log(shp3$n_sps+1)) * 255

col_n_sps <- rgb(135,0,0,
                 alpha=alpha_n_sps,
                 maxColorValue = 255)

par(mar=c(2,2,2,2))

plot(worldmapframe)
plot(w_map,add=T,col="gray80",border=NA)
plot(shp3,add=T,col="white")
plot(shp3,col=col_n_sps,add=T)

col_leg <- colorRampPalette(c("white", rgb(135,0,0,
                                           alpha=255,
                                           maxColorValue = 255)))

# could not plot values the way I want (log) adapt the function
myGradientLegend(valRange = c(0, max(shp3$n_sps)), 
                 pos = c(0.2,0.13,0.8,.145),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = c(0,max(shp3$n_sps)/4,max(shp3$n_sps)/2,
                           max(shp3$n_sps)*3/4,max(shp3$n_sps)),
                 values = c("0",paste(round(exp(log(max(shp3$n_sps))/4))),
                            paste(round(exp(log(max(shp3$n_sps))/2))),
                            paste(round(exp(log(max(shp3$n_sps))*3/4))),
                            paste(max(shp3$n_sps))),
                 cex = 3)

## save 2000 width

##### PLOT THE CONFIRMED MAP

#create vector to populate with the colours
col_confirmed <- rep("xx",nrow(shp3)) 

#create vector to populate with the transparency
alpha_confirmed <- shp3$confirmed[which(!is.na(shp3$confirmed))] * 2.55

col_confirmed[which(!is.na(shp3$confirmed))] <- rgb(40,40,148,
                                                    alpha=alpha_confirmed,
                                                    maxColorValue = 255)

col_confirmed[which(col_confirmed=="xx")] <- "white"

par(mar=c(2,2,2,2))

plot(worldmapframe)
plot(w_map,add=T,col="gray80",border=NA)
plot(shp3,col="white",add=T)
plot(shp3,col=col_confirmed,add=T)
plot(shp3[which(shp3$n_sps == 0),],add=T,density = 100)


col_leg <- colorRampPalette(c("white", rgb(40,40,148,
                                           alpha=255,
                                           maxColorValue = 255)))

myGradientLegend(valRange = c(0, 100),
                 pos = c(0.2,0.13,0.8,.145),
                 color = col_leg(20),
                 side = 1,
                 n.seg = 0,
                 values = c("0","100%"),
                 cex = 3)

## save 2000 width

##### PLOT THE MODELLING MAP

#create vector to populate with the colours
col_modelling <- rep("xx",nrow(shp3)) 

#create vector to populate with the transparency
alpha_modelling <- shp3$modelling[which(!is.na(shp3$modelling))] * 2.55

col_modelling[which(!is.na(shp3$modelling))] <- rgb(191,144,0,
                                                    alpha=alpha_modelling,
                                                    maxColorValue = 255)

col_modelling[which(col_modelling=="xx")] <- "white"

par(mar=c(2,2,2,2))

plot(worldmapframe)
plot(w_map,add=T,col="gray80",border=NA)
plot(shp3,col="white",add=T)
plot(shp3,col=col_modelling,add=T)
plot(shp3[which(shp3$n_sps == 0),],add=T,density = 100)


col_leg <- colorRampPalette(c("white", rgb(191,144,0,
                                           alpha=255,
                                           maxColorValue = 255)))

myGradientLegend(valRange = c(0, 100),
                 pos = c(0.2,0.13,0.8,.145),
                 color = col_leg(20),
                 side = 1,
                 n.seg = 0,
                 values = c("0","100%"),
                 cex = 3)

## save 2000 width

##### PLOT THE RANGE DYNAMICS MAP

#create vector to populate with the colours
col_Rd <- rep("xx",nrow(shp3)) 

#create vector to populate with the transparency
alpha_Rd <- shp3$Rd[which(!is.na(shp3$Rd))] * 2.55

col_Rd[which(!is.na(shp3$Rd))] <- rgb(56,87,35,
                                      alpha=alpha_Rd,
                                      maxColorValue = 255)

col_Rd[which(col_Rd=="xx")] <- "white"

par(mar=c(2,2,2,2))

plot(worldmapframe)
plot(w_map,add=T,col="gray80",border=NA)
plot(shp3,col="white",add=T)
plot(shp3,col=col_Rd,add=T)
plot(shp3[which(shp3$n_sps == 0),],add=T,density = 100)


col_leg <- colorRampPalette(c("white", rgb(56,87,35,
                                           alpha=255,
                                           maxColorValue = 255)))

myGradientLegend(valRange = c(0, 100),
                 pos = c(0.2,0.13,0.8,.145),
                 color = col_leg(20),
                 side = 1,
                 n.seg = 0,
                 values = c("0","100%"),
                 cex = 3)

## save 2000 width

