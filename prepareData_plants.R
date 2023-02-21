library(plyr);library(rgdal);library(raster);library(data.table)
library(plotfunctions);library(maptools);library(rworldmap)

#list WDs
wd_shp <-  "/Users/carloseduardoaribeiro/Documents/GloNAF/Modified shapefile"
wd_table <- "/Users/carloseduardoaribeiro/Documents/GloNAF"
wd_harmo_cl <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Tables/Final checklists"
wd_pts_cont <- "C:/Users/ca13kute/Documents/2nd_Chapter/Figures/SI/Points_continent"

#load shp
shp <- readOGR("GloNAF_modified",dsn = wd_shp,
               use_iconv=TRUE, encoding="UTF-8")

#check if all regions listed in the table are represented
#in the shapefile
setwd(wd_table)
sps_reg_list <- read.csv("Taxon_x_List_GloNAF_fixed.csv") #load table


#########   NOT READING SPECIAL CHARACTER HERE ON THE MAC SOLVE #########

regions <- read.csv("Region_GloNAF_vanKleunenetal2018Ecology.csv",
                    encoding="Latin-1") #table 

#translating regions ID, names etc

#make a table to include region name and obj ID into the tables
merge_tab <- regions[,c(1,3,5)]

#include region names and obj ID into the table with occurrence counts
sps_reg_list2 <- merge(sps_reg_list,merge_tab,by="region_id",sort = F, all.x = T)

#check if all regions in the table are represented in the shp
regs <- sort(unique(sps_reg_list2$name))
shp_regs <- sort(unique(shp$name))
missing <- regs[-which(regs %in% shp_regs)]

missing

#fix the inconsistencies between regions in the shapefile and those listed in
#the master file

#eliminate entries not represented in the shp
sps_reg_list3 <- sps_reg_list2[-which(sps_reg_list2$name %in% missing),]

write.csv(sps_reg_list3,"Taxon_x_List_GloNAF_modified.csv", row.names = F)

#make a sps list
sps_list <- unique(sps_reg_list3$standardized_name)

#save sps_list
setwd(wd_table)
saveRDS(sps_list,"Sps_list_plants")


##### Use taxonomicHarmonisation script and then get occ from cluster

#load table with occurrence counts (calculated by script occRegionAnts)
setwd(wd_table)
sps_reg_count <- readRDS("Plants_occurrence_region_count")

names(sps_reg_count)[4] <- "n" #rename species counting column

#create column with species and region info in the occurrence count table
sps_reg_count$sps_reg <- paste0(sps_reg_count$species,"_",
                                sps_reg_count$Region)

#include the harmonised names into the data base table
setwd(wd_table)
harmo <- read.csv("Plants_aliens_harmonised.csv")
harmo2 <- harmo[,c(1:2)]

sps_reg_list4 <- merge(sps_reg_list3,harmo2,
                       by.x = "standardized_name",
                       by.y = "entry")

#create column with species and region info in the plants table
sps_reg_list4$sps_reg <- paste0(sps_reg_list4$gbifDarwinCore,"_",
                                sps_reg_list4$OBJIDsic)

#eliminate duplicated rows in the checklists file (probably due to synonyms
#in the original names that have been resolved)

sps_reg_list5 <- unique(as.data.table(sps_reg_list4), #the table has to be in 
                        by = c("sps_reg"))            #data.table

#save final checklist table (harmonised names and no duplicates)

setwd(wd_harmo_cl)
write.csv(sps_reg_list5,"Final_checklist_plants.csv")

#eliminate rows combining sps_reg_count that are not listed in the taxon occurrence table
sps_reg_count2 <- sps_reg_count[which(sps_reg_count$sps_reg %in% 
                                        sps_reg_list5$sps_reg),]

#check which sps_region combination in the taxon table have at least 1 GBIF 
#occurrence
sps_reg_list5$confirmed <- as.numeric(sps_reg_list5$sps_reg %in% 
                                        sps_reg_count2$sps_reg)

#calculate the percentage of species per regions confirmed by GBIF and
#the regional species burden
perc_confirmed <- ddply(sps_reg_list5,.(OBJIDsic),summarise,
                        confirmed=mean(confirmed)*100,
                        n_sps=length(c(OBJIDsic)))

#include the number of species and the percentage of species listed confirmed in 
#the shapefile

shp2 <- shp #create a copy of the shp
shp2$confirmed <- rep(9999,nrow(shp2))  #include percentage of confirmed sps
shp2$n_sps <- rep(9999,nrow(shp2))  #include n_species  

for(i in 1:nrow(shp2))
{
  a <- which(perc_confirmed$OBJIDsic == shp2$OBJIDsic[i])
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
sps_reg_list6 <- merge(sps_reg_list5,reg_continent,
                       by = "OBJIDsic")

sps_reg_list6$sps_cont <- paste(sps_reg_list6$gbifDarwinCore,
                                sps_reg_list6$Continent,
                                sep="_")

#save checklist table with continent info to calculate the burden
setwd(wd_table)

write.csv(sps_reg_list6,"Plants_continent.csv",row.names = F)


#merge continent info into sps_reg_count
names(sps_reg_count)[3] <- "Region"

sps_reg_count3 <- merge(sps_reg_count,reg_continent,
                        by.x="Region",
                        by.y="OBJIDsic")

sps_reg_count3$sps_cont <- paste(sps_reg_count3$species,
                                 sps_reg_count3$Continent,
                                 sep="_")

#save count with continent info
setwd(wd_pts_cont)
write.csv(sps_reg_count3,"Plants_continent.csv",row.names = F)

#count sps_continent number of occurrences
sps_cont_n <- ddply(sps_reg_count3,.(sps_cont),nrow)

#eliminate rows with less than 50 occurrences
sps_cont_n2 <- sps_cont_n[which(sps_cont_n$V1 >=50),]

#check which sps_continent combination in the ants table have at 
#least 50 GBIF occurrence
sps_reg_list6$modelling <- as.numeric(sps_reg_list6$sps_cont %in% 
                                        sps_cont_n2$sps_cont)

#calculate the percentage of species per regions having at least 50 records
perc_modelling <- ddply(sps_reg_list6,.(name),summarise,
                        perc_modelling = mean(modelling)*100)


#include the percentage of species with at least 50 records in the continent,
#and name of continent in the shapefile

shp2$modelling <- rep(9999,nrow(shp2))  #include percentage of sps with 50 recs
shp2$continent <- rep(9999,nrow(shp2))  #include continent

for(i in 1:nrow(shp2))
{
  a <- which(perc_modelling$name == shp2$name[i])
  b <- which(reg_continent$Region == shp2$name[i])
  
  shp2$continent[i] <- reg_continent$Continent[b]
  
  if(length(a) == 1)
  {
    shp2$modelling[i] <- perc_modelling$perc_modelling[a]  
  }else{
    shp2$modelling[i] <- NA 
  }
}


###### calculate range dynamics evidence

#eliminate rows corresponding to years before 1970 and after 2019
#as well as rows not containing year information
sps_reg_count3 <- sps_reg_count2[which(!is.na(sps_reg_count2$year)),]
sps_reg_count3 <- sps_reg_count3[which(sps_reg_count3$year >= 1970 &
                                         sps_reg_count3$year <= 2019),]

#create column informing to with lustre the occurrences belong
sps_reg_count3$lustre <- floor((sps_reg_count3$year - 1970) / 5) + 1

#count sps_reg occurrence in the 5 year period
sps_reg_count4 <- ddply(sps_reg_count3,.(species,Region,sps_reg,lustre),
                        summarise, n_5y = sum(n))

#eliminate rows with combination sps_reg_n_5y < 10
sps_reg_count5 <- sps_reg_count4[-which(sps_reg_count4$n_5y < 10),]

#count how many periods of five years per region have at least 10 rec
sps_reg_count6 <- ddply(sps_reg_count5,.(Region),nrow)

#merge sps number per region to range dynamics value
tab_rd_n <- merge(sps_reg_count6,shp2@data,
                  by.x = "Region", by.y = "OBJIDsic")

#calculate Rd
tab_rd_n$Rd <- tab_rd_n$V1/tab_rd_n$n_sps*10

#include the range dynamics value in the shp

shp2$Rd <- rep(9999,nrow(shp2))  #include percentage of confirmed sps

for(i in 1:nrow(shp2))
{
  a <- which(tab_rd_n$Region == shp2$OBJIDsic[i])
  if(length(a) == 1)
  {
    shp2$Rd[i] <- tab_rd_n$Rd[a]  
  }else{
    shp2$Rd[i] <- ifelse(shp2$OBJIDsic[i] %in% 
                           sps_reg_list6$OBJIDsic,0,NA)
  }
}

#save tables

table_res <- shp2@data
table_res2 <- table_res[,c(6,10,8,7,9,11)]
names(table_res2)[1] <- "Region"

setwd("/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Results")
write.csv(table_res2,"MDA_plants_region.csv",row.names = F)


### plot maps

# Load world map frame and continent outline
setwd("/Users/carloseduardoaribeiro/Documents/Soup/Map stuff")

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

#load world map
w_map <- getMap(resolution = "coarse")
w_map <- spTransform(w_map,CRS(proj4string(world)))

#### SOLUTION TO AVOID FIJI AND RUSSIA EAST SCREWING UP THE MAP ####

# b <- as(extent(-180, 180, -21, -12.483), 'SpatialPolygons')
# fiji <- crop(shp2[112,],b)
# shp2 <- shp2[-112,]
# shp2 <- spRbind(shp2,fiji)
# 
# b2 <- as(extent(-179.998, 179.998, 42.2925, 77.148), 'SpatialPolygons')
# rus_east <- crop(shp2[337,],b2)
# shp2 <- shp2[-337,]
# shp2 <- spRbind(shp2,rus_east)

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
shp3 <- spTransform(shp2,CRS(proj4string(world)))


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
                 pos=c(0.3,0,0.7,.015),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = c(0,max(shp3$n_sps)/4,max(shp3$n_sps)/2,
                           max(shp3$n_sps)*3/4,max(shp3$n_sps)),
                 values = c("0",paste(round(exp(log(max(shp3$n_sps))/4))),
                            paste(round(exp(log(max(shp3$n_sps))/2))),
                            paste(round(exp(log(max(shp3$n_sps))*3/4))),
                            paste(max(shp3$n_sps))),
                 cex = 1.5)

#########################################
##### second option of colours
#########################################

#create colour ramp to represent the values
# colramp <- colorRampPalette(c("#9e0142", "#d53e4f", "#f46d43",
#                               "#fdae61", "#fee08b", "#ffffbf",
#                               "#e6f598", "#abdda4", "#66c2a5",
#                               "#3288bd", "#5e4fa2"))

#populate the table with the colours to be plotted 

# col_n_sps <- colramp(100)[cut(log(c(1,shp3$n_sps)),
#                                breaks = 100)][-1]
# 
# par(mar=c(3,2,2,2))
# 
# plot(worldmapframe)
# plot(w_map,add=T,col="gray80",border=NA)
# plot(shp3,col=col_n_sps,add=T,border=NA)
# 
# 
# # could not plot values the way I want (log) adapt the function
# myGradientLegend(valRange = c(1, max(shp3$n_sps)),
#                  pos=c(0.3,0,0.7,.015),
#                  color = colramp(100),
#                  side = 1,
#                  n.seg = c(1,max(shp3$n_sps)/4,max(shp3$n_sps)/2,
#                            max(shp3$n_sps)*3/4,max(shp3$n_sps)),
#                  values = c("1",paste(round(exp(log(max(shp3$n_sps))/4))),
#                             paste(round(exp(log(max(shp3$n_sps))/2))),
#                             paste(round(exp(log(max(shp3$n_sps))*3/4))),
#                             paste(max(shp3$n_sps))),
#                  cex = 3)

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
                 pos=c(0.3,0,0.7,.015),
                 color = col_leg(20),
                 side = 1,
                 n.seg = 0,
                 values = c("0","100%"),
                 cex = 1.5)

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
                 pos=c(0.3,0,0.7,.015),
                 color = col_leg(20),
                 side = 1,
                 n.seg = 0,
                 values = c("0","100%"),
                 cex = 1.5)


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
                 pos=c(0.3,0,0.7,.015),
                 color = col_leg(20),
                 side = 1,
                 n.seg = 0,
                 values = c("0","100%"),
                 cex = 1.5)





