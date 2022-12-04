library(raster);library(rgdal);library(rgeos);library(rworldmap)

#load world map
world <- getMap()

############## Ants and mammals ##########################


#list WDs
wd_shp <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Data/Mammals/Bentity2_shapefile_fullres"
wd_IPBES <- "/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/IPBES/Simplified_shp"

#load shps
shp_ants_mammals <- readOGR("Bentity2_shapefile_fullres",dsn = wd_shp,
                            use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#loop though shapefile features to identify each of their continents

pts_regs <- list()
for(i in 1:nrow(shp_ants_mammals))
{
  pts_regs[[i]] <- spsample(shp_ants_mammals[i,], n=100, type='regular') #seed points in each region
  print(i)
}

#get continent of each region

continents <- character()
for(i in 428:nrow(shp_ants_mammals))
{ 
  a <- over(pts_regs[[i]],shp_IPBES)
  b <- table(a$Region)
  c <- which.max(b)
  d <- names(c)
  continents[i] <- d
  print(i)
}

### manually fix what does not work

shp_ants_mammals[i,]
plot(world)
plot(pts_regs[[i]],add=T,pch=19,col="red")

#i=32 Baker Island
continents[i] <- "Oceania"

#i=85 Coral Sea Islands
continents[i] <- "Oceania"

#i=153 Howland Island
continents[i] <- "Oceania"

#i=178 Johnston Atoll
continents[i] <- "Oceania"

#i=387 Spratly Islands
continents[i] <- "South-East Asia"

#i=416 Tokelau
continents[i] <- "Oceania"

#i=421 Tromelin Island
continents[i] <- "East Africa and adjacent islands"

#i=427 Tuvalu
continents[i] <- "Oceania"

#join info
reg_cont <- data.frame(Region = shp_ants_mammals$BENTITY2_N,
                       Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Continent)),]

missing_cont

#save lookup tabe 
setwd("/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Data/Mammals")
write.csv(reg_cont,"Lookup_table_region_cont.csv")


############################### DONE UP TO HERE ##################################

############## Amphibians and Reptiles ##########################


#list WDs
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Regions_shapefile"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

#load shps
shp_amph <- readOGR("Regions_reptiles_amphibians",dsn = wd_shp,
               use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#get regions centroids
centroids <- gCentroid(shp_amph,byid=TRUE)
centroids <- SpatialPointsDataFrame(centroids,shp_amph@data)

#get continent of each region
continents <- over(centroids,shp_IPBES)

#join info
reg_cont <- cbind(BENTITY2_N = centroids$BENTITY2_N,Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Region)),]

i=1 

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=2

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South Asia"


i=3

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=4

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=5

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=6

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "West Africa"


i=7

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=8

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South Asia"


i=9

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South-East Asia"


i=10

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "East Africa and adjacent islands"


i=11

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=12

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South America"


i=13

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South America"


i=14

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=15

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=16

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=17

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South-East Asia"


i=18

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "North-East Asia"


i=19

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South America"


i=20

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=21

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "North-East Asia"


i=22

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South-East Asia"


i=23

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South Asia"


i=24

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=25

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "East Africa and adjacent islands"


i=26

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=27

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South Asia"


i=28

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=29

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South-East Asia"


i=30

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=31

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Central and Western Europe"


i=32

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Mesoamerica"


i=33

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=34

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "North America"


i=35

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=36

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "East Africa and adjacent islands"


i=37

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=38

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Central and Western Europe"


i=39

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=40

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=41

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=42

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=43

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Central and Western Europe"


i=44

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=45

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=46

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


unique(reg_cont$Region)

#manually fix regions that have not been asigned to a continent
missing_cont2 <- reg_cont[which(is.na(reg_cont$Region)),]
missing_cont2 


#save lookup tabe 
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles")
write.csv(reg_cont,"Lookup_table_region_cont.csv")




############## Freshwater ##########################


#list WDs
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Freshwater/Simplified_FreshWater_shp"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

#load shps
shp_fresh <- readOGR("Basin042017_3119",dsn = wd_shp,
                    use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#loop though shapefile features to identify each of their continents

pts_regs <- list()
for(i in 1:nrow(shp_fresh))
{
  pts_regs[[i]] <- spsample(shp_fresh[i,], n=100, type='regular') #seed points in each region
  print(i)
}

#get continent of each region

continents <- character()
for(i in 3081:nrow(shp_fresh))
{
  a <- over(pts_regs[[i]],shp_IPBES)
  b <- table(a$Region)
  c <- which.max(b)
  d <- names(c)
  continents[i] <- d
  print(i)
}

#join info
reg_cont <- data.frame(BasinName = shp_fresh$BasinName,Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Continent)),]

missing_cont

#save lookup tabe 
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Freshwater")
write.csv(reg_cont,"Lookup_table_region_cont.csv")



############## Plants ##########################


#list WDs
wd_shp <-  "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GloNAF_modified_shp"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

#load shps
shp_plants <- readOGR("GloNAF_modified",dsn = wd_shp,
                            use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#loop though shapefile features to identify each of their continents

pts_regs <- list()
for(i in 1:nrow(shp_plants))
{
  pts_regs[[i]] <- spsample(shp_plants[i,], n=100, type='regular') #seed points in each region
  print(i)
}

#get continent of each region

continents <- character()

for(i in 849:nrow(shp_plants))
{
  a <- over(pts_regs[[i]],shp_IPBES)
  b <- table(a$Region)
  c <- which.max(b)
  d <- names(c)
  continents[i] <- d
  print(i)
}

### manually fix what does not work

shp_plants[i,]$name
plot(world)
plot(pts_regs[[i]],add=T,pch=19,col="red")

plot(world,border = NA)
plot(shp_IPBES[which(shp_IPBES$Region == "Oceania"),],add=T,
     border = NA, col = "orange")
plot(shp_IPBES[which(shp_IPBES$Region == "South-East Asia"),],add=T,
     border = NA, col = "lightgreen")
plot(shp_IPBES[which(shp_IPBES$Region == "North-East Asia"),],add=T,
     border = NA, col = "lightblue")
plot(shp_IPBES[which(shp_IPBES$Region == "East Africa and adjacent islands"),],add=T,
     border = NA, col = "yellow")
plot(shp_IPBES[which(shp_IPBES$Region == "South Asia"),],add=T,
     border = NA, col = "green")
plot(shp_IPBES[which(shp_IPBES$Region == "Southern Africa"),],add=T,
     border = NA, col = "red")


#i=19 French Frigate Shoals
continents[i] <- "Oceania"

#i=27 Ka'ula Rock
continents[i] <- "Oceania"

#i=42 Nassau, Cook Islands
continents[i] <- "Oceania"

#i=43 Palmerston, Cook Islands
continents[i] <- "Oceania"

#i=47 Kure
continents[i] <- "Oceania"

#i=55 Alexander Island (Australia, not Antarctica)
continents[i] <- "Oceania"

#i=56 Beacon Island
continents[i] <- "Oceania"

#i=59 Gilbert Island
continents[i] <- "Oceania"

#i=60 Gun Island
continents[i] <- "Oceania"

#i=61 Helms Island
continents[i] <- "Oceania"

#i=62 Hummock Island
continents[i] <- "Oceania"

#i=63 Leo Island
continents[i] <- "Oceania"

#i=82 Fatu Hiva, French Polynesia (Marquesas Islands)
continents[i] <- "Oceania"

#i=83 Fatu Huku, French Polynesia (Marquesas Islands)
continents[i] <- "Oceania"

#i=86 Mohotani, French Polynesia (Marquesas Islands)
continents[i] <- "Oceania"

#i=91 Oeno, French Polynesia
continents[i] <- "Oceania"

#i=100 Leysan
continents[i] <- "Oceania"

#i=101 Lehua
continents[i] <- "Oceania"

#i=103 Lisianski
continents[i] <- "Oceania"

#i=112 Nihoa
continents[i] <- "Oceania"

#i=118 Pearl & Hermes
continents[i] <- "Oceania"

#i=172 Aunuu
continents[i] <- "Oceania"

#i=173 Long Island, Houtman Abrolhos
continents[i] <- "Oceania"

#i=174 Little Rat Island
continents[i] <- "Oceania"

#i=175 Morley Island
continents[i] <- "Oceania"

#i=176 Middle Island
continents[i] <- "Oceania"

#i=177 Murray Island
continents[i] <- "Oceania"

#i=178 Newman Island
continents[i] <- "Oceania"

#i=179 North Island, Houtman Abrolhos
continents[i] <- "Oceania"

#i=195 Pigeon Island
continents[i] <- "Oceania"

#i=211 Rat Island
continents[i] <- "Oceania"

#i=225 Serventy Island
continents[i] <- "Oceania"

#i=226 Seagull Island
continents[i] <- "Oceania"

#i=227 Suomi Island
continents[i] <- "Oceania"

#i=230 Uncle Margie Island
continents[i] <- "Oceania"

#i=257 White Island
continents[i] <- "Oceania"

#i=258 Wooded Island
continents[i] <- "Oceania"

#i=313 Cocos (Keeling) North Keeling Island
continents[i] <- "South-East Asia"

#i=382 Suwarrow, Cook Islands
continents[i] <- "Oceania"

#i=383 Takutea, Cook Islands
continents[i] <- "Oceania"

#i=403 Aiwa, Lau Islands
continents[i] <- "Oceania"

#i=405 Nasoata, Fiji Islands
continents[i] <- "Oceania"

#i=489 McKean, Phoenix Islands
continents[i] <- "Oceania"

#i=559 McKean, Ailinginae Atoll
continents[i] <- "Oceania"

#i=614 Salvage Islands
continents[i] <- "Central and Western Europe"

#i=615 Agakauitai, French Polynesia (Gambier Islands)
continents[i] <- "Oceania"

#i=616 Maria, French Polynesia (Austral Islands)
continents[i] <- "Oceania"

#i=624 Aukena, French Polynesia (Gambier Islands)
continents[i] <- "Oceania"

#i=627 Tauna, French Polynesia (Gambier Islands)
continents[i] <- "Oceania"

#i=628 Tekava, French Polynesia (Gambier Islands)
continents[i] <- "Oceania"

#i=630 Tarauru Roa, French Polynesia (Gambier Islands)
continents[i] <- "Oceania"

#i=638 Mehetia, French Polynesia (Society Islands)
continents[i] <- "Oceania"

#i=643 Tetiaroa, French Polynesia (Society Islands)
continents[i] <- "Oceania"

#i=654 Tepoto North, French Polynesia (Tuamotu Archipelago)
continents[i] <- "Oceania"

#i=661 Demina, Kuril Islands
continents[i] <- "North-East Asia"

#i=674 Storozhevoy, Kuril Islands
continents[i] <- "North-East Asia"

#i=717 African Banks, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=720 Aride, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=723 Bird, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=724 Booby Island, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=725 Cocos, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=727 Conception, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=729 Cousin, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=730 Cousine, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=732 Darros, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=733 Denis, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=734 Desroches, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=735 Desnoefs, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=737 Felicite, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=739 Grande Soeur, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=740 Ile au Cerf, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=741 Ile Anonyme, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=742 Ile Longue, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=743 Ile St Anne, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=744 Ile aux Vaches Marines, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=747 Marianne, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=748 Marie-Louise, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=750 Platte, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=753 Providence, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=754 Petit Soeur, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=755 Recifs, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=756 Remire, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=757 St Francois, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=760 St Pierre, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=761 Therese, Seychelles
continents[i] <- "East Africa and adjacent islands"

#i=812 Fanuatapu
continents[i] <- "Oceania"

#i=813 Namua
continents[i] <- "Oceania"

#i=814 Nuulua
continents[i] <- "Oceania"

#i=815 Nuutela
continents[i] <- "Oceania"

#i=822 Marion Island
continents[i] <- "Oceania"

#i=848 Prince Edward Island, South Africa
continents[i] <- "Southern Africa"

#join info
reg_cont <- data.frame(Region = shp_plants$name,
                       OBJIDsic = shp_plants$OBJIDsic,
                       Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Continent)),]

missing_cont

#save lookup tabe 
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_data")
write.csv(reg_cont,"Lookup_table_region_cont.csv")


############## Birds ##########################


#list WDs
wd_shp <-  "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/Shapefile"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

#load shps
shp_birds <- readOGR("Shapefile_birds",dsn = wd_shp,
                      use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#loop though shapefile features to identify each of their continents

pts_regs <- list()
for(i in 292:nrow(shp_birds))
{
  pts_regs[[i]] <- spsample(shp_birds[i,], n=100, type='regular') #seed points in each region
  print(i)
}

#get continent of each region

continents <- character()

for(i in 293:nrow(shp_birds))
{
  a <- over(pts_regs[[i]],shp_IPBES)
  b <- table(a$Region)
  c <- which.max(b)
  d <- names(c)
  continents[i] <- d
  print(i)
}

### manually fix what does not work

shp_birds[i,]$GAVIARg
plot(world)
plot(pts_regs[[i]],add=T,pch=19,col="blue")


#i=292 "UNITED STATES MINOR OUTLYING ISLAND"
continents[i] <- "Oceania"

#join info
reg_cont <- data.frame(Region = shp_birds$GAVIARg,
                       Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Continent)),]

missing_cont

#save lookup tabe 
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA")
write.csv(reg_cont,"Lookup_table_region_cont.csv")




############## Spiders ##########################


#list WDs
wd_shp <-  "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/Shapefile"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

#load shps
shp_spiders <- readOGR("Shapefile_spiders",dsn = wd_shp,
                     use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#loop though shapefile features to identify each of their continents

pts_regs <- list()
for(i in 1:nrow(shp_spiders))
{
  pts_regs[[i]] <- spsample(shp_spiders[i,], n=100, type='regular') #seed points in each region
  print(i)
}

#get continent of each region

continents <- character()

for(i in 189:nrow(shp_spiders))
{
  a <- over(pts_regs[[i]],shp_IPBES)
  b <- table(a$Region)
  c <- which.max(b)
  d <- names(c)
  continents[i] <- d
  print(i)
}

### manually fix what does not work

shp_spiders[i,]$SpdrRgn
plot(world)
plot(pts_regs[[i]],add=T,pch=19,col="blue")


#i=188 Marion-Prince Edward Is.
continents[i] <- "Oceania"

#join info
reg_cont <- data.frame(Region = shp_spiders$SpdrRgn,
                       Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Continent)),]

missing_cont

#save lookup tabe 
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Spiders")
write.csv(reg_cont,"Lookup_table_region_cont.csv")



############## Fungi ##########################


#list WDs
wd_shp <-  "C:/Users/ca13kute/Documents/2nd_Chapter/Fungi/Shapefile"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

#load shps
shp_fungi <- readOGR("Shapefile_fungi",dsn = wd_shp,
                       use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#loop though shapefile features to identify each of their continents

pts_regs <- list()
for(i in 1:nrow(shp_fungi))
{
  pts_regs[[i]] <- spsample(shp_fungi[i,], n=100, type='regular') #seed points in each region
  print(i)
}

#get continent of each region

continents <- character()

for(i in 1:nrow(shp_fungi))
{
  a <- over(pts_regs[[i]],shp_IPBES)
  b <- table(a$Region)
  c <- which.max(b)
  d <- names(c)
  continents[i] <- d
  print(i)
}


#join info
reg_cont <- data.frame(Region = shp_fungi$FungRgn,
                       Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Continent)),]

missing_cont

#save lookup tabe 
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Fungi")
write.csv(reg_cont,"Lookup_table_region_cont.csv")



