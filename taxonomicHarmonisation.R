#load libraries
library(taxize)

#################### Amphibians ##################

#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Amphibians"
#read species table
setwd(wd_species)
list <- readRDS("Sps_list_amph")


#################### Reptiles ##################

#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Reptiles"
#read species table
setwd(wd_species)
list <- readRDS("Sps_list_rep")


#################### Freshwater ##################

#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/Freshwater"
#read species table
setwd(wd_species)
list <- readRDS("Sps_list_freshwater")


#################### Mammals #################################
#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/Mammals"

#read species table
setwd(wd_species)
list <- read.csv("Alien_mammal_checklist.csv")
list <- sort(unique(list[,2]))

#ubiquitous species
wd_ubi <- '/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Data/Mammals'

#read species table
setwd(wd_ubi)
list <- read.csv('Ubiquitous mammals Eduardo.csv')
list <- sort(unique(list$Species))

#################### Ants #################################
#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants"

#read species table
setwd(wd_species)
list <- readRDS("Sps_list_ants")


#################### Plants #################################
#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GLONAF"

#read species table
setwd(wd_species)
list <- readRDS("Sps_list_plants")


#################### Birds #################################
#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA"

#read species table
setwd(wd_species)
list <- readRDS("Sps_list_birds")


#################### Spiders #################################
#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders"

#read species table
setwd(wd_species)
list <- readRDS("Sps_list_spiders")


#################### Fungi #################################
#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/Fungi"

#read species table
setwd(wd_species)
list <- readRDS("Sps_list_fungi")
list <- list[-which(list == "")]




###################### HARMONISATION ######################



#make list of GBIF resolved names WHERE IS THE PROBLEM?????
gbifDarwinCore <- character()
all_names <- character()

for(i in 1:length(list))
{
  match <- get_ids_(list[i], db = 'gbif',rows=1:10)
  match2 <- match[[1]][[1]]
  match3 <- match2[which(match2$matchtype == "EXACT"),]
  
  if(nrow(match2) == 0){
    gbifDarwinCore[i] <- "not matched"
  }
  if(!"species" %in% names(match2)){
    gbifDarwinCore[i] <- "not to species level"
    all_names[i] <- NA
  }else{
    gbifDarwinCore[i] <- unique(match3$species)[1]
  }
  if(length(unique(match3$species)) == 1){
    all_names[i] <- NA
  }else{
    all_names[i] <- paste(unique(match3$species),collapse = " / ")
  }
  print(i)
}


table <- data.frame(entry = list,gbifDarwinCore = gbifDarwinCore,
           otherNames = all_names)

changed_names <- table[table$entry != table$gbifDarwinCore |
                         is.na(table$gbifDarwinCore),]

other_names <- table[!is.na(table$otherNames),]



####save amphibians
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Amphibians")

write.csv(table,"Amphibia_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


####save reptiles
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Reptiles")

write.csv(table,"Reptilia_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


####save freshwater
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Freshwater")

write.csv(table,"Freshwater_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


#### save mammals
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Mammals")

write.csv(table,"Mammalia_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


#### save uniquitous mammals
setwd("/Users/carloseduardoaribeiro/Documents/Global Alien Patterns/Data/Mammals")

write.csv(table,"Mammalia_ubiquitous_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_ubiquitous_differences.csv",row.names = F)


#### save ants
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Ants")

write.csv(table,"Ants_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


#### save plants
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GLONAF")

write.csv(table,"Plants_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


#### save birds
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA")

write.csv(table,"Birds_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


#### save spiders
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Spiders")

write.csv(table,"Spiders_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


#### save fungi
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Fungi")

write.csv(table,"Fungi_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)

