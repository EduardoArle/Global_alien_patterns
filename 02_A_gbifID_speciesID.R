# this script loads the columns "species" and "gbifID" for all the GBIF data and creates two files
# 1: a table relating each unique species name to a species ID
# 2: a table containing all GBIF IDs related to the corresponding species ID

print("Start")

library("data.table")

setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/Thematic_fields")

#list files containing the desired info
files <- list.files(pattern = "gbifID_speciesName_")

#loop through them to get unique entries in the desired field
unic <- list()
for(i in seq_along(files))
{
  if(i == 1){   #only the first file comes with headers
    
    gbifID <- read.csv(files[i],sep="\t", header = TRUE) #read file in
    #gbifID <- read.csv(files[i],sep="\t",nrow=100000) #read file in for test
    col_names <- colnames(gbifID)
    
  }else{
    
    gbifID <- read.csv(files[i],sep="\t", header = FALSE) #read file in
    #gbifID <- read.csv(files[i],sep="\t",nrow=100000, header = FALSE) #read file in for test
    colnames(gbifID) <- col_names
  }
  
  unic[[i]] <- unique(gbifID$species)
  
  print(paste0("table ",i," in"))
}

#concatenate all objects in the list to get unique species names
unic2 <- unlist(unic)

#make unique list
unic3 <- unique(unic2)

#create a vector transforming species names in integer to make unique spsID
speciesID <- as.integer(unic3) 

#combine spsName and spsID in a data.frame
sps_spsID <- data.frame(species = unic3, speciesID = speciesID)

#save look up table
setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField")
saveRDS(sps_spsID,"species_speciesID")

#create temporay directory to save parts of the job
dir.create("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_date_speciesID")

#loop through files to concatenate spsNames to spsID
for(i in seq_along(files))
{
  #set wd with files
  setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/Thematic_fields")
  
  if(i == 1){   #only the first file comes with headers
    
    gbifID <- read.csv(files[i],sep="\t", header = TRUE) #read file in
    #gbifID <- read.csv(files[i],sep="\t",nrow=100000) #read file in for test
    col_names <- colnames(gbifID)
    
  }else{
    
    gbifID <- read.csv(files[i],sep="\t", header = FALSE) #read file in
    #gbifID <- read.csv(files[i],sep="\t",nrow=100000, header = FALSE) #read file in for test
    colnames(gbifID) <- col_names
  }
  
  #turn this condition on only for testing the script
  # if(i == 3){
  #   gbifID <- gbifID[c(1:23459),]
  # }
  
  #loop through blocs matching sps names to unique sps IDs
  for(j in 1:ceiling(nrow(gbifID)/10000000))
  #for(j in 1:ceiling(nrow(gbifID)/10000))
  {
    a <- gbifID[c((((j-1)*10000000)+1):(j*10000000)),]
    #a <- gbifID[c((((j-1)*10000)+1):(j*10000)),]
    b <- a[which(!is.na(a$gbifID)),] # get rid of empty rows
    c <- merge(b, sps_spsID, by = "species")
    d <- c[,-1]
    
    setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_date_speciesID")
    saveRDS(d,paste0("Temp_gbifID_spsID_",i,"_",j))
    print(paste0(i,"_",j))
  }
}

#combine parts of the job in tables up to 1 bi rows

for(i in seq_along(files))
{
  setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_date_speciesID")
  
  res <- list.files(pattern = paste0("Temp_gbifID_spsID_",i))
  res2 <- lapply(res,readRDS)
  res3 <- rbindlist(res2)
  
  #save
  setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField")
  
  saveRDS(res3,paste0("gbifID_speciesID_",i)) #save gbifID_speciesID
}






