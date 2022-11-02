# this script loads the columns "gbifID" and "occurrenceStatus" for all the GBIF data and creates two files
# 1: a table relating each occurrenceStatus combination name to an occurrenceStatusID
# 2: a table containing all GBIF IDs related to the corresponding occurrenceStatusID

print("Start")

library("data.table")

setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/Thematic_fields")

#list files containing the desired info
files <- list.files(pattern = "gbifID_occurrenceStatus_")

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
  
  gbifID <- as.data.table(gbifID)
  unic[[i]] <- unique(gbifID, by = "occurrenceStatus")
  
  print(paste0("table ",i," in"))
}

#concatenate all objects in the list to get unique establishmentMeans info
unic2 <- rbindlist(unic)

#make unique list
unic3 <- unique(unic2, by = "occurrenceStatus")

#create a vector transforming establishmentMeans info in integer to make unique establishmentMeanslD
unic3$occurrenceStatusID <- as.integer(as.factor(unic3$occurrenceStatus))

#combine establishmentMeans and establishmentMeansID in a data.frame
occurrenceStatus_occurrenceStatusID <- data.frame(occurrenceStatus = unic3$occurrenceStatus, 
                                                  occurrenceStatusID = unic3$occurrenceStatusID)

#save look up table
setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField")
saveRDS(occurrenceStatus_occurrenceStatusID,"occurrenceStatus_occurrenceStatusID")

#create temporay directory to save parts of the job
dir.create("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_occurrenceStatus")

#loop through files to concatenate decimalLongitude and decimalLatitude to locationID
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
  
  #loop through blocs matching required fields to unique thematic IDs
  for(j in 1:ceiling(nrow(gbifID)/10000000))
  #for(j in 1:ceiling(nrow(gbifID)/10000))
  {
    a <- gbifID[c((((j-1)*10000000)+1):(j*10000000)),]
    #a <- gbifID[c((((j-1)*10000)+1):(j*10000)),]
    b <- a[which(!is.na(a$gbifID)),] # get rid of empty rows
    c <- merge(b, occurrenceStatus_occurrenceStatusID, by = "occurrenceStatus")
    d <- c[,-1]
    
    setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_occurrenceStatus")
    saveRDS(d,paste0("Temp_gbifID_occurrenceStatusID_",i,"_",j))
    print(paste0(i,"_",j))
  }
}

#combine parts of the job in tables up to 1 bi rows

for(i in seq_along(files))
{
  setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_occurrenceStatus")
  
  res <- list.files(pattern = paste0("Temp_gbifID_occurrenceStatusID_",i))
  res2 <- lapply(res,readRDS)
  res3 <- rbindlist(res2)
  
  #save
  setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField")
  
  saveRDS(res3,paste0("gbifID_occurrenceStatusID_",i)) #save gbifID_speciesID
}