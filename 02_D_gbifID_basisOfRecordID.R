# this script loads the columns "gbifID" and "basisOfRecords" for all the GBIF data and creates two files
# 1: a table relating each basisOfRecord combination name to a basisOfRecordID
# 2: a table containing all GBIF IDs related to the corresponding basisOfRecordID

print("Start")

library("data.table")

setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/Thematic_fields")

#list files containing the desired info
files <- list.files(pattern = "gbifID_basisOfRecord_")

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
  unic[[i]] <- unique(gbifID, by = "basisOfRecord")
  
  print(paste0("table ",i," in"))
}

#concatenate all objects in the list to get unique dates
unic2 <- rbindlist(unic)

#make unique list
unic3 <- unique(unic2, by = "basisOfRecord")

#create a vector transforming unique date in integer to make unique temporalD
unic3$basisOfRecordID <- as.integer(as.factor(unic3$basisOfRecord))

#combine decimalLongitude, decimalLatitude, and locationID in a data.frame
basisOfRecord_basisOfRecordID <- data.frame(basisOfRecord = unic3$basisOfRecord, 
                                            basisOfRecordID = unic3$basisOfRecordID)

#save look up table
setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField")
saveRDS(basisOfRecord_basisOfRecordID,"basisOfRecord_basisOfRecordID")

#create temporay directory to save parts of the job
dir.create("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_basisOfRecord")

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
    c <- merge(b, basisOfRecord_basisOfRecordID, by = "basisOfRecord")
    d <- c[,-1]
    
    setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_basisOfRecord")
    saveRDS(d,paste0("Temp_gbifID_basisOfRecordID_",i,"_",j))
    print(paste0(i,"_",j))
  }
}

#combine parts of the job in tables up to 1 bi rows

for(i in seq_along(files))
{
  setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_basisOfRecord")
  
  res <- list.files(pattern = paste0("Temp_gbifID_basisOfRecordID_",i))
  res2 <- lapply(res,readRDS)
  res3 <- rbindlist(res2)
  
  #save
  setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField")
  
  saveRDS(res3,paste0("gbifID_basisOfRecordID_",i)) #save gbifID_speciesID
}