# this script loads the columns "gbifID" and "basisOfRecords" for all the GBIF data and creates two files
# 1: a table relating each basisOfRecord combination name to a basisOfRecordID
# 2: a table containing all GBIF IDs related to the corresponding basisOfRecordID

print("Start")

library("data.table")

setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/Thematic_fields")

#list files containing the desired info
files <- list.files(pattern = "gbifID_establishmentMeans_")

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
  unic[[i]] <- unique(gbifID, by = "establishmentMeans")
  
  print(paste0("table ",i," in"))
}

#concatenate all objects in the list to get unique establishmentMeans info
unic2 <- rbindlist(unic)

#make unique list
unic3 <- unique(unic2, by = "establishmentMeans")

#create a vector transforming establishmentMeans info in integer to make unique establishmentMeanslD
unic3$establishmentMeansID <- as.integer(as.factor(unic3$establishmentMeans))

#combine establishmentMeans and establishmentMeansID in a data.frame
establishmentMeans_establishmentMeansID <- data.frame(establishmentMeans = unic3$establishmentMeans, 
                                                      establishmentMeansID = unic3$establishmentMeansID)

#save look up table
setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField")
saveRDS(establishmentMeans_establishmentMeansID,"establishmentMeans_establishmentMeansID")

#create temporay directory to save parts of the job
dir.create("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_establishmentMeans")

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
    c <- merge(b, establishmentMeans_establishmentMeansID, by = "establishmentMeans")
    d <- c[,-1]
    
    setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_establishmentMeans")
    saveRDS(d,paste0("Temp_gbifID_establishmentMeansID_",i,"_",j))
    print(paste0(i,"_",j))
  }
}

#combine parts of the job in tables up to 1 bi rows

for(i in seq_along(files))
{
  setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField/Temp_establishmentMeans")
  
  res <- list.files(pattern = paste0("Temp_gbifID_establishmentMeansID_",i))
  res2 <- lapply(res,readRDS)
  res3 <- rbindlist(res2)
  
  #save
  setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022/gbifByField")
  
  saveRDS(res3,paste0("establishmentMeansID__",i)) #save gbifID_speciesID
}