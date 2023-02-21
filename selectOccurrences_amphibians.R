# this script uses a the function selectRecords do retrieve GBIF and related external data 
# from the relational database in the cluster

wd_list <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/GlobalAlienPatterns/Amphibians"
wd_out <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/GlobalAlienPatterns/Amphibians"

setwd(wd_list)
list <- read.csv("Final_checklist_amphibians.csv")

# create a sps_list

sps_list <- unique(list$gbifDarwinCore)

selectRecords <- function(list,sel.by="species",fields=NULL,original.data=TRUE,external.fields=NULL,
                          wd.data="/gpfs1/data/idiv_meyer/00_data/original/GBIF/27_june_2022",wd.out,
                          file="Selected_GBIF_occurrences",verbose=TRUE){
  if(verbose){
    if(is.null(fields)){
      warning(paste("\n No fields selected.\n", "The function will only return the input data and the gbifID."))
    }
    if(original.data==FALSE){
      warning(paste("\n The function will only return thematic fields IDs.",
                    "\n Set 'original.data' to TRUE to include orginal data in the output."))
    }
  }

  setwd(paste0(wd.data,"/gbifByField")) #set wd with the thematic field files from GBIF

  list_listID <- readRDS(paste0(sel.by,"_",sel.by,"ID"))
  list_listID <- list_listID[list_listID[,paste(sel.by)] %in% list,]  #select only rows corresponding to species listed in sps_list

  #loop through the files containing the 'sel.by' entries
  sel.by_files <- list.files(pattern = paste0("gbifID_",sel.by,"ID"))
  
  tab_0 <- list()
  
  for(i in 1:length(sel.by_files))
  {
    gbifID_listID <- readRDS(paste0("gbifID_",sel.by,"ID_",i)) #read table with all gbifIDs and corresponding IDs of objects in the list
    #gbifID_listID <- gbifID_listID[c(1:100000000),] #TEST DELETE
    
    gbifID_sel <- gbifID_listID[gbifID_listID[,paste0(sel.by,"ID")] %in% list_listID[,paste0(sel.by,"ID")],]  #select rows corresponding to the IDs of objects in the list
    rm(gbifID_listID)
    
    if(original.data){
      tab_0[[i]] <- merge(gbifID_sel,list_listID,by=paste0(sel.by,"ID"),sort=F)
    }else{
      tab_0[[i]] <- gbifID_sel
    }
    
    print(paste0('table_',i))
  }
  
  #rbind all tables without using rbind list. For some weird reason, the command
  #in line 41 stops woring when the data.table package is loaded...
  tab <- tab_0[[1]]
  for(i in 2:length(tab_0))
  {
    tab <- rbind(tab, tab_0[[i]])
  }
  
  print('Tables_binded')
  
  if(!is.null(fields)){
    for(i in 1:length(fields))
    {
      #loop through the files containing the 'field' entries
      field_files <- list.files(pattern = paste0("gbifID_",fields[i],"ID"))
      
      field_0 <- list()
      
      for(j in 1:length(field_files))
      {
        field_0[[j]] <- readRDS(paste0("gbifID_",fields[i],"ID_",j)) #read table with all gbifIDs and corresponding IDs of each field
        #field_0[[j]] <- field_0[[j]][c(1:100000000),]
        field_0[[j]] <- field_0[[j]][which(field_0[[j]]$gbifID %in% tab$gbifID),] #select only the rows with gbifIDs corresponding to the query
      }
      
      #rbind all tables without using rbind list. For some weird reason, the command
      #in line 76 stops working when the data.table package is loaded...
      field <- field_0[[1]]
      for(j in 2:length(field_0))
      {
        field <- rbind(field, field_0[[j]])
      }
      
      names(field)[grep("gbifID",names(field))] <- "gbifID"
      tab <- merge(tab,field,by="gbifID",sort=F)
      
      if(original.data){
        field_fieldID <- readRDS(paste0(fields[i],"_",fields[i],"ID"))
        field_fieldID <- field_fieldID[field_fieldID[,paste0(fields[i],"ID")] %in% tab[,paste0(fields[i],"ID")],]  #select rows corresponding to the IDs of objects in the list
        tab <- merge(tab,field_fieldID,by=paste0(fields[i],"ID"),sort=F)
        
        print(paste0('field_',i))
      }
    }
  }
  
  if(!is.null(external.fields)){
    setwd(paste0(wd.data,"/External_data")) #set wd with the external fields data
    for(i in 1:length(external.fields))
    {
      ext.field <- readRDS(paste0("locationID_",external.fields[i])) #read table with all locationIDs and corresponding IDs of each field
      ext.field <- ext.field[which(ext.field$locationID %in% tab$locationID),] #select only the rows with gbifIDs corresponding to the query
      tab <- merge(tab,ext.field,by="locationID",sort=F)
    }
  }

  setwd(wd.out)
  write.csv(tab,paste0(file,".csv"),row.names=FALSE)
}

selectRecords(list = sps_list, wd.out = wd_out, file="Alien_amphibians_GBIF_occurrences",
              external.fields = "regAmphibiansReptiles",
              fields = c("location","temporal","basisOfRecord",
                         "establishmentMeans","occurrenceStatus"))


