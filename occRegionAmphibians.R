library("plyr");library("data.table")

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/GlobalAlienPatterns/Amphibians")

#read table in
table <- read.csv("Alien_amphibians_GBIF_occurrences.csv")

#eliminate fossil specimens
table2 <- table[-which(table$basisOfRecord == "FOSSIL_SPECIMEN"),]

#eliminate absence entries
table3 <- table2[-which(table2$occurrenceStatus == "ABSENT"),]

#select unique occurrences by location and time
table4 <- unique(as.data.table(table3),
                 by=c("locationID","temporalID","speciesID"))

#count observations per species per year per region
table5 <- ddply(table4,.(species,year,regAmphibiansReptiles), nrow)

#save occurrences per region file
saveRDS(table5,"Amphibians_occurrence_region_count")

#save table with absence data
abs <- table2[which(table2$occurrenceStatus == "ABSENT"),]

#select unique absences by location and time
abs2 <- unique(as.data.table(abs),
                 by=c("locationID","temporalID","speciesID"))

#count observations per species per year per region
abs3 <- ddply(abs2,.(species,year,regAmphibiansReptiles), nrow)

#save absences per region file
saveRDS(abs3,"Amphibians_absence_region_count")