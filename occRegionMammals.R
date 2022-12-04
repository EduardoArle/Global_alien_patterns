library("plyr");library("data.table")

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/GlobalAlienPatterns/Mammals")

#read table in
table <- read.csv("Alien_mammals_GBIF_occurrences.csv")

#eliminate fossil specimens
table2 <- table[-which(table$basisOfRecord == "FOSSIL_SPECIMEN"),]

#select unique occurrences by location and time
table3 <- unique(as.data.table(table2),
                 by=c("locationID","temporalID","speciesID"))

#count observations per species per year per region
table4 <- ddply(table3,.(species,year,regAntsMammals), nrow)

saveRDS(table4,"Mammals_occurrence_region_count")