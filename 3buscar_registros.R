#setwd("/Users/Sanchez/Documents/2 Coleguinhas/Mariana/Iguatemy/")
library(dplyr)
spp <- read.csv("./output/gettaxaresults.csv", row.names = 1)
library(rgbif)
help("rgbif")
spp <- spp %>% filter(!is.na(id)) %>% filter(no_authors!= "?")
dim(spp)
nombres <- unique(droplevels(spp$no_authors))
    occs <- occ_search(nombres)
key <- vector(length= length(nombres))


occs <- list()
for (i in c(1:length(nombres)))) {
	key <- name_suggest(q = nombres[i], rank='species')$key[1]
    occs[[i]] <- occ_search(taxonKey = key, limit = 100000, hasCoordinate = TRUE, basisOfRecord = "PRESERVED_SPECIMEN", hasGeospatialIssue = F)
  write.csv(occs[[i]])
}

	help(rgbif)
     occs[[i]] <- occs[[i]][!(is.na(occs[[i]]$decimalLatitude)), ]
# }
#
# gbif_all <- rbindlist(l = occs, use.names = T, fill = T, idcol = "id")
# head(gbif_all)
# dim(gbif_all)
splist <- c('Cyanocitta stelleri', 'Junco hyemalis', 'Aix sponsa')
keys <- sapply(splist, function(x) name_suggest(x)$key[1], USE.NAMES= FALSE)
occ_data()


hasCoordinate=TRUE,
hasGeospatialIssue= F,


key <- vector(length = length(spp$id))
occs <- list()
# for(i in c(1:13)) {
#     ciata
#     occs[[i]] <- occs[[i]][!(is.na(occs[[i]]$decimalLatitude)), ]
# }
#
# gbif_all <- rbindlist(l = occs, use.names = T, fill = T, idcol = "id")
# head(gbif_all)
# dim(gbif_all)
#
# write.csv(gbif_all, "./data/rgbif/GBIFALL.csv")
	