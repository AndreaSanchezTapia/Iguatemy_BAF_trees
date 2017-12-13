library(dplyr)
spp <- read.csv("./output/gettaxaresults.csv", row.names = 1)
library(rgbif)

spp <- spp %>% filter(!is.na(id)) %>% filter(no_authors != "?")
dim(spp)
nombres <- sort(unique(droplevels(spp$no_authors)))

key <- vector(length = length(nombres))

occs <- list()

write.table(nombres,"./data/nombres.txt")
nombres_processed <- read.table("./data/nombres_processed.txt")
head(nombres_processed)
nombres <- sort(unique(nombres_processed$V2))

i
length(nombres)
for (i in c(701:742)) {
print(nombres[i])
	key <- name_suggest(q = nombres[i], rank = 'species')$key[1]
	print(key)
	if (!is.null(key)){
  occs[[i]] <- occ_search(taxonKey = key,
			limit = 100000,
			                       hasCoordinate = TRUE,
                       basisOfRecord = "PRESERVED_SPECIMEN",
                       hasGeospatialIssue = F,
                       return = 'data',
											 fields = "minimal")

	print(dim(occs[[i]]))
if (!is.null(dim(occs[[i]]))) {
	write.csv(x = data.frame(occs[[i]]), file = paste0("./data/occs/",nombres[i],".csv"))
cat(paste(nombres[i], "DONE","\n"))
}
}

}
list.files("./data/occs")
# gbif_all <- rbindlist(l = occs, use.names = T, fill = T, idcol = "id")
# head(gbif_all)
# dim(gbif_all)


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
