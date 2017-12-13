library(dplyr)
spp <- read.csv("./output/gettaxaresults.csv", row.names = 1)
library(rgbif)

spp <- spp %>% filter(!is.na(id)) %>% filter(no_authors != "?")
dim(spp)
nombres <- sort(unique(droplevels(spp$no_authors)))

key <- vector(length = length(nombres))

occs <- list()
write.table(nombres, "./data/nombres.txt")
#aqui fiz a mão para tirar famílias, gëneros, sp. aff. e cf.
nombres_processed <- read.table("./data/nombres_processed.txt")
head(nombres_processed)
nombres <- sort(unique(nombres_processed$V2))

length(nombres)
#aqui deve ser 1 a 742 mas fiz por levas
for (i in c(1:742)) {
    print(nombres[i])
    key <- name_suggest(q = nombres[i], rank = 'species')$key[1]
    print(key)
    if (!is.null(key)) {
        occs[[i]] <- occ_search(
            taxonKey = key,
            limit = 100000,
            hasCoordinate = TRUE,
            basisOfRecord = "PRESERVED_SPECIMEN",
            hasGeospatialIssue = F,
            return = 'data',
            fields = "minimal"
        )
        print(dim(occs[[i]]))
        if (!is.null(dim(occs[[i]]))) {
            write.csv(
                x = data.frame(occs[[i]]),
                file = paste0("./data/occs/", nombres[i], ".csv")
            )
            cat(paste(nombres[i], "DONE", "\n"))
        }
    }
}
